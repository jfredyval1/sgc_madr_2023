library(sf)
library(tidyr)
library(dplyr)
library(leaflet)
library(htmlwidgets)

# 0 Limpiar area de trabajo
rm(list = ls())
# 1. Leer datos
# Rutas
path_gpkg = '/home/jfvl/Documentos/Guajira/DRM.gpkg'
path_map = '/home/jfvl/Documentos/Guajira/docs/index.html'

# Lectura de área y puntos DRM para contexto espacial
aoi <- st_read(path_gpkg, layer = "aoi_drm")
# Drenaje doble
drenaje <- st_read(path_gpkg, layer = "drenaje_doble")
# Puntos muestroe DRM
drm_points <- st_read(path_gpkg, layer = "drm_puntos")
# MHC 2016 - Inventario
inv_2016 <- st_read(path_gpkg, layer = "inv_mhc_2016")
# Filtrar solo por Uribia
inv_2016 <- inv_2016[inv_2016$municipio == 'URIBIA',]
# Renombrar columna ce a CE (mayúscula) para consistencia
names(inv_2016)[names(inv_2016) == 'ce'] <- 'CE'
# Filtrar por muestras
drm_points<- drm_points[drm_points$tipo_dato == 'Muestra',]
# Estraer coordenadas
drm_points <- cbind(drm_points,st_coordinates(drm_points))
# Lectura de datos base puntuales
inv <- st_read(path_gpkg, layer = "inventario")
carbonatos <- st_read(path_gpkg, layer = "caracteristicas_agua", quiet = TRUE) %>% 
  st_drop_geometry()  # No tiene geometría
carbonatos <- carbonatos[!is.na(carbonatos$carbonatos),]
# Espacializar  con inv
carbonatos<- left_join(carbonatos,
                       inv[,c("ID","geom")],
                       by=c('fk_id_punto'='ID'))

valores_fq <- st_read(path_gpkg, layer = "muestreo_fq", quiet = TRUE) %>% 
  st_drop_geometry()  # No tiene geometría
# Renombrar CE por CE
valores_fq <- valores_fq %>%
  mutate(variable = ifelse(variable == "Conduct", "CE", variable))
# 2. Pivotar valores de largo a ancho (una columna por variable)
datos_wide <- valores_fq %>%
#  filter(censurado == "NO") %>%  # Filtrar censurados si es necesario
  select(fk_id_punto, variable, valor) %>%
  pivot_wider(
    names_from = variable,
    values_from = valor,
    values_fn = mean  # Si hay múltiples mediciones, promediar
  )

# 3. Join con geometría
puntos <- inv %>%
  right_join(datos_wide, by = c("ID" = "fk_id_punto"))

# 3.1 Agregar carbonatos desde caracteristicas_agua
carbonatos_simple <- carbonatos %>%
  st_drop_geometry() %>%
  select(fk_id_punto, CO3_mgL = carbonatos)

puntos <- puntos %>%
  left_join(carbonatos_simple, by = c("ID" = "fk_id_punto"))

# 4. Definir las variables a mapear
variables <- c("CE","Na_mgl","Cl_mgL","SO4_mgL","F_mgL","CO3_mgL", "As_ugl", "Co_ugl", "Pb_ugl","Cd_ugl","CE_MHC" )

# 5. Clasificar cada variable
# 5.1 Clasificación especial para CE (Rhoades et al., 1992)
if("CE" %in% names(puntos)) {
  puntos$CE_clase <- cut(
    puntos$CE,
    breaks = c(0, 700, 2000, 10000, 25000, 45000, Inf),
    labels = c("No salina", "Ligeramente salina", "Moderadamente salina",
               "Altamente salina", "Extremadamente salina", "Salmuera"),
    include.lowest = TRUE,
    right = FALSE
  )
}

# 5.1b Clasificación para CE_MHC en inv_2016 (mismo criterio que CE)
if("CE" %in% names(inv_2016)) {
  inv_2016$CE_clase <- cut(
    inv_2016$CE,
    breaks = c(0, 700, 2000, 10000, 25000, 45000, Inf),
    labels = c("No salina", "Ligeramente salina", "Moderadamente salina",
               "Altamente salina", "Extremadamente salina", "Salmuera"),
    include.lowest = TRUE,
    right = FALSE
  )
}

# 5.2 Clasificación según Resolución 2115 de 2007 para variables con límite
# Arsénico
if("As_ugl" %in% names(puntos)) {
  puntos$As_ugl_clase <- cut(
    puntos$As_ugl,
    breaks = c(0, 10, Inf),
    labels = c("Admisible", "No admisible"),
    include.lowest = TRUE,
    right = FALSE
  )
}

# Cadmio
if("Cd_ugl" %in% names(puntos)) {
  puntos$Cd_ugl_clase <- cut(
    puntos$Cd_ugl,
    breaks = c(0, 3, Inf),
    labels = c("Admisible", "No admisible"),
    include.lowest = TRUE,
    right = FALSE
  )
}
# Cloruros
if("Cl_mgL" %in% names(puntos)) {
  puntos$Cl_mgL_clase <- cut(
    puntos$Cl_mgL,
    breaks = c(0, 250, Inf),
    labels = c("Admisible", "No admisible"),
    include.lowest = TRUE,
    right = FALSE
  )
}


# Plomo
if("Pb_ugl" %in% names(puntos)) {
  puntos$Pb_ugl_clase <- cut(
    puntos$Pb_ugl,
    breaks = c(0, 10, Inf),
    labels = c("Admisible", "No admisible"),
    include.lowest = TRUE,
    right = FALSE
  )
}

# Fluoruros
if("F_mgL" %in% names(puntos)) {
  puntos$F_mgL_clase <- cut(
    puntos$F_mgL,
    breaks = c(0, 1, Inf),
    labels = c("Admisible", "No admisible"),
    include.lowest = TRUE,
    right = FALSE
  )
}

# Carbonatos
if("CO3_mgL" %in% names(puntos)) {
  puntos$CO3_mgL_clase <- cut(
    puntos$CO3_mgL,
    breaks = c(0, 200, Inf),
    labels = c("Admisible", "No admisible"),
    include.lowest = TRUE,
    right = FALSE
  )
}

# Sulfatos
if("SO4_mgL" %in% names(puntos)) {
  puntos$SO4_mgL_clase <- cut(
    puntos$SO4_mgL,
    breaks = c(0, 250, Inf),
    labels = c("Admisible", "No admisible"),
    include.lowest = TRUE,
    right = FALSE
  )
}

# Cobalto (Decreto 1076 de 2015 - uso agrícola)
if("Co_ugl" %in% names(puntos)) {
  puntos$Co_ugl_clase <- cut(
    puntos$Co_ugl,
    breaks = c(0, 50, Inf),
    labels = c("Admisible", "No admisible"),
    include.lowest = TRUE,
    right = FALSE
  )
}

# 5.3 Clasificación por quantiles para otras variables (Sodio)
# Guardar los rangos de cuantiles para la leyenda
cuantiles_Na <- NULL
for(var in c("Na_mgl")) {
  if(var %in% names(puntos)) {
    # Calcular cuantiles
    breaks_Na <- quantile(puntos[[var]], probs = seq(0, 1, 0.2), na.rm = TRUE)
    cuantiles_Na <- breaks_Na

    puntos[[paste0(var, "_clase")]] <- cut(
      puntos[[var]],
      breaks = breaks_Na,
      labels = c("Cuantil 1", "Cuantil 2", "Cuantil 3", "Cuantil 4", "Cuantil 5"),
      include.lowest = TRUE,
      right = FALSE
    )
  }
}

# 6. Paletas de colores
# Paleta para CE (salinidad) - incluir todas las 6 clases posibles
colores_salinidad <- c('#00441b', '#a6dba0', '#ffffbf', '#9970ab', '#762a83', '#4d004b')
pal_salinidad <- colorFactor(palette = colores_salinidad,
                             levels = c("No salina", "Ligeramente salina", "Moderadamente salina",
                                       "Altamente salina", "Extremadamente salina", "Salmuera"))

# Paleta para variables con límite de admisibilidad (Resolución 2115 de 2007)
colores_admisibilidad <- c('#4d9221', '#d7191c')
pal_admisibilidad <- colorFactor(palette = colores_admisibilidad,
                                 levels = c("Admisible", "No admisible"))

# Paleta para otras variables (quantiles)
colores_cuantiles <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
pal_cuantiles <- colorFactor(palette = colores_cuantiles,
                             levels = c("Cuantil 1", "Cuantil 2", "Cuantil 3", "Cuantil 4", "Cuantil 5"))
# Cargar simbolos por DRM
triangleIcon <- makeIcon(
  iconUrl = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='20' height='20' viewBox='0 0 20 20'%3E%3Cpolygon points='10,2 18,18 2,18' fill='black'/%3E%3C/svg%3E",
  iconWidth = 10,
  iconHeight = 10,
  iconAnchorX = 10,
  iconAnchorY = 10
)

# 7. Crear mapa base
mapa <- leaflet(puntos) %>%
  addProviderTiles(providers$Stadia.StamenTerrain) |> 
  addMarkers(lng = drm_points$X, 
             lat = drm_points$Y, 
             icon = triangleIcon)|>  # o triangleIcon
  addPolygons(data = aoi,color = 'red',fill = F, weight = 2) |> # AOI DRM
  addPolygons(data = drenaje,color = 'blue',fill = T, weight = 2) |> # Dreajes Dobles
  setView(lng = mean(st_coordinates(puntos)[,1], na.rm = TRUE), 
          lat = mean(st_coordinates(puntos)[,2], na.rm = TRUE), 
          zoom = 10)

# 8. Añadir capas por variable
for(var in variables) {
  # Caso especial para CE_MHC que usa inv_2016
  if(var == "CE_MHC" && "CE" %in% names(inv_2016)) {
    # Calcular radio proporcional para inv_2016
    valores_mhc <- inv_2016$CE
    radios_mhc <- scales::rescale(valores_mhc, to = c(5, 20),
                                  from = range(valores_mhc, na.rm = TRUE))
    radios_mhc[is.na(radios_mhc)] <- 5

    mapa <- mapa %>%
      addCircleMarkers(
        data = inv_2016,
        group = "CE_MHC",
        radius = radios_mhc,
        color = "#000",
        weight = 1,
        fillColor = ~pal_salinidad(inv_2016$CE_clase),
        fillOpacity = 0.7,
        popup = ~paste0(
          "<b>ID:</b> ", id_sgc, "<br>",
          "<b>Municipio:</b> ", municipio, "<br>",
          "<b>Tipo:</b> ", tipo_punto, "<br>",
          "<b>Profundidad:</b> ", profund, "<br>",
          "<b>Fecha:</b> ", fecha, "<br>",
          "<b>CE:</b> ", round(CE, 2), " μS/cm<br>",
          "<b>Clase:</b> ", CE_clase
        )
      )
  } else if(var %in% names(puntos)) {

    # Calcular radio proporcional (normalizar entre 5 y 20)
    valores <- puntos[[var]]
    radios <- scales::rescale(valores, to = c(5, 20),
                              from = range(valores, na.rm = TRUE))

    # Reemplazar NA con valor mínimo
    radios[is.na(radios)] <- 5

    # Seleccionar paleta según la variable
    if(var == "CE") {
      pal_actual <- pal_salinidad
    } else if(var %in% c("Cl_mgL","As_ugl", "Cd_ugl", "Pb_ugl", "F_mgL", "CO3_mgL", "SO4_mgL", "Co_ugl")) {
      pal_actual <- pal_admisibilidad
    } else {
      pal_actual <- pal_cuantiles
    }

    mapa <- mapa %>%
      addCircleMarkers(
        data = puntos,
        group = var,
        radius = radios,
        color = "#000",
        weight = 1,
        fillColor = ~pal_actual(puntos[[paste0(var, "_clase")]]),
        fillOpacity = 0.7,
        popup = ~paste0(
          "<b>ID:</b> ", ID, "<br>",
          "<b>Nombre:</b> ", Nombre_Pun, "<br>",
          "<b>Tipo:</b> ", Tipo_P, "<br>",
          "<b>Profundidad (m):</b> ", Prof_punto, "<br>",
          "<b>", var, ":</b> ", round(puntos[[var]], 2), "<br>",
          "<b>Clase:</b> ", puntos[[paste0(var, "_clase")]]
        )
      )
  }
}

# 9. Control de capas (solo una visible)
mapa <- mapa %>%
  addLayersControl(
    baseGroups = variables,
    options = layersControlOptions(collapsed = T)
  ) %>%
  hideGroup(variables[-1])

# 10. Calcular rangos de valores para cada variable
rangos_vars <- list()
for(var in variables) {
  # Caso especial para CE_MHC
  if(var == "CE_MHC" && "CE" %in% names(inv_2016)) {
    valores <- inv_2016$CE
    valores <- valores[!is.na(valores)]
    if(length(valores) > 0) {
      rangos_vars[["CE_MHC"]] <- list(
        min = round(min(valores), 2),
        max = round(max(valores), 2),
        unit = "μS/cm"
      )
    }
  } else if(var %in% names(puntos)) {
    valores <- puntos[[var]]
    valores <- valores[!is.na(valores)]
    if(length(valores) > 0) {
      rangos_vars[[var]] <- list(
        min = round(min(valores), 2),
        max = round(max(valores), 2),
        unit = case_when(
          var == "CE" ~ "μS/cm",
          var %in% c("Cl_mgL","Na_mgl","SO4_mgL","F_mgL","CO3_mgL" ) ~ "mg/L",
          var %in% c("As_ugl" ,"Co_ugl","Pb_ugl","Cd_ugl") ~ "μg/L",
          TRUE ~ ""
        )
      )
    }
  }
}

# Convertir a JSON para JavaScript
rangos_json <- jsonlite::toJSON(rangos_vars, auto_unbox = TRUE)

# Convertir cuantiles de Sodio a JSON
if(!is.null(cuantiles_Na)) {
  cuantiles_Na_json <- jsonlite::toJSON(round(cuantiles_Na, 1), auto_unbox = FALSE)
} else {
  cuantiles_Na_json <- "null"
}

# 11. Agregar control HTML para la leyenda dinámica y título
library(htmltools)

# JavaScript para título y leyenda dinámica
js_code <- HTML(paste0('
<script>
  // Esperar a que el mapa se cargue
  setTimeout(function() {
    var map = HTMLWidgets.find(".leaflet").getMap();

    // Crear control de leyenda
    var legend = L.control({position: "bottomright"});

    legend.onAdd = function(map) {
      var div = L.DomUtil.create("div", "info legend");
      div.style.backgroundColor = "white";
      div.style.padding = "10px";
      div.style.borderRadius = "5px";
      div.style.boxShadow = "0 0 15px rgba(0,0,0,0.2)";
      return div;
    };

    legend.addTo(map);

    // Definir información para cada variable
    var varInfo = {
      "CE": {
        title: "Conductividad eléctrica (μS·cm⁻¹)",
        categories: [
          {color: "#00441b", label: "No salina", range: "<700"},
          {color: "#a6dba0", label: "Ligeramente salina", range: "700-2000"},
          {color: "#ffffbf", label: "Moderadamente salina", range: "2000-10000"},
          {color: "#9970ab", label: "Altamente salina", range: "10000-25000"},
          {color: "#762a83", label: "Extremadamente salina", range: "25000-45000"},
          {color: "#4d004b", label: "Salmuera", range: "≥45000"}
        ],
        footer: "Rhoades et al., 1992"
      },
      "CE_MHC": {
        title: "Conductividad eléctrica MHC 2016 (μS·cm⁻¹)",
        categories: [
          {color: "#00441b", label: "No salina", range: "<700"},
          {color: "#a6dba0", label: "Ligeramente salina", range: "700-2000"},
          {color: "#ffffbf", label: "Moderadamente salina", range: "2000-10000"},
          {color: "#9970ab", label: "Altamente salina", range: "10000-25000"},
          {color: "#762a83", label: "Extremadamente salina", range: "25000-45000"},
          {color: "#4d004b", label: "Salmuera", range: "≥45000"}
        ],
        footer: "Rhoades et al., 1992"
      },
      "Cl_mgL": {
        title: "Cloruros (mg/L)",
        categories: [
          {color: "#4d9221", label: "Admisible", range: "<250"},
          {color: "#d7191c", label: "No admisible", range: "≥250"}
        ],
        footer: "Resolución 2115 de 2007"
      },
      "Na_mgl": {
        title: "Sodio (mg/L)",
        categories: [],  // Se llenará dinámicamente
        footer: "Clasificación por cuantiles",
        useQuantiles: true
      },
      "SO4_mgL": {
        title: "Sulfatos (mg/L)",
        categories: [
          {color: "#4d9221", label: "Admisible", range: "<250"},
          {color: "#d7191c", label: "No admisible", range: "≥250"}
        ],
        footer: "Resolución 2115 de 2007"
      },
      "F_mgL": {
        title: "Fluoruros (mg/L)",
        categories: [
          {color: "#4d9221", label: "Admisible", range: "<1"},
          {color: "#d7191c", label: "No admisible", range: "≥1"}
        ],
        footer: "Resolución 2115 de 2007"
      },
      "CO3_mgL": {
        title: "Carbonatos (mg/L)",
        categories: [
          {color: "#4d9221", label: "Admisible", range: "<200"},
          {color: "#d7191c", label: "No admisible", range: "≥200"}
        ],
        footer: "Resolución 2115 de 2007"
      },
      "As_ugl": {
        title: "Arsénico (μg/L)",
        categories: [
          {color: "#4d9221", label: "Admisible", range: "<10"},
          {color: "#d7191c", label: "No admisible", range: "≥10"}
        ],
        footer: "Resolución 2115 de 2007"
      },
      "Co_ugl": {
        title: "Cobalto (μg/L)",
        categories: [
          {color: "#4d9221", label: "Admisible", range: "<50"},
          {color: "#d7191c", label: "No admisible", range: "≥50"}
        ],
        footer: "Decreto 1076 de 2015 (Uso agrícola)"
      },
      "Cd_ugl": {
        title: "Cadmio (μg/L)",
        categories: [
          {color: "#4d9221", label: "Admisible", range: "<3"},
          {color: "#d7191c", label: "No admisible", range: "≥3"}
        ],
        footer: "Resolución 2115 de 2007"
      },
      "Pb_ugl": {
        title: "Plomo (μg/L)",
        categories: [
          {color: "#4d9221", label: "Admisible", range: "<10"},
          {color: "#d7191c", label: "No admisible", range: "≥10"}
        ],
        footer: "Resolución 2115 de 2007"
      }
    };

    // Cuantiles de Sodio calculados desde R
    var cuantilesNa = ', cuantiles_Na_json, ';

    // Función para actualizar la leyenda
    function updateLegend(varName) {
      var div = document.querySelector(".legend");
      var info = varInfo[varName];

      if(!info) {
        div.innerHTML = "<h4>Variable no encontrada</h4>";
        return;
      }

      var html = "<h4 style=\\"margin: 0 0 5px; font-size: 14px;\\">" + info.title + "</h4>";

      // Si es Sodio, construir categorías dinámicamente con los cuantiles
      if(info.useQuantiles && cuantilesNa && cuantilesNa.length === 6) {
        var colores = ["#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"];
        for(var i = 0; i < 5; i++) {
          var min = cuantilesNa[i];
          var max = cuantilesNa[i+1];
          html += "<div style=\\"margin: 2px 0;\\"><i style=\\"background:" + colores[i] +
                  "; width: 18px; height: 18px; float: left; margin-right: 8px; opacity: 0.7;\\"></i>" +
                  "Cuantil " + (i+1) + " (" + min + "-" + max + ")</div>";
        }
      } else {
        // Para otras variables, usar las categorías definidas
        info.categories.forEach(function(cat) {
          var rangeText = cat.range ? " (" + cat.range + ")" : "";
          html += "<div style=\\"margin: 2px 0;\\"><i style=\\"background:" + cat.color +
                  "; width: 18px; height: 18px; float: left; margin-right: 8px; opacity: 0.7;\\"></i>" +
                  cat.label + rangeText + "</div>";
        });
      }

      html += "<div style=\\"clear: both; margin-top: 5px;\\"><small>" + info.footer + "</small></div>";
      div.innerHTML = html;
    }
    

    // Escuchar cambios de capa
    map.on("baselayerchange", function(e) {
      updateLegend(e.name);
    });

    // Inicializar con Coductividad eléctrica
    updateLegend("CE");

    // Crear leyenda de tamaños (símbolos proporcionales)
    var sizeLegend = L.control({position: "bottomleft"});

    sizeLegend.onAdd = function(map) {
      var div = L.DomUtil.create("div", "info size-legend");
      div.style.backgroundColor = "white";
      div.style.padding = "10px";
      div.style.borderRadius = "5px";
      div.style.boxShadow = "0 0 15px rgba(0,0,0,0.2)";
      return div;
    };

    sizeLegend.addTo(map);

    // Rangos de valores para cada variable (calculados desde R)
    var varRanges = ', rangos_json, ';

    // Función para actualizar la leyenda de tamaños
    function updateSizeLegend(varName) {
      var div = document.querySelector(".size-legend");
      var range = varRanges[varName];

      if(!range) {
        div.innerHTML = "<h4 style=\\"margin: 0 0 5px; font-size: 14px;\\">Tamaño del símbolo</h4>" +
          "<div style=\\"text-align: center;\\"><small>Proporcional al valor</small></div>";
        return;
      }

      var min = range.min;
      var max = range.max;
      var mid = (min + max) / 2;
      var unit = range.unit;

      div.innerHTML = "<h4 style=\\"margin: 0 0 5px; font-size: 14px;\\">Tamaño del símbolo</h4>" +
        "<div style=\\"text-align: center;\\"><small>Proporcional al valor</small></div>" +
        "<div style=\\"margin-top: 8px;\\"><svg width=\\"150\\" height=\\"110\\">" +
        "<circle cx=\\"30\\" cy=\\"100\\" r=\\"5\\" fill=\\"#666\\" opacity=\\"0.5\\"/>" +
        "<text x=\\"40\\" y=\\"103\\" font-size=\\"9\\">" + min.toFixed(1) + " " + unit + "</text>" +
        "<circle cx=\\"30\\" cy=\\"65\\" r=\\"12\\" fill=\\"#666\\" opacity=\\"0.5\\"/>" +
        "<text x=\\"47\\" y=\\"68\\" font-size=\\"9\\">" + mid.toFixed(0) + " " + unit + "</text>" +
        "<circle cx=\\"30\\" cy=\\"25\\" r=\\"20\\" fill=\\"#666\\" opacity=\\"0.5\\"/>" +
        "<text x=\\"55\\" y=\\"28\\" font-size=\\"9\\">" + max.toFixed(0) + " " + unit + "</text>" +
        "</svg></div>";
    }

    // Escuchar cambios de capa para actualizar leyenda de tamaños
    map.on("baselayerchange", function(e) {
      updateSizeLegend(e.name);
    });

    // Inicializar leyenda de tamaños con Conductividad eléctrica
    updateSizeLegend("CE");

  }, 1000);
</script>
'))
mapa <- htmlwidgets::prependContent(mapa, js_code)
# 12. Guardar
saveWidget(mapa, path_map, selfcontained = TRUE)
#Ver en navegador
browseURL(path_map)