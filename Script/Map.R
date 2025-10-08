library(sf)
library(dplyr)
library(tidyr)
library(leaflet)
library(htmlwidgets)

# 0 Limpiar area de trabajo
rm(list = ls())
# 1. Leer datos
# Rutas
path_gpkg = '/home/jfvl/Documentos/Guajira/DRM.gpkg'
path_map = '/home/jfvl/Documentos/Guajira/html/mapa_drm.html'

inv <- st_read(path_gpkg, layer = "inventario")
valores_fq <- st_read(path_gpkg, layer = "muestreo_fq", quiet = TRUE) %>% 
  st_drop_geometry()  # No tiene geometría

# 2. Pivotar valores de largo a ancho (una columna por variable)
datos_wide <- valores_fq %>%
  filter(censurado == "NO") %>%  # Filtrar censurados si es necesario
  select(fk_id_punto, variable, valor) %>%
  pivot_wider(
    names_from = variable,
    values_from = valor,
    values_fn = mean  # Si hay múltiples mediciones, promediar
  )

# 3. Join con geometría
puntos <- inv %>%
  left_join(datos_wide, by = c("ID" = "fk_id_punto"))

# 4. Definir las variables a mapear
variables <- c("Conduct", "As_ugl", "Cd_ugl", "Co_ugl", "Pb_ugl", "F_mgL", "SO4_mgL", "Na_mgl")

# 5. Clasificar cada variable
# 5.1 Clasificación especial para Conductividad (Rhoades et al., 1992)
if("Conduct" %in% names(puntos)) {
  puntos$Conduct_clase <- cut(
    puntos$Conduct,
    breaks = c(0, 700, 2000, 10000, 25000, 45000, Inf),
    labels = c("No salina", "Ligeramente salina", "Moderadamente salina",
               "Altamente salina", "Extremadamente salina", "Salmuera"),
    include.lowest = TRUE,
    right = FALSE
  )
}

# 5.2 Clasificación por quantiles para otras variables
for(var in variables) {
  if(var %in% names(puntos) && var != "Conduct") {
    puntos[[paste0(var, "_clase")]] <- cut(
      puntos[[var]],
      breaks = quantile(puntos[[var]], probs = seq(0, 1, 0.2), na.rm = TRUE),
      labels = c("Muy Bajo", "Bajo", "Medio", "Alto", "Muy Alto"),
      include.lowest = TRUE,
      right = FALSE
    )
  }
}

# 6. Paletas de colores
# Paleta para conductividad (salinidad) - orden invertido para que mejor calidad sea verde
colores_salinidad <- c('#4d9221', '#a1d76a', '#e6f5d0', '#fde0ef', '#e9a3c9', '#c51b7d')
pal_salinidad <- colorFactor(palette = colores_salinidad,
                             levels = c("No salina", "Ligeramente salina", "Moderadamente salina",
                                       "Altamente salina", "Extremadamente salina", "Salmuera"))

# Paleta para otras variables (quantiles)
colores <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
pal <- colorFactor(palette = colores,
                   levels = c("Muy Bajo", "Bajo", "Medio", "Alto", "Muy Alto"))

# 7. Crear mapa base
mapa <- leaflet(puntos) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = mean(st_coordinates(puntos)[,1], na.rm = TRUE), 
          lat = mean(st_coordinates(puntos)[,2], na.rm = TRUE), 
          zoom = 10)

# 8. Añadir capas por variable
for(var in variables) {
  if(var %in% names(puntos)) {
    
    # Calcular radio proporcional (normalizar entre 5 y 20)
    valores <- puntos[[var]]
    radios <- scales::rescale(valores, to = c(5, 20), 
                              from = range(valores, na.rm = TRUE))
    
    # Reemplazar NA con valor mínimo
    radios[is.na(radios)] <- 5
    
    # Seleccionar paleta según la variable
    pal_actual <- if(var == "Conduct") pal_salinidad else pal

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
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(variables[-1])

# 10. Agregar control HTML para la leyenda dinámica
library(htmltools)

# JavaScript para leyenda dinámica
js_code <- HTML('
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
      "Conduct": {
        title: "Conductividad (μS·cm⁻¹)",
        categories: [
          {color: "#4d9221", label: "No salina", range: "<700"},
          {color: "#a1d76a", label: "Ligeramente salina", range: "700-2000"},
          {color: "#e6f5d0", label: "Moderadamente salina", range: "2000-10000"},
          {color: "#fde0ef", label: "Altamente salina", range: "10000-25000"},
          {color: "#e9a3c9", label: "Extremadamente salina", range: "25000-45000"},
          {color: "#c51b7d", label: "Salmuera", range: ">45000"}
        ],
        footer: "Rhoades et al., 1992"
      },
      "As_ugl": {
        title: "Arsénico (μg/L)",
        categories: [
          {color: "#2c7bb6", label: "Muy Bajo"},
          {color: "#abd9e9", label: "Bajo"},
          {color: "#ffffbf", label: "Medio"},
          {color: "#fdae61", label: "Alto"},
          {color: "#d7191c", label: "Muy Alto"}
        ],
        footer: "Clasificación por cuantiles"
      },
      "Cd_ugl": {
        title: "Cadmio (μg/L)",
        categories: [
          {color: "#2c7bb6", label: "Muy Bajo"},
          {color: "#abd9e9", label: "Bajo"},
          {color: "#ffffbf", label: "Medio"},
          {color: "#fdae61", label: "Alto"},
          {color: "#d7191c", label: "Muy Alto"}
        ],
        footer: "Clasificación por cuantiles"
      },
      "Co_ugl": {
        title: "Cobalto (μg/L)",
        categories: [
          {color: "#2c7bb6", label: "Muy Bajo"},
          {color: "#abd9e9", label: "Bajo"},
          {color: "#ffffbf", label: "Medio"},
          {color: "#fdae61", label: "Alto"},
          {color: "#d7191c", label: "Muy Alto"}
        ],
        footer: "Clasificación por cuantiles"
      },
      "Pb_ugl": {
        title: "Plomo (μg/L)",
        categories: [
          {color: "#2c7bb6", label: "Muy Bajo"},
          {color: "#abd9e9", label: "Bajo"},
          {color: "#ffffbf", label: "Medio"},
          {color: "#fdae61", label: "Alto"},
          {color: "#d7191c", label: "Muy Alto"}
        ],
        footer: "Clasificación por cuantiles"
      },
      "F_mgL": {
        title: "Fluoruros (mg/L)",
        categories: [
          {color: "#2c7bb6", label: "Muy Bajo"},
          {color: "#abd9e9", label: "Bajo"},
          {color: "#ffffbf", label: "Medio"},
          {color: "#fdae61", label: "Alto"},
          {color: "#d7191c", label: "Muy Alto"}
        ],
        footer: "Clasificación por cuantiles"
      },
      "SO4_mgL": {
        title: "Sulfatos (mg/L)",
        categories: [
          {color: "#2c7bb6", label: "Muy Bajo"},
          {color: "#abd9e9", label: "Bajo"},
          {color: "#ffffbf", label: "Medio"},
          {color: "#fdae61", label: "Alto"},
          {color: "#d7191c", label: "Muy Alto"}
        ],
        footer: "Clasificación por cuantiles"
      },
      "Na_mgl": {
        title: "Sodio (mg/L)",
        categories: [
          {color: "#2c7bb6", label: "Muy Bajo"},
          {color: "#abd9e9", label: "Bajo"},
          {color: "#ffffbf", label: "Medio"},
          {color: "#fdae61", label: "Alto"},
          {color: "#d7191c", label: "Muy Alto"}
        ],
        footer: "Clasificación por cuantiles"
      }
    };

    // Función para actualizar la leyenda
    function updateLegend(varName) {
      var div = document.querySelector(".legend");
      var info = varInfo[varName];

      if(!info) {
        div.innerHTML = "<h4>Variable no encontrada</h4>";
        return;
      }

      var html = "<h4 style=\\"margin: 0 0 5px; font-size: 14px;\\">" + info.title + "</h4>";

      info.categories.forEach(function(cat) {
        var rangeText = cat.range ? " (" + cat.range + ")" : "";
        html += "<div style=\\"margin: 2px 0;\\"><i style=\\"background:" + cat.color +
                "; width: 18px; height: 18px; float: left; margin-right: 8px; opacity: 0.7;\\"></i>" +
                cat.label + rangeText + "</div>";
      });

      html += "<div style=\\"clear: both; margin-top: 5px;\\"><small>" + info.footer + "</small></div>";
      div.innerHTML = html;
    }

    // Escuchar cambios de capa
    map.on("baselayerchange", function(e) {
      updateLegend(e.name);
    });

    // Inicializar con Conductividad
    updateLegend("Conduct");

  }, 1000);
</script>
')

mapa <- htmlwidgets::prependContent(mapa, js_code)

# 11. Guardar
saveWidget(mapa, path_map, selfcontained = TRUE)

# Ver en navegador
browseURL(path_map)
