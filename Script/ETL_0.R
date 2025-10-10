library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(stringr)
library(lubridate)
# Limpiar área de trabajo
rm(list = ls())
drm <- read_excel("/home/jfvl/Documentos/Guajira/XLSX/Muestreo_Guajira_DRM.xls")
#Filtrar campos
drm<-drm[,c("ID","Tipo_dato","Longitud","Latitud")]
# Espacializar
drm<-st_as_sf(drm,coords = c("Longitud","Latitud"),crs=4686)
names(drm)<-c('id_drm','tipo_dato','geometry')
#Guardar en GPKG: Todo en MAGNA-SIRGAS 4686
#st_write(drm,'Documentos/Guajira/DRM.gpkg',layer = 'drm_puntos')
#
# Marco geológico
#
ug <- st_read('/home/jfvl/Documentos/Guajira/Resultados_Consolidados2023/UG_250K.shp')
# Proyectar y limpiar
ug <- st_transform(ug,st_crs(4326))
ug <- st_transform(ug,st_crs(4686))
# Limpiar campos
ug<-ug[,c("COD","class_hidr","geometry")]
names(ug)<-c('ug_cod','class_hg','geometry')
#Guardar
#st_write(ug,'Documentos/Guajira/DRM.gpkg',layer = 'ug_250k')
#

#
# Diagnostico
#
diagnstco<-st_read('/home/jfvl/Documentos/Guajira/Resultados_Consolidados2023/Diagnostico.shp')
# Filtrar campos
diagnstco<-diagnstco[,c("ID","Tipo_P","Fecha","Nombre_Pun","Sitio","Usos_agua","Prof_punto","pH","Cond_μS_c","Alcalini_1","Problemas_","geometry")]
diagnstco$Alcalini_1[diagnstco$Alcalini_1 == 0]<- NA
#
# Separar ubicciones de resultados por fecha
#
captaciones <- diagnstco[,c("ID","Tipo_P","Nombre_Pun","Sitio","Usos_agua","Prof_punto","geometry")]
# Caracteristicas
caracteristicas <-st_drop_geometry(diagnstco)[,c("ID","Tipo_P","Fecha","pH","Cond_μS_c","Alcalini_1","Problemas_")]
# Eliminar tiplas sin datos
caracteristicas <- caracteristicas[!is.na(caracteristicas$Cond_μS_c),]
# Renombrar
names(caracteristicas)<-c('fk_id_punto','tipo_punto','fecha','ph','ce','carbonatos','observaciones')
#
# Muestreo FQ
#
muestreo_fq <- read_csv("/home/jfvl/Documentos/Guajira/Resultados_Consolidados2023/muestreo_fq.csv")
# Filtrar
muestreo_fq<-muestreo_fq[,c("Seleccione ID del punto","Tipo de punto","Fecha","pH","Conductividad eléctrica en μS/cm","Observaciones del muestro de físico-químico")]

# Verificar conexion con puntos
muestreo_fq<- left_join(st_drop_geometry(captaciones)[,c("ID","Tipo_P")],
                         muestreo_fq,
                         by=c("ID"= "Seleccione ID del punto"))

# ELiminar tuplas sin datos
muestreo_fq<-muestreo_fq[!is.na(muestreo_fq$`Conductividad eléctrica en μS/cm`),]
# Renombrar
muestreo_fq<-muestreo_fq[,c("ID","Tipo de punto","Fecha","pH","Conductividad eléctrica en μS/cm","Observaciones del muestro de físico-químico")]
names(muestreo_fq)<-c('fk_id_punto','tipo_punto','fecha','ph','ce','observaciones')
# Identificar tanques
# Cambiar tipo_punto a 'tanque' cuando observaciones contiene la palabra "tanque"
muestreo_fq$tipo_punto[grepl("tanque", muestreo_fq$observaciones, ignore.case = TRUE)] <- "tanque"
muestreo_fq$tipo_punto[grepl("llave", muestreo_fq$observaciones, ignore.case = TRUE)] <- "tanque"
muestreo_fq$tipo_punto[grepl("almacenamiento", muestreo_fq$observaciones, ignore.case = TRUE)] <- "tanque"
# Añadir columna de carbonatos

# Separar por momento
caracteristicas$visita<-1
muestreo_fq$visita<-2
# Estandar fechas
caracteristicas$fecha<-dmy(caracteristicas$fecha)
caracteristicas$fecha<-ymd(caracteristicas$fecha)
muestreo_fq$fecha<-ymd(format(mdy_hms(muestreo_fq$fecha),"%Y/%m/%d"))
# Incluir columnas vacia de carbonatos
muestreo_fq$carbonatos <- NA
# Reorndenas nuevamente
muestreo_fq<-muestreo_fq[,c("fk_id_punto","tipo_punto","fecha","ph","ce","carbonatos","observaciones","visita")]
# unificar
caracteristicas_agua<-rbind(caracteristicas,muestreo_fq)
# Forzar variales a numericas
caracteristicas_agua$ph<-as.numeric(caracteristicas_agua$ph)
caracteristicas_agua$ce<-as.numeric(caracteristicas_agua$ce)

# Valores de ce superioes a 50000 divir por 1000
caracteristicas_agua[caracteristicas_agua$ce > 40000,]
# Corregir
# Forma correcta - sin la coma
caracteristicas_agua$ce[caracteristicas_agua$ce > 40000] <- caracteristicas_agua$ce[caracteristicas_agua$ce > 40000] / 1000
summary(caracteristicas_agua$ce)
#
# Microbiologicos
#
micro<-st_read('/home/jfvl/Documentos/Guajira/Resultados_Consolidados2023/Consolidado_FQ_Microbiologico.shp')
# Filtrar
micro = st_drop_geometry(micro)[,c("ID","fecha","Tipo_P","Conduct","As_ugl",'Cl_mgL',"Cd_ugl","Co_ugl","Pb_ugl","F_mgL","SO4_mgL","Na_mgl",'HCO3_mgl')]
# Transpoer tabla
micro <- micro %>%
  mutate(across(c(Conduct, As_ugl,Cl_mgL, Cd_ugl, Co_ugl, Pb_ugl, F_mgL, SO4_mgL, Na_mgl,HCO3_mgl), 
                as.character)) %>%
  pivot_longer(
    cols = c(Conduct, As_ugl,Cl_mgL,Cd_ugl, Co_ugl, Pb_ugl, F_mgL, SO4_mgL, Na_mgl,HCO3_mgl),
    names_to = "variable",
    values_to = "valor_texto"
  ) %>%
  mutate(
    censurado = if_else(str_detect(valor_texto, "<"), "SI", "NO"),
    valor = str_remove(valor_texto, "<") %>% 
      str_replace(",", ".") %>% 
      as.numeric()
  ) %>%
  select(ID, fecha, Tipo_P, variable, valor, censurado)
# Verificar valores que se unen con captaciones
micro <- inner_join(st_drop_geometry(captaciones)[,c("ID","Tipo_P")],
                  micro,
                  by=c("ID"= "ID","Tipo_P"="Tipo_P"))

#
# Corregir valores execivos de CE por CE calculada
micro$valor[micro$ID == 'I4088' & micro$variable == 'Conduct'] <- 18383

# Resumen por varible
micro_resumen <- micro %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    n_censurados = sum(censurado == "SI"),
    min = min(valor, na.rm = TRUE),
    max = max(valor, na.rm = TRUE),
    media = round(mean(valor, na.rm = TRUE),2),
    mediana = round(median(valor, na.rm = TRUE),2 ),
    sd = sd(valor, na.rm = TRUE)
  )
# Estandarizar fechas
micro$fecha = ymd(micro$fecha)
# Estandarizar nombres
names(micro) = c("fk_id_punto","tipo_punto","fecha","variable","valor","censurado")
# Escribir puntos y tabla de caracteristicas
#st_write(captaciones,'Documentos/Guajira/DRM.gpkg',layer='inventario')
# Escribir tablas
#st_write(caracteristicas_agua,'/home/jfvl/Documentos/Guajira/DRM.gpkg',layer='caracteristicas_agua')
# Escribir 
#st_write(micro,'/home/jfvl/Documentos/Guajira/DRM.gpkg',layer='muestreo_fq')
#
# MHC 2016
#
inv <- st_read('/home/jfvl/Documentos/Guajira/Resultados_Consolidados2023/MHC2016/inventario_Guajira_2016.shp')
# Proyectar y limpiar
inv <- st_transform(inv,st_crs(4326))
inv <- st_transform(inv,st_crs(4686))
# Contruir Id  unico
inv$id_sgc<-paste(inv$F25000,inv$consecutiv,sep = '')
# Filtrar campos utiles
inv<-inv[,c("id_sgc","Fecha","captacion","Profundi_1","Profundi_2","Fecha1","Fecha2","Cond","Cond1","Observac_1","Observac_2","Observacio")]
# Convertir 0 en NA
inv$Cond[inv$Cond == 0] <- NA
inv$Cond1[inv$Cond1 == 0] <- NA

# Crear columna unica de conductividad y profundidad
inv$prof_unic<-ifelse(is.na(inv$Profundi_1),inv$Profundi_2,inv$Profundi_1)
inv$ce_unic<-ifelse(is.na(inv$Cond),inv$Cond1,inv$Cond)
# Filtrar solo campor utiles

# Conservar solo registro con conductividad y fecha
inv<-inv[!is.na(inv$ce_unic) & !is.na(inv$Fecha),]
inv<-inv[,c("id_sgc","Fecha","captacion","prof_unic","ce_unic")]
# Renombrar
names(inv)<-c('id_sgc','fecha','tipo_punto','profund','ce','geometry')
# Estandarizar fechas
inv$fecha <- ymd(inv$fecha)
#
# Guardar datos en GPKG
# 
#st_write(inv,'/home/jfvl/Documentos/Guajira/DRM.gpkg',layer='inv_mhc_2016')
