rm(list=ls())

setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Censo 2002/")

# Carga de paquetes requeridos
if (!require(devtools)) install.packages("devtools")
if (!require(foreign)) install.packages("foreign")
if (!require(data.table)) install.packages("data.table")
if (!require(maptools)) install.packages("maptools")
if (!require(rgdal)) install.packages("rgdal", dependencies = TRUE)
if (!require(stringi)) install.packages("stringi")
if (!require(dplyr)) install.packages("dplyr")
if (!require(ggmap)) install.packages("ggmap")

require(parallel)

datos_spss = list.files(pattern = ".sav$")

size = sapply(datos_spss, object.size)
datos = gsub(".sav","",datos_spss)


portafolio =read.spss("portafolio.sav", use.value.labels = FALSE, to.data.frame = TRUE)

portafolio = portafolio %>% filter(Comuna==8401, Area==1)

manzanas = readOGR("manzanaine/manzanaine.shp")

manzanas = manzanas %>% subset(Cod_Comuna=="08401")

