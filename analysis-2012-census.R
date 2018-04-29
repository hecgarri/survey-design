rm(list=ls())

setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Censo2012 SPSS/R08")

# Carga de paquetes requeridos
if (!require(devtools)) install.packages("devtools")
if (!require(foreign)) install.packages("foreign")
if (!require(data.table)) install.packages("data.table")
if (!require(maptools)) install.packages("maptools")
if (!require(rgdal)) install.packages("rgdal", dependencies = TRUE)
if (!require(stringi)) install.packages("stringi")
if (!require(dplyr)) install.packages("dplyr")
if (!require(ggmap)) install.packages("ggmap")
if (!require(RColorBrewer)) install.packages("RColorBrewer")

require(parallel)

files = list.files(pattern = "$.shp")

manzanas = readOGR("Manzana_Precensal.shp")

manzanas = manzanas %>% subset(COMUNA == "8401") 


set.seed(1234)
manzanas = manzanas[sample(nrow(manzanas@data), 100),]

colors <- brewer.pal(9, "BuGn")

mapImage <- get_map(location = c(long = -72.09714,lat = -36.59811),
                    color = "color",
                    source = "google",
                    # maptype = "terrain",
                    zoom = 17)

area.points = fortify(manzanas)

ggmap(mapImage) +
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group),
               data = area.points,
               color = colors[9],
               fill = colors[6],
               alpha = 0.5)


