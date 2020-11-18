# Jack Kauphusman
# Obtaining Elevation Data within R

library(maptools)
library(raster)
library(plyr)
library(ggplot2)
library(rgdal)

vnm<-getData("GADM", country="VNM", level=0)

###country code: http://kirste.userpage.fu-berlin.de/diverse/doc/ISO_3166.html
#vnm2<-getData("GADM", country="VNM", level=1)

plot(vnm)

library(elevatr) ##install.packages

v_ele = get_elev_raster(vnm, z = 9)
## z is for spatial resolution (1-14)
##higher z = finer spatial resolution 
##https://mapzen.com/documentation/terrain-tiles/data-sources/#what-is-the-ground-resolution

plot(v_ele)

plot(vnm, add = TRUE)

writeRaster(v_ele,"viet_demz9.tif")
