##### JACK KAUPHUSMAN
#### SETTING UP A CRS OF THE RASTER DATA

library(raster)
library(rgdal)
library(dismo)
library(rgbif)

###elevation data
elv<-raster("join_59_60.tif")

plot(elv)

elv

#utm projection for nothern borneo
ref = "+proj=utm +zone=50 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

projectedU = projectRaster(elv, crs = ref)

s=raster("slp2.tif")

plot(s)

s

## utm to lat-long

ref= "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

projectedL = projectRaster(s, crs = ref)

projectedL

