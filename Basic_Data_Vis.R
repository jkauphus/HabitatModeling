#Jack Kauphusman
# Basic Data Visualization
# Looking at Tiger Rattlesnakes

#Libraries
packages<-c("dismo", "spocc", "rgdal", "rgbif","rdryad","utils","readxl","ggridges","viridis","rasterVis","maps","mapr", "ggmap",
            "mapdata","raster", "sp", "ggplot2")
sapply(packages, require, character.only=T)

# My favorite Arizona Rattlesnakes
spp= c("Crotalus atrox", "Crotalus cerastes", "Crotalus molossus", "Crotalus tigris")

dat <- occ(query = spp, from = 'gbif', gbifopts = list(hasCoordinate = TRUE))
dat

# Visualizing Records

map_leaflet(dat)

map_ggplot(dat)
