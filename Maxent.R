#Jack Kauphusman
## Maxent SDM

# Packages
packages<-c("raster", "rgdal", "maptools","rgbif", "dplyr", "ggplot2", "sf","sp", "gistr", "dismo", "smoothr", "adehabitatHR",
            "ggmap", "GISTools", "ggspatial", "maps", "devtools", "ggsn", "rgeos", "rjava")
sapply(packages, require, character.only=T)

#Exent
ext<-extent(-90.33333, -81.83333, 34.5, 36.83333)

# Elevation Predictors
srtm1<-getData(name = "SRTM", lat = 35, lon = -85, path = "./Data")
srtm2<-getData(name = "SRTM", lat = 35, lon = -90, path = "./Data")
srtm3<-getData(name = "SRTM", lat = 40, lon = -85, path = "./Data")
srtm4<-getData(name = "SRTM", lat = 40, lon = -90, path = "./Data")
srtm5<-getData(name = "SRTM", lat = 30, lon = -85, path = "./Data")
srtm6<-getData(name = "SRTM", lat = 30, lon = -90, path = "./Data")
mosaic_strm<-mosaic(srtm1,srtm2,srtm3, srtm4,srtm5,srtm6, fun = mean)

# Bioclim indicators
all.worldclim = raster::getData("worldclim", res = 10, var = 'bio')

#Cropping Rasters to match extant
elevation = setExtent(mosaic_strm, ext)
tn.worldclim = crop(all.worldclim, ext)

elevation
tn.worldclim

#Resampling to match cell size
elevation <- resample(elevation, tn.worldclim, method = "bilinear")

#Stacking Rasters
## MUST HAVE SAME EXTENT AND RESOLUTION
stck<-stack(elevation, tn.worldclim)

plot(stck,1)

#Presence Data
cottonmouth<- read.csv("akg.csv")
cottonmouth1<-cottonmouth[,c(2,3,4)]
cottonmouth<-cottonmouth[,c(3,4)]
glimpse(cottonmouth)

points(cottonmouth, col = 'blue')

#Training and Test Set
group<- kfold(cottonmouth,5)
pres_train <- cottonmouth[group !=1, ]
pres_test <- cottonmouth[group ==1, ]

#Implimenting maxent on the presence-only dataset
xm = maxent(stck, pres_train)
plot(xm)

#Implimenting pseudo-absences
backg <- randomPoints(stck, n=1000, ext=ext, extf = 1.25)
colnames(backg) = c('lon', 'lat')
group = kfold(backg, 5)

#pseuo-training model
backg_train <- backg[group !=1, ]
backg_test <- backg[group ==1, ]

e = evaluate(pres_test, backg_test, xm, stck)
e

p = predict(stck, xm, ext=ext, progress='')
#0-1 scale where 1 indicates the most suitable habitat
#and 0 leat suitable habitat

par(mfrow=c(1,2))
plot(p, main = "Maxent, raw values")

#Utilizing the red package to detail information using MAXENT
library(red)

## learn more about your data 
# Makes a map with pdf output, and expresses the min and max of a raster layer. 
map.easy(cottonmouth1, layers = stck, habitat = NULL, zone = NULL,
         thin = TRUE, error = NULL, move = TRUE, dem = elevation, pca = 0,
         filename = NULL, mapoption = NULL, testpercentage = 20, mintest = 20,
         runs = 0, subset = 0)

##Maxent
map.sdm(cottonmouth, stck, error = NULL, categorical = NULL, thres = 0,
        testpercentage = 20, mcp = TRUE, eval = TRUE, runs = 0, subset = 0)

