## Jack Kauphusman
# Domain modeling for SDM Techniques

# Packages
packages<-c("raster", "rgdal", "maptools","rgbif", "dplyr", "ggplot2", "sf","sp", "gistr", "dismo", "smoothr", "adehabitatHR",
            "ggmap", "GISTools", "ggspatial", "maps", "devtools", "ggsn", "rgeos", "rJava")
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

### Presence only data modeling
cottonmouth<-read.csv("akg.csv")
cottonmouth1<-cottonmouth[,c(2,3,4)]
cottonmouth<-cottonmouth[,c(3,4)]
glimpse(cottonmouth)

points(cottonmouth, col ='blue')

#Training and Test Set
group<- kfold(cottonmouth,5)
pres_train <- cottonmouth[group !=1, ]
pres_test <- cottonmouth[group ==1, ]

#Implimenting pseudo-absences
backg <- randomPoints(stck, n=1000, ext=ext, extf = 1.25)
colnames(backg) = c('lon', 'lat')
group = kfold(backg, 5)

r = raster(stck, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
plot(ext, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-',  cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')

require(dismo)
dm =domain(stck, pres_train) #domain model- presence data only 
e = evaluate(pres_test, backg_test, dm, stck)
e

#### predictive mapping

pd = predict(stck, dm, ext=ext, progress='') #predict for p mapping
par(mfrow=c(1,2))
plot(pd, main='Domain, raw values')  
