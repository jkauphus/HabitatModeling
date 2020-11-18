#Jack Kauphusman 
#Pre-Processing for ML

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

### Presence only data modeling
cottonmouth<-read.csv("akg.csv")
cottonmouth1<-cottonmouth[,c(2,3,4)]
cottonmouth<-cottonmouth[,c(3,4)]
glimpse(cottonmouth)

#Filter Data into the extent extent
cottonmouth_TN<-filter(cottonmouth, lon >= -90.33333 & lat >= 34.5)
cottonmouth_TN<-filter(cottonmouth_TN, lon <= -81.33333 & lat <= 36.83333)
plot(TN)
points(cottonmouth_TN, col = "blue")

#Processing Presence & Absence Data
prs1= extract(stck, cottonmouth_TN)

set.seed(1)

backgr = randomPoints(stck, 500) #500 random points
absvals = extract(stck, backgr) #choose absence values from the background
pb = c(rep(1, nrow(prs1)), rep(0, nrow(absvals)))
sdmdata = data.frame(cbind(pb, rbind(prs1, absvals)))

head(sdmdata)

sdmdata=na.omit(sdmdata)
summary(sdmdata)

tail(sdmdata)

### select an area of absence based on ecological consideration
e = drawExtent()
abs=crop(stck,e)
plot(abs)

backgr = randomPoints(abs, 135)
absvals2<-extract(abs, backgr)
pb = c(rep(1, nrow(prs1)), rep(0, nrow(absvals2)))

sdmdata = data.frame(cbind(pb, rbind(prs1, absvals2)))

head(sdmdata)


####################### Pre-processing Part Two

write.csv(sdmdata,"Pres_abs.csv")

pa=read.csv("Pres_abs.csv")

#pa=na.omit(pa)

head(pa)

summary(pa)

library(caret)

set.seed(1) #pseudo-repeatability
trainIndex = createDataPartition(pa$pb, p = .75, 
                                 list = FALSE, 
                                 times = 1) #y as basis of splitting

training = pa[ trainIndex,] #75% data for model training
testing= pa[-trainIndex,] #25% for model testing

head(training)
