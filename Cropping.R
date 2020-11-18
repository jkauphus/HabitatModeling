#Jack Kauphusman
# Raster and Species Distribution Models (SDMs) 
# Clip Raster Data to an Extent

packages<-c("dismo","rgdal", "rgbif","rdryad","utils","readxl","ggridges","viridis","rasterVis","maps","mapdata","raster", "sp")
sapply(packages, require, character.only=T)

#Bioclime Data from Climate & ElevationR

tmean = climate$bio1
tmean

plot(tmean)

tmin= climate$bio6
tmin

#Elevation

srtm1<-getData(name = "SRTM", lat = 35, lon = -85, path = "./Data")
srtm2<-getData(name = "SRTM", lat = 35, lon = -90, path = "./Data")
srtm3<-getData(name = "SRTM", lat = 40, lon = -85, path = "./Data")
srtm4<-getData(name = "SRTM", lat = 40, lon = -90, path = "./Data")
srtm5<-getData(name = "SRTM", lat = 30, lon = -85, path = "./Data")
srtm6<-getData(name = "SRTM", lat = 30, lon = -90, path = "./Data")
mosaic_strm<-mosaic(srtm1,srtm2,srtm3, srtm4,srtm5,srtm6, fun = mean)

plot(mosaic_strm)

# read in a shapefile

TN = readOGR("Data/Tennessee-Shape.shp")
TN

# Formatting CRS
TN<- spTransform(TN, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
TN

#Cropping Raster to Shapefile
elevation<-crop(mosaic_strm, TN)
plot(elevation)


