library(raster)

#Data comes from DIVA
my0 = getData('GADM', country='USA', level=0) #country outline

my1 = getData('GADM', country='USA', level=1) #states included

par(mfrow=c(1,2))

plot(my0, main="Adm. Boundaries USA Level 0")
plot(my1, main="Adm. Boundaries USA Level 1")

## world climate

climate = getData('worldclim', var='bio', res=2.5) #resolution 2.5 

plot(climate$bio1, main="Annual Mean Temperature")
plot(climate$bio5, main="Maximum Temperature")

####################################################################
############
# Working with Elevation Data

library(rgdal)
srtm1<-getData(name = "SRTM", lat = 35, lon = -85, path = "./Data")
srtm2<-getData(name = "SRTM", lat = 35, lon = -90, path = "./Data")
srtm3<-getData(name = "SRTM", lat = 40, lon = -85, path = "./Data")
srtm4<-getData(name = "SRTM", lat = 40, lon = -90, path = "./Data")
srtm5<-getData(name = "SRTM", lat = 30, lon = -85, path = "./Data")
srtm6<-getData(name = "SRTM", lat = 30, lon = -90, path = "./Data")
mosaic_strm<-mosaic(srtm1,srtm2,srtm3, srtm4,srtm5,srtm6, fun = mean)

elevation = setExtent(mosaic_strm, ext)

elevation
######## Compute Slope

slp=terrain(elevation, opt='slope', unit='radians', neighbors=8, filename='slp.tif')

plot(slp)

aspect=terrain(elevation, opt='aspect')

## hillshade- obtained from both slope & aspect

hills = hillShade(slp, aspect, angle=40, direction=270)

plot(hills)

########## CRS of the Raster Data#####################

