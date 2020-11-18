# Jack Kauphusman
## Classical SDMs
## Bioclim (https://www.worldclim.org/data/bioclim.html)

#Packages
packages<-c("raster", "rgdal", "maptools","rgbif", "dplyr", "ggplot2", "sf","sp", "gistr", "dismo", "smoothr", "adehabitatHR",
            "ggmap", "GISTools", "ggspatial", "maps", "devtools", "ggsn", "rgeos")
sapply(packages, require, character.only=T)

#Data
cottonmouth<- read.csv("akg.csv")
glimpse(cottonmouth)

ext<-extent(-90.36,-81.82, 35.07, 36.78)

library(rgdal)

all.worldclim = raster::getData("worldclim", res = 10, var = 'bio')
tn.worldclim = crop(all.worldclim, ext)

##Map 

snake.map<- get_stamenmap(bbox = c(left =-90.36,
                                   bottom = 34.5,
                                   right = -81.82,
                                   top = 36.78),
                         maptype = "terrain", zoom = 6)
ggmap(snake.map)+
  labs(x="Longitude (WGS84)", y="Latitude")+
  geom_point(data=cottonmouth, aes(lon, lat), color = "black", size = 1)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  coord_equal()+
  ggtitle("Tennessee Distribution of Agkistrodon piscivorus")+
  theme_bw(base_size = 20)+
  theme(plot.title = element_text(hjust = 0.5))

## Use the bioclim function, which takes your climate layers and the long and lat columns (in that order)
s.bc = bioclim(tn.worldclim, cottonmouth[,c('lon','lat')])

par(mfrow = c(4,4))
response(s.bc)
par(mfrow=(c(1,1)))

#Predicting distribution of cottonmouths base on bioclim
snake.pred<- predict(object = s.bc, tn.worldclim)
plot(snake.pred, main = 'sdm predictions using climate layers')
points(cottonmouth[,c('lon', 'lat')], pch = 16, cex = 0.25)

#### evaluate model performance
#### background data (pseudo-absences) needed for this
#### determine if the model can differentiate bw the habitat & the background

plot(tn.worldclim)

## background pseudo-absences
backg<- randomPoints(tn.worldclim, ncell =1000, ext = ext, extf = 1.25)

cottonmouth<-cottonmouth[,c(3,4)]
## Evaluation
e<- evaluate(cottonmouth, backg, s.bc, tn.worldclim)

#presence, background, model, predictors
e

plot(e, 'ROC')
