#Jack Kauphusman
# Evaluating Point-Density

vir<- read.csv("Primate_loc.csv")

library(tidyverse)

vir8<-sample_n(vir, 80)

ggplot(vir8, aes(x =vir8$decimalLongitude, y=vir8$decimalLatitude))+
  geom_point()+
  coord_equal()+
  xlab('Longitude')+
  ylab('Latitude')

#Mapping the density

ggplot(vir8, aes(x =vir8$decimalLongitude, y=vir8$decimalLatitude))+
  geom_point()+
  coord_equal()+
  xlab('Longitude')+
  ylab('Latitude')+
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = vir8) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

######## Density of Fires 

library(mapdata)
world <- map_data("world")

ggplot(vir8, aes(x =vir8$decimalLongitude, y=vir8$decimalLatitude))+
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_bin2d(bins=100)

ggplot(vir8, aes(x =decimalLongitude, y=decimalLatitude))+
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3)+stat_density2d(aes(fill = ..level..), alpha = .5,                                                                                               geom = "polygon", data = vir8) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

