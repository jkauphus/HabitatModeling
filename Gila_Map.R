#By Jack Kauphusman

#Mapping of Gila Monster Distribution


devtools::install_github('oswaldosantos/ggsn')
#Packages
packages<-c("raster", "rgdal", "maptools","rgbif", "dplyr", "ggplot2", "sf","sp", "gistr", "dismo", "smoothr", "adehabitatHR",
            "ggmap", "GISTools", "ggspatial", "maps", "devtools", "ggsn")
sapply(packages, require, character.only=T)



#Extent
extent<- extent(-120, -107, 31, 40)

#Loading Gila Monster Data
Gila<-data.frame(gbif("Heloderma", species = "suspectum", geo = TRUE, sp = TRUE, download = TRUE, removeZeros = TRUE, ext = extent))
names(Gila)<-c("Date", "x", "y", "verified")                  
#Gila<-unique(Gila)

#Creating Dataset into points shp.
Gila<- Gila[,2:3]
gila_points<- SpatialPoints(Gila,
                            proj4string = CRS("+proj=longlat +datum=WGS84"))


#Loading Gila Data in SW USA
#southwest<-map_data("state", region = c('california', 'utah', 'arizona', 'nevada'))

#Creating MCP to Mimic Species Distribution

Gila.Dist<- mcp(gila_points, percent = 100)

dist<-fortify(Gila.Dist, region = "id")

#Creating KDE to mimic species distribution

kde<-kernelUD(gila_points, h ="href", kern = "bivnorm", grid = 100)

ver99<-getverticeshr(kde, 99)

kde.poly99<-fortify(ver99, region = "id")

#Mapping Gila Monster Distribution with MCP

my.Map<-get_stamenmap(bbox = c(left =-119.1094,
                       bottom = 28.8899,
                       right = -107.4212,
                       top = 40.0428),
              maptype = "terrain", zoom = 6)
  
mcp_map<-ggmap(my.Map)+
  labs(x="Longitude (WGS84)", y="Latitude")+
  geom_polygon(data = dist, aes(x = dist$long, dist$lat), color = "coral4", fill = "coral2", alpha = 0.6)+
  geom_point(data=Gila, aes(x,y), fill = NA, color = "coral4")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  coord_equal()+
  ggtitle("Species Distribution of Heloderma suspectum")+
  theme_bw(base_size = 20)+
  theme(plot.title = element_text(hjust = 0.5))

#Mapping Gila Monster Distribution with KDE

kde_map<-ggmap(my.Map)+
  labs(x="Longitude (WGS84)", y="Latitude") +
  geom_polygon(data = kde.poly99, aes(x = kde.poly99$long, kde.poly99$lat), color = "coral4", fill = "coral2", alpha = 0.6)+
  geom_point(data=Gila, aes(x,y), fill = NA, color = "coral4")+
  theme_bw()+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  coord_equal()+
  ggtitle("Species Distribution of H. suspectum")+
  theme_bw(base_size = 20)+
  theme(plot.title = element_text(hjust = 0.5))

#Better depicts species distribution

# Creating a scalebar manually

create_scale_bar <- function(lon,lat,distance_lon,distance_lat,distance_legend, dist_units = "km"){
  # First rectangle
  bottom_right <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon, dist.units = dist_units, model = "WGS84")
  
  topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_lat, dist.units = dist_units, model = "WGS84")
  rectangle <- cbind(lon=c(lon, lon, bottom_right[1,"long"], bottom_right[1,"long"], lon),
                     lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
  rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
  
  # Second rectangle t right of the first rectangle
  bottom_right2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon*2, dist.units = dist_units, model = "WGS84")
  rectangle2 <- cbind(lon = c(bottom_right[1,"long"], bottom_right[1,"long"], bottom_right2[1,"long"], bottom_right2[1,"long"], bottom_right[1,"long"]),
                      lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
  rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
  
  # Now let's deal with the text
  on_top <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_legend, dist.units = dist_units, model = "WGS84")
  on_top2 <- on_top3 <- on_top
  on_top2[1,"long"] <- bottom_right[1,"long"]
  on_top3[1,"long"] <- bottom_right2[1,"long"]
  
  legend <- rbind(on_top, on_top2, on_top3)
  legend <- data.frame(cbind(legend, text = c(0, distance_lon, distance_lon*2)), stringsAsFactors = FALSE, row.names = NULL)
  return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
}

scale_bar <- function(lon, lat, distance_lon, distance_lat, distance_legend, dist_unit = "km", rec_fill = "white", rec_colour = "black", rec2_fill = "black", rec2_colour = "black", legend_colour = "black", legend_size = 3, orientation = TRUE, arrow_length = 500, arrow_distance = 300, arrow_north_size = 6){
  the_scale_bar <- create_scale_bar(lon = lon, lat = lat, distance_lon = distance_lon, distance_lat = distance_lat, distance_legend = distance_legend, dist_unit = dist_unit)
  # First rectangle
  rectangle1 <- geom_polygon(data = the_scale_bar$rectangle, aes(x = lon, y = lat), fill = rec_fill, colour = rec_colour)
  
  # Second rectangle
  rectangle2 <- geom_polygon(data = the_scale_bar$rectangle2, aes(x = lon, y = lat), fill = rec2_fill, colour = rec2_colour)
  
  # Legend
  scale_bar_legend <- annotate("text", label = paste(the_scale_bar$legend[,"text"], dist_unit, sep=""), x = the_scale_bar$legend[,"long"], y = the_scale_bar$legend[,"lat"], size = legend_size, colour = legend_colour)
  
  res <- list(rectangle1, rectangle2, scale_bar_legend)
  
  if(orientation){# Add an arrow pointing North
    coords_arrow <- create_orientation_arrow(scale_bar = the_scale_bar, length = arrow_length, distance = arrow_distance, dist_unit = dist_unit)
    arrow <- list(geom_segment(data = coords_arrow$res, aes(x = x, y = y, xend = xend, yend = yend)), annotate("text", label = "N", x = coords_arrow$coords_n[1,"x"], y = coords_arrow$coords_n[1,"y"], size = arrow_north_size, colour = "black"))
    res <- c(res, arrow)
  }
  return(res)
}

#Adding scalebar to map

fmap2<-kde_map+scale_bar(lon = -110.2, lat = 29.2, distance_lon = 100, distance_lat = 20, distance_legend = 75, dist_unit = "km", orientation = FALSE)

# Adding a north arrow using gnss package

north2(fmap2, x = 0.73, y =0.88, symbol=10)

#Recommd exporting on the rstudio export
#If you need to add those study site points I suggest another geom_point and the geom_label to ggmap check out this guys tutorial its pretty helpful
#https://ryanpeek.org/2016-09-28-static_maps_in_R/

#There is no easy way to place these scalebars and north arrows in ggmap

 
