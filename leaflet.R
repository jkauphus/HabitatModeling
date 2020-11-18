# Jack Kauphusman
# Working with Leaflet 101

vir<- read.csv("Primate_loc.csv")


library(leaflet)

### default: open street maps 
m = leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=-46, lat=-23.5, popup="Sao Paulo")

m  # Print the map

m = leaflet() 
m=  addTiles(m)  # Add default OpenStreetMap map tiles
m=  addMarkers(m,lng=-0.12, lat=51.5, popup="London")

m  # Print the map

## nat geo map
m = leaflet() %>%
  addTiles() %>%  # 
  addMarkers(lng=-0.12, lat=51.5, popup="Sao Paulo")%>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap)
m


m = leaflet() %>%
  addTiles() %>%  # 
  addMarkers(lng=-0.12, lat=51.5, popup="Sao Paulo")%>% 
  addProviderTiles(providers$CartoDB.Positron)
m

vir<- read.csv("Primate_loc.csv")

names(vir)<-c("Species", "latitude", "longitude")
head(vir)

leaflet(data = vir) %>% addTiles() %>%
  addMarkers(~longitude, ~latitude, popup = ~as.character(Species), label = ~as.character(Species))

leaflet(data = vir[1:15,]) %>% addTiles() %>%
  addMarkers(~longitude, ~latitude, popup = ~as.character(Species), label = ~as.character(Species))
