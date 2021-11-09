library(sf)
library(leaflet)
library(rgdal)

fname_parks = 'C:/r_stuff/bd_kml_good.kml'
fname_trails = 'C:/r_stuff/trails_kml.kml'
fname_oahu_perimeter = 'C:/r_stuff/Coastline.kml'

parks <- st_read(fname_parks)
trails <- st_read(fname_trails)
oahu_perimeter <- st_read(fname_oahu_perimeter)

bbox <- st_bbox(parks) %>% as.vector
map <- leaflet() %>% 
  addTiles() %>% 
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) 
for( i in 1:length(parks$geometry)){
	map <- map %>% addPolygons(lng=parks$geometry[[i]][[1]][,1],lat=parks$geometry[[i]][[1]][,2])
}

for( i in 1:length(trails$geometry)){
	map <- map %>% addPolylines(lng=trails$geometry[[i]][[1]][,1],lat=trails$geometry[[i]][[1]][,2],color = "red")
}
map
