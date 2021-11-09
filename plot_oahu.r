library(sf)

fname_parks = 'C:/r_stuff/bd_kml_good.kml'
fname_trails = 'C:/r_stuff/trails_kml.kml'
fname_oahu_perimeter = 'C:/r_stuff/Coastline.kml'
parks <- st_read(fname_parks)
trails <- st_read(fname_trails)
oahu_perimeter <- st_read(fname_oahu_perimeter)
newthing = append(oahu_perimeter$geometry,trails$geometry)
newthing = append(newthing,parks$geometry)
plot(newthing)
