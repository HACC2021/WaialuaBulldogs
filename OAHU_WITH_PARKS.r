library(ggplot2)
library(sf)
library(rnaturalearth)
sf::sf_use_s2(FALSE)	#---- turn off strict boundry/error checking...might hurt us later but not sure yet.

worldmap <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf')
USA <- worldmap[worldmap$name == 'United States',]
europe <- worldmap[worldmap$continent == 'Europe',]
europe_cropped <- st_crop(worldmap, xmin = -20, xmax = 45, ymin = 30, ymax = 73)
hawaii <- st_crop(worldmap, xmin = -161, xmax = -154, ymin = 18, ymax = 23, check=F)

############# have fun with xlim and ylim to zoom in on the worldmap ###############################
############# use this cool link to get xlim and ylim                ###############################
#-----  https://www.findlatitudeandlongitude.com/
#-----  ggplot() + geom_sf(data = worldmap) + theme_bw()
#-----  ggplot() + geom_sf(data = USA) + theme_bw()
#-----  ggplot() + geom_sf(data = europe) + theme_bw()
#-----  ggplot() + geom_sf(data = europe_cropped) + theme_bw() + coord_sf(expand = FALSE)
#-----  ggplot() + geom_sf(data = hawaii) + theme_bw()
#-----  This way without cropping the set but cropping the image
#-----  ggplot() + geom_sf(data = worldmap) + coord_sf(xlim = c(-154, -161), ylim = c(23, 18)) +theme_bw()
############# have fun ###############################

#-----	This is on what we need to draw our polygons!!
oahu <- st_crop(worldmap, xmin = -158.5, xmax = -157.5, ymin = 21, ymax = 22)
ggplot() + geom_sf(data = oahu) + theme_bw()

############################################
# import some kml we pulled off https://opendata.arcgis.com/api/v3/datasets/6c4ad137c0e54d398712fd2e3e68e50e_1/downloads/data?format=kml&spatialRefId=4326 
# we manually edited our kml file for simplicity 
############################################
fname = 'C:/r_stuff/bd_kml_good.kml'

stuff <- st_read(fname)
plot(stuff[1])

newthing = append(stuff$geometry,oahu$geometry)
plot(newthing)
