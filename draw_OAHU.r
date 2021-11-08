library(ggplot2)
library(sf)
library(rnaturalearth)

worldmap <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf')
head(worldmap[c('name', 'continent')])
head(worldmap[])
ggplot() + geom_sf(data = worldmap) + theme_bw()


USA <- worldmap[worldmap$name == 'United States',]
ggplot() + geom_sf(data = USA) + theme_bw()

europe <- worldmap[worldmap$continent == 'Europe',]
ggplot() + geom_sf(data = europe) + theme_bw()

europe_cropped <- st_crop(worldmap, xmin = -20, xmax = 45, ymin = 30, ymax = 73)
ggplot() + geom_sf(data = europe_cropped) + theme_bw() + coord_sf(expand = FALSE)

sf::sf_use_s2(FALSE)	#---- turn off strict boundry/error checking...might hurt us later but not sure yet.
hawaii <- st_crop(worldmap, xmin = -161, xmax = -154, ymin = 18, ymax = 23, check=F)
ggplot() + geom_sf(data = hawaii) + theme_bw()

#----- or this way without cropping the set but cropping the image
ggplot() + geom_sf(data = worldmap) + coord_sf(xlim = c(-154, -161), ylim = c(23, 18)) +theme_bw()


#-----	This is on what we need to draw our polygons!!
oahu <- st_crop(worldmap, xmin = -158.5, xmax = -157.5, ymin = 21, ymax = 22)
ggplot() + geom_sf(data = oahu) + theme_bw()
