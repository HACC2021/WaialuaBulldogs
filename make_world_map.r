
library(ggplot2)
library(sf)
library(rnaturalearth)

worldmap <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf')
head(worldmap[c('name', 'continent')])
head(worldmap[])
ggplot() + geom_sf(data = worldmap) + theme_bw()
