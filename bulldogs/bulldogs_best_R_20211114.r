library(sf)
library(leaflet)
library(rnaturalearth)
library(rgdal)
library(ggplot2)
library(htmlwidgets)
library(webshot)
library(mapview)
library(pander)
library(htmltools)
library(gifski)

sf::sf_use_s2(FALSE)	
fname_parks = 'C:/r_stuff/waahila_SRA.kml'
fname_trails = 'C:/r_stuff/waahila_trail.kml'
fname_oahu_perimeter = 'C:/r_stuff/Coastline.kml'

parks <- st_read(fname_parks)
trails <- st_read(fname_trails)
oahu_perimeter <- st_read(fname_oahu_perimeter)
pingdata_parking_lot <-read.table(file='C:/r_stuff/ping_2043336_clean.txt',header=FALSE,sep='\t',
	col.names=c('epochtime','coordinates'))
pingdata_trail <-read.table(file='C:/r_stuff/ping_204283_clean.txt',header=FALSE,sep='\t',
	col.names=c('epochtime','coordinates'))

pingdata <- rbind(pingdata_parking_lot,pingdata_trail)

coordinates <- pingdata[order(as.numeric(pingdata$epochtime)),]$coordinates
datetimepoch <- as.numeric(pingdata[order(as.numeric(pingdata$epochtime)),]$epochtime)
data_datetime <- as.POSIXct(datetimepoch,origin='1970-01-01')

dots_to_plot <- data.frame(matrix(ncol=3,nrow=length(coordinates)))
colnames(dots_to_plot) <- c('data_datetime','datetimepoch','coordinates')

dots_to_plot$data_datetime=data_datetime
dots_to_plot$datetimepoch=datetimepoch
dots_to_plot$coordinates=coordinates

bbox <- st_bbox(parks) %>% as.vector
bbox1 <- st_bbox(trails) %>% as.vector
x1 <-	ifelse (bbox[1]<bbox1[1],bbox[1],bbox1[1])
y1 <-	ifelse (bbox[2]<bbox1[2],bbox[2],bbox1[2])
x2 <-	ifelse (bbox[3]>bbox1[3],bbox[3],bbox1[3])
y2 <-	ifelse (bbox[4]>bbox1[4],bbox[4],bbox1[4])
newthing = append(parks$geometry,trails$geometry)
thebox <- st_bbox(newthing) %>% as.vector

for(x in 1:length(dots_to_plot$coordinates)){
	fname = paste('C:/r_stuff/thinger/ping_',dots_to_plot$datetimepoch[x],'.png',sep='')
		xpoint <- as.numeric(unlist(strsplit(dots_to_plot$coordinates[x],',',''))[1])
		ypoint <- as.numeric(unlist(strsplit(dots_to_plot$coordinates[x],',',''))[2])
		lblthing <-  paste(as.POSIXct(dots_to_plot$datetimepoch[x],origin='1970-01-01'),sep="")

		map <- leaflet() %>% 
				addTiles() %>% 
					addMarkers(
						-157.7881, 21.32377,
						label = lblthing,
						labelOptions = labelOptions(noHide = T, direction = "bottom",textOnly = TRUE,
						style = list(
							"color" = "BLACK",
							"font-family" = "fixedsys",
							"font-style" = "bold",
							"box-shadow" = "3px 3px rgba(0,0,0,0.25)",
							"font-size" = "12px",
							"border-color" = "rgba(0,0,0,0.5)"
					)))
	for( i in 1:length(parks$geometry)){
			map <- map %>% addPolygons(lng=parks$geometry[[i]][[1]][,1],lat=parks$geometry[[i]][[1]][,2])
		}
		for( i in 1:length(trails$geometry)){
			map <- map %>% addPolylines(lng=trails$geometry[[i]][[1]][,1],lat=trails$geometry[[i]][[1]][,2],color = "red")
		}
		map <- map %>% addCircles(lng=xpoint,lat=ypoint, radius=20, color = "black", group = "trails")
saveWidget(map, "temp.html", selfcontained = FALSE)
webshot("temp.html", file=fname,cliprect = "viewport")
}

png_files <- list.files("C:/r_stuff/thinger/", pattern = ".*png$", full.names = TRUE)
png_files <- png_files[order(png_files,decreasing=F)]
gifski(png_files, gif_file = "C:/r_stuff/thinger/wahillaparkpings.gif", width = 992, height = 744, delay = .033)

