#-----  https://geoportal.hawaii.gov/datasets/reserves/explore?location=20.616327%2C-157.661250%2C8.35
#-----  https://www.rdocumentation.org/search?q=kml


url = 'https://opendata.arcgis.com/api/v3/datasets/6c4ad137c0e54d398712fd2e3e68e50e_1/downloads/data?format=kml&spatialRefId=4326'

#-----	open a blank plot 
plot(1, 1, col = "white", xlab = "X", ylab = "Y")
#-----	draw polygon
polygon(x = c(0.7, 1.3, 1.2, 0.8), y = c(0.6, 0.8, 1.4, 1),col = "#1b98e0")

#-----	Import KML to R

library(sf)
ccg <- st_read(url)
plot(ccg[1])

#-----	Polygons
x <- c(0.66, 0.26, 0.90, 0.06, 0.94, 0.37)
y <- c(0.99, 0.20, 0.38, 0.77, 0.71, 0.17)
xnew <- x[order(Arg(scale(x) + scale(y) * 1i))]
ynew <- y[order(Arg(scale(x) + scale(y) * 1i))]

plot(xnew, ynew, type = "n")
polygon(xnew ,ynew)

text(x, y, 1:length(x))
