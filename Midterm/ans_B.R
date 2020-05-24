setwd("D:/workspace/r/INFSCI 2809 Spatial Data Analytics/Midterm")

library(sp)
library(spdep)
library(classInt)
library(rgeos)
library(maptools)
library(rgdal)
library(ggplot2)
library(weights)
library(ape)
library(GISTools)
library(maps)
library(raster)
library(plyr)
library(dplyr)
library(spatstat)

# load data set
filename <- "PALocs/PALocs.shp"
locs <- shapefile(filename, stringsAsFactors=T)
plot(locs)
plot(locs$Long, locs$Lat)
summary(locs)

mypattern <- ppp(locs$Long, locs$Lat, c(-80.4, -74.98), c(39.8, 42.04))

G <- Gest(mypattern)
plot(G)
plot(G$r, G$km, type = 'l', xlab = 'Distance, d', ylab = 'G(d)', main = 'G function - PALocs')

F_function <- Fest(mypattern)
plot(F_function)
plot(F_function$r, F_function$km, type = 'l', xlab = 'Distance, d', ylab = 'F(d)', main = 'F function - PALocs')

plot(G$r, G$km, type = 'l', xlab = 'Distance, d', ylab = 'G(d), F(d)', col = 'red', main = 'G function, F function - PALocs')
lines(F_function$r, F_function$km, col = 'blue', lty=2)
legend(0, 1, legend = c('G', 'F'), col=c("red", "blue"), lty=1:2, cex=0.8)


#####################################################################################################################################

filename <- "PACoals/PACoals.shp"
coals <- shapefile(filename, stringsAsFactors=T)
plot(coals)
plot(coals$Long, coals$Lat)
summary(coals)

mypattern <- ppp(coals$Long, coals$Lat, c(-80.12, -76), c(39.67, 41.32))

G <- Gest(mypattern)
plot(G)
plot(G$r, G$km, type = 'l', xlab = 'Distance, d', ylab = 'G(d)', main = 'G function - PACoals')

F_function <- Fest(mypattern)
plot(F_function)
plot(F_function$r, F_function$km, type = 'l', xlab = 'Distance, d', ylab = 'F(d)', main = 'F function - PACoals')

plot(G$r, G$km, type = 'l', xlab = 'Distance, d', ylab = 'G(d), F(d)', col = 'red', main = 'G function, F function - PACoals')
lines(F_function$r, F_function$km, col = 'blue', lty=2)
legend(0, 1, legend = c('G', 'F'), col=c("red", "blue"), lty=1:2, cex=0.8)
