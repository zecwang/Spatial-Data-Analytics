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

xdim = 48
ydim = 20
n <- 67
quadrat_num <- xdim * ydim
mean <- n / quadrat_num

dx = (-74.98 - -80.4)/xdim
dy = (42.04 - 39.8)/ydim

x_top = -74.98 - dx
y_top = 42.04 - dy
mypattern <- ppp(locs$Long, locs$Lat, c(-80.4, -74.98), c(39.8, 42.04))
plot(mypattern, main="Random Quadrat Sample Approach - PALocs")
map.scale(x = -80.2, y = 39.6, ratio=TRUE, relwidth=0.15, metric=FALSE)
north.arrow(xb=-75.01, yb= 42.3, len=0.03, lab="N",col='Grey') 
map.axes(cex.axis=0.8)
title(xlab = 'LONGITUDE', ylab = 'LATITUDE')
res <- data.frame("K" = 0, "X" = 0)

for (i in 1:(12*5)) {
    # generate random x and y
    x = runif(1, min=-80.4, max=x_top)
    y = runif(1, min=39.8, max=y_top)
    # count events
    K <- sum(locs$Long >= x & locs$Long <= x+dx & locs$Lat >= y & locs$Lat <= y+dy)
    rect(x, y, x+dx, y+dy, border="red")
    # create new row if K is not in the table, otherwise increase 1
    if (K %in% res$K) {
        res$X[res$K==K] <- res$X[res$K==K] + 1
    } else {
        res <- rbind(res, data.frame("K"=K, "X" = 1))
    }
}

res <- cbind(res, res$K - mean, (res$K - mean)^2, res$X * (res$K - mean)^2)
names(res) <- c("K", "X", "V3", "V4", "V5")
VMR_Locs <- sum(res$V5)/(sum(res$X) - 1)/mean
head(res)
VMR_Locs
#write.csv(res, file = "PALocs.csv")

########################################################################################

filename <- "PACoals/PACoals.shp"
coals <- shapefile(filename, stringsAsFactors=T)
plot(coals)
plot(coals$Long, coals$Lat)
summary(coals)

xdim = 48
ydim = 20
n <- 4106
quadrat_num <- xdim * ydim
mean <- n / quadrat_num

dx = (-76 - -80.12)/xdim
dy = (41.32 - 39.67)/ydim

x_top = -76 - dx
y_top = 41.32 - dy
mypattern <- ppp(coals$Long, coals$Lat, c(-80.12, -76), c(39.67, 41.32))
plot(mypattern, main="Random Quadrat Sample Approach - PACoals")
map.scale(x = -80, y = 39.5, ratio=TRUE, relwidth=0.15, metric=FALSE)
north.arrow(xb=-76.03, yb= 41.5, len=0.03, lab="N",col='Grey') 
map.axes(cex.axis=0.8)
title(xlab = 'LONGITUDE', ylab = 'LATITUDE')
res <- data.frame("K" = 0, "X" = 0)

for (i in 1:(12*5)) {
    # generate random x and y
    x = runif(1, min=-80.12, max=x_top)
    y = runif(1, min=39.67, max=y_top)
    # count events
    K <- sum(coals$Long >= x & coals$Long <= x+dx & coals$Lat >= y & coals$Lat <= y+dy)
    rect(x, y, x+dx, y+dy, border="red")
    # create new row if K is not in the table, otherwise increase 1
    if (K %in% res$K) {
        res$X[res$K==K] <- res$X[res$K==K] + 1
    } else {
        res <- rbind(res, data.frame("K"=K, "X" = 1))
    }
}

res <- cbind(res, res$K - mean, (res$K - mean)^2, res$X * (res$K - mean)^2)
names(res) <- c("K", "X", "V3", "V4", "V5")
VMR_Coals <- sum(res$V5)/(sum(res$X) - 1)/mean
head(res)
VMR_Coals
#write.csv(res, file = "PACoals.csv")
