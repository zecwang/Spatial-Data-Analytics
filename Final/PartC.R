setwd("D:/workspace/r/INFSCI 2809 Spatial Data Analytics/Final Exam")

rm(list=ls())
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
library(lctools)
library(tmap)
library(kedd)
library(ggsn)
library(sf)
library(gstat)
library(automap)

filename <- "DataFiles_FinalExam/3_Area1/3_Area1.shp"
p1 <- shapefile(filename)
filename <- "DataFiles_FinalExam/3_Area2/3_Area2.shp"
p2 <- readOGR(filename)

filename <- "DataFiles_FinalExam/3_TrailPoints/3_TrailPoints.shp"
p3 <- readOGR(filename)

filename <- "DataFiles_FinalExam/3_State_Roads/3_State_Roads.shp"
p4 <- readOGR(filename)

### a
# P12
p12 <- raster::intersect(p1, p2)
p12 <- spChFIDs(p12, paste("p", c(1:length(p12)), sep = "."))
summary(p12)
plot(p12)
north.arrow(xb= -77.7, yb= 41.1, len=0.02, lab="N",col='Grey')
map.axes(cex.axis=0.8)
map.scale(x = -79.4, y = 40.12, ratio=TRUE, relwidth=0.2, metric=FALSE)
title(xlab = 'x', ylab = 'y', main = 'P12')

getArea<-function(PolygonData){
    n<-length(PolygonData)
    result<-vector()
    for(i in 1:n){
        v<-nrow(PolygonData@polygons[[i]]@Polygons[[1]]@coords)
        area=0
        for(j in 1:v){
            if(j==v){
                p=1
            }else{
                p=j+1
            }
            x1<-PolygonData@polygons[[i]]@Polygons[[1]]@coords[j,1]
            y1<-PolygonData@polygons[[i]]@Polygons[[1]]@coords[j,2]
            x2<-PolygonData@polygons[[i]]@Polygons[[1]]@coords[p,1]
            y2<-PolygonData@polygons[[i]]@Polygons[[1]]@coords[p,2]
            area<-area+(x1*y2-y1*x2)/2
        }
        result[i]<-abs(area)
    }
    return(result)
}

sum(getArea(p12))

# P1' = P1 - P12
p1ap <- raster::erase(p1, p12)
summary(p1ap)
plot(p1ap)
north.arrow(xb= -77.2, yb= 41.9, len=0.02, lab="N",col='Grey')
map.axes(cex.axis=0.8)
map.scale(x = -79.2, y = 40.8, ratio=TRUE, relwidth=0.2, metric=FALSE)
title(xlab = 'x', ylab = 'y', main = "P1'")

# P2' = P2 - P12
p2ap <- raster::erase(p2, p12)
summary(p2ap)
plot(p2ap)
north.arrow(xb= -77.7, yb= 40.6, len=0.02, lab="N",col='Grey')
map.axes(cex.axis=0.8)
map.scale(x = -79.8, y = 39.6, ratio=TRUE, relwidth=0.2, metric=FALSE)
title(xlab = 'x', ylab = 'y', main = "P2'")

### b
# p3 p1
projection(p1)
projection(p3)
TA <- CRS(projection(p1))
p3TA <- spTransform(p3, TA)

result <- over(p3TA, p1)
p3TA <- subset(p3TA, !is.na(result[,1]))
nrow(p3TA)

summary(p1)

plot(p1)
points(p3TA)
north.arrow(xb= -77.2, yb= 41.9, len=0.02, lab="N",col='Grey')
map.axes(cex.axis=0.8)
map.scale(x = -79.5, y = 40.1, ratio=TRUE, relwidth=0.2, metric=FALSE)
title(xlab = 'x', ylab = 'y', main = "P1 Points")


# p3 p2
p3TA <- spTransform(p3, TA)
result <- over(p3TA, p2)
p3TA <- subset(p3TA, !is.na(result[,1]))
nrow(p3TA)

summary(p2)

plot(p2)
points(p3TA)
north.arrow(xb= -77.5, yb= 41.2, len=0.02, lab="N",col='Grey')
map.axes(cex.axis=0.8)
map.scale(x = -79.9, y = 41.25, ratio=TRUE, relwidth=0.15, metric=FALSE)
title(xlab = 'x', ylab = 'y', main = "P2 Points")

# p3 p12
p3TA <- spTransform(p3, TA)
result <- over(p3TA, p12)
p3TA <- subset(p3TA, !is.na(result[,1]))
nrow(p3TA)

summary(p12)

plot(p12)
points(p3TA)
north.arrow(xb= -77.7, yb= 41.1, len=0.02, lab="N",col='Grey')
map.axes(cex.axis=0.8)
map.scale(x = -79.4, y = 40.12, ratio=TRUE, relwidth=0.2, metric=FALSE)
title(xlab = 'x', ylab = 'y', main = 'P12 Points')

### c
plot(p4)
projection(p1)
projection(p4)
TA <- CRS(projection(p1))
# p4 p1
p4TA <- spTransform(p4, TA)

result <- over(p4TA, p1)
p4TA <- subset(p4TA, !is.na(result[,1]))
sum(p4TA$SEG_LNGTH_)

plot(p1)
lines(p4TA)
north.arrow(xb= -77.2, yb= 41.9, len=0.02, lab="N",col='Grey')
map.axes(cex.axis=0.8)
map.scale(x = -79.5, y = 40.1, ratio=TRUE, relwidth=0.2, metric=FALSE)
title(xlab = 'x', ylab = 'y', main = "P1 Road Segments")

# p4 p2
p4TA <- spTransform(p4, TA)

result <- over(p4TA, p2)
p4TA <- subset(p4TA, !is.na(result[,1]))
sum(p4TA$SEG_LNGTH_)

plot(p2)
lines(p4TA)
north.arrow(xb= -77.5, yb= 41.2, len=0.02, lab="N",col='Grey')
map.axes(cex.axis=0.8)
map.scale(x = -79.9, y = 41.25, ratio=TRUE, relwidth=0.15, metric=FALSE)
title(xlab = 'x', ylab = 'y', main = "P2 Road Segments")

# p4 p12
p4TA <- spTransform(p4, TA)

result <- over(p4TA, p12)
p4TA <- subset(p4TA, !is.na(result[,1]))
sum(p4TA$SEG_LNGTH_)

plot(p12)
lines(p4TA)
north.arrow(xb= -77.7, yb= 41.1, len=0.02, lab="N",col='Grey')
map.axes(cex.axis=0.8)
map.scale(x = -79.4, y = 40.12, ratio=TRUE, relwidth=0.2, metric=FALSE)
title(xlab = 'x', ylab = 'y', main = 'P12 Road Segments')
