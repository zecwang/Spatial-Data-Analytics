setwd("D:/workspace/r/INFSCI 2809 Spatial Data Analytics/Final Exam")

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

filename <- "DataFiles_FinalExam/1_Neighbor1/1_Neighbor1.shp"
df <- readOGR(filename)
summary(df)
dim(df)
head(df, n=12L)
plot(df)
text(coordinates(df), label=df$POP_ARR02, cex=1)
north.arrow(xb= -78.8, yb= 41.5, len=0.02, lab="N",col='Grey')
map.axes(cex.axis=0.8)
map.scale(x = -79.0, y = 40.0, ratio=TRUE, relwidth=0.2, metric=FALSE)
title(xlab = 'x', ylab = 'y', main = '1_Neighbor1.shp')

df.rook <- poly2nb(df, queen = F)
df.rook.w <- nb2listw(df.rook, style = "W", zero.policy = T)

df.rook.matrix <- nb2mat(df.rook, style = "W", zero.policy = T)
df.rook.result <- moransI.w(df$POP_ARR02, df.rook.matrix)
df.rook.result

moran.test(df$POP_ARR02, df.rook.w)
geary.test(df$POP_ARR02, df.rook.w)

df.queen <- poly2nb(df, queen = T)
df.queen.w <- nb2listw(df.queen, style = "W", zero.policy = T)

moran.test(df$POP_ARR02, df.queen.w)
geary.test(df$POP_ARR02, df.queen.w)


filename <- "DataFiles_FinalExam/1_Neighbor2/1_Neighbor2.shp"
df <- readOGR(filename)
summary(df)
dim(df)
head(df, n=12L)
plot(df)
text(coordinates(df), label=df$POP_ARR02, cex=1)
north.arrow(xb= -76.2, yb= 41.5, len=0.02, lab="N",col='Grey')
map.axes(cex.axis=0.8)
map.scale(x = -78.5, y = 40.2, ratio=TRUE, relwidth=0.2, metric=FALSE)
title(xlab = 'x', ylab = 'y', main = '1_Neighbor2.shp')

df.rook <- poly2nb(df, queen = F)
df.rook.w <- nb2listw(df.rook, style = "W", zero.policy = T)

moran.test(df$POP_ARR02, df.rook.w)
geary.test(df$POP_ARR02, df.rook.w)

df.queen <- poly2nb(df, queen = T)
df.queen.w <- nb2listw(df.queen, style = "W", zero.policy = T)

moran.test(df$POP_ARR02, df.queen.w)
geary.test(df$POP_ARR02, df.queen.w)

