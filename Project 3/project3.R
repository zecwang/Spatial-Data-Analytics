setwd("D:/workspace/r/INFSCI 2809 Spatial Data Analytics/Project 3")

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

df <- read.csv("ParticulateMatter.csv", header = T, sep = ',')
summary(df)
plot(df$PM25 ~ df$Lat)
plot(df$PM25 ~ df$Lon)
plot(df)

df.dists <- as.matrix(dist(cbind(df$Lon, df$Lat)))
df.dists.inv <- 1/df.dists
diag(df.dists.inv) <- 0

n = length(df$PM25)
yMean = mean(df$PM25)
var1 <- 0
for (i in 1:n) {
    var1 <- var1 + (df[i, "PM25"] - yMean) ^ 2
}
var2 <- 0
for (i in 1:42) {
    for (j in 1:42) {
        var2 <- var2 + df.dists.inv[i, j] * (df[i, "PM25"] - yMean) * (df[j, "PM25"] - yMean)
    }
}
var3 <- 0
for (i in 1:42) {
    for (j in 1:42) {
        var3 <- var3 + df.dists.inv[i, j]
    }
}
I = (n / var1) * (var2 / var3)


###############################################################################


filename <- "OilGasLocationPA/OilGasLocationPA.shp"
s <- shapefile(filename, stringsAsFactors=T)
summary(s)
summary(s@coords)
Coords <- s@coords
plot(s)
map.scale(x = 0.5e+05, y = 1e+05, ratio=TRUE, relwidth=0.25, metric=FALSE)
north.arrow(xb= 2.6e+05, yb= 3.5e+05, len=0.6e+04, lab="N",col='Grey') 
map.axes(cex.axis=0.8)
legend(-215841, 3.8e+05, legend = "point", pch = 3)
title(xlab = 'x', ylab = 'y', main = 'OilGasLocationPA')

mypattern <- ppp(Coords[,1], Coords[,2], c(-215841, 256931), c(80496, 364307))

par(mfrow = c(2, 2))

G <- Gest(mypattern)
#plot(G)
plot(G$r, G$km, type = 'l', xlab = 'Distance, d', ylab = 'G(d)', main = 'G function - OilGasLocationPA')

F_function <- Fest(mypattern)
#plot(F_function)
plot(F_function$r, F_function$km, type = 'l', xlab = 'Distance, d', ylab = 'F(d)', main = 'F function - OilGasLocationPA')

K <- Kest(mypattern, nlarge = 160000)
#K <- Kest(mypattern)
#plot(K)
plot(K$r, K$border, type = 'l', xlab = 'Distance, d', ylab = 'K(d)', main = 'K function - OilGasLocationPA')

L <- Lest(mypattern, nlarge = 160000)
#plot(L)
plot(L$r, L$border, type = 'l', xlab = 'Distance, d', ylab = 'L(d)', main = 'L function - OilGasLocationPA')

par(mfrow = c(1, 1))

plot(G$r, G$km, type = 'l', xlab = 'Distance, d', ylab = 'G(d), F(d)', col = 'red', main = 'G function, F function - PALocs')
lines(F_function$r, F_function$km, col = 'blue', lty=2)
legend(0, 1, legend = c('G', 'F'), col=c("red", "blue"), lty=1:2, cex=0.8)

##

filename <- "IndustrialMineralMiningPA/IndustrialMineralMiningOperations2014_10.shp"
ss <- shapefile(filename, stringsAsFactors=T)
plot(ss)
map.scale(x = -2e+05, y = 0.5e+05, ratio=TRUE, relwidth=0.2, metric=FALSE)
north.arrow(xb= 2.6e+05, yb= 3.5e+05, len=0.6e+04, lab="N",col='Grey') 
map.axes(cex.axis=0.8)
legend(-214271, 3.9e+05, legend = "point", pch = 3)
title(xlab = 'x', ylab = 'y', main = 'IndustrialMineralMiningPA')

summary(ss@coords)
ss.Coords <- ss@coords

mypattern <- ppp(ss.Coords[,1], ss.Coords[,2], c(-214271, 278346), c(78927, 361067))
plot(mypattern)

par(mfrow = c(2, 2))

Gc <- Gest(mypattern)
#plot(Gc)
plot(Gc$r, Gc$km, type = 'l', xlab = 'Distance, d', ylab = 'G(d)', main = 'G function - IndustrialMineralMiningPA')

Fc <- Fest(mypattern)
#plot(Fc)
plot(Fc$r, Fc$km, type = 'l', xlab = 'Distance, d', ylab = 'F(d)', main = 'F function - IndustrialMineralMiningPA')

Kc <- Kest(mypattern, nlarge = 5000)
#plot(Kc)
plot(Kc$r, Kc$border, type = 'l', xlab = 'Distance, d', ylab = 'K(d)', main = 'K function - IndustrialMineralMiningPA')

Lc <- Lest(mypattern, nlarge = 5000)
#plot(Lc)
plot(Lc$r, Lc$border, type = 'l', xlab = 'Distance, d', ylab = 'L(d)', main = 'L function - IndustrialMineralMiningPA')

par(mfrow = c(1, 1))

plot(Gc$r, Gc$km, type = 'l', xlab = 'Distance, d', ylab = 'G(d), F(d)', col = 'red', main = 'G function, F function - PALocs')
lines(Fc$r, Fc$km, col = 'blue', lty=2)
legend(0, 1, legend = c('G', 'F'), col=c("red", "blue"), lty=1:2, cex=0.8)

