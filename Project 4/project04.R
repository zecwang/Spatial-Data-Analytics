setwd("D:/workspace/r/INFSCI 2809 Spatial Data Analytics/Project 4")

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

### Part A
# load data set
filename <- "AlleghenyCounty_Council/AlleghenyCounty_Council.shp"
df <- readOGR(filename)
summary(df)

plot(df)
north.arrow(xb= 1430400, yb= 499000, len=2e+03, lab="N",col='Grey') 
map.axes(cex.axis=0.8)
map.scale(x = 1.25e+06, y = 3.3e+05, ratio=TRUE, relwidth=0.2, metric=FALSE)
title(xlab = 'x', ylab = 'y', main = 'AlleghenyCounty_Council.shp')

cacb <- poly2nb(df, queen = F)
cacw <- nb2listw(cacb, style = "W", zero.policy = T)

n = length(df)
yMean = mean(df$SHAPE_area)
var1 <- 0
var2 <- 0
var3 <- 0
for (i in 1:n) {
    var1 <- var1 + (df$SHAPE_area[i] - yMean) ^ 2
    k <- 0
    for (j in cacw$neighbours[[i]]) {
        k <- k + 1
        w = cacw$weights[[i]]
        var2 <- var2 + w[k] * (df$SHAPE_area[i] - yMean) * (df$SHAPE_area[j] - yMean)
        var3 <- var3 + w[k]
    }
}

I = n / var1 * var2 / var3
I

var1 <- 0
var2 <- 0
var3 <- 0
for (i in 1:n) {
    var1 <- var1 + (df$SHAPE_area[i] - yMean) ^ 2
    k <- 0
    for (j in cacw$neighbours[[i]]) {
        k <- k + 1
        w = cacw$weights[[i]]
        var2 <- var2 + w[k] * (df$SHAPE_area[i] - df$SHAPE_area[j]) ^ 2
        var3 <- var3 + w[k] * 2
    }
}

C = n / var1 * var2 / var3
C

plot(df, col='lightgrey')
map.scale(x = 1.25e+06, y = 3.3e+05, ratio=TRUE, relwidth=0.2, metric=FALSE)
north.arrow(xb= 1440000, yb= 495000, len=2e+03, lab="N",col='Grey') 
map.axes(cex.axis=0.8)
title(xlab = 'x', ylab = 'y', main = "AlleghenyCounty_Council Rook's adjacency")
plot(cacb, coords=coordinates(df),
     add=T,col='red',lwd=3)

cacb <- poly2nb(df, queen = T)
cacw <- nb2listw(cacb, style = "W", zero.policy = T)

n = length(df)
yMean = mean(df$SHAPE_area)
var1 <- 0
var2 <- 0
var3 <- 0
for (i in 1:n) {
    var1 <- var1 + (df$SHAPE_area[i] - yMean) ^ 2
    k <- 0
    for (j in cacw$neighbours[[i]]) {
        k <- k + 1
        w = cacw$weights[[i]]
        var2 <- var2 + w[k] * (df$SHAPE_area[i] - yMean) * (df$SHAPE_area[j] - yMean)
        var3 <- var3 + w[k]
    }
}

I = n / var1 * var2 / var3
I

var1 <- 0
var2 <- 0
var3 <- 0
for (i in 1:n) {
    var1 <- var1 + (df$SHAPE_area[i] - yMean) ^ 2
    k <- 0
    for (j in cacw$neighbours[[i]]) {
        k <- k + 1
        w = cacw$weights[[i]]
        var2 <- var2 + w[k] * (df$SHAPE_area[i] - df$SHAPE_area[j]) ^ 2
        var3 <- var3 + w[k] * 2
    }
}

C = n / var1 * var2 / var3
C

plot(df, col='lightgrey')
map.scale(x = 1.25e+06, y = 3.3e+05, ratio=TRUE, relwidth=0.2, metric=FALSE)
north.arrow(xb= 1440000, yb= 495000, len=2e+03, lab="N",col='Grey') 
map.axes(cex.axis=0.8)
title(xlab = 'x', ylab = 'y', main = "AlleghenyCounty_Council Queen's adjacency")
plot(cacb, coords=coordinates(df),
     add=T,col='red',lwd=3)

#-------------------------------------------------------------

filename <- "AlleghenyCounty_Municipal/AlleghenyCounty_Municipal.shp"
df <- readOGR(filename)
summary(df)

plot(df)
north.arrow(xb= 1433000, yb= 498000, len=2e+03, lab="N",col='Grey') 
map.axes(cex.axis=0.8)
map.scale(x = 1.25e+06, y = 3.3e+05, ratio=TRUE, relwidth=0.2, metric=FALSE)
title(xlab = 'x', ylab = 'y', main = 'AlleghenyCounty_Municipal.shp')

cacb <- poly2nb(df, queen = F)
cacw <- nb2listw(cacb, style = "W", zero.policy = T)

n = length(df)
yMean = mean(df$SHAPE_area)
var1 <- 0
var2 <- 0
var3 <- 0
for (i in 1:n) {
    var1 <- var1 + (df$SHAPE_area[i] - yMean) ^ 2
    k <- 0
    for (j in cacw$neighbours[[i]]) {
        k <- k + 1
        w = cacw$weights[[i]]
        var2 <- var2 + w[k] * (df$SHAPE_area[i] - yMean) * (df$SHAPE_area[j] - yMean)
        var3 <- var3 + w[k]
    }
}

I = n / var1 * var2 / var3
I

var1 <- 0
var2 <- 0
var3 <- 0
for (i in 1:n) {
    var1 <- var1 + (df$SHAPE_area[i] - yMean) ^ 2
    k <- 0
    for (j in cacw$neighbours[[i]]) {
        k <- k + 1
        w = cacw$weights[[i]]
        var2 <- var2 + w[k] * (df$SHAPE_area[i] - df$SHAPE_area[j]) ^ 2
        var3 <- var3 + w[k] * 2
    }
}

C = n / var1 * var2 / var3
C

plot(df, col='lightgrey')
map.scale(x = 1.25e+06, y = 3.3e+05, ratio=TRUE, relwidth=0.2, metric=FALSE)
north.arrow(xb= 1440400, yb= 489000, len=2e+03, lab="N",col='Grey') 
map.axes(cex.axis=0.8)
title(xlab = 'x', ylab = 'y', main = "AlleghenyCounty_Municipal Rook's adjacency")
plot(cacb, coords=coordinates(df),
     add=T,col='red',lwd=3)

cacb <- poly2nb(df, queen = T)
cacw <- nb2listw(cacb, style = "W", zero.policy = T)

n = length(df)
yMean = mean(df$SHAPE_area)
var1 <- 0
var2 <- 0
var3 <- 0
for (i in 1:n) {
    var1 <- var1 + (df$SHAPE_area[i] - yMean) ^ 2
    k <- 0
    for (j in cacw$neighbours[[i]]) {
        k <- k + 1
        w = cacw$weights[[i]]
        var2 <- var2 + w[k] * (df$SHAPE_area[i] - yMean) * (df$SHAPE_area[j] - yMean)
        var3 <- var3 + w[k]
    }
}

I = n / var1 * var2 / var3
I

var1 <- 0
var2 <- 0
var3 <- 0
for (i in 1:n) {
    var1 <- var1 + (df$SHAPE_area[i] - yMean) ^ 2
    k <- 0
    for (j in cacw$neighbours[[i]]) {
        k <- k + 1
        w = cacw$weights[[i]]
        var2 <- var2 + w[k] * (df$SHAPE_area[i] - df$SHAPE_area[j]) ^ 2
        var3 <- var3 + w[k] * 2
    }
}

C = n / var1 * var2 / var3
C

plot(df, col='lightgrey')
map.scale(x = 1.25e+06, y = 3.3e+05, ratio=TRUE, relwidth=0.2, metric=FALSE)
north.arrow(xb= 1440400, yb= 489000, len=2e+03, lab="N",col='Grey') 
map.axes(cex.axis=0.8)
title(xlab = 'x', ylab = 'y', main = "AlleghenyCounty_Municipal Queen's adjacency")
plot(cacb, coords=coordinates(df),
     add=T,col='red',lwd=3)


### Part B

filename <- "Crime_PA2002/Crime_PA2002.shp"
df <- readOGR(filename)
summary(df)
plot(df, col='lightgrey')
map.scale(x = -80, y = 39.5, ratio=TRUE, relwidth=0.2, metric=FALSE)
north.arrow(xb= -75, yb= 42, len=0.06, lab="N",col='Grey') 
map.axes(cex.axis=0.8)
title(xlab = 'x', ylab = 'y', main = "Crime_PA2002")

cacb <- poly2nb(df, queen = T)
cacw <- nb2listw(cacb, style = "B", zero.policy = T)
# globalG.test(df$INDEX01, cacw)

n = length(df)
var1 <- 0
var2 <- 0
for (i in 1:n) {
    k <- 0
    for (j in cacw$neighbours[[i]]) {
        k <- k + 1
        w = cacw$weights[[i]]
        var1 <- var1 + w[k] * df$INDEX01[i] * df$INDEX01[j]
    }
    for (j in 1:n) {
        if (i != j) {
            var2 <- var2 + df$INDEX01[i] * df$INDEX01[j]
        }
    }
}
G = var1 / var2
G

X <- cbind(rep(1, length=length(df)), df$POP_CRI01, df$AG_CRI01, df$Area)
X
# use a Gaussian kernel function to assign weights
W <- diag(x = kernel.fun(as.matrix(dist(coordinates(df)))[61,], deriv.order = 0, kernel = "gaussian")$kx, 67, 67)
W
b <- solve(t(X) %*% W %*% X) %*% t(X) %*% W * df$INDEX01
b

# Check the regression result on Mifflin County
df <- df[df$COUNTY=='Mifflin County',]
X <- cbind(rep(1, length=length(df)), df$POP_CRI01, df$AG_CRI01, df$Area)
X
sum(X %*% b) # 308.3213, pretty close
df$INDEX01 # 215


### Part C

filename <- "Ozone_Sensor_Locs/Ozone_Sensor_Locs.shp"
ozone <- readOGR(filename)
filename <- "PA_County_Select/PA_County_Select.shp"
country <- readOGR(filename)
ozone_value <- read.delim("Data/Ozone_Value.dat", header = F, sep = "|", as.is = T, na.strings = "NA")
head(ozone, n=11L)
head(ozone_value)
ozone_value[ozone_value$V9=="Allegheny County",]
dim(ozone)
ozone_value[ozone_value$V9=="Pennsylvania Dept. of Environmental Protection" &
                ozone_value$V4=="KITT" & ozone_value$V6=="OZONE", "V8"]
df <- data.frame()
df <- rbind(df, 33)
colnames(df) <- 'ozone_value'
for (i in 2:length(ozone)) {
    df <- rbind(df, ozone_value[ozone_value$V9==ozone[i,]$f &
                                    ozone_value$V4==ozone[i,]$c & ozone_value$V6=="OZONE", "V8"])
}
df
# add column ozone_value to Ozone data set
ozone$ozone_value <- df
head(ozone, n=11L)
head(country, n=10L)
plot(country)
points(coordinates(country))
coordinates(country)
head(ozone[, c("long", "lat")], n=11L)

x <- coordinates(country)[,1]
y <- coordinates(country)[,2]
idw <- data.frame()
for (j in 1:length(country)) {
    d <- data.frame()
    for (i in 1:length(ozone)) {
        distance <- sqrt((x[j] - ozone[i,]$long)^2 + (y[j] - ozone[i,]$lat) ^ 2)
        d <- rbind(d, distance)
    }
    colnames(d) <- 'distance'
    print(d)
    print(order(d)[1:5])
    print(d[order(d)[1:5], 'distance'])
    w <- 0
    denominator <- 0
    for (i in 1:5) {
        denominator <- denominator + 1 / d[order(d)[i], 'distance']
    }
    result <- data.frame(diag(x = 0, 6, 6))
    colnames(result) <- c('Control point', 'Ozone value', 'Distance', 'Inverse distance', 'Weight', 'Weighted value')
    for (i in 1:5) {
        w <- 1 / d[order(d)[i], 'distance'] / denominator
        result[i, 1] <- i
        result[i, 2] <- ozone[order(d)[i], ]$ozone_value
        result[i, 3] <- d[order(d)[i], 'distance']
        result[i, 4] <- 1/d[order(d)[i], 'distance']
        result[i, 5] <- w
        result[i, 6] <- w * result[i, 2]
    }
    result[6, 4] <- sum(result[1:5, 4])
    result[6, 5] <- sum(result[1:5, 5])
    result[6, 6] <- sum(result[1:5, 6])
    idw <- rbind(idw, result[6, 6])
    print(result)
    print("-----------------------")
}
colnames(idw) <- 'idw'
country$idw <- idw$idw
head(country, n=10L)
country.points <- fortify(country, region = 'COUNTY')
colnames(country.points)[6] <- 'COUNTY'
country.points <- join(country.points, country@data, by='COUNTY')
head(country.points)
ggplot(data = country.points) +
    geom_polygon(aes(x = long, y = lat, group = COUNTY, fill = idw), inherit.aes = F) + 
    ggtitle("IDW") + theme(plot.title = element_text(hjust = 0.5)) +
    north(country.points) +
    scalebar(country.points, dist = 4, dist_unit = "km",
             transform = T, model = "WGS84")

savefile <- country[, c('COUNTY', 'idw')]
head(savefile, n=10L)
write.csv(savefile, file = 'result - IDW.csv')

# --------------------------------------------------------------------------------------

df <- as.matrix(dist(cbind(ozone$long, ozone$lat)))
df <- as.data.frame(df)
df <- 0 + 60 * df
df <- cbind(df, rep(1, 11))
df <- rbind(df, rep(1, 11))
df[12, 12] <- 0
colnames(df)[12] <- 12
df <- as.matrix(df)

result <- data.frame()
for (p in 1:length(country)) {
    b <- data.frame()
    for (i in 1:length(ozone)) {
        distance <- sqrt((x[p] - ozone[i,]$long)^2 + (y[p] - ozone[i,]$lat) ^ 2)
        b <- rbind(b, distance)
    }
    b <- rbind(b, 1)
    b <- as.matrix(b)
    print(b)
    print('------------------------------------------')
    w <- solve(df) %*% b
    print(w)
    print('++++++++++++++++++++++++++++++++++++++++++')
    var1 <- 0
    for (i in 1:length(ozone)) {
        var1 <- var1 + w[i] * ozone[i, ]$ozone_value
    }
    print(var1)
    result <- rbind(result, var1)
    print('===========================================')
}
result
colnames(result) <- 'OK'
country$OK <- result$OK
head(country, n=10L)
country.points <- fortify(country, region = 'COUNTY')
colnames(country.points)[6] <- 'COUNTY'
country.points <- join(country.points, country@data, by='COUNTY')
head(country.points)
ggplot(data = country.points) +
    geom_polygon(aes(x = long, y = lat, group = COUNTY, fill = OK), inherit.aes = F) +
    ggtitle("OK") + theme(plot.title = element_text(hjust = 0.5)) +
    north(country.points) +
    scalebar(country.points, dist = 4, dist_unit = "km",
             transform = T, model = "WGS84")

savefile <- country[, c('COUNTY', 'OK')]
head(savefile, n=10L)
write.csv(savefile, file = 'result - OK.csv')
