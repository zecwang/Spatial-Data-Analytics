setwd("D:/workspace/r/INFSCI 2809 Spatial Data Analytics/Project 2")

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
filename <- "OilGasLocationPA/OilGasLocationPA.shp"
s <- shapefile(filename, stringsAsFactors=T)
plot(s)
summary(s)
head(s)

# define the amount of quadrats
xdim = 66
ydim = 30
mypattern <- ppp(s$LONGITUDE, s$LATITUDE, c(-80.52, -74.92), c(39.72, 42.27))
# count events in each quadrat
qt <- quadratcount(mypattern, nx = xdim, ny = ydim)
plot(mypattern, main="Regular Quadrant Sample Approach")
plot(qt, add=T, col="red")

# generate the table
df <- data.frame(table(qt))
names(df) <- c("K", "X")
n <- 156259
quadrat_num <- xdim * ydim
mean <- n / quadrat_num
df$K <- as.numeric(as.character(df$K))
df <- cbind(df, df$K - mean, (df$K - mean)^2, df$X * (df$K - mean)^2)
names(df) <- c("K", "X", "V3", "V4", "V5")
VMR_regular <- sum(df$V5)/(sum(df$X) - 1)/mean
write.csv(df, file = "regular.csv")

# define the same width and height quadrat as in Regular method
dx = (-74.92 - -80.52)/xdim
dy = (42.27 - 39.72)/ydim
# define the max limit of x and y
x_top = -74.92 - dx
y_top = 42.27 - dy

# generate table and plot
res <- data.frame("K" = 0, "X" = 0)
plot(mypattern, main="Random Quadrant Sample Approach")
for (i in 1:(xdim*ydim)) {
    # generate random x and y
    x = runif(1, min=-80.52, max=x_top)
    y = runif(1, min=39.72, max=y_top)
    # count events
    K <- sum(s$LONGITUDE >= x & s$LONGITUDE <= x+dx & s$LATITUDE >= y & s$LATITUDE <= y+dy)
    rect(x, y, x+dx, y+dy, border="red")
    # create new row if K is not in the table, otherwise increase 1
    if (K %in% res$K) {
        res$X[res$K==K] <- res$X[res$K==K] + 1
    } else {
        res <- rbind(res, data.frame("K"=K, "X" = 1))
    }
}

n <- 156259
quadrat_num <- xdim * ydim
mean <- n / quadrat_num
res <- cbind(res, res$K - mean, (res$K - mean)^2, res$X * (res$K - mean)^2)
names(res) <- c("K", "X", "V3", "V4", "V5")
VMR_random <- sum(res$V5)/(sum(res$X) - 1)/mean
write.csv(res, file = "random.csv")
