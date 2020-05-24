getwd()
setwd("D:/workspace/r/INFSCI 2809 Spatial Data Analytics/Project 1")

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
library(dplyr)
library(plyr)

filename <- "pgh_streets/pgh_streets/pgh_streets.shp"
s <- shapefile(filename, stringsAsFactors=T)
help("shapefile")
summary(s)
n <- length(s)
plot(s, col=rainbow(n))

s_Rd <- s[s$FETYPE=="Rd",]
nrow(s_Rd)
# total number of road segments is 1195
summary(s_Rd)
# Calculate min, max and mean
s_Rd_filtered <- s_Rd[s_Rd$LENGTH >= mean(s_Rd$LENGTH),]
plot(s_Rd_filtered)

summary(s)
s_filtered <- s[s$LENGTH >= mean(s$LENGTH),]
plot(s_filtered)


#################################################

summary(lnd)
summary(stations)
plot(stations)
plot(lnd)
names(lnd)
names(stations)
plot(stations$Value)

aggregate(lnd[,c(names(lnd))], by=list(stations$Value[1:33]), FUN=mean, na.rm=T)
help("aggregate")
dim(lnd)
dim(stations)
stations$Value[1:33]
lnd@data
stations@data
stations[stations$NAME.x=="Bromley Station",]
stations$NAME.x
head(stations$NAME.x)
head(stations$NAME.y)
head(lnd$name)
head(lnd)
head(stations)

summary(stations)

gsub(" Station", "", stations$NAME.x)

map <- merge(lnd, gsub(" Station", "", stations$NAME.x), by.x="name", by.y="NAME.x")

plot(stations[,c("GIS.y")])
plot(stations[,c("GIS.y", "coords.x1")])
plot(stations[,c("coords.x2")])

summary(stations[,c("GIS.y")])

plot(lnd[,c("name")])
names(lnd)

lnd@polygons
stations@coords
plot(stations@coords)
plot(stations)

##########################

par(mfrow=c(1,2))
plot(lnd)
plot(stations)
par(mfrow=c(1,1))
plot(lnd)
points(stations)
plot(stations@coords)

stations_agg <- aggregate(stations[, c("coords.x1", "coords.x2")], by=list(stations$CODE.x), mean)
names(stations)
head(stations)
plot(stations_agg)
points(stations)
head(stations)
head(stations_agg)
summary(stations$CODE.x)
stations$CODE.x
summary(stations_agg)
summary(stations)
nrow(stations_agg)
plot(stations_agg)
stations_agg
ggplot(data = stations_agg, aes(x=stations_agg$coords.x1, y=stations_agg$coords.x2))+geom_point()
plot(stations_agg@coords)
plot(stations_agg@coords$`2`)

head(stations)


#########################################

head(stations[c("Value")])
head(stations$Value)
plot(stations[c("Value")])
summary(stations[c("Value")])
head(lnd)
head(stations)

stations[c("coords.x1", "coords.x2")]
plot(over(lnd, stations[c("coords.x1", "coords.x2")], fun=mean))

lnd@polygons[1]

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load("lnd.RData")
load("stations.RData")

stations_agg <- over(lnd, stations[c("coords.x1", "coords.x2", "Value")], fn = mean)
plot(lnd)
points(stations_agg, col=stations_agg$Value)

lnd_stations <- lnd

lnd_stations@data <- cbind(lnd@data, stations_agg)

head(lnd_stations)

lnd_stations <- merge(lnd, stations_agg)

colnames(lnd_stations@data)[1] <- "id"

head(lnd_stations@data)

lnd_stations.points <- fortify(lnd_stations, region = "id")
lnd_stations.points <- join(lnd_stations.points, lnd_stations@data, by="id")
lnd_stations.points

ggplot(data = lnd_stations.points, aes(x = long, y = lat, group = id, fill = Value)) + geom_polygon() + ggtitle("Value for regions") +
    theme(plot.title = element_text(hjust = 0.5))



north.arrow(xb=560000, yb=200000, len=10000, lab="N")           

help("north.arrow")


help("join")


plot(lnd)
points(stations_agg, pch = 16, col=as.factor(stations_agg$Value))
legend("topright", legend = levels(as.factor(stations_agg$Value)), cex=0.9, pch = 16, pt.cex = 1, col=as.factor(stations_agg$Value))

ggplot(lnd@polygons)


plot(lnd)
points(stations_agg, pch = 16, col=rainbow(length(stations_agg$Value)))
legend("topright", legend = rainbow(length(stations_agg$Value)), cex=0.9, pch = 16, pt.cex = 1, col=as.factor(stations_agg$Value))

over(lnd, stations[c("coords.x1", "coords.x2", "Value")], returnList = TRUE, fn = mean)


lnd.points <- fortify(lnd, region = "ons_label")

lnd.points

head(lnd)

lnd@data

head(lnd.points)

ggplot(data = lnd.points, aes(x = long, y = lat, group = id, fill = id)) + geom_polygon()

head(stations_agg)



plot(lnd)
points(stations_agg, col=as.factor(stations_agg$Value))

ggplot(lnd@polygons)

ggplot(lnd@data) + geom_polygon()
length(stations_agg$Value)


plot(lnd)
qplot(x = stations_agg$coords.x1, y = stations_agg$coords.x2, data = stations_agg, color = as.factor(stations_agg$Value))

summary(as.factor(stations$Value))

head(stations_agg)

plot(lnd)
points(stations)

coordinates(stations)

stations_agg <- over(lnd, coordinates(stations), fn = mean)
plot(lnd)
points(stations_agg)

stations.coordinates <- stations[c("coords.x1", "coords.x2")]
head(stations.coordinates)
coordinates(stations.coordinates) <- ~x+y

head(stations)
help("over")

lnd@polygons[1][[1]]@Polygons
geom_polygon(data = lnd@polygons[1][[1]]@Polygons)

plot(coordinates(lnd@polygons))
head(lnd)

plot(lnd)
points(stations)
plot(lnd)
points(stations_agg)


#########################################

load("lnd.RData")
load("stations.RData")

par(mfrow=c(2,2))

head(stations)
lm_before <- lm(Value ~ coords.x1 + coords.x2, data = stations)
summary(lm_before)
plot(lm_before)

lm_after <- lm(Value ~ coords.x1 + coords.x2, data = stations_agg)
summary(lm_after)
plot(lm_after)

lm_before <- lm(Value ~ coords.x1, data = stations)
summary(lm_before)

lm_after <- lm(Value ~ coords.x1, data = stations_agg)
summary(lm_after)
