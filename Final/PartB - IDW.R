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

filename <- "DataFiles_FinalExam/2_Community/2_Community.shp"
df <- readOGR(filename)
summary(df)
dim(df)
head(df, n=12L)
plot(df)


x <- df$S_Long
y <- df$S_Lat
centroidX <- coordinates(df)[,1]
centroidY <- coordinates(df)[,2]
nx <- length(x)
ncenter <- length(centroidX)

k <- 1

Rslt <- list()
ctr <- data.frame()
for (j in 1:nx) {
    data <- data.frame()
    for (i in 1:nx) {
        distance<-sqrt((x[i]-centroidX[j])^2+(y[i]-centroidY[j])^2)
        distance<-as.numeric(distance)
        data<-rbind(data,
                    data.frame(x=x[i],y=y[i],distance=distance, Intensity=df$Intensity[i],centroidX=centroidX[j],centroidY=centroidY[j]))
    }
    data <- data[, c("Intensity", "x", "y", "distance")]
    names(data)[1] <- "z"
    inverse_distance <- 1 / data$distance^k
    inverse_distance
    weight <- (inverse_distance) / sum(inverse_distance)
    weight
    weight_z<-data$z*weight
    weight_z
    IDW.table<-cbind(data,data.frame("Inverse distance"=inverse_distance,weight=weight,"weighted value"=weight_z))
    IDW.table
    z<-sum(IDW.table$weighted.value)
    ctr<-rbind(ctr,data.frame(x=centroidX[j],y=centroidY[j],z=z))
    Rslt[[j]]<-IDW.table
}
Rslt
ctr

IDW<-as.data.frame(ctr)
IDW
coordinates(IDW)=~x+y
x.range<-as.numeric(bbox(IDW)[1,])
y.range<-as.numeric(bbox(IDW)[2,])

grd<-expand.grid(x=seq(from=x.range[1], to=x.range[2], by=0.01), y=seq(from=y.range[1],to=y.range[2], by=0.01))# expand points to grid
coordinates(grd)<-~x+y
gridded(grd)<-TRUE



dataset <- df[, c("S_Long", "S_Lat", "Intensity")]
dataset <- as.data.frame(dataset)
dataset
names(dataset) <- c("x","y","z")
sensor.length<-nrow(dataset)
sensor.length
centers.data<-data.frame(x=centroidX,y=centroidY,z=NA)
dataset<-rbind(dataset,centers.data)
rownames(dataset)<-1:nrow(dataset)
dataset

vgm1=dataset[1:sensor.length,]
coordinates(vgm1)=~x+y
vgm1<-autofitVariogram(z~x+y,vgm1,model="Exp")
vgm1
plot(vgm1)

# function
gamma<-function(distance)
{
    result=5447+(5447 - 3835)*(1-exp(-(abs(distance)/0.12)))
    return(result)
}

#calculate D
points<-nrow(dataset)
dist<-matrix(nrow=points+1,ncol=points+1)
dim(dist)
for(i in 1:points){
    for(j in 1:points){
        distance<-sqrt((dataset[i,1]-dataset[j,1])^2+(dataset[i,2]-dataset[j,2])^2)
        distance<-as.numeric(distance)
        dist[i,j]=distance
        dist[j,i]=distance
    }
}

A<-dist
A<-A[1:(sensor.length+1),1:(sensor.length+1)]
dim(A)
A
# functions, need discussing
gama.a<-gamma(A)
gama.a[sensor.length+1,]=c(rep(1,sensor.length),0)
gama.a[,sensor.length+1]=c(rep(1,sensor.length),0)
diag(gama.a)<-0
gama.a
idx<-which(is.na(dataset$z))#get the index whose value is NA
idx

for(k in idx){
    #calcualte b and d
    d<-dist[k,1:sensor.length]
    d
    # functions, need discussing
    b<-gamma(d)
    b[length(b)+1]=1
    b
    w<-solve(gama.a,b)
    z<-w[1:sensor.length]*dataset$z[1:sensor.length]
    z<-sum(z)
    dataset$z[k]<-z
}
dataset
cat('the centers\' info are')

result<-dataset[idx,]
result
# OK result
dat<-as.data.frame(result)
dat
coordinates(dat)=~x+y
vgm2<-autofitVariogram(z~x+y,dat,model="Exp")
vgm2
plot(vgm2)
nugget2=18
sill2=60
range2=0.15

m<-vgm(range=range2, "Exp", nugget=nugget2, psill=sill2)
dat.krg<-krige(dat$z~1, dat, grd, model=m)
length(dat.krg)

#---------------------------------------------------------------#

x <- df$S_Long
y <- df$S_Lat
centroidX <- coordinates(df)[,1]
centroidY <- coordinates(df)[,2]
nx <- length(x)
ncenter <- length(centroidX)


# r=0.009 k=0.2

indexes <- data.frame()
tbl <- data.frame()
best.mse <- 1e+10

for (k in seq(0.1, 0.5, by=0.05)) {
    for (r in seq(0.008, 0.01, by=0.001)) {
        Rslt <- list()
        ctr <- data.frame()
        for (j in 1:nx) {
            data <- data.frame()
            for (i in 1:nx) {
                distance<-sqrt((x[i]-centroidX[j])^2+(y[i]-centroidY[j])^2)
                distance<-as.numeric(distance)
                data<-rbind(data,
                            data.frame(x=x[i],y=y[i],distance=distance, Intensity=df$Intensity[i],centroidX=centroidX[j],centroidY=centroidY[j]))
            }
            data <- data[, c("Intensity", "x", "y", "distance")]
            names(data)[1] <- "z"
            inverse_distance <- 1 / data$distance^k
            inverse_distance
            weight <- (inverse_distance) / sum(inverse_distance)
            weight
            weight_z<-data$z*weight
            weight_z
            IDW.table<-cbind(data,data.frame("Inverse distance"=inverse_distance,weight=weight,"weighted value"=weight_z))
            IDW.table
            z<-sum(IDW.table$weighted.value)
            ctr<-rbind(ctr,data.frame(x=centroidX[j],y=centroidY[j],z=z))
            Rslt[[j]]<-IDW.table
        }
        Rslt
        ctr
        
        IDW<-as.data.frame(ctr)
        coordinates(IDW)=~x+y
        x.range<-as.numeric(bbox(IDW)[1,])
        y.range<-as.numeric(bbox(IDW)[2,])
        
        grd<-expand.grid(x=seq(from=x.range[1], to=x.range[2], by=r), y=seq(from=y.range[1],to=y.range[2], by=r))# expand points to grid
        coordinates(grd)<-~x+y
        gridded(grd)<-TRUE
        dat.idw<-gstat::idw(ctr$z~1, locations=IDW, newdata=grd)
        dat.idw
        
        dat.krg.df <- data.frame(x=dat.krg@coords[,1], y=dat.krg@coords[,2], v=dat.krg$var1.pred)
        dat.idw.df <- data.frame(x=dat.idw@coords[,1], y=dat.idw@coords[,2], v=dat.idw$var1.pred)
        dat.idw.df <- dat.idw.df[dat.idw.df$x %in% dat.krg.df$x & dat.idw.df$y %in% dat.krg.df$y,]
        dat.krg.df <- dat.krg.df[dat.krg.df$x %in% dat.idw.df$x & dat.krg.df$y %in% dat.idw.df$y,]
        mse <- sum((dat.idw.df$v - dat.krg.df$v)^2) / nrow(dat.idw.df)
        if (mse < best.mse) {
            best.mse <- mse
            best.r <- r
            best.k <- k
            
            OP<-par(mar=c(0,0,0,0))
            png(filename = paste('IDW/IDW r=', r, ' k=', k, '.png', sep = ''), width = 1280, height = 1280)
            image(dat.idw,"var1.pred",col=terrain.colors(20))
            contour(dat.idw,"var1.pred", add=TRUE, nlevels=10, col="#656565")
            plot(IDW, add=TRUE, pch=16, cex=0.5)
            text(coordinates(df), as.character(round(ctr$z,1)), pos=4, cex=0.8, col="blue")
            north.arrow(xb= -79.05, yb= 41.35, len=0.01, lab="N",col='Grey')
            map.axes(cex.axis=1)
            map.scale(x = -79.25, y = 40.25, ratio=TRUE, relwidth=0.1, metric=FALSE)
            title(xlab = 'x', ylab = 'y', main = 'IDW Interpolated Surface Map')
            par(OP)
            dev.off()
        }
        
        
        print(paste('r= ', r, ' k= ', k, sep = ''))
        
        indexes <- rbind(indexes,
                         data.frame(r = r, k = k))
        
        tbl <- rbind(tbl,
                     summary(dat.idw$var1.pred)[1:6])
    }
}
best.mse
best.r
best.k

names(tbl) <- names(summary(dat.idw$var1.pred))
tbl <- cbind(indexes, tbl)
tbl

write.csv(tbl, file = 'IDW Summary Table.csv')
