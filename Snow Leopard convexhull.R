#--------------------------------
# Project title: Snow leopards
# Script title: Snow Leopard Convex hulls/AlphaShapes
# Purpose: To look at the collar data 
#--------------------------------

#--- Libraries
library(ggplot2)
library(alphahull)
library(RColorBrewer)

#--- Directories
DataDir<- "/Volumes/elizabethmoorcroft/Documents/PhD stuff/Snow leopard/Data"
CodeDir<- "/Volumes/elizabethmoorcroft/Documents/PhD stuff/Snow leopard"
SaveDir<- "/Volumes/elizabethmoorcroft/Documents/PhD stuff/Snow leopard/Graphs"

#--- Colours
COL<-c(brewer.pal(n=12, name="Set3"),brewer.pal(n=9, name="Set1"),"black")

#--- Source Code
setwd(CodeDir)
source("AlphaHullToShape.R") 
# Note: Doesn't work in all situations needs to be fixed
#       Used to change an alphashape object into a shape
#       obj<-ahull(data)
#       shape<-ah2sp(obj)

#---Fills in the convex hull 
find_hull <- function(df) df[chull(df$Latitude, df$Longitude), ]
hulls <- ddply(Data, "AnimID", find_hull)
plot <- ggplot(data = Data, aes(y = Latitude, x = Longitude, colour=AnimID, fill = AnimID)) +
geom_point() + 
geom_polygon(data = hulls, alpha = 0.2) +
labs(x = "Longitude", y = "Latitude")
plot

# --- Try to make work with ahull
Animals<-unique(Data$AnimID)

minlat<-min(Data$Latitude,na.rm=T)
maxlat<-max(Data$Latitude,na.rm=T)
minlon<-min(Data$Longitude,na.rm=T)
maxlon<-max(Data$Longitude,na.rm=T)

plot(type="n",0,0,ylim=c(minlat,maxlat),xlim=c(minlon,maxlon))

for(i in 1:length(Animals)){
#for(i in 1){
  print(i)
  d<-Data[which(Data$AnimID==Animals[i]),]
  d<-unique(d[,c(6:7)])
  d<-d[sample(dim(d)[1],dim(d)[1]),]
  
  setwd(SaveDir)
  pdf(paste("AlphaHullsForAnimID",Animals[i],".pdf",sep=""))
  par(mfrow=c(2,2))
  
  obj<-ahull(y=d$Latitude, x=d$Longitude, alpha=1)
  plot(obj, col=c(COL[i],NA,COL[i],NA),
       ylim=c(minlat,maxlat),xlim=c(minlon,maxlon),
       xlab="Longitude",ylab="Latitude",
       sub="Alpha=1")
  box()

  obj<-ahull(y=d$Latitude, x=d$Longitude, alpha=0.25)
  plot(obj, col=c(COL[i],NA,COL[i],NA),
       ylim=c(minlat,maxlat),xlim=c(minlon,maxlon),
       xlab="Longitude",ylab="Latitude",
       sub="Alpha=0.25")
  box()

  obj<-ahull(y=d$Latitude, x=d$Longitude, alpha=0.1)
  plot(obj, col=c(COL[i],NA,COL[i],NA),
       ylim=c(minlat,maxlat),xlim=c(minlon,maxlon),
       xlab="Longitude",ylab="Latitude",
       sub="Alpha=0.1")
  box()
  
  obj<-ahull(y=d$Latitude, x=d$Longitude, alpha=0.05)
  plot(obj, col=c(COL[i],NA,COL[i],NA),
      ylim=c(minlat,maxlat),xlim=c(minlon,maxlon),
      xlab="Longitude",ylab="Latitude",
      sub="Alpha=0.05")
  box()
  dev.off()
}





