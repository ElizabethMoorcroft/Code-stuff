#--------------------------------
# Project title: Snow leopards
# Script title: Snow Leopard envirnment altitude
# Purpose: Plot altitude
#--------------------------------


setwd(DataDir)
x<-raster("ETOPO1_Ice_g_gmt4.grd")
e<-extent(99.6,101.4,42.6,43.9)
rc <- crop(x, e)
col=(colorRampPalette(brewer.pal(9,"Greys")[1:9])(100))
animals<-sort(unique(Data$AnimID))

plot(rc,col=col)
for(i in 1:length(animals)){
  d<-Data[which(Data$AnimID==animals[i]),]
  points(d$Longitude,d$Latitude,pch=".",col=COL[i])
}