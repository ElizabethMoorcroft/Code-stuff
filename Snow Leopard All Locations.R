#--------------------------------
# Project title: Snow leopards
# Script title: Snow Leopard All locations
# Purpose: Creates plots of snow leopard land use
#--------------------------------

PlotAllLocations<-function(Data, minlat,maxlat,minlon,maxlon,COLS){
  
  pdf("AllLocationsByIndividual.pdf")
  plot(0,0,
       #xlim=c(minlon,maxlon), ylim=c(minlat,maxlat)
       #,xlab="Longitude",ylab="Latitude"
       xlim=c(min(Data$UTM_X_Zone,na.rm=T),max(Data$UTM_X_Zone,na.rm=T)),ylim=c(min(Data$UTM_Y_Zone,na.rm=T),max(Data$UTM_Y_Zone,na.rm=T))
       ,xlab=NA,ylab=NA
       )
  
  animals<-sort(unique(Data$AnimID))
  for(i in 1:length(animals)){
    tempdata<-Data[which(Data$AnimID %in% animals[i]),]
    points(x=tempdata$UTM_X_Zone,y=tempdata$UTM_Y_Zone,col=COLS[i],pch=".")
  }
  dev.off()
}