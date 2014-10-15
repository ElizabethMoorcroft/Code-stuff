locationsAndDensitiesIndivid<-function(Data, minlat=MINLAT, maxlat=MAXLAT, minlon=MINLON, maxlon=MAXLON,COLS=COL,ALPHA=0.25){
  
  animals<-sort(unique(Data$AnimID))
    
  for(i in 1:length(animals)){
    tempdata<-Data[which(Data$AnimID==animals[i]),]
    pdf(paste("LocationsAndDensities",animals[i],".pdf",sep=""))
    tryCatch(
      locationsAndDensities(tempdata, animals[i] ,MINLAT=minlat, MAXLAT=maxlat, MINLON=minlon, MAXLON=maxlon,COLS=COL[i],ALPHA=ALPHA)
    , error = function(e) {print(paste("Error in ",animals[i]))})
    dev.off()
    }
}

locationsAndDensities<-function(Data, NAME,MINLAT=MINLAT, MAXLAT=MAXLAT, MINLON=MINLON, MAXLON=MAXLON,COLS=COL[1],ALPHA=ALPHA){
  
  tempdata<-unique(Data[,6:7])
  l<-dim(tempdata)[1]
  tempdata<-tempdata[sample(1:l, l, replace=F),]
  
  par(oma=c(3,3,3,3))
  layout(matrix(c(1,1,2,1,1,3,1,1,4),3,3,byrow=T))
  chull.obj <- chull(x=tempdata$Longitude,y=tempdata$Latitude)
  ahull.obj <- ahull(x=tempdata$Longitude,y=tempdata$Latitude, alpha = ALPHA)
  #areahull(ahull.obj)
  plot(ahull.obj, col = c(COLS, rep(NA, 5)), xlim=c(MINLON,MAXLON), ylim=c(MINLAT,MAXLAT),
         main=NA,
        sub=paste("AnimID = ",NAME,"; Area of convex hull = ", 
                  round(earth.poly(tempdata[chull.obj,2:1])$area,2), 
                  "km^2",sep=""),
       xlab="Longitude",ylab="Latitude"
       )
  points( x=Data$Longitude,y=Data$Latitude,
          col=COLS, pch="."
      )
  lines( tempdata[c(chull.obj,chull.obj[1]),2:1],
           col=COLS,lty=2)
  legend(x=MINLON,y=MAXLAT,
         legend = c("Locations","Convex Hull",paste("Alpha Hull (Alpha:",ALPHA,")",sep="")),
         lty=c(NA,2,1),col=c(COLS,COLS,COLS),pch=c(".",NA,NA)
         )
  box()
  
  plot(density(Data[,which(names(Data) %in% "Dist_Difference")],na.rm=T),
       main="Displacement",
       xlab="Meters",
       col=COLS)
  plot(density(Data[,which(names(Data) %in% "Speed_mps")],na.rm=T),
       main="Minimum speed",
       xlab="Meters/Second",
       col=COLS)
  
  plot(density(Data[,which(names(Data) %in% "ChangeAngle")],na.rm=T),
       main="Change in bearing",
       xlab="Radians",
       col=COLS)

}
