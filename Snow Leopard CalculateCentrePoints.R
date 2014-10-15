
#---------------
# Taken from this thread: http://stackoverflow.com/questions/6671183/calculate-the-center-point-of-multiple-latitude-longitude-coordinate-pairs
#---------------

CentrePoints<-function(Data){

  total = dim(Data)[1] # Number of values

  X = 0; Y = 0;Z = 0;

  for(i in 1:total){
    lat = Data$Latitude[i] * pi / 180;
    lon = Data$Longitude[i] * pi / 180;
    x = cos(lat) * cos(lon);
    y = cos(lat) * sin(lon);
    z = sin(lat);

    X = X+ x;
    Y = Y+ y;
    Z = Z+ z;
  }

  X = X / total
  Y = Y / total
  Z = Z / total

  Lon = atan2(Y, X);
  Hyp = sqrt(X * X + Y * Y);
  Lat = atan2(Z, Hyp);
  
  return (list(Lon = Lon*180/pi,Lat = Lat*180/pi))
}



distancelocationstoCentrePointsIndivid<-function(Data,COLS,MINLON,MAXLON,MINLAT,MAXLAT){
  animals<-sort(unique(Data$AnimID))
  for(i in 1:length(animals)){
    tempdata<-Data[which(Data$AnimID %in% animals[i]),]
    distancelocationstoCentrePoints(tempdata,animals[i],COL=COLS[i],MINLON=MINLON,MAXLON=MAXLON,MINLAT=MINLAT,MAXLAT=MAXLAT)
  }
}

distancelocationstoCentrePoints<-function(Data,Name,COL,MINLON,MAXLON,MINLAT,MAXLAT){
  cp<-CentrePoints(Data)
  NoDataPoints<-dim(Data)[1]
  diff<-vector(length = NoDataPoints)
  for(i in 1:NoDataPoints){
    diff[i]<-sqrt((Data$Longitude[i]- cp$Lon)^2+(Data$Latitude[i]- cp$Lat)^2)
  }
  pdf(paste("DistanceFromCentrePoint",Name,".pdf",sep=""))
  plot(density(diff),col=COL)
  dev.off()
  pdf(paste("AllPoints",Name,".pdf",sep=""))
    plot(Data$Longitude, Data$Latitude, col=COL,pch=".",xlim=c(MINLON,MAXLON),ylim=c(MINLAT,MAXLAT))
    points(cp$Lon,cp$Lat,col="Black",pch=8)
  dev.off()
  }