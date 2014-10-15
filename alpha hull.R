Datatemp<-unique(Data[,which(names(Data) %in% c("Longitude","Latitude"))])
l<-dim(Datatemp)[1]
Datatemp<-Datatemp[sample(1:l, l, replace=F),]
    # creates an alpha hull object
ahull.obj <- ahull(x=Datatemp$Longitude,y=Datatemp$Latitude, alpha = 1)

areaahull(ahull.obj)
plot(ahull.obj)
for(i in 1:1){#dim(ahull.obj$arcs)[1]){
  rad<-ahull.obj$arcs[i,3]
  clat<-ahull.obj$arcs[i,2]
  clon<-ahull.obj$arcs[i,1]
  xlat<-Data[ahull.obj$arcs[i,7],]$Latitude
  xlon<-Data[ahull.obj$arcs[i,7],]$Longitude
  ylat<-Data[ahull.obj$arcs[i,8],]$Latitude
  ylon<-Data[ahull.obj$arcs[i,8],]$Longitude
  #print(paste("xlat=",xlat, " xlon=",xlon, " clon=", ahull.obj$arcs[i,1] , " clat=", ahull.obj$arcs[i,2], sep=""))
  
  temp1<-atan2(ahull.obj$arcs[i,5],ahull.obj$arcs[i,4])
  temp2<-temp1-ahull.obj$arcs[i,6]
  print(paste(temp1,temp2))
  rx<-sqrt((ahull.obj$arcs[i,1]-xlon)^2+(ahull.obj$arcs[i,2]-xlat)^2)
  ry<-sqrt((ahull.obj$arcs[i,1]-ylon)^2+(ahull.obj$arcs[i,2]-ylat)^2)
  #print(paste("rx=",rx, " ry=",ry,sep=""))
  
}