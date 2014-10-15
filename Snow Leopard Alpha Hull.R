#--------------------------------
# Project title: Snow leopards
# Script title: Snow Leopard Alpha plots
# Purpose: Creates plots of snow leopard land use
#--------------------------------

convexhull<-function(Data, COL, NAME, minlat, maxlat, minlong, maxlong){
    
    Data<-unique(Data[,which(names(Data) %in% c("Longitude","Latitude"))])
    l<-dim(Data)[1]
    Data<-Data[sample(1:l, l, replace=F),]
    # creates an alpha hull object
    chull.obj <- chull(x=Data$Longitude,y=Data$Latitude)
    # Creates a plot and saves 
    pdf(paste("ConvexHulls",NAME,".pdf",sep=""))
      plot( Data[c(chull.obj,chull.obj[1]),2:1],
            col=COL,type="l",
            xlim=c(minlong,maxlong),ylim=c(minlat,maxlat),
            main=NA,
            sub=paste("AnimID = ",NAME,"; Area = ", round(earth.poly(Data[chull.obj,2:1])$area,2), "km^2",sep="")
      )
    box()
    dev.off()
    
}

ConvexIndivid<-function(Data, COLS, minlat, maxlat, minlong, maxlong){
  IDS<-sort(unique(Data$AnimID))
  for(i in 1:length(IDS)){
    tryCatch(
      convexhull(Data=Data[which(Data$AnimID %in% IDS[i]),], COL=COLS[i], NAME=IDS[i],
                minlat=minlat, maxlat=maxlat, minlong=minlong, maxlong=maxlong)
      , error = function(e) {print(paste("Error in ",IDS[i]))})
  }
}



alphahull<-function(Data, COL, NAME, minlat, maxlat, minlong, maxlong, ALPHA){
    
    Data<-unique(Data[,which(names(Data) %in% c("Longitude","Latitude"))])
    l<-dim(Data)[1]
    Data<-Data[sample(1:l, l, replace=F),]
    # creates an alpha hull object
    ahull.obj <- ahull(x=Data$Longitude,y=Data$Latitude, alpha = ALPHA)
    # Creates a plot and saves 
    pdf(paste("AlphaHulls",NAME,"Alpha=",ALPHA,".pdf",sep=""))
    plot(ahull.obj,col=COL,xlim=c(minlong,maxlong),ylim=c(minlat,maxlat),
         main=NA,sub=paste("AnimID = ",NAME,"; Alpha = ",ALPHA,sep=""))
    box()
    dev.off()
    
}

alphahullindivid<-function(Data, COLS, minlat, maxlat, minlong, maxlong, ALPHA){
  IDS<-sort(unique(Data$AnimID))
  for(i in 1:length(IDS)){
    tryCatch(
      alphahull(Data=Data[which(Data$AnimID %in% IDS[i]),], COL=COLS[i], NAME=IDS[i],
                ALPHA=ALPHA,
                minlat=minlat, maxlat=maxlat, minlong=minlong, maxlong=maxlong)
      , error = function(e) {print(paste("Error in ",IDS[i]))})
  }
}


Contours<-function(Data, COL, NAME, FACTOR){
  d<-round(Data[,which(names(Data) %in% c("Longitude","Latitude"))]*FACTOR)
  
  Longitude<-unlist(lapply(d$Longitude,factor,levels=c(min(d$Longitude):max(d$Longitude))))
  Latitude<-unlist(lapply(d$Latitude,factor,levels=c(min(d$Latitude):max(d$Latitude))))
  long<-c(min(d$Longitude):max(d$Longitude))
  lat<-c(min(d$Latitude):max(d$Latitude))
  
  t<-table(Latitude,Longitude)  
  pdf(paste("Contour",NAME,".pdf",sep=""))
  image(z=t(t),col = paste(COL,as.hexmode(c(0, 10:25*10+5)),sep=""),main=NA,sub=NAME,axes=FALSE) #as.hexmode creates the transpancy term
  box()
  axis(side=1,at=c(0:4)/4, label=quantile(long)/FACTOR)
  axis(side=2,at=c(0:4)/4, label=quantile(lat)/FACTOR)
  contour(t(t),add=TRUE,nlevels=5)
  dev.off()
}

Contoursindivid<-function(Data, COLS, FACTOR){
  IDS<-sort(unique(Data$AnimID))
  for(i in 1:length(IDS)){
    tryCatch(
      Contours(Data=Data[which(Data$AnimID %in% IDS[i]),], COL=COLS[i], NAME=IDS[i], FACTOR=FACTOR)  
      , error = function(e) {print(paste("Error in ",IDS[i]))})
  }
}

AlphaShapeSize<-function(Data){
    d<-unique(Data[,which(names(Data) %in% c("UTM_X_Zone","UTM_Y_Zone"))])
    #d$UTM_Y_Zone<-d$UTM_Y_Zone-4780000
    #d$UTM_X_Zone<-d$UTM_X_Zone-550000
    l<-dim(d)[1]
    d<-d[sample(1:l, l, replace=F),]
    par(mfrow=c(3,3))
    #d<-d[1000:2000,]
  for(i in 0:8){
    indent<-10000
    ahull.obj <- ahull(x=d$UTM_X_Zone,y=d$UTM_Y_Zone, alpha = 100000-(i*indent))
    print(areaahull(ahull.obj))
    plot(ahull.obj)
  }
  
}