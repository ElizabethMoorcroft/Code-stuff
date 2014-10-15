imageplotangle<-function(mindist,maxdist, data, roundangle, rounddist,Name,ylimmax){
  pdf(paste(Name,".pdf",sep=""))
    data$roundabsangle<-round(abs(data$ChangeAngle*roundangle))/roundangle
    data$dist<-data$Dist_Difference
    d<-data[which(data$Dist_Difference<maxdist),]
    d<-d[which(d$Dist_Difference>mindist),]
     print(summary(d$dist))
    d$dist<-round(log(d$dist)*rounddist)/rounddist
    print(summary(d$dist))
    z<-table(d$roundabsangle,d$dist)
    z[z==0]<-NA;
    print(dim(z))
    x<-sort(unique(d$roundabsangle)[which(!is.na(unique(d$roundabsangle)))])
    y<-sort(unique(d$dist)[which(!is.na(unique(d$dist)))])
    print(y)
    filled.contour(x,y,z,xlab="Change in angle (radians)", 
          ylab= "log of displacement",#ylim=c(0,ylimmax),
          color.palette=colorRampPalette(brewer.pal(n=9, name="YlOrRd"),space="Lab")
         ,cex.lab=1.5
          )
  dev.off()
}

imageplotDTvsTp1<-function(data, Var,roundval){
  # Distance at t, vs distance at t+1
  heatmapmat<-matrix(ncol=2,nrow=dim(data)[1]-1)
  heatmapmat[,1]<-data[1:(dim(data)[1]-1),which(names(data) %in% Var)]
  heatmapmat[,2]<-data[2:dim(data)[1],which(names(data) %in% Var)]
  zeros<-which(heatmapmat[,1]==0 )
  heatmapmat[zeros,1]<-1
  zeros<-which(heatmapmat[,2]==0 )
  heatmapmat[zeros,2]<-1
  #heatmapmat<-heatmapmat[-delete,]
  #heatmapmat<-abs(heatmapmat)
  heatmapmat<-round(log(heatmapmat)*roundval)/roundval
  z<-table(heatmapmat[,1],heatmapmat[,2])
  z[z==0]<-NA;z<-log(z)
  x<-sort(unique(heatmapmat[,1]))
  y<-sort(unique(heatmapmat[,2]))
  
  #image(x,y,z,col=rev(heat.colors(12)))
  filled.contour(x,y,z
                ,color.palette=colorRampPalette(brewer.pal(n=9, name="YlOrRd"),space="Lab")
                 ,main=""
                 ,zlim=c(0,max(z,na.rm=T))
                 ,xlab="log of distance moved at time t [log(meters)]"
                 ,ylab="log of distance moved at time t+1 [log(meters)]"
                 ,cex.lab=1.5)
}


imageplotSTvsTp1<-function(data, Var,roundval){
  # Distance at t, vs distance at t+1
  heatmapmat<-matrix(ncol=2,nrow=dim(data)[1]-1)
  heatmapmat[,1]<-data[1:(dim(data)[1]-1),which(names(data) %in% Var)]
  heatmapmat[,2]<-data[2:dim(data)[1],which(names(data) %in% Var)]
  #heatmapmat<-heatmapmat[-delete,]
  #heatmapmat<-abs(heatmapmat)
  heatmapmat[which(heatmapmat[,1]==0),1]<-0.0001
  heatmapmat[which(heatmapmat[,2]==0),2]<-0.0001
  heatmapmat<-round(log(heatmapmat)*roundval)/roundval
  z<-table(heatmapmat[,1],heatmapmat[,2])
  z[z==0]<-NA; z<-log(z)
  x<-sort(unique(heatmapmat[,1]))
  y<-sort(unique(heatmapmat[,2]))
  
  #image(x,y,z,col=rev(heat.colors(12)))
  filled.contour(x,y,z
                ,color.palette=colorRampPalette(brewer.pal(n=9, name="YlOrRd"),space="Lab")
                 ,main=""
                 ,zlim=c(0,max(z,na.rm=T))
                 ,xlab="log speed at time t [log(m/s)]"
                 ,ylab="log speed at time t+1 [log(m/s)]"
                 ,cex.lab=1.5)
}


imageplotATvsTp1<-function(data, Var,roundval){
  # Distance at t, vs distance at t+1
  heatmapmat<-matrix(ncol=2,nrow=dim(data)[1]-1)
  heatmapmat[,1]<-data[1:(dim(data)[1]-1),which(names(data) %in% Var)]
  heatmapmat[,2]<-data[2:dim(data)[1],which(names(data) %in% Var)]
  heatmapmat<-abs(heatmapmat)
  heatmapmat<-round((heatmapmat)*roundval)/roundval
  z<-table(heatmapmat[,1],heatmapmat[,2])
  z[z==0]<-NA; z<-log(z)
  x<-sort(unique(heatmapmat[,1]))
  y<-sort(unique(heatmapmat[,2]))
  #image(x,y,z,col=rev(heat.colors(12)))
  filled.contour(x,y,z
                ,color.palette=colorRampPalette(brewer.pal(n=9, name="YlOrRd"),space="Lab")
                 ,main=""
                 ,xlab="Change of direction at time t [radians]"
                 ,ylab="Change of direction moved at time t+1 [radians]"
                 ,cex.lab=1.5)
}


