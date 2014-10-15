#--------------------------------
# Project title: Snow leopards
# Script title: Snow Leopard Explore
# Purpose: Exploratory plots  and descriptive stats of the data
#--------------------------------

PlotDensity<-function(Data,Variable,XLAB,savename=paste("Density",Variable,sep=""),YMIN,YMAX,XMIN,XMAX,Legx=XMIN,Legy=YMAX,adjust=1){
  PlotDensityAll(Data=Data,savename=savename,Variable=Variable,XLAB=XLAB,YMIN=YMIN,YMAX=YMAX,XMIN=XMIN,XMAX=XMAX,adjust=adjust)
  PlotDensitySex(Data=Data,savename=savename,Variable=Variable,XLAB=XLAB,YMIN=YMIN,YMAX=YMAX,XMIN=XMIN,XMAX=XMAX,Legx=Legx,Legy=Legy,adjust=adjust)
  PlotDensityIndividuals(Data=Data,savename=savename,Variable=Variable,XLAB=XLAB,YMIN=YMIN,YMAX=YMAX,XMIN=XMIN,XMAX=XMAX,Legx=Legx,Legy=Legy,adjust=adjust)
}

PlotDensitylog<-function(Data,Variable,XLAB,savename=paste("DensityLog",Variable,sep=""),YMIN,YMAX,XMIN,XMAX,Legx=XMIN,Legy=YMAX,adjust=1){
  PlotDensityAlllog(Data=Data,savename=savename,Variable=Variable,XLAB=XLAB,YMIN=YMIN,YMAX=YMAX,XMIN=XMIN,XMAX=XMAX,adjust=adjust)
  PlotDensitySexlog(Data=Data,savename=savename,Variable=Variable,XLAB=XLAB,YMIN=YMIN,YMAX=YMAX,XMIN=XMIN,XMAX=XMAX,Legx=Legx,Legy=Legy,adjust=adjust)
  PlotDensityIndividualslog(Data=Data,savename=savename,Variable=Variable,XLAB=XLAB,YMIN=YMIN,YMAX=YMAX,XMIN=XMIN,XMAX=XMAX,Legx=Legx,Legy=Legy,adjust=adjust)
  PlotDensityMonthlog(Data=Data,savename=savename,Variable=Variable,XLAB=XLAB,YMIN=YMIN,YMAX=YMAX,XMIN=XMIN,XMAX=XMAX,Legx=Legx,Legy=Legy,adjust=adjust)
  PlotDensitySeasonlog(Data=Data,savename=savename,Variable=Variable,XLAB=XLAB,YMIN=YMIN,YMAX=YMAX,XMIN=XMIN,XMAX=XMAX,Legx=Legx,Legy=Legy,adjust=adjust)

  }

PlotDensityAll<-function(Data,Variable,XLAB,savename=paste("Density",Variable,sep=""),YMIN,YMAX,XMIN,XMAX,adjust=1){
  colnumber<-which(colnames(Data) %in% Variable)
  pdf(paste(savename,"(All).pdf",sep=""))
  plot(density(Data[,colnumber],na.rm=T,adjust=adjust),type="l",cex.lab=1.5,cex.axis=1.5,lwd=2,col=COL[19],xlab=XLAB,ylab="Density",main=NA)
  dev.off()
}

PlotDensityAlllog<-function(Data,Variable,XLAB,savename=paste("DensityLog",Variable,sep=""),YMIN,YMAX,XMIN,XMAX,adjust=1){
  colnumber<-which(colnames(Data) %in% Variable)
  loggedvalues<-log(Data[,colnumber])
  pdf(paste(savename,"(All).pdf",sep=""))
  plot(density(loggedvalues,na.rm=T,adjust=adjust),type="l",lwd=2,cex.lab=1.5,cex.axis=1.5,col=COL[19],xlab=XLAB,ylab="Density",main=NA)
  dev.off()
}

PlotDensityIndividuals<-function(Data,Variable,XLAB,savename=paste("Density",Variable,sep=""),YMIN,YMAX,XMIN,XMAX,Legx,Legy,adjust=1){
  colnumber<-which(colnames(Data) %in% Variable)
  Animals<-sort(unique(Data$AnimID))
  # - Create plots 
  pdf(paste(savename,"(Individ).pdf",sep=""))
  plot(0,0,type="n",ylim=c(YMIN,YMAX),xlim=c(XMIN,XMAX),cex.lab=1.5,cex.axis=1.5,xlab=XLAB,ylab="Density")
  for(i in 1:length(Animals)){
    points(density(Data[which(Data$AnimID==Animals[i]),colnumber],na.rm=T,adjust=adjust),type="l",col=COL[i])
  }
  legend(x=Legx,y=Legy, legend=as.character(Animals),lty=rep(1,length(Animals)),col=COL[1:length(Animals)],cex=0.8,bg="white")
  dev.off()
}

PlotDensityIndividualslog<-function(Data,Variable,XLAB,savename=paste("DensityLog",Variable,sep=""),YMIN,YMAX,XMIN,XMAX,Legx,Legy,adjust=1){
  colnumber<-which(colnames(Data) %in% Variable)
  Data[,colnumber]<-log(Data[,colnumber])
  Animals<-sort(unique(Data$AnimID))
  # - Create plots 
  pdf(paste(savename,"(Individ).pdf",sep=""))
  plot(0,0,type="n",ylim=c(YMIN,YMAX),xlim=c(XMIN,XMAX),cex.lab=1.5,cex.axis=1.5,xlab=XLAB,ylab="Density")
  for(i in 1:length(Animals)){
    points(density(Data[which(Data$AnimID==Animals[i]),colnumber],na.rm=T,adjust=adjust),lwd=2,type="l",col=COL[i])
  }
  legend(x=Legx,y=Legy, legend=as.character(Animals),lty=rep(1,length(Animals)),col=COL[1:length(Animals)],cex=0.8,bg="white",lwd=2)
  dev.off()
}

PlotDensityMonthlog<-function(Data,Variable,XLAB,savename=paste("DensityLog",Variable,sep=""),YMIN,YMAX,XMIN,XMAX,Legx,Legy,adjust=1){
  colnumber<-which(colnames(Data) %in% Variable)
  Data[,colnumber]<-log(Data[,colnumber])
  months<-as.POSIXlt(Data$Date_Time)$mon
  # - Create plots 
  pdf(paste(savename,"(Month).pdf",sep=""))
  plot(0,0,type="n",ylim=c(YMIN,YMAX),xlim=c(XMIN,XMAX),xlab=XLAB,cex.lab=2,ylab="Density")
  for(i in 1:12){
    points(density(Data[which((months+1)==i),colnumber],na.rm=T,adjust=adjust),type="l",col=COL[i],lwd=2)
  }
  legend(x=Legx,y=Legy, legend=c("Jan", "Feb", "Mar","Apr","May", "Jun", "Jul","Aug","Sep","Oct","Nov","Dec")
    ,lty=rep(1,12),col=COL[1:12],cex=0.8,bg="white",lwd=2)
  dev.off()
}

PlotDensitySeasonlog<-function(Data,Variable,XLAB,savename=paste("DensityLog",Variable,sep=""),YMIN,YMAX,XMIN,XMAX,Legx,Legy,adjust=1){
  colnumber<-which(colnames(Data) %in% Variable)
  Data[,colnumber]<-log(Data[,colnumber])
  months<-as.POSIXlt(Data$Date_Time)$mon
  season<-vector(length=length(months))
  for(i in 1:length(months)){ 
    if(months[i]<4){season[i]<-"winter"}
    else if (months[i]>10) {season[i]<-"winter"}
    else if (months[i]>=4 & months[i]<=10) {season[i]<-"summer"}
  }  
  # - Create plots 
  pdf(paste(savename,"(Season).pdf",sep=""))
  plot(0,0,type="n",ylim=c(YMIN,YMAX),xlim=c(XMIN,XMAX),xlab=XLAB,cex.lab=1.5,cex.axis=1.5,ylab="Density")
  points(density(Data[which((season)=="winter"),colnumber],na.rm=T,adjust=adjust),type="l",col=COL[3],lwd=2)
  points(density(Data[which((season)=="summer"),colnumber],na.rm=T,adjust=adjust),type="l",col=COL[4],lwd=2)
  legend(x=Legx,y=Legy, legend=c("Winter - November to April", "Summer  - May to October")
    ,lty=rep(1,2),col=COL[3:4],cex=0.8,bg="white",lwd=2)
  dev.off()
}

PlotDensitySexlog<-function(Data,Variable,XLAB,savename=paste("DensityLog",Variable,sep=""),YMIN,YMAX,XMIN,XMAX,Legx,Legy,adjust=1){
  colnumber<-which(colnames(Data) %in% Variable)
  Data[,colnumber]<-log(Data[,colnumber])
  Sex<-c("M","F")
  # - Create plots 
  pdf(paste(savename,"(Sex).pdf",sep=""))
  plot(0,0,type="n",ylim=c(YMIN,YMAX),xlim=c(XMIN,XMAX),xlab=XLAB,cex.lab=1.5,cex.axis=1.5,ylab="Density")
  for(i in 1:length(Sex)){
    points(density(Data[which(Data$Sex==Sex[i]),colnumber],na.rm=T,adjust=adjust),type="l",col=COL[i+2])
  }
  legend(x=Legx,y=Legy, legend=c("Males","Females"),lty=rep(1,length(Sex)),col=COL[(1:length(Sex))+2],cex=0.8,bg="white")
  dev.off()
}

PlotDensitySex<-function(Data,Variable,XLAB,savename=paste("Density",Variable,sep=""),YMIN,YMAX,XMIN,XMAX,Legx,Legy,adjust=1){
  colnumber<-which(colnames(Data) %in% Variable)
  Sex<-c("M","F")
  # - Create plots 
  pdf(paste(savename,"(Sex).pdf",sep=""))
  plot(0,0,type="n",ylim=c(YMIN,YMAX),xlim=c(XMIN,XMAX),xlab=XLAB,cex.lab=1.5,cex.axis=1.5,ylab="Density")
  for(i in 1:length(Sex)){
    points(density(Data[which(Data$Sex==Sex[i]),colnumber],na.rm=T,adjust=adjust),type="l",col=COL[i+2])
  }
  legend(x=Legx,y=Legy, legend=c("Males","Females"),lty=rep(1,length(Sex)),col=COL[(1:length(Sex))+2],cex=0.8,bg="white")
  dev.off()
}


fillinmatrix<-function(m,d){
    m<-matrix(nrow=1,ncol=4)
    m[1,1]<-round(min(d,na.rm=T),2)
    m[1,2]<-paste(round(mean(d,na.rm=T),2)," (",round(sd(d,na.rm=T),2),")" ,sep="")
    m[1,3]<-round(median(d,na.rm=T) ,2)
    m[1,4]<-round(max(d,na.rm=T),2)
    
    return(m)
}


#-- descriptive statistics all
#-- distances
DescriptiveStatTable<-function(Data,Variable,Factor){
  Sex<-c("M","F")
  Animals<-sort(unique(Data$AnimID))
  m<-matrix(nrow=length(unique(Sex))+1+length(unique(Animals)),ncol=4)
  colnumber<-which(names(Data) %in% Variable)
  colnames(m)<-c("min","mean (sd)","median","max")
  rownames(m)<-c("All",unique(Sex),as.character(unique(Animals)))
  print("here")
  m[1,]<-fillinmatrix(m,Data[,colnumber]/Factor)
    #-- descriptive statistics for males and females
  for(i in 1:length(unique(Sex))){ print("here2")
    d<-Data[which(Data$Sex==unique(Sex)[i]),colnumber]/Factor
    m[1+i,]<-fillinmatrix(m,d)
  }
  #-- descriptive statistics for each individual
  for(i in 1:length(unique(Animals))){
    d<-Data[which(Data$AnimID==unique(Animals)[i]),colnumber]/Factor
    m[i+1+length(unique(Sex)),]<-fillinmatrix(m,d)
  }
  write.csv(m,paste("DescriptiveStatistics",Variable,".csv",sep=""))
}


tablemostcommoncontin<-function(Data,Variable,Factor,Max=NA){
  
  d<-Data[,which(names(Data)==Variable)]
  if(!is.na(Max)){d[which(d>Max)]<-Max}
  t<-table(round(d/Factor))
  if(!is.na(Max)){names(t)[length(names(t))]<-paste(Max/Factor,"+",sep="")}
  
  write.csv(t,paste("TableOfMostCommonValues",Variable,".csv",sep=""),row.names=FALSE)
  
}

displacementvsangle<-function(Data){
  pdf("Distance vs ChangeAngle.pdf")
  plot(0,0,type="n",xlim=c(0,70000),ylim=c(-pi,pi),cex.lab=1.5,cex.axis=1.5,xlab="Displacement between locations (m)",ylab="Change in angle")
  for(i in 1:length(unique(Data$AnimID))){
    points(Data[which(Data$AnimID==unique(Data$AnimID)[i]),]$Dist_Difference,Data[which(Data$AnimID==unique(Data$AnimID)[i]),]$ChangeAngle,col=COL[i])
  }
  legend(x=60000,y=3, legend=unique(Data$AnimID),lty=rep(1,18),col=COL[1:18],cex=0.8,bg="white")
  dev.off()
}