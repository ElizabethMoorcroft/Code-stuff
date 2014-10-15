endtime<-format(round(as.POSIXlt(as.POSIXlt(Data$Date_Time)-(2.5*60*60)),units="hours"),format="%H:%M")
t<-names(table(endtime))
#boxplot(Data$Dist_Difference~endtime)
library(vioplot)

months<-as.POSIXlt(Data$Date_Time)$mon
season<-vector(length=length(months))
for(i in 1:length(months)){ 
    if(months[i]<4){season[i]<-"winter"}
    else if (months[i]>10) {season[i]<-"winter"}
    else if (months[i]>=4 & months[i]<=10) {season[i]<-"summer"}
  }  
setwd(SaveDir)
pdf("SeasonAngle.pdf")
  op<-par(oma=c(1, 1, 1, 1) + 0.1)
  tempdata<-Data$ChangeAngle
  NAS<-!is.na(tempdata)
  data<-tempdata[NAS]
  seasonnarm<-season[NAS]
  tab<-table(round(data,1))/length(data)
  polar.plot(as.vector(tab*100),as.numeric(names(tab))*180/pi,
             main=NULL,
             lwd=2,line.col=COL[19],
             rp.type="polygon",
             add=F,
             radial.lim=c(0,4),radial.labels=""
             )
  data_s<-data[which(seasonnarm=="summer")]
  data_s<-data_s[!is.na(data_s)]
  tab<-table(round(data_s,1))/length(data_s)
  polar.plot(as.vector(tab*100),as.numeric(names(tab))*180/pi,
             main=NULL,
             lwd=2,line.col=COL[4],
             rp.type="polygon",
             add=T,
             radial.lim=c(0,4),radial.labels=""
             )
  data_w<-data[which(seasonnarm=="winter")]
  data_w<-data_w[!is.na(data_w)]
  tab<-table(round(data_w,1))/length(data_w)
  polar.plot(as.vector(tab*100),as.numeric(names(tab))*180/pi,
             main=NULL,
             lwd=2,line.col=COL[3],
             rp.type="polygon",
             add=T,
             radial.lim=c(0,4),radial.labels=""
             )
      par(op) # Leave the last plot
    op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)
    legend(x=0.8,y=1,c("All year","Winter","Summer"),col=COL[c(19,4:3)],lty=1)
dev.off()



pdf("HourVsSeasonAngle.pdf",width=7,height=9)
par(mfrow=c(6,4))
  tempdata1<-Data$ChangeAngle
  for(i in 1:24){
  tempdata<-tempdata1[which(endtime==t[i])]
  NAS<-!is.na(tempdata)
  data<-tempdata[NAS]
  seasonnarm<-season[NAS]
  tab<-table(round(data,1))/length(data)
  polar.plot(as.vector(tab*100),as.numeric(names(tab))*180/pi,
             main=t[i],
             lwd=2,line.col=COL[19],
             rp.type="polygon",
             add=F,
             radial.lim=c(0,4),radial.labels="",labels=c(0,90,180,270),label.pos=c(0,90,180,270)#label.pos=c(0,pi/2,pi,1.5*pi)
             )
  #data_s<-data[which(seasonnarm=="summer")]
  #data_s<-data_s[!is.na(data_s)]
  #tab<-table(round(data_s,1))/length(data_s)
  #polar.plot(as.vector(tab*100),as.numeric(names(tab))*180/pi,
  #           main=t[i],
  #           lwd=2,line.col=COL[4],
  #           rp.type="polygon",
  #           add=T,
  #           radial.lim=c(0,4),radial.labels=""
  #           )
  #data_w<-data[which(seasonnarm=="winter")]
  #data_w<-data_w[!is.na(data_w)]
  #tab<-table(round(data_w,1))/length(data_w)
  #polar.plot(as.vector(tab*100),as.numeric(names(tab))*180/pi,
  #           main=t[i],
  #           lwd=2,line.col=COL[3],
  #           rp.type="polygon",
  #           add=T,
  #           radial.lim=c(0,4),radial.labels=""
  #           )  
 }
dev.off()




pdf("SeasonalityVsHour.pdf")
par(mfrow=c(3,1))
plot(0, 0, xlim=c(0,23), ylim=c(0,10),type="n",main="All year",xlab="Hour",ylab="log of displacement",cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
t<-names(table(endtime))
for(i in 1:24){
  print(i)
  vioplot(log(Data[which(endtime==t[i]),]$Dist_Difference+1),add=T,at=i-1,col=COL[19])
  }
plot(0, 0, xlim=c(0,23), ylim=c(0,10),type="n",main="Summer: May - October",xlab="Hour",ylab="log of displacement",cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
for(i in 1:24){
  print(i)
  vioplot(log(Data[which(endtime==t[i] & season=="summer"),]$Dist_Difference+1),add=T,at=i-1,col=COL[4])
  }
plot(0, 0, xlim=c(0,23), ylim=c(0,10),type="n",main="Winter: November-April",xlab="Hour",ylab="log of displacement",cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
for(i in 1:24){
  print(i)
  vioplot(log(Data[which(endtime==t[i] & season=="winter"),]$Dist_Difference+1),add=T,at=i-1,col=COL[3])
  }
dev.off()


pdf("SexVsHour.pdf")
par(mfrow=c(3,1))
plot(0, 0, xlim=c(0,23), ylim=c(0,10),type="n",main="All animals",xlab="Hour",ylab="log of displacement",cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
t<-names(table(endtime))
for(i in 1:24){
  print(i)
  vioplot(log(Data[which(endtime==t[i]),]$Dist_Difference+1),add=T,at=i-1,col=COL[19])
  }
plot(0, 0, xlim=c(0,23), ylim=c(0,10),type="n",main="Females",xlab="Hour",ylab="log of displacement",cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
for(i in 1:24){
  print(i)
  vioplot(log(Data[which(endtime==t[i] & Data$Sex=="F"),]$Dist_Difference+1),add=T,at=i-1,col=COL[4])
  }
plot(0, 0, xlim=c(0,23), ylim=c(0,10),type="n",main="Males",xlab="Hour",ylab="log of displacement",cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
for(i in 1:24){
  print(i)
  vioplot(log(Data[which(endtime==t[i] & Data$Sex=="M"),]$Dist_Difference+1),add=T,at=i-1,col=COL[3])
  }
dev.off()