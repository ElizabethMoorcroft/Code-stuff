## Colour

## Basic data analysis

pdf("Distance_Male_Female.pdf")
plot(0,xlim=c(0,16000),ylim=c(0,8*10^-4),ylab="Density",xlab="Distance [m]",type="n")
for(i in 1:14){temp.data<-test[[i]]$hours5.distance
               if(sum(!is.na(temp.data))>2){ points(density(temp.data,na.rm=T,from=0),type="l",col=paired[5])}
}
for(i in 15:24){temp.data<-test[[i]]$hours5.distance
                if(sum(!is.na(temp.data))>2){ points(density(temp.data,na.rm=T,from=0),type="l",col=paired[1])}
}
legend(10000,8*10^-4,legend=c("Male","Female"),col=c(paired[1],paired[5]),lty=c(1,1),lwd=c(2,2))
dev.off()

pdf("Distance_Male_Female_logged.pdf")
plot(0,xlim=c(-3,12),ylim=c(0,0.4),ylab="Density",xlab="Logged displacement [log(m)]",type="n")
for(i in 1:14){temp.data<-log(test[[i]]$hours5.distance)
               if(sum(!is.na(temp.data))>2){ points(density(temp.data,na.rm=T),type="l",col=paired[5])}
}
points(density(log(unlist(lapply(1:14,function(x){test[[x]]$hours5.distance}))),na.rm=T),type="l",col=paired[6],lty=2,lwd=4)
for(i in 15:24){temp.data<-log(test[[i]]$hours5.distance)
                if(sum(!is.na(temp.data))>2){ points(density(temp.data,na.rm=T),type="l",col=paired[1])}
}
points(density(log(unlist(lapply(15:24,function(x){test[[x]]$hours5.distance}))),na.rm=T),type="l",col=paired[2],lty=2,lwd=4)
legend(-2,0.4,
       legend=c("Indivdiual males","Indivdiual female","Average male","Average female"),
       col=c(paired[1],paired[5],paired[2],paired[6]),
       lty=c(1,1,2,2),lwd=c(2,2,2,2),
       cex=0.8)
dev.off()

##
pdf("Distance_Male_Female_winter.pdf")
plot(0,xlim=c(0,16000),ylim=c(0,12*10^-4),ylab="Density",xlab="Distance [m]",type="n")
for(i in 1:9){temp.data<-test.winter[[i]]$hours5.distance
               if(sum(!is.na(temp.data))>2){ points(density(temp.data,na.rm=T,from=0),type="l",col=paired[5])}
}
for(i in 10:19){temp.data<-test.winter[[i]]$hours5.distance
                if(sum(!is.na(temp.data))>2){ points(density(temp.data,na.rm=T,from=0),type="l",col=paired[1])}
}
legend(10000,8*10^-4,legend=c("Male","Female"),col=c(paired[1],paired[5]),lty=c(1,1),lwd=c(2,2))
dev.off()

pdf("Distance_Male_Female_winter_logged.pdf")
plot(0,xlim=c(-3,12),ylim=c(0,0.4),ylab="Density",xlab="Logged displacement [log(m)]",type="n")
for(i in 1:9){temp.data<-log(test.winter[[i]]$hours5.distance)
               if(sum(!is.na(temp.data))>2){ points(density(temp.data,na.rm=T),type="l",col=paired[5])}
}
points(density(log(unlist(lapply(1:9,function(x){test.winter[[x]]$hours5.distance}))),na.rm=T),type="l",col=paired[6],lty=2,lwd=4)
for(i in 10:19){temp.data<-log(test.winter[[i]]$hours5.distance)
                if(sum(!is.na(temp.data))>2){ points(density(temp.data,na.rm=T),type="l",col=paired[1])}
}
points(density(log(unlist(lapply(10:19,function(x){test.winter[[x]]$hours5.distance}))),na.rm=T),type="l",col=paired[2],lty=2,lwd=4)
legend(-2,0.4,
       legend=c("Indivdiual males","Indivdiual female","Average male","Average female"),
       col=c(paired[1],paired[5],paired[2],paired[6]),
       lty=c(1,1,2,2),lwd=c(2,2,2,2),
       cex=0.8)
dev.off()

### ANGLE
pdf("Angle_Male_Female.pdf",height=9,width=3)
par(mfrow=c(3,1))
op<-par(oma=c(1, 1, 1, 1) + 0.1)
PlotCircularGraphs(test[[1]]$hours5.angle,round=1,COL=paired[5],ADD=F,Radius=10)
for(i in 1:14){temp.data<-test[[i]]$hours5.angle
               if(sum(!is.na(temp.data))>2){ 
                 PlotCircularGraphs(temp.data,round=1,COL=paired[5],ADD=T,Radius=10)
               }
}
PlotCircularGraphs(test[[15]]$hours5.angle,round=1,COL=paired[1],ADD=F,Radius=10)
for(i in 15:24){temp.data<-test[[i]]$hours5.angle
               if(sum(!is.na(temp.data))>2){ 
                 PlotCircularGraphs(temp.data,round=1,COL=paired[1],ADD=T,Radius=10)
               }
}

t1<-unlist(lapply(1:14,function(x){test[[x]]$hours5.angle}))
PlotCircularGraphs(t1,round=1,COL=paired[2],ADD=F,Radius=4)
t2<-unlist(lapply(15:24,function(x){test[[x]]$hours5.angle}))
ks.test(t1,t2)
PlotCircularGraphs(t1,round=1,COL=paired[6],ADD=T,Radius=4)
#par(op) # Leave the last plot
#op <- par(usr=c(0,3,0,3), xpd=NA)
legend(x=0,y=6,
       c("Individual female","Individual male","Average female","Average male"),
       col=c(paired[5],paired[1],paired[6],paired[2]),
       lty=1,lwd=2,bg="white")
dev.off()

pdf("Angle_Male_Female_winter.pdf",height=9,width=3)
par(mfrow=c(3,1))
op<-par(oma=c(1, 1, 1, 1) + 0.1)
PlotCircularGraphs(test.winter[[1]]$hours5.angle,round=1,COL=paired[5],ADD=F,Radius=10)
for(i in 1:9){temp.data<-test.winter[[i]]$hours5.angle
               if(sum(!is.na(temp.data))>2){ 
                 PlotCircularGraphs(temp.data,round=1,COL=paired[5],ADD=T,Radius=10)
               }
}
PlotCircularGraphs(test.winter[[10]]$hours5.angle,round=1,COL=paired[1],ADD=F,Radius=10)
for(i in 10:19){temp.data<-test.winter[[i]]$hours5.angle
                if(sum(!is.na(temp.data))>2){ 
                  PlotCircularGraphs(temp.data,round=1,COL=paired[1],ADD=T,Radius=10)
                }
}

t1<-unlist(lapply(1:9,function(x){test.winter[[x]]$hours5.angle}))
PlotCircularGraphs(t1,round=1,COL=paired[2],ADD=F,Radius=4)
t2<-unlist(lapply(10:19,function(x){test.winter[[x]]$hours5.angle}))
ks.test(t1,t2)
PlotCircularGraphs(t1,round=1,COL=paired[6],ADD=T,Radius=4)
legend(x=0,y=6,
       c("Individual female","Individual male","Average female","Average male"),
       col=c(paired[5],paired[1],paired[6],paired[2]),
       lty=1,lwd=2,bg="white")
dev.off()


## heat maps

pdf("HeatMap_Female.pdf")
d<-unlist(lapply(1:14,function(x){test[[x]]$hours5.distance}))
d[d<0.5]<-(0.5)
d<-round(d/1000)*1000
a<-unlist(lapply(1:14,function(x){test[[x]]$hours5.angle}))
a<-round(a,1)
z<-table(a,d)
z.dim<-dim(z)[1];z<-cbind(z,rep(0,z.dim)); colnames(z)[dim(z)[2]]<-16*1000
d.names<-c(as.numeric(names(table(d))),16*1000)
filled.contour(x=as.numeric(names(table(a))),
               y=d.names,
               z, #log(z),
               color.palette=colorRampPalette(brewer.pal(n=9, name="YlOrRd"),space="Lab")
               ,xlab="Angle [radians]",ylab="Distance [m]"
               )
dev.off()
pdf("HeatMap_Male.pdf")
d<-unlist(lapply(15:24,function(x){test[[x]]$hours5.distance}))
d[d<0.5]<-(0.5)
d<-round(d/1000)*1000
a<-unlist(lapply(15:24,function(x){test[[x]]$hours5.angle}))
a<-round(a,1)
z<-table(a,d)
z.dim<-dim(z)[1];z<-cbind(z,rep(0,z.dim)); colnames(z)[dim(z)[2]]<-16*1000
d.names<-c(as.numeric(names(table(d))),16*1000)
filled.contour(x=as.numeric(names(table(a))),
               y=d.names,
               z, #log(z),
               color.palette=colorRampPalette(brewer.pal(n=9, name="YlOrRd"),space="Lab")
               ,xlab="Angle [radians]",ylab="Distance [m]"
)
dev.off()
pdf("HeatMap_Female_logged.pdf")
d<-unlist(lapply(1:14,function(x){test[[x]]$hours5.distance}))
d[d<0.5]<-(0.5)
d<-round(log(d)/3,1)*3
a<-unlist(lapply(1:14,function(x){test[[x]]$hours5.angle}))
a<-round(a/4,1)*4
z<-table(a,d)
z.dim<-dim(z)[1];z<-cbind(z,rep(0,z.dim)); colnames(z)[dim(z)[2]]<-10
d.names<-c(as.numeric(names(table(d))),10)
filled.contour(x=as.numeric(names(table(a))),
               y=d.names,
               z,
               color.palette=colorRampPalette(brewer.pal(n=9, name="YlOrRd"),space="Lab")
               ,xlab="Angle [radians]",ylab="Logged distance [log(m)]"
)
dev.off()
pdf("HeatMap_Male_Logged.pdf")
d<-unlist(lapply(15:24,function(x){test[[x]]$hours5.distance}))
d[d<0.5]<-(0.5)
d<-round(log(d)/3,1)*3
a<-unlist(lapply(15:24,function(x){test[[x]]$hours5.angle}))
a<-round(a/3,1)*3
z<-table(a,d)
z.dim<-dim(z)[1];z<-cbind(z,rep(0,z.dim)); colnames(z)[dim(z)[2]]<-10
d.names<-c(as.numeric(names(table(d))),10)
filled.contour(x=as.numeric(names(table(a))),
               y=d.names,
               z,#log(z),
               color.palette=colorRampPalette(brewer.pal(n=9, name="YlOrRd"),space="Lab")
               ,xlab="Angle [radians]",ylab="Logged distance [log(m)]"
)
dev.off()


###HR

pdf("HomeRange_MF_individ.pdf")
plot(0,type="n",xlim=c(0,100),ylim=c(0,100),ylab="Percentage of locations within area",xlab="Percentage of area")
area.valid<-c()
for(j in 1:14){
  validation.erosion<-NA
  try(validation.erosion<-plot.erosion.of.convex.hull(x.y.locations(lat=test[[j]]$Latitude,lon=test[[j]]$Longitude),no.levels=500,index.divider=500))
  if(!is.na(validation.erosion[1])){points(validation.erosion[[1]][,1:2],type="l",col=paired[5])}
  area.valid<-c(area.valid,as.numeric(validation.erosion[[2]]))
}
for(j in 15:24){
  validation.erosion<-NA
  try(validation.erosion<-plot.erosion.of.convex.hull(x.y.locations(lat=test[[j]]$Latitude,lon=test[[j]]$Longitude),no.levels=500,index.divider=500))
  if(!is.na(validation.erosion[1])){points(validation.erosion[[1]][,1:2],type="l",col=paired[1])}
  area.valid<-c(area.valid,as.numeric(validation.erosion[[2]]))
}
legend(x=0,y=100,
       c("Individual females","Individual males"),
       col=c(paired[5],paired[1]),
       lty=1,lwd=2,bg="white",cex=0.9)
dev.off()
pdf("HomeRange_MF.pdf")
plot(density(area.valid[1:14],from=0),col=paired[5],xlab="Area [m^2]",main="",ylim=c(0,3*10^-9),xlim=c(0,1.2*10^9))
points(density(area.valid[15:24],from=0),type="l",col=paired[1])
legend(x=8*10^8,y=3*10^-9,
       c("Females","Males"),
       col=c(paired[5],paired[1]),
       lty=1,lwd=2,bg="white",cex=0.9)
ks.test(area.valid[1:14],area.valid[15:24])
dev.off()

pdf("HomeRange_MF_individ_winter.pdf")
plot(0,type="n",xlim=c(0,100),ylim=c(0,100),ylab="Percentage of locations within area",xlab="Percentage of area")
area.valid<-c()
for(j in 1:9){
  validation.erosion<-NA
  try(validation.erosion<-plot.erosion.of.convex.hull(x.y.locations(lat=test.winter[[j]]$Latitude,lon=test.winter[[j]]$Longitude),no.levels=500,index.divider=500))
  if(!is.na(validation.erosion[1])){points(validation.erosion[[1]][,1:2],type="l",col=paired[5])}
  area.valid<-c(area.valid,as.numeric(validation.erosion[[2]]))
}
for(j in 10:19){
  validation.erosion<-NA
  if(length(test.winter[[j]]$Latitude)>=5){
    try(validation.erosion<-plot.erosion.of.convex.hull(x.y.locations(lat=test.winter[[j]]$Latitude,lon=test.winter[[j]]$Longitude),no.levels=500,index.divider=500))
    if(!is.na(validation.erosion[1])){points(validation.erosion[[1]][,1:2],type="l",col=paired[1])}
    area.valid<-c(area.valid,as.numeric(validation.erosion[[2]]))
  }
}
legend(x=0,y=100,
       c("Individual females","Individual males"),
       col=c(paired[5],paired[1]),
       lty=1,lwd=2,bg="white",cex=0.9)
dev.off()
pdf("HomeRange_MF_winter.pdf")
plot(density(area.valid[1:9],from=0),col=paired[5],xlab="Area [m^2]",main="",ylim=c(0,3*10^-9),xlim=c(0,1.2*10^9))
points(density(area.valid[10:19],from=0,na.rm=T),type="l",col=paired[1])
legend(x=8*10^8,y=3*10^-9,
       c("Females","Males"),
       col=c(paired[5],paired[1]),
       lty=1,lwd=2,bg="white",cex=0.9)
ks.test(area.valid[1:9],area.valid[10:19])
dev.off()
