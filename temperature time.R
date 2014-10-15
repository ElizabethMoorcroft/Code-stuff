library("mclust")
library("mvtnorm")



l.data<-dim(Data)[1]
mn.temp<-min(Data$Temp);mx.temp<-max(Data$Temp)

dist<-Data$Dist_Difference
dist[dist<1]<-1
dist<-log(dist)

m<-matrix(ncol=(mx.temp-mn.temp+1),nrow=ceiling(max(dist,na.rm=T)+1),0)

for(i in 1:l.data){
  row<-(Data$Temp[i]-mn.temp)+1
  col<-round(dist[i])
  print(paste("col",col,"row",row))
  if(!is.na(dist[i]) & !is.na(Data$Temp[i]))
  m[col,row]<-m[col,row]+1
}

######################################################

b<-blocks.summer[[2]]

all.blocks<-b[[1]][[1]][1,][-1,]
for(i in 1:length(b)){
  tempdata<-b[[i]]
  l.tempdata<-(length(tempdata))
  if(l.tempdata>0){for(j in 1:length(tempdata)){
    all.blocks<-rbind(all.blocks,tempdata[[j]])
  }}
}



########################## tempature
l.data<-dim(all.blocks)[1]
mn.temp<-min(all.blocks$Temp,na.rm=T);mx.temp<-max(all.blocks$Temp,na.rm=T)

dist<-all.blocks$block_dist.difference
dist[dist<1]<-1
dist<-log(dist)*5

m<-matrix(ncol=(mx.temp-mn.temp+1),nrow=ceiling(max(dist,na.rm=T)+1),0)

for(i in 1:l.data){
  row<-(all.blocks$Temp[i]-mn.temp)+1
  col<-round(dist[i])
  print(paste("col",col,"row",row))
  if(!is.na(dist[i]) & !is.na(all.blocks$Temp[i]))
    m[col,row]<-m[col,row]+1
}
m<-t(m)
image((m),x=mn.temp:(mx.temp+1),y=0:ceiling(max(dist,na.rm=T)+1),xlab="temperature",ylab="distance")
image(log(m),x=mn.temp:(mx.temp+1),y=(0:ceiling(max(dist,na.rm=T)+1))/5,xlab="temperature",ylab="log of distance")


############ hour
l.data<-dim(all.blocks)[1]
mn.temp<-min(all.blocks$Hour,na.rm=T);mx.temp<-max(all.blocks$Hour,na.rm=T)

dist<-all.blocks$block_dist.difference
dist[dist<1]<-1
dist<-log(dist)*5

m<-matrix(ncol=(mx.temp-mn.temp+1),nrow=ceiling(max(dist,na.rm=T)+1),0)

for(i in 1:l.data){
  row<-(all.blocks$Hour[i]-mn.temp)+1
  col<-round(dist[i])
  print(paste("col",col,"row",row))
  if(!is.na(dist[i]) & !is.na(all.blocks$Hour[i]))
    m[col,row]<-m[col,row]+1
}
m<-t(m)
image((m),x=mn.temp:(mx.temp+1),y=0:ceiling(max(dist,na.rm=T)+1),xlab="time of day",ylab="distance")
image(log(m),x=mn.temp:(mx.temp+1),y=(0:ceiling(max(dist,na.rm=T)+1))/5,xlab="time of day",ylab="log of distance")


############ hour vs temp
l.data<-dim(all.blocks)[1]
mn.hour<-min(all.blocks$Hour,na.rm=T);mx.hour<-max(all.blocks$Hour,na.rm=T)

mn.temp<-min(all.blocks$Temp,na.rm=T);mx.temp<-max(all.blocks$Temp,na.rm=T)

m<-matrix(ncol=(mx.hour-mn.hour+1),nrow=(mx.temp-mn.temp+1),0)

for(i in 1:l.data){
  col<-(all.blocks$Temp[i]-mn.temp)+1
  row<-(all.blocks$Hour[i]-mn.hour)+1
  print(paste("col",col,"row",row))
  if(!is.na(all.blocks$Temp[i]) & !is.na(all.blocks$Hour[i]))
    m[col,row]<-m[col,row]+1
}
m<-t(m)
image(m)
image(m,y=mn.temp:(mx.temp+1),x=mn.hour:(mx.hour+1),xlab="time of day",ylab="temperature")
image(log(m),y=mn.temp:(mx.temp+1),x=mn.hour:(mx.hour+1),xlab="time of day",ylab="temperature")


############ altitdude

l.data<-dim(all.blocks)[1]
height<-all.blocks$Height
height<-log(height)*10
mn.height<-min(height,na.rm=T)

dist<-all.blocks$block_dist.difference
dist[dist<1]<-1
dist<-log(dist)*5

m<-matrix(ncol=ceiling((max(height,na.rm=T)-min(height,na.rm=T))+1),nrow=ceiling(max(dist,na.rm=T)+1),0)

for(i in 1:l.data){
  row<-round(height[i]-mn.height)
  col<-round(dist[i])
  print(paste("col",col,"row",row))
  if(!is.na(dist[i]) & !is.na(height[i]))
    m[col,row]<-m[col,row]+1
}
m<-t(m)
image((m),x=0:(ceiling((max(height,na.rm=T)-min(height,na.rm=T))+1)),y=(0:ceiling(max(dist,na.rm=T)+1))/5,xlab="altitude",ylab="distance")
image(log(m),x=0:(ceiling((max(height,na.rm=T)-min(height,na.rm=T))+1)),y=(0:ceiling(max(dist,na.rm=T)+1))/5,xlab="time of day",ylab="log of distance")


################## adjust for time


dist<-all.blocks$block_dist.difference
dist[dist<1.01]<-1.01
#dist<-log(dist)
dist<-exp(dist)

reg<-glm(dist~as.factor(all.blocks$Hour),family="Gamma")
summary(reg)
par(mfrow=c(2,2));plot(reg)

mn.hr<-3;mx.hr<-12 ### Check that these are ok 

hour.cat<-all.blocks$Hour
hour.cat[which(hour.cat>=mn.hr & hour.cat<=mx.hr)]<-"day"
hour.cat[(which(hour.cat!="day"))]<-"night"

par(mfrow=c(2,1))
boxplot(log(dist[which(hour.cat=="day")]))
boxplot(log(dist[which(hour.cat=="night")]))

dist<-dist*5

angle<-all.blocks$block_angle.difference
angle<-angle*5
mn.angle<-min(angle,na.rm=T)

m<-matrix(ncol=ceiling((max(angle,na.rm=T)-min(angle,na.rm=T))+1),nrow=ceiling(max(dist,na.rm=T)+1),0)
day<-which(hour.cat=="day")
for(i in day){
  row<-round(angle[i]-mn.angle)
  col<-round(dist[i])
  print(paste("col",col,"row",row))
  if(!is.na(dist[i]) & !is.na(angle[i]))
    m[col,row]<-m[col,row]+1
}
image(log(m),y=0:(ceiling((max(angle,na.rm=T)-min(angle,na.rm=T))+1))/5,x=(0:ceiling(max(dist,na.rm=T)+1))/5,main=paste(mn.hr,"-",mx.hr,"hrs",sep=""),ylab="angle",xlab="log of distance")

m<-matrix(ncol=ceiling((max(angle,na.rm=T)-min(angle,na.rm=T))+1),nrow=ceiling(max(dist,na.rm=T)+1),0)
day<-which(hour.cat=="night")
for(i in day){
  row<-round(angle[i]-mn.angle)
  col<-round(dist[i])
  print(paste("col",col,"row",row))
  if(!is.na(dist[i]) & !is.na(angle[i]))
    m[col,row]<-m[col,row]+1
}
image(log(m),y=0:(ceiling((max(angle,na.rm=T)-min(angle,na.rm=T))+1))/5,x=(0:ceiling(max(dist,na.rm=T)+1))/5,main=paste(mx.hr+1,"-",mn.hr-1,"hrs",sep=""),ylab="angle",xlab="log of distance")


################## adjust for temp


dist<-all.blocks$block_dist.difference
dist[dist<1.01]<-1.01
#dist<-log(dist)

reg<-glm(dist~all.blocks$Temp,family="Gamma")
summary(reg)
par(mfrow=c(2,2));plot(reg)

mn.hr<-3;mx.hr<-12 ### Check that these are ok 

hour.cat<-all.blocks$Hour
hour.cat[which(hour.cat>=mn.hr & hour.cat<=mx.hr)]<-"day"
hour.cat[(which(hour.cat!="day"))]<-"night"

par(mfrow=c(2,1))
boxplot(log(dist[which(hour.cat=="day")]))
boxplot(log(dist[which(hour.cat=="night")]))

dist<-dist*5

angle<-all.blocks$block_angle.difference
angle<-angle*5
mn.angle<-min(angle,na.rm=T)

m<-matrix(ncol=ceiling((max(angle,na.rm=T)-min(angle,na.rm=T))+1),nrow=ceiling(max(dist,na.rm=T)+1),0)
day<-which(hour.cat=="day")
for(i in day){
  row<-round(angle[i]-mn.angle)
  col<-round(dist[i])
  print(paste("col",col,"row",row))
  if(!is.na(dist[i]) & !is.na(angle[i]))
    m[col,row]<-m[col,row]+1
}
image(log(m),y=0:(ceiling((max(angle,na.rm=T)-min(angle,na.rm=T))+1))/5,x=(0:ceiling(max(dist,na.rm=T)+1))/5,main=paste(mn.hr,"-",mx.hr,"hrs",sep=""),ylab="angle",xlab="log of distance")

m<-matrix(ncol=ceiling((max(angle,na.rm=T)-min(angle,na.rm=T))+1),nrow=ceiling(max(dist,na.rm=T)+1),0)
day<-which(hour.cat=="night")
for(i in day){
  row<-round(angle[i]-mn.angle)
  col<-round(dist[i])
  print(paste("col",col,"row",row))
  if(!is.na(dist[i]) & !is.na(angle[i]))
    m[col,row]<-m[col,row]+1
}
image(log(m),y=0:(ceiling((max(angle,na.rm=T)-min(angle,na.rm=T))+1))/5,x=(0:ceiling(max(dist,na.rm=T)+1))/5,main=paste(mx.hr+1,"-",mn.hr-1,"hrs",sep=""),ylab="angle",xlab="log of distance")


##########

test.data<-cbind(all.blocks$Temp,dist)
test.data<-test.data[which(!is.na(test.data[,1]) & !is.na(test.data[,2])),]
fit <- Mclust(test.data,G=2)

################
par(mfrow=c(2,2))
all.test.data<-cbind(all.blocks$Temp,all.blocks$Sex,dist,angle)
all.test.data<-all.test.data[which(!is.na(all.test.data[,1]) & !is.na(all.test.data[,2])  & !is.na(all.test.data[,3]) & !is.na(all.test.data[,4])),]
cluster<-list()
test.dist<-list();test.angle<-list()
for(j in 1:2){
  all.test.data.sex<-all.test.data[which(all.test.data[,2]==j),]
  test.data<-all.test.data.sex[,-2]

  n<-1:dim(test.data)[1]
  size.sam<-round(2*dim(test.data)[1]/3)
  set.seed(2)
  sam<-sample(size=size.sam,n,replace=FALSE)
  not.sam<-n[!(n %in% sam)]
  test.data.sam<-test.data[sam,]

  fit <- Mclust(test.data.sam[,2:3])
  cluster[[j]]<-summary(fit)[]
  print(summary(fit))
  plot(fit);
  sim.data<-matrix(ncol=summary(fit)[]$d,nrow=0)
  for(i in 1:summary(fit)[]$G){
    number.of.points<-round(10000*summary(fit)[]$pro[i])
    for(k in 1:number.of.points){
      point<-matrix(ncol=2,nrow=1)
      while(is.na(point[,1])| is.na(point[,2])){
        point<-rmvnorm(n=1, mean = summary(fit)[]$mean[,i], sigma = summary(fit)[]$variance[,,i])
        if(point[,1]<0){point[,1]<-NA}
        if(point[,2]<0 | point[,2]>pi){point[,2]<-NA}
      }
      sim.data<-rbind(sim.data,point)
    }
  }
  
  plot(sim.data);points(test.data[not.sam,2],test.data[not.sam,3],col="red")
  test.dist[[j]]<-test.data[not.sam,2];test.angle[[j]]<-test.data[not.sam,3]
  print(ks.test(sim.data[,1],test.data[not.sam,2])$p.value)
  print(ks.test(sim.data[,2],test.data[not.sam,3])$p.value)
}
###########

sim.temp<-sort(unique(round(sim.data[,1])))
x<-c()
for(i in sim.temp){x<-c(x,mean(sim.data[which(round(sim.data[,1])==i),2]))}


## cluster sex
all.test.data<-cbind(all.blocks$Sex,dist,angle)
all.test.data.f<-all.test.data[which(all.test.data[,1]==1),]
all.test.data.m<-all.test.data[which(all.test.data[,1]==2),]

test.data<-all.test.data.m[which(!is.na(all.test.data.m[,1]) & !is.na(all.test.data.m[,2])  & !is.na(all.test.data.m[,3])),]
test.data<-all.test.data[which(!is.na(all.test.data[,1]) & !is.na(all.test.data[,2])  & !is.na(all.test.data[,3])),]

n<-1:dim(test.data)[1]
sam<-sample(size=1000,n,replace=FALSE)
not.sam<-n[!(n %in% sam)]
test.data.sam<-test.data[sam,]

fit <- Mclust(test.data.sam[,2:3],G=2)
sim.data<-rbind(rmvnorm(n=round(1000*summary(fit)[]$pro[1]), mean = summary(fit)[]$mean[,1], sigma = summary(fit)[]$variance[,,1])
                ,rmvnorm(n=round(1000*summary(fit)[]$pro[2]), mean = summary(fit)[]$mean[,2], sigma = summary(fit)[]$variance[,,2])
                #,rmvnorm(n=round(1000*summary(fit)[]$pro[3]), mean = summary(fit)[]$mean[,3], sigma = summary(fit)[]$variance[,,3])
               )

ks.test(sim.data[,2],test.data[not.sam,2])
ks.test(sim.data[,1],test.data[not.sam,3])


############


all.test.data<-cbind(dist,angle)
test.data<-all.test.data[which(!is.na(all.test.data[,1]) & !is.na(all.test.data[,2]))),]
n<-1:dim(test.data)[1]
sam<-sample(size=1478,n,replace=FALSE)
not.sam<-n[!(n %in% sam)]
test.data.sam<-test.data[sam,]

fit <- Mclust(test.data.sam,G=2)
sim.data<-rbind(rmvnorm(n=round(1000*summary(fit)[]$pro[1]), mean = summary(fit)[]$mean[,1], sigma = summary(fit)[]$variance[,,1]),
                rmvnorm(n=round(1000*summary(fit)[]$pro[2]), mean = summary(fit)[]$mean[,2], sigma = summary(fit)[]$variance[,,2]))

plot(sim.data)
ks.test(sim.data[,2],test.data[not.sam,2])
ks.test(sim.data[,1],test.data[not.sam,1])





test.data<-cbind(all.blocks$Hour,dist)
test.data<-test.data[which(!is.na(test.data[,1]) & !is.na(test.data[,2])),]
fit <- Mclust(test.data)