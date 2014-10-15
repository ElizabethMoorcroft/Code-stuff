#--------------------------------
# Project title: Snow leopards
#
# Elizabeth Moorcroft: Created: 19 August 2014
# 
# Script title: Snow Leopard Simulation of movement
# Purpose: Creates dummy movement data
#--------------------------------


###
library(RColorBrewer)
library(GISTools)
library(CircStats)



###
# Functions
#	  x
#    ___
#	|  /
# y | / h
#	|/
# sin(theta) = x/h
#  -> x=sin(theta)*h
# cos(theta) = y/h
#  -> y=cos(theta)*h 

new.location<-function(current.location, distance, angle){
	new.location<-c()
	new.location[1]<-distance*sin(angle)+current.location[1]
	new.location[2]<-distance*cos(angle)+current.location[2]
	return(new.location)
}

new.point<-function(cluster,seed){
  point<-matrix(ncol=2,nrow=1)
  set.seed(seed)
  seed.vector<-sample(size=30,x=1*10^6)[c(10,20,30)]
  set.seed(seed.vector[1])
  i<-which(rmultinom(1, size=1, prob=cluster$pro)==1)
  seed<-set.seed(seed.vector[2])
  while(is.na(point[,1])| is.na(point[,2])){
    set.seed(seed)
    seed.vector.loop<-sample(size=20,x=1*10^6)[c(10,20)]
    set.seed(seed.vector.loop[1])
    point<-rmvnorm(n=1, mean = cluster$mean[,i], sigma = cluster$variance[,,i])
    seed<-seed.vector.loop[2]
    if(point[,1]<0 | point[,1]>15000){point[,1]<-NA}
    if(point[,2]<0 | point[,2]>pi){point[,2]<-NA}
  }
  set.seed(seed.vector[3])
  sign<-rbinom(1,1,0.5)*2-1
  point[,1]<-point[,1]
  point[,2]<-point[,2]*sign
  return(point)
}

path.of.animal<-function(number.of.steps,seed,cluster){
	# Creates a stream of random numbers
	set.seed(seed)
	random.stream<-sample(size=number.of.steps,x=number.of.steps*10^6)
	
	# Sets the initial time and location and state
	new.time<-as.POSIXlt('2000-1-1 00:00:00')
	current.state<-rbinom(1,1,0.5)
	new.location.est<-c(rnorm(1,0,100),rnorm(1,0,100))
	current.location<-matrix(ncol=5,c(new.location.est,as.character(new.time),NA,NA))
	
	#print(current.location)
	
	for(step in 1:number.of.steps){
		set.seed(random.stream[step])
		random.stream.two<-sample(size=50,x=50*10^6)

    point<-new.point(cluster,random.stream.two[10])
		new.time<-new.time+5*60*60
		new.location.est<-new.location(new.location.est, distance=point[1], angle=point[2])
		
		current.location<-rbind(current.location,c(new.location.est,as.character(new.time),distance=point[1],angle=point[2]))		
	}
	colnames(current.location)<-c("x","y","Date_Time","Dist_Difference","ChangeAngle")
	return(current.location)
}
################################

males.females.blocks<-unlist(lapply(b,function(x){unlist(lapply(x,function(y){y$Sex[1]}))}))

set.seed(1)
female.blocks.no<-which(males.females.blocks==1)
females.sample<-sort(sample(female.blocks.no,round(length(female.blocks.no)*.9),replace=FALSE))
females.valid<-female.blocks.no[!(female.blocks.no %in% females.sample)]

set.seed(1)
male.blocks.no<-which(males.females.blocks==2)
males.sample<-sort(sample(male.blocks.no,round(length(male.blocks.no)*.9),replace=FALSE))
males.valid<-male.blocks.no[!(male.blocks.no %in% males.sample)]

blocks<-unlist(b,recursive = FALSE)
blocks<-blocks[c(females.sample,males.sample)]

blocks.valid<-unlist(b,recursive = FALSE)
blocks.valid<-blocks.valid[c(females.valid,males.valid)]


all.blocks<-blocks[[1]][1,][-1,]
for(i in 1:length(blocks)){
  tempdata<-blocks[[i]]
  l.tempdata<-(dim(tempdata)[1])
  if(l.tempdata>0){
    all.blocks<-rbind(all.blocks,tempdata)
  }
}
  
par(mfrow=c(2,1))
dist<-all.blocks$block_dist.difference; dist[dist<1.00]<-1.00;#dist<-log(dist)
angle<-abs(all.blocks$block_angle.difference)
all.test.data<-cbind(all.blocks$Sex,dist ,angle)
all.test.data<-all.test.data[which(!is.na(all.test.data[,1]) & !is.na(all.test.data[,2])  & !is.na(all.test.data[,3]) ),]
cluster<-list()
test.dist<-list();test.angle<-list()
for(j in 1:2){
  all.test.data.sex<-all.test.data[which(all.test.data[,1]==j),]
  test.data<-all.test.data.sex[,-1]
  
  fit <- Mclust(test.data)
  cluster[[j]]<-summary(fit)[]
  print(summary(fit))
  if(j==1){name="female_cluster.pdf"}else{name="male_cluster.pdf"}
  pdf(name)
  plot(fit);
  dev.off()

}
2
0
2
0

blocks.valid<-unlist(b,recursive = FALSE)
blocks.valid<-blocks.valid[c(females.valid)]
all.blocks.valid.f<-blocks.valid[[1]][1,][-1,]
for(i in 1:length(blocks.valid)){
  tempdata<-blocks.valid[[i]]
  l.tempdata<-(dim(tempdata)[1])
  if(l.tempdata>0){
    all.blocks.valid.f<-rbind(all.blocks.valid.f,tempdata)
  }
}

blocks.valid<-unlist(b,recursive = FALSE)
blocks.valid<-blocks.valid[c(males.valid)]
all.blocks.valid.m<-blocks.valid[[1]][1,][-1,]
for(i in 1:length(blocks.valid)){
  tempdata<-blocks.valid[[i]]
  l.tempdata<-(dim(tempdata)[1])
  if(l.tempdata>0){
    all.blocks.valid.m<-rbind(all.blocks.valid.m,tempdata)
  }
}


path.f<-path.of.animal(number.of.steps=144*3,seed=1,cluster=cluster[[1]])
setwd(SaveDir)
pdf("female_validation_dist_ang.pdf")
par(mfrow=c(2,1))
plot(density(as.numeric(path.f[,4]),na.rm=T,from=0),sub="female",xlab="distance",main=NA)
  points(density(all.blocks.valid.f$block_dist.difference,na.rm=T,from=0),col="red",type="l")
  p.value.dist<-ks.test(all.blocks.valid.f$block_dist.difference,as.numeric(path.f[,4]))$p.value
  text(x=6000,y=5.5*10^-4,paste("ks p-value =",round(p.value.dist,3)))
  legend(x=4000,y=4*10^-4,col=c(1,2),lty=c(1,1),legend=c("simulated data","test data set"),cex=0.5)
plot(density(as.numeric(path.f[,5]),na.rm=T,from=-pi,to=pi),ylim=c(0,0.25),sub="female",xlab="angle",main=NA)
  points(density(all.blocks.valid.f$block_angle.difference,na.rm=T,from=-pi,to=pi),col="red",type="l")
  p.value.angle<-ks.test(all.blocks.valid.f$block_angle.difference,as.numeric(path.f[,5]))$p.value
  text(x=0,y=0.15,paste("ks p-value =",round(p.value.angle,3)))
  legend(x=-2,y=0.1,col=c(1,2),lty=c(1,1),legend=c("simulated data","test data set"),cex=0.5)
dev.off()


path.m<-path.of.animal(number.of.steps=144*3,seed=1,cluster=cluster[[2]])
setwd(SaveDir)
pdf("male_validation_dist_ang.pdf")
par(mfrow=c(2,1))
plot(density(as.numeric(path.m[,4]),na.rm=T,from=0),sub="male",xlab="distance",main=NA)
  points(density(all.blocks.valid.m$block_dist.difference,na.rm=T,from=0),col="red",type="l")
  p.value.dist<-ks.test(all.blocks.valid.m$block_dist.difference,as.numeric(path.m[,4]))$p.value
  text(x=6000,y=1.5*10^-4,paste("ks p-value =",round(p.value.dist,3)))
  legend(x=4000,y=1*10^-4,col=c(1,2),lty=c(1,1),legend=c("simulated data","test data set"),cex=0.5)
plot(density(as.numeric(path.m[,5]),na.rm=T,from=-pi,to=pi),ylim=c(0,0.25),sub="male",xlab="angle",main=NA)
  points(density(all.blocks.valid.m$block_angle.difference,na.rm=T,from=-pi,to=pi),col="red",type="l")
  p.value.angle<-ks.test(all.blocks.valid.m$block_angle.difference,as.numeric(path.m[,5]))$p.value
  text(x=0,y=0.1,paste("ks p-value =",round(p.value.angle,3)))
  legend(x=-2,y=0.1,col=c(1,2),lty=c(1,1),legend=c("simulated data","test data set"),cex=0.5)
dev.off()

pdf("female_validation_areause.pdf")
par(mfrow=c(1,1))
plot(0,type="n",xlim=c(0,100),ylim=c(0,100),ylab="%age locations within area",xlab="%age of inner area",sub="female")
f<-plot.erosion.of.convex.hull(cbind(as.numeric(path.f[,1]),as.numeric(path.f[,2])),no.levels=500,index.divider=100000)
points(f[,1:2],type="l")
c.blocks<-names(table(all.blocks.valid.f[!is.na(all.blocks.valid.f[,13]),]$block_number))
count=0
for(i in c.blocks){
  time.spent.convex.hull<-plot.erosion.of.convex.hull(alwl.blocks.valid.f[,13:12][which(!is.na(all.blocks.valid.f[,13]) & all.blocks.valid.f$block_number==i),],
                                                      no.levels=500,index.divider=100000)
  points(time.spent.convex.hull[,1:2],type="l",col="red")
  p.value<-ks.test(time.spent.convex.hull[,2],f[,2])$p.value
  print(p.value)
 text(90,20-(count*5),paste("ks.test pvalue = ",round(p.value,3)),cex=0.8);count=count+1
}
legend(0,100,col=c(1,2),lty=c(1,1),legend=c("simulated data","test data set"),cex=0.5)
dev.off()

pdf("male_validation_areause.pdf")
plot(0,type="n",xlim=c(0,100),ylim=c(0,100),ylab="%age locations within area",xlab="%age of inner area",sub="male")
m<-plot.erosion.of.convex.hull(cbind(as.numeric(path.m[,1]),as.numeric(path.m[,2])),no.levels=500,index.divider=100000)
points(m[,1:2],type="l")
c.blocks<-names(table(all.blocks.valid.m[!is.na(all.blocks.valid.m[,13]),]$block_number))
count=0
for(i in c.blocks){
  time.spent.convex.hull<-plot.erosion.of.convex.hull(all.blocks.valid.m[,13:12][which(!is.na(all.blocks.valid.m[,13]) & all.blocks.valid.m$block_number==i),],
                                                      no.levels=500,index.divider=100000)
  points(time.spent.convex.hull[,1:2],type="l",col="red")
  p.value<-ks.test(time.spent.convex.hull[,2],m[,2])$p.value
  print(p.value)
  text(90,20-(count*5),paste("ks.test pvalue = ",round(p.value,3)),cex=0.8);count=count+1
}
legend(0,100,col=c(1,2),lty=c(1,1),legend=c("simulated data","test data set"),cex=0.5)
dev.off()

pdf("male_simulated_movement.pdf")
plot(cbind(as.numeric(path.m[,1]),as.numeric(path.m[,2])),type="l",xlab="x",ylab="y",main="movement of male")
dev.off()
pdf("female_simulated_movement.pdf")
plot(cbind(as.numeric(path.f[,1]),as.numeric(path.f[,2])),type="l",xlab="x",ylab="y",main="movement of male")
dev.off()