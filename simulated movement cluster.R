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
    if(point[,1]<0){point[,1]<-NA}
    if(point[,2]<0 | point[,2]>pi){point[,2]<-NA}
  }
  set.seed(seed.vector[3])
  sign<-rbinom(1,1,0.5)*2-1
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

path<-path.of.animal(number.of.steps=144,seed=1,cluster=cluster[[1]])



###
# Parameters
#N=5000 # Number of 
#SHAPE<-c(7,3);SCALE<-c(NA,2.5)
#EPS<-5.5; SCALE[1] <-SCALE[2]+EPS
#MU<-c(0,3.14);RHO<-c(0.5,0.5);
#STATE=c(0.5,0.5)
#parameters<-list(SHAPE=SHAPE,SCALE=SCALE,MU=MU,RHO=RHO,STATE=STATE)


#simulated.movement<-as.data.frame(matrix(ncol=8,nrow=0))
#names(simulated.movement)<-c("UTM_X_Zone","UTM_Y_Zone","Date_Time","Dist_Difference","ChangeAngle","True_State","AnimID","Sex")
#no.of.steps<-24*(12*30)/5
#I=8;J=2
#count<-0
#for(i in 1:I){
#	if(i<I/2){Sex<-"F"}else{Sex<-"M"}
#	for(j in 1:J){
#		count<-count+1
#		move<-path.of.animal(no.of.steps,seed = count,parameters)
#		AnimID<-rep(i,dim(move)[1]) # where i becomes the ID of the animal
#		new.move<-cbind(move,AnimID,Sex)
#		simulated.movement<-rbind(simulated.movement,new.move)
#	}
#}

# Runs through the problem though the monthly thing
#test.sim<-Months(simulated.movement,0,2)

#for(count in I*J){
#	simulated.movement[[count]]<-plot.erosion.of.convex.hull(simulated.movement[test.sim[[3]][[count]],1:2], index.divider=10000,no.levels=50)
#}

#plot(0,xlim=c(0,100),ylim=c(0,100))
#for(i in 1:1000){
#	points(simulated.movement[[i]],type="l",col=state.list[[i]]*100)
#}
