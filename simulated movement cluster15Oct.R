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
  seed.vector<-sample(size=300,x=1*10^6)[c(100,200,300)]
  set.seed(seed.vector[1])
  i<-which(rmultinom(1, size=1, prob=cluster$pro)==1)
  seed<-seed.vector[2]
  while(is.na(point[,1])| is.na(point[,2])){
    set.seed(seed)
    seed.vector.loop<-sample(size=200,x=1*10^6)[c(100,200)]
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
	random.stream<-sample(size=number.of.steps*10,x=number.of.steps*10^6)[(1:number.of.steps)*10]
	
	# Sets the initial time and location and state
	new.time<-as.POSIXlt('2000-1-1 00:00:00')
	new.location.est<-c(0,0)#c(rnorm(1,0,100),rnorm(1,0,100))
	current.location<-matrix(ncol=5,c(new.location.est,as.character(new.time),NA,NA))
	
	#print(current.location)
	
	for(step in 1:number.of.steps){
		set.seed(random.stream[step])
		random.stream.two<-sample(size=50,x=50*10^6)

    point<-new.point(cluster,random.stream.two[50])
		new.time<-new.time+5*60*60
		new.location.est<-new.location(new.location.est, distance=point[1], angle=point[2])
		
		current.location<-rbind(current.location,c(new.location.est,as.character(new.time),distance=point[1],angle=point[2]))		
	}
	colnames(current.location)<-c("x","y","Date_Time","Dist_Difference","ChangeAngle")
	return(current.location)
}
################################



classifcation.simulation.validation<-function(data,seed){
  all.block.numbers<-1:length(unlist(data,recursive=FALSE))
  
  cluster.list<-vector(mode="list",length=length(unlist(data,recursive=FALSE)))
  temp<-matrix(ncol=6,nrow=length(unlist(data,recursive=FALSE)))
  
  for(i in all.block.numbers){
    print(i)
    valid=i
    
    blocks<-unlist(data,recursive = FALSE)
    blocks.valid<-blocks[c(valid)]
    sex<-blocks.valid[[1]]$Sex[1]
    sample<-all.block.numbers[-i]
    blocks.sample<-blocks[c(sample)]
    
    print(which(unlist(lapply(blocks.sample,function(x){x$Sex[1]}))==sex))
    
    blocks.sample<-blocks.sample[which(unlist(lapply(blocks.sample,function(x){x$Sex[1]}))==sex)]
    
    
    pdf(paste("CrossValidation_block",i,"_sex",sex,".pdf",sep=""))
    par(mfrow=c(2,2))

    
    ## combine sample data into one dataframe
    all.blocks<-blocks.sample[[1]][1,][-1,]
    for(j in 1:length(blocks.sample)){
      tempdata<-blocks.sample[[j]]
      l.tempdata<-(dim(tempdata)[1])
      if(l.tempdata>0){
        all.blocks<-rbind(all.blocks,tempdata)
      }
    }
    
    cluster<-cluster.data(all.blocks,name=paste(sex,"cluster",i,".pdf",sep="")) 
    
    all.blocks.valid<-blocks.valid[[1]][1,][-1,]
    for(j in 1:length(blocks.valid)){
      tempdata<-blocks.valid[[j]]
      l.tempdata<-(dim(tempdata)[1])
      if(l.tempdata>0){
        all.blocks.valid<-rbind(all.blocks.valid,tempdata)
      }
    }
    
    temp[i,4:6]<-validation(cluster=cluster,validation.data=all.blocks.valid,seed=seed,name=paste("test",i,sep=""))
    dev.off()
    temp[i,1]<-i;temp[i,2]<-sex;temp[i,3]<-cluster$G; 
    cluster.list[[i]]<-cluster
    print(temp)
  }
  
  return(list(cluster.list,temp))
}


cluster.data<-function(data,name){
  all.test.data<-cbind(data$block_dist.difference,abs(data$block_angle.difference))
  all.test.data<-all.test.data[which(!is.na(all.test.data[,1]) & !is.na(all.test.data[,2]) ),]
    
  fit <- Mclust(all.test.data)
  cluster<-summary(fit)[]
  #pdf(name); 
  plot(fit); 
  #dev.off()

  return(cluster)
}


validation<-function(cluster,validation.data,seed,name){
  path<-path.of.animal(number.of.steps=144*3,seed=seed,cluster=cluster)
  #pdf(paste(name,"_validation_dist_ang.pdf"))
  
  #par(mfrow=c(2,1))
  plot(density(as.numeric(path[,4]),na.rm=T,from=0),sub=name,xlab="distance",main=NA,xlim=c(0,15000),ylim=c(0,6.5*10^-4))
  points(density(validation.data$block_dist.difference,na.rm=T,from=0),col="red",type="l")
  p.value.dist<-ks.test(validation.data$block_dist.difference,as.numeric(path[,4]))$p.value
  text(x=6000,y=5.5*10^-4,paste("ks p-value =",round(p.value.dist,3)))
  legend(x=4000,y=4*10^-4,col=c(1,2),lty=c(1,1),legend=c("simulated data","test data set"),cex=0.5)
  
  plot(density(as.numeric(path[,5]),na.rm=T,from=-pi,to=pi),ylim=c(0,0.35),sub=name,xlab="angle",main=NA)
  points(density(validation.data$block_angle.difference,na.rm=T,from=-pi,to=pi),col="red",type="l")
  p.value.angle<-ks.test(validation.data$block_angle.difference,as.numeric(path[,5]))$p.value
  text(x=0,y=0.15,paste("ks p-value =",round(p.value.angle,3)))
  legend(x=-2,y=0.1,col=c(1,2),lty=c(1,1),legend=c("simulated data","test data set"),cex=0.5)
  
  #dev.off()
  
  #pdf(paste(name,"_validation_areause.pdf"))
  #par(mfrow=c(1,1))
  
  plot(0,type="n",xlim=c(0,100),ylim=c(0,100),ylab="%age locations within area",xlab="%age of inner area",sub=name)
  
  validation.erosion<-plot.erosion.of.convex.hull(cbind(as.numeric(path[,1]),as.numeric(path[,2])),no.levels=500,index.divider=50000)
  points(validation.erosion[,1:2],type="l")
  c.blocks<-names(table(validation.data[!is.na(validation.data[,13]),]$block_number))
  count=0
  for(i in c.blocks){
    time.spent.convex.hull<-plot.erosion.of.convex.hull(validation.data[,13:12][which(!is.na(validation.data[,13]) & validation.data$block_number==i),],
                                                        no.levels=500,index.divider=50000)
    points(time.spent.convex.hull[,1:2],type="l",col="red")
    p.value<-ks.test(time.spent.convex.hull[,2],validation.erosion[,2])$p.value
    print(p.value)
    text(70,15-(count*5),paste("ks.test pvalue = ",round(p.value,3)),cex=0.8);count=count+1
  }
  legend(0,100,col=c(1,2),lty=c(1,1),legend=c("simulated data","test data set"),cex=0.5)
  #dev.off()
  
  return(matrix(ncol=3,nrow=1,c(p.value.dist,p.value.angle,p.value)))
}



#############################

b<-data.for.classifcation.summer.min50[[2]]
test<-classifcation.simulation.validation(b,seed=1)
## add animial ID and the number of validation points to table

