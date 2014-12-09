#--------------------------------
# Project title: Snow leopards
#
# Elizabeth Moorcroft: Created: 19 August 2014
# 
# Script title: Snow Leopard Simulation of movement
# Purpose: Creates dummy movement data
#--------------------------------


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

new.location<-function(current.location, distance, angle,log.distance){
	new.location<-c()
  if(log.distance==TRUE){distance<-exp(distance)}
	new.location[1]<-distance*sin(angle)+current.location[1]
	new.location[2]<-distance*cos(angle)+current.location[2]
	return(new.location)
}

new.point<-function(cluster,seed,log.distance){
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
    if(log.distance==FALSE){ if(point[,1]<0 | point[,1]>15000){point[,1]<-NA}}
                             else{ if(exp(point[,1])<0 | exp(point[,1])>15000){point[,1]<-NA}}
    if(point[,2]<0 | point[,2]>pi){point[,2]<-NA}
  }
  set.seed(seed.vector[3])
  sign<-rbinom(1,1,0.5)*2-1
  point[,1]<-point[,1]
  point[,2]<-point[,2]*sign
  return(point)
}

# Bearing of animal
direction.of.animal<-function(data){
  
  if(!is.na(data$x[2]) & !is.na(data$x[1]) & !is.na(data$y[2]) & !is.na(data$y[1])){
    Bearing<-atan2((data$x[2]-data$x[1]),(data$y[2]-data$y[1]))
    if(Bearing<0){Bearing<-Bearing+2*pi}
  }else{Bearing<-NA}
  return(Bearing)
}

# Change in direction
change.in.animal.direction<-function(data){   
  # - Calculates the change in bearing
  # - this produces a value between -2pi and 2pi which needs to be corrected
  if(!is.na(data$block_bearing[1]) & !is.na(data$block_bearing[2]) ){
    ChangeAngle<-data$block_bearing[2]-data$block_bearing[1]
    if(ChangeAngle>pi){ChangeAngle<-ChangeAngle-2*pi}
    else if(ChangeAngle<(-pi)){ChangeAngle<-2*pi+ChangeAngle}
  }else{ChangeAngle<-NA}
  return(ChangeAngle)
}


path.of.animal<-function(number.of.steps,seed,cluster,log.distance){
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

    point<-new.point(cluster,random.stream.two[50],log.distance)
		new.time<-new.time+5*60*60
		new.location.est<-new.location(new.location.est, distance=point[1], angle=point[2],log.distance)
		
		current.location<-rbind(current.location,c(new.location.est,as.character(new.time),distance=point[1],angle=point[2]))	
    
	}
	colnames(current.location)<-c("x","y","Date_Time","Dist_Difference","ChangeAngle")
  
	current.location<-distance.x.hours(data=current.location,hours=c(10,15,20,25),lat="y",long="x",method="NotGPS")
  
	return(current.location)
}
################################

logging.distance<-function(data){
  
  logged.data<-lapply(1:length(data),
                      function(y){
                        if(length(data[[y]])>=1){
                          lapply(1:length(data[[y]]),function(x){
                            #print(paste("y",y,"x",x)); 
                            log(data[[y]][[x]]$block_dist.difference)})
                        }
                      }
  )
  
  for(i in 1:length(data)){
    if(length(data[[i]])>=1){for(j in 1:length(logged.data[[i]])){
      #print(paste("i",i,"j",j))
      data[[i]][[j]]$block_dist.difference<-logged.data[[i]][[j]]
      data[[i]][[j]]$block_dist.difference[is.infinite(data[[i]][[j]]$block_dist.difference)]<-0
    }
    }}
  
  return(data)
}

testing.valid.data<-function(data, validation.number,all.block.numbers){
  valid=validation.number
  
  blocks<-unlist(data,recursive = FALSE)
  
  blocks.valid<-blocks[c(valid)]
  sex<-blocks.valid[[1]]$Sex[1]
  sample<-all.block.numbers[-valid]
  
  blocks.sample<-blocks[c(sample)]
  blocks.sample<-blocks.sample[which(unlist(lapply(blocks.sample,function(x){x$Sex[1]}))==sex)]
  
  ## combine sample data into one dataframe
  all.blocks<-blocks.sample[[1]][1,][-1,]
  for(j in 1:length(blocks.sample)){
    tempdata<-blocks.sample[[j]]
    l.tempdata<-(dim(tempdata)[1])
    if(l.tempdata>0){
      all.blocks<-rbind(all.blocks,tempdata)
    }
  }
  
  all.blocks.valid<-blocks.valid[[1]][1,][-1,]
  for(j in 1:length(blocks.valid)){
    tempdata<-blocks.valid[[j]]
    l.tempdata<-(dim(tempdata)[1])
    if(l.tempdata>0){
      all.blocks.valid<-rbind(all.blocks.valid,tempdata)
    }
  }
  
  return(list(all.blocks,all.blocks.valid,sex))
  
}

classifcation.simulation.validation<-function(data,seed,name,number.of.sims,log.distance=FALSE,number.of.clusters=c(NA),pval,month,choose.blocks=NA){
  
  # log distance data if needed
  if(log.distance==TRUE){ data<-logging.distance(data) }
    
  all.block.numbers<-1:length(unlist(data,recursive=FALSE))
  cluster.list<-vector(mode="list",length=length(unlist(data,recursive=FALSE)))
  list.all.blocks<-vector(mode="list",length=length(unlist(data,recursive=FALSE)))
  
  # results table
  temp<-matrix(ncol=29+3*number.of.sims,nrow=length(unlist(data,recursive=FALSE))*length(number.of.clusters))
  colnames(temp)<-c("BlockNo","Sex","NoCluster","BIC","AnimalsInTraining","NoOfTrainingLocations","AnimalsInValidation","NoOfValidationLocations",
                    paste("%PvalueOfSpaceUse<",pval,sep=""),
                    "NoOfValidLocs_Disp_5hrs",paste("%Pvalue_Disp_5hrs<",pval,sep=""),"NoOfValidLocs_ChAng_5hrs",paste("%Pvalue_ChAng_5hrs<",pval,sep=""),
                    "NoOfValidLocs_Disp_10hrs",paste("%Pvalue_Disp_10hrs<",pval,sep=""),"NoOfValidLocs_ChAng_10hrs",paste("%Pvalue_ChAng_10hrs<",pval,sep=""),
                    "NoOfValidLocs_Disp_15hrs",paste("%Pvalue_Disp_15hrs<",pval,sep=""),"NoOfValidLocs_ChAng_15hrs",paste("%Pvalue_ChAng_15hrs<",pval,sep=""),
                    "NoOfValidLocs_Disp_205hrs",paste("%Pvalue_Disp_20hrs<",pval,sep=""),"NoOfValidLocs_ChAng_20hrs",paste("%Pvalue_ChAng_20hrs<",pval,sep=""),
                    "NoOfValidLocs_Disp_25hrs",paste("%Pvalue_Disp_25hrs<",pval,sep=""),"NoOfValidLocs_ChAng_25hrs",paste("%Pvalue_ChAng_25hrs<",pval,sep=""),
                    
                    rep("PvalueDistbutionOfDisplacement",number.of.sims),rep("PValueDistributionOfAngle",number.of.sims),rep("PvalueOfSpaceUse",number.of.sims))
  
  # counter 
  counter<-0
  
  if(!is.na(choose.blocks[1])){run.blocks<-choose.blocks}else{run.blocks<-all.block.numbers}
  for(i in run.blocks){
    
    #creates blocks for training and validation
    blocks<-testing.valid.data(data, validation.number=i,all.block.numbers)
    all.blocks<-blocks[[1]]
    all.blocks.valid<-blocks[[2]]
    sex<-blocks[[3]]
    
    # length of clusters
    l.number.of.clusters<-length(number.of.clusters)
    
    # Loop through the number of clusters
    for(cluster.number in 1:l.number.of.clusters){
      
      counter<-counter+1
      print(paste(counter,"/",(l.number.of.clusters*length(all.block.numbers))))
      
      pdf.name<-paste(name,"_block",i,"_clusters",number.of.clusters[cluster.number],"_sex",sex,sep="")
      # creates a pdf
      pdf(paste(pdf.name,".pdf",sep=""))
      par(mfrow=c(2,2))

      # Clusters the data 
      print("cluster")
      cluster<-cluster.data(data = all.blocks,number.of.clusters = number.of.clusters[cluster.number]) 
      save.cluster.data(cluster,save.name=paste(pdf.name,".txt",sep=""),i,sex,month)
    
      classification.of.cluster<-rep(NA,length=dim(all.blocks)[1])
      all.blocks<-cbind(all.blocks,classification.of.cluster)
      all.blocks$classification.of.cluster[!is.na(all.blocks$block_angle.difference)]<-cluster$classification
      list.all.blocks[[counter]]<-all.blocks
      
      print("validation")
      res<-validation(cluster=cluster,
                      validation.data=all.blocks.valid,
                      seed=seed,
                      name=paste(pdf.name,sep=""),
                      number.of.sims=number.of.sims,
                      log.distance=log.distance)
    
      dev.off()
    
      # plots the distribution of pvalues
      pvalue.density.plots(res=res[1:3],name=paste(pdf.name,"_PvalueDistn",".pdf",sep=""))
      results.from.add.val<-plot.additional.validation(path=res[[4]],number.of.sims,validation.data=all.blocks.valid,save.name=paste(pdf.name,"_AddValid",".pdf"))
      
      print("save")
      # puts values into results table
      temp[counter,1]<-i; temp[counter,2]<-sex;
      temp[counter,3]<-cluster$G; # Number of clusters 
      temp[counter,4]<-cluster$bic
      
      temp[counter,5]<-paste(unique(all.blocks$AnimID),collapse = ",");
      temp[counter,6]<-sum(!is.na(all.blocks$ECEF_X))
      
      temp[counter,7]<-paste(unique(all.blocks.valid$AnimID),collapse = ",");
      temp[counter,8]<-sum(!is.na(all.blocks.valid$ECEF_X))
      temp[counter,9]<-100*sum(res[[3]]<pval)/length(res[[3]])
      
      temp[counter,10]<-sum(!is.na(all.blocks.valid$block_dist.difference))
      temp[counter,11]<-100*sum(res[[1]]<pval)/length(res[[1]])
      temp[counter,12]<-sum(!is.na(all.blocks.valid$block_angle.difference))
      temp[counter,13]<-100*sum(res[[2]]<pval)/length(res[[2]])
      
      
      #results.from.add.val
      temp[counter,14]<-sum(!is.na(all.blocks.valid$block_dist.difference_10_hrs))
      temp[counter,15]<-100*sum(results.from.add.val[[1]]<pval)/length(results.from.add.val[[1]])
      temp[counter,16]<-sum(!is.na(all.blocks.valid$block_angle.difference_10_hrs))
      temp[counter,17]<-100*sum(results.from.add.val[[2]]<pval)/length(results.from.add.val[[2]])
      #results.from.add.val 15
      temp[counter,18]<-sum(!is.na(all.blocks.valid$block_dist.difference_15_hrs))
      temp[counter,19]<-100*sum(results.from.add.val[[3]]<pval)/length(results.from.add.val[[3]])
      temp[counter,20]<-sum(!is.na(all.blocks.valid$block_angle.difference_15_hrs))
      temp[counter,21]<-100*sum(results.from.add.val[[4]]<pval)/length(results.from.add.val[[4]])
      #results.from.add.val 20
      temp[counter,22]<-sum(!is.na(all.blocks.valid$block_dist.difference_20_hrs))
      temp[counter,23]<-100*sum(results.from.add.val[[5]]<pval)/length(results.from.add.val[[5]])
      temp[counter,24]<-sum(!is.na(all.blocks.valid$block_angle.difference_20_hrs))
      temp[counter,25]<-100*sum(results.from.add.val[[6]]<pval)/length(results.from.add.val[[6]])
      #results.from.add.val 25
      temp[counter,26]<-sum(!is.na(all.blocks.valid$block_dist.difference_25_hrs))
      temp[counter,27]<-100*sum(results.from.add.val[[7]]<pval)/length(results.from.add.val[[7]])
      temp[counter,28]<-sum(!is.na(all.blocks.valid$block_angle.difference_25_hrs))
      temp[counter,29]<-100*sum(results.from.add.val[[8]]<pval)/length(results.from.add.val[[8]])
      
      
      temp[counter,30:(29+(number.of.sims))]<-res[[1]]
      temp[counter,(30+(number.of.sims)):(29+2*(number.of.sims))]<-res[[2]]
      temp[counter,(30+2*number.of.sims):(29+3*(number.of.sims))]<-res[[3]]

      cluster.list[[counter]]<-cluster
    }
  }
  
  write.csv(temp,paste(name,"_ValidationTable.csv"))
  return(list(cluster.list,temp,list.all.blocks))
}


cluster.data<-function(data,number.of.clusters){
  all.test.data<-cbind(data$block_dist.difference,abs(data$block_angle.difference))
  #print(all.test.data[1:10,])
  all.test.data<-all.test.data[which(!is.na(all.test.data[,1]) & !is.na(all.test.data[,2]) ),]
    
  if(!is.na(number.of.clusters)){fit <- Mclust(data = all.test.data, G = number.of.clusters)} else {fit <- Mclust(data = all.test.data)}
  cluster<-summary(fit)[]
  plot(fit); 

  return(cluster)
}

pvalue.density.plots<-function(res,name){
  pdf(name)
  par(mfrow=c(3,1))
  pvalue.density.plot.indiv(res[[1]],title.plot="Distribution of p-values: displacement")
  pvalue.density.plot.indiv(res[[2]],title.plot="Distribution of p-values: change in direction")
  pvalue.density.plot.indiv(res[[3]],title.plot="Distribution of p-values: use of space")
  #plot(density(res[[1]],from=0,to=1)); abline(v=0.05, col="red"); text(x=0.8,y=1,paste("%less than 0.05 =",round(sum(res[[1]]<.05)/number.of.sims,3)*100))
  #plot(density(res[[2]],from=0,to=1)); abline(v=0.05, col="red"); text(x=0.8,y=1,paste("%less than 0.05 =",round(sum(res[[2]]<.05)/number.of.sims,3)*100))
  #plot(density(res[[3]],from=0,to=1)); abline(v=0.05, col="red"); text(x=0.8,y=1,paste("%less than 0.05 =",round(sum(res[[3]]<.05)/number.of.sims,3)*100))
  dev.off()
}

pvalue.density.plot.indiv<-function(data,title.plot){
  den<-density(data,from=0,to=1)
  plot(den,main=title.plot,ylab="density",xlab="p-value")
  abline(v=0.05, col="red")
  text(x=0.8,y=(max(den$y)-max(den$y)/5),paste("%less than 0.05 =",round(sum(data<.05)/length(data),3)*100))
  
}

validation<-function(cluster,validation.data,seed,name,number.of.sims,log.distance){
  
  print(validation.data[1:10,])
  length.of.validation<-sum(!is.na(validation.data$block_angle.difference))
  
  set.seed(seed)
  new.seed<-sample(x=number.of.sims*10^6,number.of.sims)
  path<-lapply(1:number.of.sims,function(x){path.of.animal(number.of.steps=length.of.validation,seed=new.seed[x],cluster=cluster,log.distance=log.distance)})
  print(path[[1]][1:10,])
  p.value.dist<-c();p.value.angle<-c();p.value<-c()
  
  if(log.distance==FALSE){dist.xlim<-15000; dist.ylim=6.5*10^-4; index.divider=50000}
  else{dist.xlim<-15; dist.ylim=0.5; index.divider=50000}
    
    
  plot(0,type="n",sub=name,xlab="distance",main=NA,xlim=c(0,dist.xlim),ylim=c(0,dist.ylim))
  for(i in 1:number.of.sims){points(density(as.numeric(as.character(path[[i]]$Dist_Difference)),na.rm=T,from=0),type="l")}
  points(density(validation.data$block_dist.difference,na.rm=T,from=0),col="red",type="l")
  for(i in 1:number.of.sims){
    p.value.dist<-c(p.value.dist,ks.test(validation.data$block_dist.difference,as.numeric(path[[i]]$Dist_Difference))$p.value)
  }
  #text(x=6000,y=5.5*10^-4,paste("ks p-value =",round(p.value.dist,3)))
  #legend(x=4000,y=4*10^-4,col=c(1,2),lty=c(1,1),legend=c("simulated data","test data set"),cex=0.5)
  
  plot(0,type="n",xlim=c(-pi,pi), ylim=c(0,0.35),sub=name,xlab="angle",main=NA)
  for(i in 1:number.of.sims){points(density(as.numeric(as.character(path[[i]]$ChangeAngle)),na.rm=T,from=-pi,to=pi),type="l")}
  points(density(validation.data$block_angle.difference,na.rm=T,from=-pi,to=pi),col="red",type="l")
  for(i in 1:number.of.sims){ p.value.angle<-c(p.value.angle,ks.test(validation.data$block_angle.difference,as.numeric(path[[i]]$ChangeAngle))$p.value)}
 # text(x=0,y=0.15,paste("ks p-value =",round(p.value.angle,3)))
 # legend(x=-2,y=0.1,col=c(1,2),lty=c(1,1),legend=c("simulated data","test data set"),cex=0.5)

  
  plot(0,type="n",xlim=c(0,100),ylim=c(0,100),ylab="%age locations within area",xlab="%age of inner area",sub=name)
  for(i in 1:number.of.sims){
    validation.erosion<-plot.erosion.of.convex.hull(cbind(as.numeric(path[[i]][,1]),as.numeric(path[[i]][,2])),no.levels=500,index.divider=index.divider)
    points(validation.erosion[,1:2],type="l")

    c.blocks<-names(table(validation.data[!is.na(validation.data[,13]),]$block_number))
    count=0
    for(j in c.blocks){
      time.spent.convex.hull<-plot.erosion.of.convex.hull(validation.data[,13:12][which(!is.na(validation.data[,13]) & validation.data$block_number==j),],
                                                        no.levels=500,index.divider=index.divider)
     points(time.spent.convex.hull[,1:2],type="l",col="red")
      p.value<-c(p.value,ks.test(time.spent.convex.hull[,2],validation.erosion[,2])$p.value)
    #print(p.value)
    #text(70,15-(count*5),paste("ks.test pvalue = ",round(p.value,3)),cex=0.8);count=count+1
    }
  }
  legend(0,100,col=c(1,2),lty=c(1,1),legend=c("simulated data","test data set"),cex=0.5)
  #dev.off()

 
 
 return(list(p.value.dist,p.value.angle,p.value,path))
}

plot.additional.validation<-function(path,number.of.sims,validation.data,save.name){
  
  p.value.dist.10<-c();  p.value.dist.15<-c();  p.value.dist.20<-c();  p.value.dist.25<-c()
  p.value.angle.10<-c();  p.value.angle.15<-c();  p.value.angle.20<-c();  p.value.angle.25<-c()
  
  pdf(save.name)
  par(mfrow=c(2,4))
  plot(0,type="n",sub=name,xlab="distance10",main=NA,xlim=c(0,20),ylim=c(0,0.5))
  for(i in 1:number.of.sims){points(density(as.numeric(path[[i]]$block_dist.difference_10_hrs)/1000,na.rm=T,from=0),type="l");
                             p.value.dist.10<-c(p.value.dist.10,ks.test(validation.data$block_dist.difference_10_hrs,as.numeric(path[[i]]$block_dist.difference_10_hrs))$p.value)}
  points(density(validation.data$block_dist.difference_10_hrs/1000,na.rm=T,from=0),col="red",type="l")

  plot(0,type="n",sub=name,xlab="distance15",main=NA,xlim=c(0,20),ylim=c(0,0.5))
  for(i in 1:number.of.sims){points(density(as.numeric(path[[i]]$block_dist.difference_15_hrs)/1000,na.rm=T,from=0),type="l")
                             p.value.dist.15<-c(p.value.dist.15,ks.test(validation.data$block_dist.difference_15_hrs,as.numeric(path[[i]]$block_dist.difference_15_hrs))$p.value)}
  points(density(validation.data$block_dist.difference_15_hrs/1000,na.rm=T,from=0),col="red",type="l")
  
  plot(0,type="n",sub=name,xlab="distance20",main=NA,xlim=c(0,20),ylim=c(0,0.5))
  for(i in 1:number.of.sims){points(density(as.numeric(path[[i]]$block_dist.difference_20_hrs)/1000,na.rm=T,from=0),type="l")
                             p.value.dist.20<-c(p.value.dist.20,ks.test(validation.data$block_dist.difference_20_hrs,as.numeric(path[[i]]$block_dist.difference_20_hrs))$p.value)}
                                
  points(density(validation.data$block_dist.difference_20_hrs/1000,na.rm=T,from=0),col="red",type="l")
  
  plot(0,type="n",sub=name,xlab="distance25",main=NA,xlim=c(0,20),ylim=c(0,0.5))
  for(i in 1:number.of.sims){points(density(as.numeric(path[[i]]$block_dist.difference_25_hrs)/1000,na.rm=T,from=0),type="l")
                             p.value.dist.25<-c(p.value.dist.25,ks.test(validation.data$block_dist.difference_25_hrs,as.numeric(path[[i]]$block_dist.difference_25_hrs))$p.value)}
                                
  points(density(validation.data$block_dist.difference_25_hrs/1000,na.rm=T,from=0),col="red",type="l")
  
  
  plot(0,type="n",sub=name,xlab="angle10",main=NA,xlim=c(-pi,pi),ylim=c(0,0.5))
  for(i in 1:number.of.sims){points(density(as.numeric(path[[i]]$block_angle.difference_10_hrs),na.rm=T,from=-pi,to=pi),type="l")
                             p.value.angle.10<-c(p.value.angle.10,ks.test(validation.data$block_angle.difference_10_hrs,as.numeric(path[[i]]$block_angle.difference_10_hrs))$p.value)}
  points(density(validation.data$block_angle.difference_10_hrs,na.rm=T,from=-pi,to=pi),col="red",type="l")
  
  plot(0,type="n",sub=name,xlab="angle15",main=NA,xlim=c(-pi,pi),ylim=c(0,0.5))
  for(i in 1:number.of.sims){points(density(as.numeric(path[[i]]$block_angle.difference_15_hrs),na.rm=T,from=-pi,to=pi),type="l")
                             p.value.angle.15<-c(p.value.angle.15,ks.test(validation.data$block_angle.difference_15_hrs,as.numeric(path[[i]]$block_angle.difference_15_hrs))$p.value)}
  points(density(validation.data$block_angle.difference_15_hrs,na.rm=T,from=-pi,to=pi),col="red",type="l")
  
  plot(0,type="n",sub=name,xlab="angle20",main=NA,xlim=c(-pi,pi),ylim=c(0,0.5))
  for(i in 1:number.of.sims){points(density(as.numeric(path[[i]]$block_angle.difference_20_hrs),na.rm=T,from=-pi,to=pi),type="l")
                             p.value.angle.20<-c(p.value.angle.20,ks.test(validation.data$block_angle.difference_20_hrs,as.numeric(path[[i]]$block_angle.difference_20_hrs))$p.value)}

  points(density(validation.data$block_angle.difference_20_hrs,na.rm=T,from=-pi,to=pi),col="red",type="l")
  
  plot(0,type="n",sub=name,xlab="angle25",main=NA,xlim=c(-pi,pi),ylim=c(0,0.5))
  for(i in 1:number.of.sims){points(density(as.numeric(path[[i]]$block_angle.difference_25_hrs),na.rm=T,from=-pi,to=pi),type="l")
                             p.value.angle.25<-c(p.value.angle.25,ks.test(validation.data$block_angle.difference_25_hrs,as.numeric(path[[i]]$block_angle.difference_25_hrs))$p.value)}
  points(density(validation.data$block_angle.difference_25_hrs,na.rm=T,from=-pi,to=pi),col="red",type="l")
  dev.off()

return(list(  p.value.dist.10, p.value.angle.10, 
              p.value.dist.15, p.value.angle.15, 
              p.value.dist.20, p.value.angle.20, 
              p.value.dist.25, p.value.angle.25))
}

save.cluster.data<-function(cluster,save.name,i,sex,month){
  write.table(cluster$G,save.name,row.names = FALSE,col.names = FALSE)
  write.table(i,save.name,append=TRUE,row.names = FALSE,col.names = FALSE)
  write.table(t(month),save.name,append=TRUE,row.names = FALSE,col.names = FALSE)
  if(sex=="F"){write.table(0,save.name,append=TRUE,row.names = FALSE,col.names = FALSE)}else{write.table(1,save.name,append=TRUE,row.names = FALSE,col.names = FALSE)}
  write.table(t(cluster$mean),save.name,append=TRUE,row.names = FALSE,col.names = FALSE)
  for(j in 1:cluster$G){
    write.table(t(as.vector(chol(cluster$variance[,,j]))),save.name,append=TRUE,row.names = FALSE,col.names = FALSE)
  }
  write.table(t(cluster$pro),save.name,append=TRUE,row.names = FALSE,col.names = FALSE)
}

#############################

setwd(SaveDir)

data.for.classifcation<-list();counter=0

for(locs in c(50)){#for(locs in c(25,50)){
  for(miss.occ in 4){
    for(start.month in c(1,4,7,10)){
      for(no.month in 3){
        counter=counter+1
        temp.list<-vector(mode="list",length=5)
        temp.list[[1]]<-locs
        temp.list[[2]]<-miss.occ
        temp.list[[3]]<-start.month
        temp.list[[4]]<-no.month
        temp.list[[5]]<-create.blocks(Data,
                                      no.of.miss.occasions=miss.occ,
                                      min.no.locations=locs,
                                      start.month=start.month,
                                      Noofmonths=no.month)
        data.for.classifcation[[counter]]<-temp.list
      }
    }
  }
}

length.data.for.classifcation<-length(data.for.classifcation)

months<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

setwd(paste(SaveDir,"/Validation",sep=""))
#for(i in 1:length.data.for.classifcation){
for(i in 3){
  current.data<-data.for.classifcation[[i]]
  blocks<-current.data[[5]][[2]]
  
  name<-paste("19Nov",
              "_NoLocs", current.data[[1]],
              "_NoMiss",  current.data[[2]],
              "_Months", months[current.data[[3]]+1],
              "--",months[current.data[[3]]+no.month],sep="")
  print(paste("classifcation.simulation.validation start:",i))
  classifcation.simulation.validation(blocks,seed=1,name,number.of.sims=200,log.distance=TRUE,number.of.clusters=c(2,3,4,5,6,7),pval=0.05,month=c(current.data[[3]]+1,current.data[[3]]+no.month),choose.blocks=NA)#,
}


# block of 10
2
0
2
0
2
0
2
0
2
0
2
0
2
0
2
0
2
0
2
0