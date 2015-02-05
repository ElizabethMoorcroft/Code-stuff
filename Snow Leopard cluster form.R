#--------------------------------
# Project title: Snow leopards
# Script title: Snow Leopard Cluster
# Purpose: Clusters data, and run cluster and validate
#--------------------------------

blocks.use<-function(data){
  for(i in 1:length(data)){
    rows<-which(!is.na(data[[i]]$hours5.distance))
    data[[i]]<-data[[i]][rows,]
  }
  return(data)
}
# create blocks of data and calculate 5hourly differences
create.blocks<-function(data,start.month,Noofmonths,margin){
  data.months.only<-selecting.months(data,start.month,Noofmonths)
  
  blocks<-into.year.blocks(data=data.months.only,Noofmonths)
  cal.blocks<-vector(mode="list")
  
  for(bl in 1:length(blocks)){
    print(paste(bl,"/",length(blocks)))
    block.data<-blocks[[bl]]
    block.data$block<-bl
    cal.blocks[[bl]]<-displacement.angle.between.location(block.data,margin)
  }
  cal.blocks<-cal.blocks[!is.na(cal.blocks)]
  
  return(cal.blocks)
}
displacement.angle.between.location<-function(block.data,margin){
  l.data<-dim(block.data)[1]
  use.hours<-c(1:5,10,34,144)
  hours.matrix<-matrix(ncol=(length(use.hours)*3+1),nrow=l.data)
  names.of.distances.angle<-lapply(paste("hours",c(use.hours*5),sep=""),function(x){paste(x,c(".before.row",".distance",".angle"),sep="")})
  colnames(hours.matrix)<-c("row",unlist(names.of.distances.angle))

  for(row in 2:l.data){
    hours.matrix[row,1]<-row;
    for(hours in 1:length(use.hours)){
      dist.angle<-calculate.x.hours(block.data,row,xhours=use.hours[hours]*5,margin)
      hours.matrix[row,(hours-1)*3+2]<-dist.angle[3];
      hours.matrix[row,(hours-1)*3+3]<-dist.angle[2];
      hours.matrix[row,(hours-1)*3+4]<-dist.angle[1]
    }
  }
  
  
  block.data<-cbind(block.data,hours.matrix)
  
  if(sum(!is.na(block.data$hours5.distance))<50){block.data<-NA}
  return(block.data)
}
selecting.months<-function(data,start.month,Noofmonths){
  months<-as.POSIXlt(data$Date_Time)$mon # The months of each data point
  Mrows<-which(months>=start.month & months<(start.month+Noofmonths)) # select rows
  if((start.month+Noofmonths)>11){ # if over 
    print("over11")
    leftover<-start.month+Noofmonths-13
    print(leftover)
    Mrows<-c(Mrows,which(months>=0 & months<=leftover))
  }
  data<-data[Mrows,]
  return(data)
}
diff.time.between.locations<-function(data){
  #print(paste("data[1]",data[1],"data[2]",data[2]))
  
  time.difference<-difftime(data[2],data[1],units="hours")
  return(time.difference)
}
# selecting locations from the same "camera trapping season" 
# so that all locations are split into blocks that are 
year.block<-function(data,Noofmonths){
  data.col<-which(names(data)=="Date_Time")
  # select each year block
  start.data<-data[1,data.col]
  row<-2
  time.between.locs<-0
  max.rows<-dim(data)[1]
  max.time<-Noofmonths*31*24
  current.rows<-c(1)  
  while(time.between.locs<max.time){
    time.between.locs<-diff.time.between.locations(data[c(1,row),data.col])
    if(time.between.locs>max.time){break;}
    current.rows<-c(current.rows,row)  # Add row to block
    #print(time.between.locs)
    if(row==max.rows){break;}
    row=row+1
  }
  return(current.rows)
}
into.year.blocks<-function(data,Noofmonths){
  inidividuals<-sort(unique(data$AnimID))
  blocks<-vector(mode="list")
  count<-0
  for(individs in inidividuals){
    # Select the inidivial
    inidividual.data<-data[which(data$AnimID == individs),]
    inidividual.data<-inidividual.data[order(inidividual.data$Date_Time),]
    while(dim(inidividual.data)[1]>1){
      rows<-year.block(data=inidividual.data,Noofmonths)
      count=count+1
      blocks[[count]]<-inidividual.data[rows,]
      inidividual.data<-inidividual.data[-rows,]
    }
  }
  return(blocks)
}
bearing.cal<-function(data.x,data.y){
  if(!is.na(data.x[2]) & !is.na(data.x[1]) & !is.na(data.y[2]) & !is.na(data.y[1])){
    Bearing<-atan2((data.x[2]-data.x[1]),(data.y[2]-data.y[1]))
    if(Bearing<0){Bearing<-Bearing+2*pi}
  }else{Bearing<-NA}
  return(Bearing)
}
# Change in direction
angle.cal<-function(data){   
  # - Calculates the change in bearing
  # - this produces a value between -2pi and 2pi which needs to be corrected
  if(!is.na(data[1]) & !is.na(data[2]) ){
    ChangeAngle<-data[2]-data[1]
    if(ChangeAngle>pi){ChangeAngle<-ChangeAngle-2*pi}
    else if(ChangeAngle<(-pi)){ChangeAngle<-2*pi+ChangeAngle}
  }else{ChangeAngle<-NA}
  return(ChangeAngle)
}#
calculate.x.hours<-function(data,row,xhours,margin){
  data.col<-which(names(data)=="Date_Time")
  data.lat<-which(names(data)=="Latitude")
  data.lon<-which(names(data)=="Longitude")
  values.for.return=c(NA,NA,NA)
  row2<-row-1;row3<-NA
  
  while(row2>0){
    time1<-diff.time.between.locations(data[c(row2,row),data.col])
    
    if(time1>(xhours-margin) & time1<(xhours+margin)){
      row3<-row2-1;
      while(row3>0){
        time2<-diff.time.between.locations(data[c(row3,row),data.col])
        
        if(time2>((xhours+xhours-margin)) & time2<(xhours+xhours+margin)){
          #print(paste("row",row,"row2", row2,"row3", row3, "time1", time1, "time2", time2))
          values.for.return<-calculate.values.for.xhours(data,row,row2,row3,data.lat,data.lon)
          break;
        }else if(time2>(xhours+xhours+margin)){ break;}
        row3=row3-1
      }
      
    }else if(time1>(xhours+margin)){break;}
    row2=row2-1
  }

  return(values.for.return)
}
calculate.values.for.xhours<-function(data,row,row2,row3,data.lat,data.lon){
  bearing1<-bearing.cal(data.x=data[c(row,row2),data.lat],data.y=data[c(row,row2),data.lon])
  bearing2<-bearing.cal(data.x=data[c(row2,row3),data.lat],data.y=data[c(row2,row3),data.lon])
  angle<-angle.cal(data=c(bearing2,bearing1))
  dist<-Distance.between.GPS.in.meters(data[c(row),data.lat],data[c(row2),data.lat],data[c(row),data.lon],data[c(row2),data.lon])
  values.for.return<-c(angle,dist,row2)
  return(values.for.return)
}



#cluster.data<-function(all.test.data,number.of.clusters){
  
  if(!is.na(number.of.clusters)){fit <- Mclust(data = all.test.data, G = number.of.clusters)} else {fit <- Mclust(data = all.test.data)}
  cluster<-summary(fit)[]
  plot(fit); 
  
  return(cluster)
}



# transition matrices
trans.matrix <- function(X, prob=T){
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]))
  if(prob) tt <- tt / rowSums(tt)
  tt
}
cal.matrix<-function(clust,all.test.data){
  print(paste("dim(all.test.data)[1]",dim(all.test.data)[1],"length(clust$classification)",length(clust$classification)))
  new.data<-cbind(all.test.data,clust$classification)
  new.data.matrix<-matrix(ncol=2,nrow=dim(new.data)[1])
  for(i in 1:dim(new.data)[1]){
    temp<-which(new.data[,4] == new.data[i,5] & new.data[,3]==new.data[i,3]); 
    new.data.matrix[i,2]<-new.data[i,6]; # current past
    #print(paste("current state",new.data[i,4]))
    if(length(temp)>0){new.data.matrix[i,1]<-new.data[temp,6]# previous state
                       #print(paste("previous state",new.data[temp,4]))
                       } 
  }
  x<-trans.matrix(new.data.matrix,prob=T)
  return(x)
  
}
cal.matrix.MM2<-function(clust,all.test.data){
  
  print(all.test.data[1:10,])
  print(paste("dim(all.test.data)[1]",dim(all.test.data)[1],"length(clust$classification)",length(clust$classification)))
  class<-clust$classification
  new.data<-cbind(all.test.data,class)
  print(new.data[1:10,])
  new.data.matrix<-matrix(ncol=3,nrow=dim(new.data)[1])
  for(i in 1:dim(new.data)[1]){
    temp<-which(new.data[,4] == new.data[i,5] & new.data[,3]==new.data[i,3]); 
    
    new.data.matrix[i,3]<-new.data[i,6]; # current past
    #print(paste("current state",new.data[i,4]))
    if(length(temp)>0){new.data.matrix[i,2]<-new.data[temp,6]# previous state
                       temp2<-which(new.data[,4] == new.data[temp,5] & new.data[,3]==new.data[i,3]); 
                       if(length(temp2)>0){
                         new.data.matrix[i,1]<-new.data[temp2,6]# previous state -1
                       }
    } 
  }
  print("into matrix")
  
  
  previous.states.matrix<-vector(mode="list")
  for(i in 1:clust$n){
    m<-matrix(ncol=clust$n,nrow=clust$n,0);
    for(j in 1:clust$n){
      temp.rows<-which(new.data.matrix[,1]==i & new.data.matrix[,2]==j)
      #print(temp.rows)
      temp.t<-table(new.data.matrix[temp.rows,3])/length(temp.rows)
      temp.names<-as.numeric(as.character(names(temp.t)))
      m[j,temp.names]<-temp.t
    }
    previous.states.matrix[[i]]<-m
  }
  return(previous.states.matrix)
  
}
caluclate.transition.matrix<-function(clust,all.test.data,dimension){
  new.data<-cbind(all.test.data,clust$classification)
  new.data.matrix<-matrix(ncol=dimension,nrow=dim(new.data)[1])
  for(i in 2:dim(new.data)[1]){
    dimension.count<-1
    new.data.matrix[i,dimension]<-new.data[i,6]; # current state
    row<-i
    while(dimension.count<dimension){
      row<-matching.i.state.to.previous.state(new.data,row)
      if(!is.na(row)){new.data.matrix[i,dimension-dimension.count]<-new.data[row,6];}else{dimension.count=dimension}
      dimension.count<-dimension.count+1
    }
  }
  
  #print(new.data.matrix[1:10,])
  
  #2d - 5
  print("2d")
  matrices<-vector(mode="list",length=dimension-1)
  matrices[[1]]<-matrix.trans(clust,new.data.matrix,dimension) # previous 5hrs
  
  #3d - 5,10
  print("3d")
  temp.matrices<-vector(mode="list",length=clust$n)
  for(cluster in 1:clust$n){
    dim.new.data.matrix<-new.data.matrix[which(new.data.matrix[,dimension-2]==cluster),]
    temp.matrices[[cluster]]<-matrix.trans(clust,dim.new.data.matrix,dimension)
  }
  matrices[[2]]<-temp.matrices
  
  #4d - 5,10, 15
  print("4d")
  temp.matrices<-vector(mode="list",length=clust$n)
  for(cluster2 in 1:clust$n){
    dim.new.data.matrix<-new.data.matrix[which(new.data.matrix[,dimension-3]==cluster2),]
    temp2.matrices<-vector(mode="list",length=clust$n)
    for(cluster in 1:clust$n){
      dim2.new.data.matrix<-dim.new.data.matrix[which(dim.new.data.matrix[,dimension-2]==cluster),]
      dim2.new.data.matrix<-matrix(ncol=dimension,dim2.new.data.matrix)
      if(dim(dim2.new.data.matrix)[1]>1){
        temp2.matrices[[cluster]]<-matrix.trans(clust,dim2.new.data.matrix,dimension)
      }
    }
    temp.matrices[[cluster2]]<-temp2.matrices
  }
  matrices[[3]]<-temp.matrices
  
  
  #5d - 5,10,15
  print("5d")
  temp.matrices<-vector(mode="list",length=clust$n)
  for(cluster3 in 1:clust$n){
    dim.new.data.matrix<-new.data.matrix[which(new.data.matrix[,dimension-3]==cluster2),]
    temp3.matrices<-vector(mode="list",length=clust$n)
    for(cluster2 in 1:clust$n){
      dim3.new.data.matrix<-dim.new.data.matrix[which(dim.new.data.matrix[,dimension-3]==cluster2),]
      temp2.matrices<-vector(mode="list",length=clust$n)
      for(cluster in 1:clust$n){
        dim2.new.data.matrix<-dim3.new.data.matrix[which(dim3.new.data.matrix[,dimension-2]==cluster),]
        dim2.new.data.matrix<-matrix(ncol=dimension,dim2.new.data.matrix)
        if(dim(dim2.new.data.matrix)[1]>1){
         temp2.matrices[[cluster]]<-matrix.trans(clust,dim2.new.data.matrix,dimension)
        }
      }
      temp3.matrices[[cluster2]]<-temp2.matrices
    }
    temp.matrices[[cluster3]]<-temp3.matrices
  }
  matrices[[4]]<-temp.matrices
  
  print(matrices[[1]])
  return(matrices)
}
matrix.trans<-function(clust,new.data.matrix,dimension){
  m<-matrix(ncol=clust$n,nrow=clust$n,0);
  for(i in 1:clust$n){
    temp.rows<-which(new.data.matrix[,dimension-1]==i)
    #print(temp.rows)
    temp.t<-table(new.data.matrix[temp.rows,dimension])/length(temp.rows)
    temp.names<-as.numeric(as.character(names(temp.t)))
    m[i,temp.names]<-temp.t
  }
  return(m)
}
matching.i.state.to.previous.state<-function(new.data,i){
  # temp is the row before i the row before
  temp<-which(new.data[,4] == new.data[i,5] # [i,5] is the name of the row previous, [,4] name of rows
              & new.data[,3]==new.data[i,3]) # block is the same
  
  if(length(temp)>0){row.before<-temp}else{row.before<-NA}
  return(row.before)
}
cal.matrix.MM3<-function(clust,all.test.data){
  #print(paste(dim(all.test.data),length(clust$classification)))
  new.data<-cbind(all.test.data,clust$classification)
  new.data.matrix<-matrix(ncol=4,nrow=dim(new.data)[1])
  for(i in 1:dim(new.data)[1]){
    temp<-which(new.data[,4] == new.data[i,5] & new.data[,3]==new.data[i,3]); 
    
    new.data.matrix[i,4]<-new.data[i,6]; # current state
    #print(paste("current state",new.data[i,4]))
    if(length(temp)>0){
      new.data.matrix[i,3]<-new.data[temp,6]# previous state
      temp2<-which(new.data[,4] == new.data[temp,5] & new.data[,3]==new.data[i,3]); 
      if(length(temp2)>0){
        new.data.matrix[i,2]<-new.data[temp2,6]# previous state -1
        temp3<-which(new.data[,4] == new.data[temp2,5] & new.data[,3]==new.data[i,3]); 
        if(length(temp3)>0){
          new.data.matrix[i,1]<-new.data[temp3,6]# previous state -2
        }
                         
       }
     #print(paste("previous state",new.data[temp,4]))
    } 
  }
    
  previous.states2.matrix<-vector(mode="list");
  for(k in 1:9){
    previous.states.matrix<-vector(mode="list");
    for(i in 1:9){
    m<-matrix(ncol=9,nrow=9,0);
    for(j in 1:9){
      temp.rows<-which(new.data.matrix[,1]==i & new.data.matrix[,2]==j)
      temp.t<-table(new.data.matrix[temp.rows,3])/length(temp.rows)
      temp.names<-as.numeric(as.character(names(temp.t)))
      m[j,temp.names]<-temp.t
    }
    
    previous.states.matrix[[i]]<-m
  
  }
  previous.states2.matrix[[k]]<-previous.states.matrix
  }
  return(previous.states2.matrix)
  
}
#caluclate.transition.matrix(clust,all.test.data=cluster.data.block,dimension=4)

#Simulate movement
new.location<-function(current.location, distance, angle, log.distance){
  new.location<-c()
  if(log.distance==TRUE){distance<-exp(distance)}
  new.location[1]<-distance*sin(angle)+current.location[1]
  new.location[2]<-distance*cos(angle)+current.location[2]
  return(new.location)
}
path.of.animal<-function(number.of.steps,seed,cluster,log.distance,matrix.trans3,matrix.trans1,transitions,max.area.hr,runin){
  # Creates a stream of random numbers
  set.seed(seed)
  random.stream<-sample(size=(number.of.steps+runin+2)*50,x=(number.of.steps+runin)*10^6)[(1:(number.of.steps+runin+2))*50]
  
  max.radius<-sqrt(max.area.hr/pi)
  # Sets the initial time and location and state
  new.time<-as.POSIXlt('2000-1-1 00:00:00')
  new.location.est<-c(0,0)#c(rnorm(1,0,100),rnorm(1,0,100))
  
  set.seed(random.stream[number.of.steps+runin+1])
  state<-which(rmultinom(1, size=1, prob=cluster$pro)==1)
  #state2<-which(rmultinom(1, size=1, prob=cluster$pro)==1)
  #state3<-which(rmultinom(1, size=1, prob=cluster$pro)==1)
  #print(state)
  set.seed(random.stream[number.of.steps+runin+2])
  current.location<-matrix(ncol=7,c(new.location.est,as.character(new.time),NA,NA,state,runif(1,-pi,pi)))
  
  #print(current.location)  
  
  for(step in 1:(number.of.steps+runin)){
    number.of.est<-0
    #print(paste(step,"/",number.of.steps))
    dist.from.centre<-0
    new.seed<-random.stream[step]
    while(dist.from.centre==0){
      #print(state)
      set.seed(new.seed)
      random.stream.two<-sample(size=100,x=100*10^6)
      
      if(number.of.est>1*10^4){
        point<-new.point(cluster,random.stream.two[50],log.distance,matrix.trans3,matrix.trans1,state=state,transitions=FALSE)
        angle<-point[2]+as.numeric(as.character(current.location[step,7]))
        new.location.temp<-new.location(new.location.est, distance=point[1], angle=angle,log.distance)
      }else{
        point<-new.point(cluster,random.stream.two[50],log.distance,matrix.trans3,matrix.trans1,state=state,transitions)
        angle<-point[2]+as.numeric(as.character(current.location[step,7]))
        new.location.temp<-new.location(new.location.est, distance=point[1], angle=angle,log.distance)
      }
     # print(sqrt(new.location.temp[1]^2+new.location.temp[2]^2))
      if(sqrt(new.location.temp[1]^2+new.location.temp[2]^2)<max.radius){
         dist.from.centre=1
        } else {
          new.seed<-random.stream.two[100]
          number.of.est<-number.of.est+1
      }
    }
    new.location.est<-new.location.temp
    state<-point[3];state2<-point[4];state3<-point[5]
    new.time<-new.time+5*60*60
    
    current.location<-rbind(current.location,c(new.location.est,as.character(new.time),distance=exp(point[1]),angle=point[2],state,angle))	
    
  }
  colnames(current.location)<-c("x","y","Date_Time","block_dist.difference_5_hrs","block_angle.difference_5_hrs","State","Bearing")
  
  current.location<-current.location[-c(1:runin),]
  
  current.location<-distance.x.hours(data=current.location,hours=c(2:5,10,34,144)*5,lat="y",long="x",method="NotGPS")
  
  return(current.location)
}
new.point<-function(cluster,seed,log.distance,matrix.trans3,matrix.trans1,state,transitions){
  point<-matrix(ncol=2,nrow=1)
  set.seed(seed)
  seed.vector<-sample(size=300,x=1*10^6)[c(100,200,300)]
  set.seed(seed.vector[1])
  if(transitions==TRUE){
    matrix.trans<-matrix.trans3
    #print(state)
    prob.trans=matrix.trans3[state,]
    if(all(prob.trans==0)){
      print("error");break;
    } else{
      i<-which(rmultinom(1, size=1, prob=matrix.trans[state,])==1)
    }
  }else{i<-which(rmultinom(1, size=1, prob=cluster$pro)==1)}
  
  #print(i)
  seed<-seed.vector[2]
  while(is.na(point[,1])| is.na(point[,2])){
    set.seed(seed)
    seed.vector.loop<-sample(size=200,x=1*10^6)[c(100,200)]
    set.seed(seed.vector.loop[1])
    point<-rmvnorm(n=1, mean = cluster$mean[,i], sigma = cluster$variance[[i]])
    seed<-seed.vector.loop[2]
    if(log.distance==FALSE){ if(point[,1]<0 | point[,1]>15000){point[,1]<-NA}}
    else{ if(exp(point[,1])<0 | exp(point[,1])>15000){point[,1]<-NA}}
    #if(point[,2]<0 | point[,2]>pi){point[,2]<-NA}
    if(point[,2]< (-pi) ){point[,2]<-point[,2]+2*pi}else if( point[,2]>pi){point[,2]<-point[,2]-2*pi}
  }
  set.seed(seed.vector[3])
  sign<-rbinom(1,1,0.5)*2-1
  point[,1]<-point[,1]
  point[,2]<-point[,2]*sign
  #print(point[,2])
  point<-c(point,i,state,NA)
  return(point)
}

#plots
plots.validation<-function(valid.data,sim.data,simulation.numbers,transitions=TRUE){
  
  print(paste("length(valid.data)",length(valid.data)))
  
  ymax<-c(0.001,0.0005,0.0004,0.0004,0.00025,0.00025,0.00015,0.00015)
  xmax<-c(20000,25000,25000,30000,30000,35000,40000,45000)
  h<-c(1:5,10,34,144)
  temp.fun<-function(x){plot.dist.angle(simulation.numbers=simulation.numbers,valid.data=valid.data,sim.data=sim.data$movement,hours=h[x]*5,ymax[x],xmax[x])}
  lapply(1:8,temp.fun)
  plot.home.range(simulation.numbers=simulation.numbers,valid.data=valid.data,sim.data=sim.data$movement)
  
}
plot.clusters<-function(Data){
  #print(Data)
  no.clusters<-length(unique(Data[,3]))
  plot(0,type="n",
       main="Clusters",
       xlab="Change in direction [radians]", xlim=c(min(Data[,2]),max(Data[,2])),
       ylab="Displacement [log(meters)]", ylim=c(min(Data[,1]),max(Data[,1]))
  )
  for(i in 1:no.clusters){
    temp<-Data[which(Data[,3]==i),2:1]
    points(temp,col=COL[i],pch=20)
  }
}
plot.dist.angle<-function(simulation.numbers,valid.data,sim.data,hours,ymax,xmax){
  op<-par()
  print(paste("hours",hours,"ymax",ymax))
  print("Distance")
  dist.name.sim<-paste("block_dist.difference_",hours,"_hrs",sep="")
  dist.name.val<-paste("hours",hours,".distance",sep="")
  par(op)
  plot(0,type="l",xlim=c(0,xmax),ylim=c(0,ymax),
       #main=paste("run.blocks=",paste(run.blocks,collapse=","),", ",hours,"hours",sep="")
       main="", sub=paste(hours,"hours",sep=""),
       xlab="Distance [meters]",ylab="Density")
  for(j in simulation.numbers){
    temp.data<-sim.data[[j]]
    col.number<-which(names(temp.data)==dist.name.sim)
    points(density(as.numeric(as.character(temp.data[,col.number])),from=0,na.rm=T),type="l",col=COL[1])
  }
  for(j in 1:length(valid.data)){
    temp.data<-valid.data[[j]]
    col.number<-which(names(temp.data)==dist.name.val)
    try(points(density((temp.data[,col.number]),na.rm=T,from=0),type="l",col=COL[4],lwd=1.5))
  }
  legend(y=ymax,x=15000,legend=c("Simulated data", "Validation data"),col=c(COL[1],COL[4]),lty=c(1,1),lwd=c(2,2))
  print("Distance - finished")
  
  print("Angle")  
  angle.name.sim<-paste("block_angle.difference_",hours,"_hrs",sep="")
  angle.name.val<-paste("hours",hours,".angle",sep="")
  PlotCircularGraphs(Data=valid.data[[1]][,which(names(valid.data[[1]])==angle.name.val)],round=1,COL=COL[4],ADD=F,Radius=6)
  for(j in simulation.numbers){
    temp.data<-sim.data[[j]]
    col.number<-which(names(temp.data)==angle.name.sim)    
    PlotCircularGraphs(Data=as.numeric(as.character(temp.data[,col.number])),round=1,COL=COL[1],ADD=T,Radius=6)
  }
  for(j in 1:length(valid.data)){
    temp.data<-valid.data[[j]]
    col.number<-which(names(temp.data)==angle.name.val)
    try(PlotCircularGraphs(Data=temp.data[,col.number],round=1,COL=COL[4],ADD=T,Radius=6))
    legend(y=7.5,x=2,legend=c("Simulated data", "Validation data"),col=c(COL[1],COL[4]),lty=c(1,1))
  }
  par(op)
}
plot.home.range<-function(simulation.numbers,valid.data,sim.data){
  area<-c()
  par(mfrow=c(1,1))
  plot(0,type="n",xlim=c(0,100),ylim=c(0,100),ylab="%age locations within area",xlab="%age of inner area",sub="")
  for(i in simulation.numbers){
    validation.erosion<-plot.erosion.of.convex.hull(cbind(as.numeric(as.character(sim.data[[i]][,1])),as.numeric(as.character(sim.data[[i]][,2]))),no.levels=500,index.divider=50)
    points(validation.erosion[[1]][,1:2],type="l",col=COL[1])
    area<-c(area,as.numeric(validation.erosion[[2]]))
  }
  
  area.valid<-c()
  for(j in 1:length(valid.data)){
    validation.erosion<-NA
    try(validation.erosion<-plot.erosion.of.convex.hull(x.y.locations(lat=valid.data[[j]]$Latitude,lon=valid.data[[j]]$Longitude),no.levels=500,index.divider=500))
    if(!is.na(validation.erosion[1])){points(validation.erosion[[1]][,1:2],type="l",col=COL[4])}
    area.valid<-c(area.valid,as.numeric(validation.erosion[[2]]))
  }
  legend(y=100,x=0,legend=c("Simulated data", "Validation data"),col=c(COL[1],COL[4]),lty=c(1,1),lwd=c(2,2))
  
  
  print(sqrt(area.valid/pi))
  
  dens<-density(area,adjust=2)
  x<-dens[]$x
  y<-dens[]$y
  temp.var<-c();for(j in 1:length(valid.data)){ temp.var<-c(temp.var,area.valid[j]) }
  plot(dens,xlim=c(0,max(max(x),max(temp.var))),xlab="Size of home range",ylim=c(0,2.5*10^-9),col=COL[1],main="")
  points(x=temp.var,y=rep(0,length(valid.data)),col=COL[4],pch=19)
  legend(y=2.5*10^-9,x=0,legend=c("Density of simulated data home range size", "Size of home range in validation data"),
         col=c(COL[1],COL[4]),lty=c(1,NA),lwd=c(2,NA),pch=c(NA,19),cex=0.8)

}

#data rearrnage
cal.cluster.data<-function(list.data,number.of.clusters){
  data<-list.data[[1]]
  if(length(list.data)>1){for(i in 2:length(list.data)){
    temp.data<-list.data[[i]];
    data<-rbind(data,temp.data)}
  }else{print("hello")}
  
  # print(data[1:10,])
  all.test.data<-cbind(data$hours5.distance,data$hours5.angle,data$block,data$row,data$hours5.before.row)
  
  # print(all.test.data[1:10,])
  all.test.data<-all.test.data[which(!is.na(all.test.data[,1]) & !is.na(all.test.data[,2]) ),]
  all.test.data[which(all.test.data[,1]<=0.5),1]<-0.5
  all.test.data[,1]<-log(all.test.data[,1])
  all.test.data[,2]<-abs(all.test.data[,2])
  
  
  return(all.test.data)
}
max.area<-function(data,hr.cof){
  area<-c()
  for(i in 1:length(data)){
    validation.erosion<-plot.erosion.of.convex.hull(x.y.locations(lat=data[[i]]$Latitude,lon=data[[i]]$Longitude),no.levels=500,index.divider=50)
    area<-c(area,as.numeric(validation.erosion[[2]]))
  }
  max.area<-max(area,na.rm=T)*sqrt(hr.cof)
  print(area)
  print(max.area)
  return(max.area)
}

#order functions
cluster.simulate.plot<-function(Data=test,simulation.numbers,run.blocks,valid.blocks,transitions=TRUE,hr.cof,cluster.seed){
  sim.data<-simulate(data=Data,run.blocks,valid.blocks,transitions,simulation.numbers,hr.cof,cluster.seed)
  plots.validation(valid.data=Data[valid.blocks],sim.data,simulation.numbers,transitions=TRUE)
  return(sim.data)
}
simulate<-function(data,run.blocks,valid.blocks,transitions,simulation.numbers,hr.cof,cluster.seed){
  print(paste("valid.blocks",valid.blocks))
  temp.candt<-cluster.and.transistions(data=data,run.blocks=run.blocks,valid.blocks=valid.blocks,transitions=transitions,cluster.seed=cluster.seed,hr.cof=hr.cof)
  clust<-temp.candt$clust
  matrix.trans<-temp.candt$matrix.trans
  matrix.trans3<-temp.candt$matrix.trans3
  matrix.trans1<-temp.candt$matrix.trans1
  
  print(paste("clust$n=",clust$n)) 
  print(paste("max.area.hr=temp.candt$max.area.hr",temp.candt$max.area.hr))
  pp<-vector(mode="list")
  for(j in simulation.numbers){
    print(paste("j:",j,"/",rev(simulation.numbers)[1]))
    pp[[j]]<-path.of.animal(number.of.steps=3*24*6,seed=j,cluster=clust,log.distance=TRUE,matrix.trans3=matrix.trans3,matrix.trans1=matrix.trans1,transitions,max.area.hr=temp.candt$max.area.hr,runin=round(30*24/5))
  }
  return(list("clust"=clust,"matrix.trans"=matrix.trans,"movement"=pp,"valid"=data[valid.blocks]))
}
cluster.and.transistions<-function(data,run.blocks,valid.blocks,transitions,cluster.seed,hr.cof){
  temp.data<-data[run.blocks];temp2.data<-data[valid.blocks]
  cluster.data.block<-cal.cluster.data(list.data=temp.data,number.of.clusters=NA)
  # clust<-cluster.data(all.test.data=cluster.data.block[,1:2],number.of.clusters=NA)
  set.seed(1)
  xem1<-mixmodCluster(as.data.frame(cluster.data.block[,1:2]),1:20,criterion = "BIC",strategy =mixmodStrategy(seed=cluster.seed))

  var<-vector(mode="list"); for(i in 1:xem1[9][1]){var[[i]]<-t(xem1[9][6][3][[i]])}
  clust<-list("n"=xem1[9][1],"pro"=xem1[9][6][1],"mean"=t(xem1[9][6][2]),"variance"=var,"classification"=xem1[12]) 
  
  plot.clusters(Data=cbind(cluster.data.block[,1:2],clust$classification))
  if(transitions==TRUE){
      matrix.trans<-caluclate.transition.matrix(clust,all.test.data=cluster.data.block,dimension=4)
      matrix.trans3<-matrix.trans[[1]]#cal.matrix.MM2(clust,cluster.data.block)
      matrix.trans1<-matrix.trans[[1]]#cal.matrix(clust,cluster.data.block)
  } else{
      matrix.trans<-matrix(ncol=length(clust$pro),nrow=length(clust$pro),clust$pro,byrow=T)

  }
  max.area.hr<-max.area(temp.data,hr.cof)
  return(list("clust"=clust,"matrix.trans3"=matrix.trans3,"matrix.trans1"=matrix.trans1,"matrix.trans"=matrix.trans,"max.area.hr"=max.area.hr))
}

#misc
x.y.locations<-function(lat,lon){
  # θ=(θ1+θ2)/2, ϕ=(ϕ1+ϕ2)/2:
  lat<-lat*pi/180;lon<-lon*pi/180
  #mid.theta<-lat[1]-lat[1:length(lat)]
  #mid.phi<-lon[1]-lat[1:length(lon)]
  # x= sinθcosϕ(θ2−θ1)−cosθsinϕ(ϕ2−ϕ1)
  # y= sinθsinϕ(θ2−θ1)+cosθcosϕ(ϕ2−ϕ1)
  #x=6371000*sin(mid.theta)*cos(mid.phi)*(lat[1:length(lat)]-lat[1])-cos(mid.theta)*sin(mid.phi)*(lon[1:length(lon)]-lon[1])
  #y=6371000*sin(mid.theta)*sin(mid.phi)*(lat[1:length(lat)]-lat[1])-cos(mid.theta)*cos(mid.phi)*(lon[1:length(lon)]-lon[1])
  
  #x = R * cos(lat) * cos(lon)
  #y = R * cos(lat) * sin(lon)
  x=6371000*cos(lat)*cos(lon)
  y=6371000*cos(lat)*sin(lon)
  
  x<-x-x[1];
  y<-y-y[1]
  
  return(cbind(x,y))
}


#summary
details<-function(data){
  matrix.animal.details<-matrix(ncol=7,nrow=length(data),"CH")
  colnames(matrix.animal.details)<-c("Name","Sex","Year","Days (hours) surveyed over","No.of locations","No.locations usable in model","Size of Home Range")
  for(i in 1:length(data)){
    print(i)
    temp.data<-data[[i]]
    n<-dim(temp.data)[1]
    matrix.animal.details[i,1]<-as.character(unique(temp.data$Name))
    matrix.animal.details[i,2]<-as.character(unique(temp.data$Sex))
    matrix.animal.details[i,3]<-paste(min(as.POSIXlt(temp.data$Date_Time)$year)+1900,"-",max(as.POSIXlt(temp.data$Date_Time)$year)+1900)
    matrix.animal.details[i,4]<-paste(round(difftime(temp.data$Date_Time[n],temp.data$Date_Time[1],units="days"),1)," (",round(difftime(temp.data$Date_Time[n],temp.data$Date_Time[1],units="hours"),1),")",sep="")
    matrix.animal.details[i,5]<-n
    matrix.animal.details[i,6]<-sum(!is.na(temp.data$hours5.distance))
    area<-NA
    try(area<-plot.erosion.of.convex.hull(x.y.locations(lat=as.numeric(as.character(temp.data$Latitude)),lon=as.numeric(as.character(temp.data$Longitude))), index.divider=500,no.levels=10)$area)
    matrix.animal.details[i,7]<-names(table(area))[1]
  }
  
  #female.rows<-which(matrix.animal.details[,6]=="F")
  #matrix.animal.details[length(data)+1,1]<-"All females"
  #matrix.animal.details[length(data)+1,2]<-"F"
  #all.years<-unique(as.numeric(unlist(strsplit(matrix.animal.details[female.rows,3], "-"))))
  #matrix.animal.details[length(data)+1,3]<-paste(min(all.years),"-",max(all.years))
  #all.days<-unique(as.numeric(unlist(strsplit(matrix.animal.details[female.rows,4], "("))))
  #matrix.animal.details[length(data)+1,4]<-paste(round(difftime(temp.data$Date_Time[n],temp.data$Date_Time[1],units="days"),1)," (",round(difftime(temp.data$Date_Time[n],temp.data$Date_Time[1],units="hours"),1),")",sep="")
  #matrix.animal.details[length(data)+1,5]<-n
  #matrix.animal.details[length(data)+1,6]<-sum(!is.na(temp.data$hours5.distance))
  
  return(matrix.animal.details)
}



#sim<-simulate(data=test,run.blocks=(c(1:11,13:14)),valid.blocks=(c(12)),COL=COL,transitions=T,simulation.numbers=1:50)
#plot.dist.angle(simulation.numbers=1:50,valid.data=test[12],sim.data=sim$movement,hours=5)
#plot.home.range(simulation.numbers=1:50,valid.data=test[12],sim.data=sim$movement)
set.seed(7);b<-sample(15:24,3)
pdf(paste("movement-",paste(b,collapse="-"),"--30Jan--hrcof5--transF.pdf",sep=""))
#test<-create.blocks(Data,start.month=4,Noofmonths=3,margin=5/60)

xx<-cluster.simulate.plot(Data=test,simulation.numbers=1:100,run.blocks=(-c(1:14,b)),valid.blocks=(c(b)),
                          hr.cof=5,cluster.seed=1,transition=F)
write.table(xx[[1]]$pro,"movement-18-20-22--30Jan--hrcof5.csv",append=F,sep=",")
write.table(xx[[1]]$mean,"movement-18-20-22--30Jan--hrcof5.csv",append=T,sep=",")
write.table(xx[[2]][[1]],"movement-18-20-22--30Jan--hrcof5.csv",append=T,sep=",")
for(i in 1:100){  plot(as.numeric(as.character(xx[[3]][[i]][,1])),as.numeric(as.character(xx[[3]][[i]][,2])),
                       type="l",xlab="",ylab="",col=COL[1],axes=FALSE
                       );
                  min.x<-min(as.numeric(as.character(xx[[3]][[i]][,1])))
                  max.y<-max(as.numeric(as.character(xx[[3]][[i]][,2])))
                  legend(x=min.x,y=max.y,legend="Simulated movement",col=COL[1],lty=1)
                  box()
}
for(i in b){plot(test[[i]]$ECEF_X,test[[i]]$ECEF_Y,type="l",col=COL[4],xlab="",ylab="",axes=F);
            min.x<-min(test[[i]]$ECEF_X)
            max.y<-max(test[[i]]$ECEF_Y)
            legend(x=min.x,y=max.y,legend="Real movement",col=COL[4],lty=1);
            box()}
dev.off()


pdf("movement-1to4--30Jan--hrcof5--transF.pdf")
#test<-create.blocks(Data,start.month=4,Noofmonths=3,margin=5/60)
b<-c(1:4);
xx<-cluster.simulate.plot(Data=test,simulation.numbers=1:100,run.blocks=(-c(15:24,b)),valid.blocks=(c(b)),hr.cof=5,
                          cluster.seed=1,transition=F)
write.table(xx[[1]]$pro,"movement-1to4--30Jan--hrcof5.csv",append=F,sep=",")
write.table(xx[[1]]$mean,"movement-1to4--30Jan--hrcof5.csv",append=T,sep=",")
write.table(xx[[2]][[1]],"movement-1to4--30Jan--hrcof5.csv",append=T,sep=",")
for(i in 1:100){
  plot(as.numeric(as.character(xx[[3]][[i]][,1])),as.numeric(as.character(xx[[3]][[i]][,2])),
       type="l",xlab="",ylab="",col=COL[1],axes=FALSE);
  min.x<-min(as.numeric(as.character(xx[[3]][[i]][,1])))
  max.y<-max(as.numeric(as.character(xx[[3]][[i]][,2])))
  legend(x=min.x,y=max.y,legend="Simulated movement",col=COL[1],lty=1)
  box()
}
for(i in b){plot(test[[i]]$ECEF_X,test[[i]]$ECEF_Y,type="l",col=COL[4],xlab="",ylab="",axes=F);
            min.x<-min(test[[i]]$ECEF_X)
            max.y<-max(test[[i]]$ECEF_Y)
            legend(x=min.x,y=max.y,legend="Real movement",col=COL[4],lty=1);
            box()}
dev.off()




test.summer<-create.blocks(Data,start.month=4,Noofmonths=3,margin=5/60)
test.winter<-create.blocks(Data,start.month=10,Noofmonths=3,margin=5/60)
d.winter<-details(test.winter)

pdf("TestAll_hrcof2.pdf")
set.seed(7);b<-sample(1:14,4)
female_transF<-cluster.simulate.plot(Data=test,simulation.numbers=1:100,run.blocks=(-c(15:24,b)),valid.blocks=(c(b)),
                          hr.cof=2,cluster.seed=1,transition=F)
female_transT<-cluster.simulate.plot(Data=test,simulation.numbers=1:100,run.blocks=(-c(15:24,b)),valid.blocks=(c(b)),
                                     hr.cof=2,cluster.seed=1,transition=T)
set.seed(7);b<-sample(15:24,3)
male_transF<-cluster.simulate.plot(Data=test,simulation.numbers=1:100,run.blocks=(-c(1:14,b)),valid.blocks=(c(b)),
                                     hr.cof=2,cluster.seed=1,transition=F)
male_transT<-cluster.simulate.plot(Data=test,simulation.numbers=1:100,run.blocks=(-c(1:14,b)),valid.blocks=(c(b)),
                                     hr.cof=2,cluster.seed=1,transition=T)
dev.off()




#### Movement 

activity.checker<-function(data){
  locations<-x.y.locations(data$Latitude,data$Longitude)
  t<-table(round(locations[,1]/10000),round(locations[,2]/10000))
  #filled.contour(t)
  number.of.squares<-sum(t>=1)
  return(number.of.squares)
}

activity.checker.sim.data<-function(data,length){
  select<-sample(1:dim(data)[1],length)
  x<-as.numeric(as.character(data[select,1]));y<-as.numeric(as.character(data[select,2]))
  t<-table(round(x/10000),round(y/10000))
  number.of.squares<-sum(t>=1)
  #filled.contour(t)
  return(number.of.squares)
}
set.seed(1)
ac_m<-c();for(i in 15:24){ac_m<-c(ac_m,activity.checker(test[[i]]))}
mean.no.of.locations<-mean(unlist(lapply(15:24,function(x){sum(!is.na(test[[x]]$Longitude))})))
ac_m_f<-c();for(i in 1:10){ac_m_f<-c(ac_m_f,activity.checker.sim.data(male_transF$movement[[i]],mean.no.of.locations))}
ac_m_t<-c();for(i in 1:10){ac_m_t<-c(ac_m_t,activity.checker.sim.data(male_transT$movement[[i]],mean.no.of.locations))}
plot(density(ac_m,from=1),ylim=c(0,0.3),col=COL[4])
points(density(ac_m_f,from=1),type="l",lty=2,col=COL[1])
points(density(ac_m_t,from=1),type="l",col=COL[1])
set.seed(1)
ac_f<-c();for(i in 1:14){ac_f<-c(ac_f,activity.checker(test[[i]]))}
mean.no.of.locations<-mean(unlist(lapply(1:14,function(x){sum(!is.na(test[[x]]$Longitude))})))
ac_f_f<-c();for(i in 1:10){ac_f_f<-c(ac_f_f,activity.checker.sim.data(female_transF$movement[[i]],mean.no.of.locations))}
ac_f_t<-c();for(i in 1:10){ac_f_t<-c(ac_f_t,activity.checker.sim.data(female_transT$movement[[i]],mean.no.of.locations))}
 plot(density(ac_f,from=1),ylim=c(0,0.4),xlim=c(1,18),col=COL[4])
points(density(ac_f_f,from=1),type="l",lty=2,col=COL[1])
points(density(ac_f_t,from=1),type="l",col=COL[1])