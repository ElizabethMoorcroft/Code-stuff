selecting.months<-function(data,start.month,Noofmonths){
  months<-as.POSIXlt(data$Date_Time)$mon # The months of each data point
  Mrows<-which(months>=start.month & months<(start.month+Noofmonths)) # select rows
  if((start.month+Noofmonths)>11){ # if over 
    leftover<-start.month+Noofmonths-13
    Mrows<-c(Mrows,which(months>=0 & months<leftover))
  }
  data<-data[Mrows,]
  return(data)
}

diff.time.between.locations<-function(data){
  time.difference<-difftime(data[2],data[1],units="hours")
  return(time.difference)
}

direction.of.animal<-function(data){

  if(!is.na(data$Longitude[2]) & !is.na(data$Longitude[1]) & !is.na(data$Latitude[2]) & !is.na(data$Latitude[1])){
    Bearing<-atan2((data$Longitude[2]-data$Longitude[1]),(data$Latitude[2]-data$Latitude[1]))
    if(Bearing<0){Bearing<-Bearing+2*pi}
  }else{Bearing<-NA}
  return(Bearing)
}

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

add.blanks.and.calculate.values<-function(data,no.of.miss.occasions){
  #print("data");print(data)
  possible.difference<-(1:no.of.miss.occasions)*5
  
  
  l<-dim(data)[1]
  time<-diff.time.between.locations(data$Date_Time[c(l-1,l)])
  
 # print(l);print(time)
  
  for(j in 1:no.of.miss.occasions){
    if(abs(time-possible.difference[j])<0.5){
      temp.blank<-as.data.frame(matrix(ncol=dim(data)[2],nrow=(j-1)));names(temp.blank)<-names(data)
      temp<-rbind(data[1:(l-1),],temp.blank,data[l,]); 
     # print("temp");print(temp)
      temp$block_time.difference[l+j-1]<-time
      temp$block_dist.difference[l+j-1]<-Distance.between.GPS.in.meters(temp$Latitude[j+l-2],temp$Latitude[j+l-1],temp$Longitude[j+l-2],temp$Longitude[j+l-1])
      temp$block_bearing[l+j-1]<-direction.of.animal(temp[c(j+l-2,j+l-1),])
      #
      temp$block_angle.difference[l+j-1]<-change.in.animal.direction(temp[c(j+l-2,j+l-1),])
      break;
    }
  }
  return(temp)
}

convert.locid.into.data<-function(data,no.of.miss.occasions,loc.ids,min.no.locations){
    
  l.data<-length(loc.ids);
  tempdata<-data[which(data$LocID %in% loc.ids),]
    
  block_number<-rep(NA,l.data);
  block_dist.difference<-rep(NA,l.data);block_time.difference<-rep(NA,l.data)
  block_bearing<-rep(NA,l.data);block_angle.difference<-rep(NA,l.data)
  
  if(l.data>=min.no.locations){
    tempdata<-as.data.frame(cbind(tempdata,block_number,block_dist.difference,block_time.difference,block_bearing,block_angle.difference))
    newdata<-tempdata[1,]
    for(i in 2:length(loc.ids)){
      newdata<-rbind(newdata,tempdata[i,])
      newdata<-add.blanks.and.calculate.values(newdata,no.of.miss.occasions)   
      #newdata<-rbind(newdata,temp)
    }
    newdata$block_number<-paste(newdata$AnimID[1],"N",i,sep="")
  }else{newdata<-NA}
  return(newdata)
}

animal.run<-function(data,no.of.miss.occasions){
  animal<-sort(unique(data$AnimID))
  animal.list<-vector(mode="list",length=length(animal))
  for(i in 1:length(animal)){
    temp.data<-data[which(data$AnimID==animal[i]),]
    animal.list[[i]]<-blocks.within.animal(temp.data,no.of.miss.occasions)
  }
  return(animal.list)
}

blocks.within.animal<-function(data,no.of.miss.occasions){
  list.of.blocks<-list()
  counter<-1
  while(dim(data)[1]>1){
    #print(dim(data)[1])
    ids<-find.next.location(data,no.of.miss.occasions)
    remove.rows<-which(data$LocID %in% ids)
    data<-data[-remove.rows,]
    list.of.blocks[[counter]]<-ids
    counter<-counter+1
  }

  return(list.of.blocks)
}

find.next.location<-function(data,no.of.miss.occasions){
  ids<-c()
  current.id<-c(data$LocID[1])
  max.loc.id<-max(data$LocID,na.rm=T)
  #print(paste("currentid",current.id,"max.id",max.loc.id))
  while(current.id<max.loc.id){ # IDs for locations are concurrent and increasin through time
    ids<-c(ids,current.id)
    current.id<-id.of.next.location(data,no.of.miss.occasions)
    #print(paste("current.id",current.id))
    if(is.na(current.id)){break;}               
    if(current.id==max.loc.id){break;}    
    row.number<-which(data$LocID==current.id)
    data<-data[-c(1:(row.number-1)),]
  }
  return(ids)
}

id.of.next.location<-function(data,no.of.miss.occasions){
  possible.difference<-(1:no.of.miss.occasions)*5
  time<-apart.X.hrs(data$Date_Time)
  location<-NA;counter<-1
  while( counter<=no.of.miss.occasions){ #is.na(location) &
    location<-close.to.t.hrs.apart(time,possible.difference[counter])
    if(!is.na(location)){counter=counter+no.of.miss.occasions}
    counter<-counter+1
  }
  if(is.na(location)){value<-NA}else{value<-data$LocID[location]}
  return(value)
}

close.to.t.hrs.apart<-function(time,t){
  abs.difference<-abs(time-t)
  mn.difference<-min(abs.difference,na.rm=T)
  location<-0
  #print(paste("mn.difference",mn.difference,"t",t))
  if(is.finite(location)){
    location<-which(abs.difference==mn.difference)
    if(abs.difference[location]>0.5){location<-NA}
  }else{location<-NA}
  #print(paste("location",location))
  return(location)
}

# calculate the time apart
apart.X.hrs<-function(data){
  current.time<-data[1]
  time.difference<-function(other.time){ abs(difftime(current.time,other.time,units="hours"))}
  difference.in.time<-c(NA,unlist(lapply(data[-1],time.difference)))
  return(difference.in.time)
}

create.blocks<-function(data,no.of.miss.occasions,min.no.locations,start.month,Noofmonths){
  data<-selecting.months(data,start.month,Noofmonths)
  blocks<-animal.run(data,no.of.miss.occasions)
  results<-list()
  for(i in 1:length(blocks)){
    animal.block<-blocks[[i]]
    animal.i.results<-list();counter=1
    l.animal.block<-length(animal.block)
    if(l.animal.block>0){
    for(j in 1:l.animal.block){
      #print(paste("length animal block",length(animal.block)))
      newblock<-convert.locid.into.data(data,no.of.miss.occasions,loc.ids=animal.block[[j]],min.no.locations)
      if(is.data.frame(newblock)){animal.i.results[[counter]]<-newblock; counter=counter+1}
    }}
    results[[i]]<-animal.i.results
  }
  return(list(blocks,results))
}

#data.for.classifcation.summer<-create.blocks(Data,no.of.miss.occasions=5,min.no.locations=25,start.month=4,Noofmonths=3)
