selecting.months<-function(data,min,Noofmonths){
  months<-as.POSIXlt(data$Date_Time)$mon # The months of each data point
  Mrows<-which(months>=min & months<(min+Noofmonths)) # select rows
  if((min+Noofmonths)>11){ # if over 
    leftover<-min+Noofmonths-13
    Mrows<-c(Mrows,which(months>=0 & months<leftover))
  }
  data<-data[Mrows,]
  return(data)
}


block.number<-function(data,vector.of.current.rows,no.of.miss.occasions){
  current.block<-1; length.current.block<-1; data$block.movement[1] <- current.block
  vector.of.current.rows<-c(1);row.numbers<-list(); 
  
  for(i in 2:dim(data)[1]){
    #print(paste(i,"/",dim(data)[1]))
    # time difference between locations
    time<- abs(difftime(data$Date_Time[i],data$Date_Time[i-1],units="hours"))
    mn<-(1:no.of.miss.occasions)*5-0.5; mx<-(1:no.of.miss.occasions)*5+0.5
  #print(mn);print(mx)
      if(any(unlist(lapply(time,function(y){y>mn & y<mx}))==TRUE) & data$AnimID[i-1]==data$AnimID[i]){
        print(data[i,])
          data$block.movement[i] <- current.block # writes down current block
          length.current.block<-length.current.block+1 # adds
          vector.of.current.rows<-c( vector.of.current.rows,i)
      }else{
          row.numbers[[length(row.numbers)+1]]<-vector.of.current.rows # New item added to list
          vector.of.current.rows<-c(i)
          data$length.block.movement[i-1] <- length.current.block
          current.block<-current.block+1; length.current.block<-1 # new block
          data$block.movement[i] <- current.block
      }
  
  }
  row.numbers[[length(row.numbers)+1]]<-vector.of.current.rows 
  print(current.block)
  return(list(data=data, row.numbers=row.numbers,current.block=current.block))
}
###################
## Not completed

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
  print("data");print(data)
  possible.difference<-(1:no.of.miss.occasions)*5
  
  
  l<-dim(data)[1]
  time<-diff.time.between.locations(data$Date_Time[c(l-1,l)])
  
  print(l);print(time)
  
  for(j in 1:no.of.miss.occasions){
    if(abs(time-possible.difference[j])<0.5){
      temp.blank<-as.data.frame(matrix(ncol=dim(data)[2],nrow=(j-1)));names(temp.blank)<-names(data)
      temp<-rbind(data[1:(l-1),],temp.blank,data[l,]); 
      print("temp");print(temp)
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

create.blocks<-function(data,no.of.miss.occasions,min.no.locations){
  blocks<-animal.run(data,no.of.miss.occasions)
  results<-list()
  for(i in 1:length(blocks)){
    animal.block<-blocks[[i]]
    animal.i.results<-list();counter=1
    for(j in 1:length(animal.block)){
      newblock<-convert.locid.into.data(data,no.of.miss.occasions,loc.ids=animal.block[[j]],min.no.locations)
      if(is.data.frame(newblock)){animal.i.results[[counter]]<-newblock; counter=counter+1}
    }
    results[[i]]<-animal.i.results
  }
  return(list(blocks,results))
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
    print(dim(data)[1])
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


###################################
## Not completed


calculating.row.numbers<-function(data,no.of.blocks,no.of.miss.occasions,minumum.no.locations,row.numbers){
  # initail values
  list.dist.angle.matrix<-vector(mode="list"); list.animal.sex<-vector(mode="list"); list.row.numbers.data<-vector(mode="list")
  list.missing.time<-vector(mode="list");list.missing.locations<-vector(mode="list");
  RunNumbers<-1:no.of.blocks
  block.number<-1
  for(i in RunNumbers){
    tempdata<-data[which(data$block.movement==RunNumbers[i]),]
    if(dim(tempdata)[1]>minumum.no.locations){
      missing.data<-addBlankrows(tempdata,no.of.miss.occasions)
      list.dist.angle.matrix[[block.number]]<-missing.data[[1]] #added blank rows
      list.animal.sex[[block.number]]<-c(as.character(unique(tempdata$AnimID)),as.character(unique(tempdata$Sex)))
      list.row.numbers.data[[block.number]]<-row.numbers[[i]]
      list.missing.time[[block.number]]<-missing.data[[2]]
      list.missing.locations[[block.number]]<-missing.data[[3]]
      block.number= block.number+1
    }
  }
  return(list(list.dist.angle.matrix=list.dist.angle.matrix,
              list.animal.sex=list.animal.sex
              ,list.row.numbers.data=list.row.numbers.data,
              list.missing.time=list.missing.time,
              list.missing.locations=list.missing.locations
              ))
}


# break into blocks
Months<-function(data,min,Noofmonths,no.of.miss.occasions=6,minumum.no.locations=25){
    
    # select correct "season" of data  
    data<-selecting.months(data,min,Noofmonths)
    
    # create new columns for the data 
    block.movement<-rep(NA,dim(data)[1]); data<-cbind(data,block.movement,no.of.miss.occasions)
    length.block.movement<-rep(NA,dim(data)[1]); data<-cbind(data,length.block.movement)
    
    # identifies blocks
    returned.values<-block.number(data,vector.of.current.rows,no.of.miss.occasions)
    data<-returned.values$data
    row.numbers<-returned.values$row.numbers
    no.of.blocks<-returned.values$current.block
    
    # calculates:
    #       - matrices for angle and displacement
    #       - vectors of animalID and sex
    #       - vectors of row numbers from the orginal data
    calculated.values<-calculating.row.numbers(data,no.of.blocks,no.of.miss.occasions,minumum.no.locations,row.numbers)
    list.dist.angle.matrix<-calculated.values$list.dist.angle.matrix
    list.animal.sex<-calculated.values$list.animal.sex
    list.row.numbers.data<-calculated.values$list.row.numbers.data
    list.missing.time<-calculated.values$list.missing.time
    list.missing.locations<-calculated.values$list.missing.locations
    
    # produces a matrix for langrock 
    maxrownum<-max(sapply(list.dist.angle.matrix,function(x) dim(x)[1]),na.rm=T) 
    DataInLangrockForm<-matrix(nrow=maxrownum,ncol=0)
   print(length(list.dist.angle.matrix))
    for(i in 1:length(list.dist.angle.matrix)){
      diff<-maxrownum-dim(list.dist.angle.matrix[[i]])[1]
      temp<-matrix(ncol=2,nrow=diff)
      tempanimal<-rbind(list.dist.angle.matrix[[i]],temp)
      DataInLangrockForm<-cbind(DataInLangrockForm,tempanimal)
    }
    
    returnvalues<-list("5HrsData" = DataInLangrockForm, "ID&Sex" = list.animal.sex,"Rownumbers"=list.row.numbers.data,
                       "missing.Times"=list.missing.time,"missing.data"=list.missing.locations)
    return(returnvalues)
}

addBlankrows<-function(data,no.of.miss.occasions){
  Output<-matrix(ncol=2,nrow=1)
  Output[1,1]<-data$Dist_Difference[1];Output[1,2]<-data$ChangeAngle[1];
  
  time.missing<-list();locations<-list()
  
  mn<-(1:no.of.miss.occasions)*5-0.5; mx<-(1:no.of.miss.occasions)*5+0.5
  
  missing.locations<-1
  
  for(i in 2:dim(data)[1]){
      time<- abs(difftime(data$Date_Time[i],data$Date_Time[i-1],units="hours"))
      distance<-as.numeric(as.character(data$Dist_Difference[i]))
      angle<-as.numeric(as.character(data$ChangeAngle[i]))
      for(j in 1:no.of.miss.occasions){
        if(time<mx[j] & time>mn[j]){
          temp<-matrix(ncol=2,nrow=(j),c(rep(NA,((j*2)-2)),distance,angle),byrow=TRUE)
          if(j>1){
            time.missing[[missing.locations]]<-c(as.POSIXct(data$Date_Time[i-1]),as.POSIXct(data$Date_Time[i-1])+(1:j)*5*60*60)
            locations[[missing.locations]]<-c(data$UTM_X_Zone[i-1],data$UTM_Y_Zone[i-1],data$UTM_X_Zone[i],data$UTM_Y_Zone[i])
            missing.locations<-missing.locations+1
          }       
        }
      }
      Output<-rbind(Output,temp)
   }
    
  Output[,1]<-Output[,1]/1000
  Output[,2]<-Output[,2]
  Output[which(Output[,2]<0),2]<-Output[which(Output[,2]<0),2]+2*pi
  return(list(Output,time.missing,locations))
}