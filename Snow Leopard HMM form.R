# selects the correct months
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

# Time between first two values
diff.time.between.locations<-function(data){
  #print(paste("data[1]",data[1],"data[2]",data[2]))
  
  time.difference<-difftime(data[2],data[1],units="hours")
  return(time.difference)
}

# Bearing of animal
direction.of.animal.form<-function(data.x,data.y){
  if(!is.na(data.x[2]) & !is.na(data.x[1]) & !is.na(data.y[2]) & !is.na(data.y[1])){
    Bearing<-atan2((data.x[2]-data.x[1]),(data.y[2]-data.y[1]))
    if(Bearing<0){Bearing<-Bearing+2*pi}
  }else{Bearing<-NA}
  return(Bearing)
}

# Change in direction
change.in.animal.direction.form<-function(data){   
  # - Calculates the change in bearing
  # - this produces a value between -2pi and 2pi which needs to be corrected
  if(!is.na(data[1]) & !is.na(data[2]) ){
    ChangeAngle<-data[2]-data[1]
    if(ChangeAngle>pi){ChangeAngle<-ChangeAngle-2*pi}
    else if(ChangeAngle<(-pi)){ChangeAngle<-2*pi+ChangeAngle}
  }else{ChangeAngle<-NA}
  return(ChangeAngle)
}#

# update data with blank rows
update.with.blanks<-function(data,time,j,l){
 
  temp.blank<-as.data.frame(matrix(ncol=dim(data)[2],nrow=(j-1)));names(temp.blank)<-names(data)
  temp<-rbind(data[1:(l-1),],temp.blank,data[l,]); 
  
  lat.col<-temp[,which(names(temp) == "Latitude")]
  lon.col<-temp[,which(names(temp) == "Longitude")]
  #Updates values in the last row
  temp$block_time.difference[l+j-1]<-time
  temp$block_dist.difference[l+j-1]<-Distance.between.GPS.in.meters(lat.col[j+l-2],lat.col[j+l-1],lon.col[j+l-2],lon.col[j+l-1])

  temp$block_bearing[l+j-1]<-direction.of.animal.form(data.x=lon.col[c(j+l-2,j+l-1)],
                                                 data.y=lat.col[c(j+l-2,j+l-1)])
  temp$block_angle.difference[l+j-1]<-change.in.animal.direction.form(temp$block_bearing[c(j+l-2,j+l-1)])
  return(temp)
}

# Adds blanks bnetween the last two rows of the dataset
add.blanks.and.calculate.values<-function(data,no.of.miss.occasions){
  
  possible.difference<-(1:no.of.miss.occasions)*5
  l<-dim(data)[1]
  time<-diff.time.between.locations(data$Date_Time[c(l-1,l)])
  
  for(j in 1:no.of.miss.occasions){
    if(!is.na(time)){
      if(abs(time-possible.difference[j])<0.5){ data<-update.with.blanks(data=data,time=time,j=j,l=l); break; }
    } else{break;}
  }
  return(data)
}

convert.locid.into.data<-function(data,no.of.miss.occasions,loc.ids,min.no.locations){
  
  l.data<-length(loc.ids);
  tempdata<-data[which(data$LocID %in% loc.ids),]
  
 # print(tempdata)  
  
  block_number<-rep(NA,l.data);
  block_dist.difference<-rep(NA,l.data); block_time.difference<-rep(NA,l.data)
  block_bearing<-rep(NA,l.data); block_angle.difference<-rep(NA,l.data)
  
  if(sum(!is.na(tempdata[,1]))>=min.no.locations){
    data.include.var<-as.data.frame(cbind(tempdata,block_number,block_dist.difference,block_time.difference,block_bearing,block_angle.difference))
    newdata<-data.include.var[1,]
    for(i in 2:dim(data.include.var)[1]){
      newdata<-rbind(newdata,data.include.var[i,])
      
      #print("data.include.var")
      #print(data.include.var)
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
      if(is.data.frame(newblock)){newblock<-distance.x.hours(newblock,hours=c(10,15,20,25),lat="Latitude",long="Longitude",method="GPS")}
      if(is.data.frame(newblock)){animal.i.results[[counter]]<-newblock; counter=counter+1}
    }}
    results[[i]]<-animal.i.results
  }
  return(list(blocks,results))
}

distance.x.hours<-function(data,hours,lat,long,method="GPS"){
  data<-as.data.frame(data)
  col.long<-which(names(data) == long) ;col.lat<-which(names(data) == lat)
                                                           
  l.hours<-length(hours)
  m.hours<-max(hours)
  if((dim(data)[1]-1)<(m.hours/5)){m.hours<-(dim(data)[1]-1)*5}
    
  temp<-matrix(ncol=(l.hours*3),nrow=dim(data)[1])
  names.for.temp<-c()
  for(i in 1:l.hours){names.for.temp<-c(names.for.temp,
                                        paste("block_dist.difference_",hours[i],"_hrs",sep=""),
                                        paste("block_bearing_",hours[i],"_hrs",sep=""),
                                        paste("block_angle.difference_",hours[i],"_hrs",sep="")
                                        )}

  colnames(temp)<-names.for.temp
  
  hours<-hours/5

  lat.data<-as.numeric(as.character(data[,col.lat]))
  lon.data<-as.numeric(as.character(data[,col.long]))
  
  for(i in 1:l.hours){
    for(j in (hours[i]+1):dim(data)[1]){
      #print(paste("j-hours",j-hours[i],"j:",j))
      #print(paste("data[j-hours,col.lat]",data[j-hours[i],col.lat]))
      if(method=="GPS"){temp[j,(i-1)*3+1]<-Distance.between.GPS.in.meters(lat.data[j-hours[i]],lat.data[j],lon.data[j-hours[i]],lon.data[j])}
      else{temp[j,(i-1)*3+1]<-sqrt((lat.data[j-hours[i]]-lat.data[j])^2+(lon.data[j-hours[i]]-lon.data[j])^2)}
      #print(paste("lat1",data$Latitude[j-hours],"lat2",data$Latitude[j],"long1",data$Longitude[j-hours],"long2",data$Longitude[j],"dist:",temp[j,(i-1)*3+1]))
      temp[j,(i-1)*3+2]<-direction.of.animal.form(data.x= lon.data[c(j-hours[i],j)],data.y= lat.data[c(j-hours[i],j)])
      temp[j,(i-1)*3+3]<-change.in.animal.direction.form(temp[c(j-hours[i],j),(i-1)*3+2])
    }
  }
  return(cbind(data,temp))
}

###
# TEST RUN FOR THE ALGORITHMS 
###
#test.of.data.classifcation<-create.blocks(Data[1:100,],no.of.miss.occasions=5,min.no.locations=25,start.month=4,Noofmonths=3)
#Distance.between.GPS.in.meters(43.16342, 43.16508,100.6758, 100.6913)
