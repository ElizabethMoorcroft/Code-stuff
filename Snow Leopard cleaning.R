#--------------------------------
# Project title: Snow leopards
# Script title: Snow Leopard cleaning
# Purpose: To clean the orginal data
#--------------------------------



RemoveRowsAndCols<-function(original){

  #- Remove empty rows
  EmptyRemoved<-original[-which(is.na(original$LocID)),]
  print(table(EmptyRemoved[,2],EmptyRemoved[,3]))#ID and Name match perfectly

  # Unique animal IDs
  Animals<-sort(unique(EmptyRemoved$AnimID))


  #----
  table(EmptyRemoved[,2],EmptyRemoved[,4]) 
  #Some animals use different collars, could be worth double checking that there is no correlation between collars?

  #-Selected varaibles only
  variables<-c("LocID",
			"AnimID",
			"Name",
			"Collar_ID",
			"Date_Time",
			"Latitude",
			"Longitude",
			"Height",
			"Temp",
      "DOP",
      "Nav",
      "Validated",
      "UTM_X_Zone",
      "UTM_Y_Zone",
      "Hour",
      "ECEF_X",
      "ECEF_Y",
      "ECEF_Z"
      )
  SelectedCols<-EmptyRemoved[,which(colnames(EmptyRemoved) %in% variables)]
  return(SelectedCols)
}

  #--------------------------------


CreatedVariables<-function(SelectedCols){
  
  #-Unique animal IDs
  Animals<-sort(unique(SelectedCols$AnimID))
  #-Creates a new data frame that will be filled with data
  Difference<-data.frame(matrix(ncol=dim(SelectedCols)[2]+5,nrow=0)) 
  
  #-Looks at each individual seperately
  for(i in 1:length(Animals)){
  	#print(paste("Animal number: ",i,"/",length(Animals),sep=""))
    # - Select only 1 animals data & check that its data/time ordered correctly
  	tempdata<-SelectedCols[which(SelectedCols$AnimID==Animals[i]),]
    tempdata<-tempdata[order(tempdata$Date_Time),]
    # - Add 5 vectors onto the dataset for:
    #       Time between locations
    #       Displacement since last location
    #       Change in Altidude
    #       The direction of travel
    #       THe change in the direction of travel
  	lengthtempdata<-dim(tempdata)[1]
  	Time_Difference<-vector(length=lengthtempdata)
  	Dist_Difference<-vector(length=lengthtempdata)
  	Alt_Difference<-vector(length=lengthtempdata)
    Bearing<-vector(length=lengthtempdata)
    ChangeAngle<-vector(length=lengthtempdata)
  	tempdata<-cbind(tempdata,Time_Difference,Dist_Difference,Alt_Difference, Bearing,ChangeAngle )
    # - Set the first value to be zero, as there is no previous location
  	tempdata$Time_Difference[1]<-NA
	  tempdata$Dist_Difference[1]<-NA
	  tempdata$Alt_Difference[1]<-NA
    tempdata$Bearing[1]<-NA
    tempdata$ChangeAngle[1]<-NA
    # - For all other locations complete the following
	  for(j in 2:lengthtempdata){
      # - Time difference calculated in seconds
	  	tempdata$Time_Difference[j]<-difftime(tempdata$Date_Time[j],tempdata$Date_Time[j-1], unit="secs")
      # - Calculate the distance in meters uisng "DistbetweenGPS.R" script
		  tempdata$Dist_Difference[j]<-Distance.between.GPS.in.meters(tempdata$Latitude[j-1],tempdata$Latitude[j],tempdata$Longitude[j-1],tempdata$Longitude[j])
		  # - Calculate the difference in altidude
      tempdata$Alt_Difference[j]<-tempdata$Height[j]-tempdata$Height[j-1]
      # - Calculate the bearing, this gives a value between -pi and pi
      tempdata$Bearing[j]<-atan2((tempdata$Longitude[j]-tempdata$Longitude[j-1]),(tempdata$Latitude[j]-tempdata$Latitude[j-1]))
      if(j>2){
        # - Calculates the change in bearing
        # - this produces a value between -2pi and 2pi which needs to be corrected
       tempdata$ChangeAngle[j]<-tempdata$Bearing[j]-tempdata$Bearing[j-1]
       if(tempdata$ChangeAngle[j]>pi){tempdata$ChangeAngle[j]<-tempdata$ChangeAngle[j]-2*pi}
       else if(tempdata$ChangeAngle[j]< (-pi)){tempdata$ChangeAngle[j]<-2*pi+tempdata$ChangeAngle[j]}
      }
      if(tempdata$Bearing[j]<0){tempdata$Bearing[j]<-tempdata$Bearing[j]+2*pi}
	  }
    # - Names the columns in the output data set
    # - Adds the new data to the matrix
	  if(i==1){colnames(Difference)<-colnames(tempdata)}
	  Difference<-rbind(Difference,tempdata)
  }

  # - Calculates the Maximum speed in meters per second
  Difference$Speed_mps<-Difference$Dist_Difference/Difference$Time_Difference

  # - Label males and females
  Difference$Sex<-substr(Difference$AnimID,1,1)

  # - Write clean file to csv
  setwd(DataDir)
  write.csv(Difference, paste("CleanedData",Sys.Date(),".csv",sep=""),row.names=FALSE)
  
  return(Difference)
}

# Calcualte month
MonthCal<-function(Data){  
  Month<-as.POSIXlt(Data)$mon
  return (Month) 
}

# Calcualte season based on McCarthy definition
SeasonMcCarthy<-function(months){
  l_data<-length(months) 
  season<-vector(length=l_data)
  for(i in 1:l_data){ ## remember months start counting at jan =0, feb=1, etc
    if(months[i]<4){season[i]<-"winter"}  # Jan-0, feb-1, mar-2, apr-3
    else if (months[i]>9) {season[i]<-"winter"} # nov-10, dec-11
    else if (months[i]>=4 & months[i]<=9) {season[i]<-"summer"} # May-4, jun-5, july-6, aug-7, sep-8, oct-9
  }  
  return(season)
}
# Calcualte season based on data definition
SeasonData<-function(months){
  l_data<-length(months) 
  season<-vector(length=l_data)
  for(i in 1:l_data){ ## remember months start counting at jan =0, feb=1, etc
    if(months[i]<4){season[i]<-"winter"}  # jan-0, feb-1, mar-2, apr-3
    else if (months[i]>10) {season[i]<-"winter"} # dec-11
    else if (months[i]>=4 & months[i]<=6) {season[i]<-"summer"} # may-4, jun-5, july-6,
    else if (months[i]>6 & months[i]<=10){season[i]<-"other"} # aug-7, sep-8, oct-9, nov-10
  }  
  return(season)
}
# Calcualte time of day of the mid-point of the movement
Timeofday<-function(date, timegap){
  timecal<-format(round(as.POSIXlt(as.POSIXlt(date)-(timegap/2)),units="hours"),format="%H:%M")
  return(timecal)
}

#Calcualte teh number of consecutive 5hr reports
IndividMove<-function(Data){
  AID<-sort(unique(Data$AnimID))
  l_data<-dim(Data)[1];w_data<-dim(Data)[2]
  
  # matrix for results
  recal_data<-matrix(ncol=w_data+2,nrow=0)
  for(i in 1:length(AID)){
    print(i)
    animal<-Data[which(Data$AnimID==AID[i]),]
    #Applies code and attaches results
    blocks<-FiveHrMovement(animal)
    animal<-cbind(animal,blocks)
    # Stick all of the animals back together
    recal_data<-rbind(recal_data,animal)
  }
  return(recal_data)
}

FiveHrMovement<-function(Data){
  l_data<-dim(Data)[1]
  movementblock<-vector(length=l_data)
  movementblockcount<-vector(length=l_data)
  movementblock[1]<-NA;
  movementblockcount[1]<-NA
  block<-0
  availablelocations<-2:l_data
  count<-0
  for(i in 2:(l_data-1)){
    location<-1
    if(movementblock[i]==0){block=block+1;movementblock[i]<-NA;
                            movementblockcount[i-1]<-count;count=0;}
    l_availablelocations<-length(availablelocations)
    j<-availablelocations[location]
    remove<-c(which(i == availablelocations))
    d_time<-difftime(Data$Date_Time[j],Data$Date_Time[i], unit="secs")
    while(d_time<=10.25*3600 & location<=l_availablelocations){
      if(
        (d_time<=5.25*3600 & d_time>=4.75*3600)
         |
        (d_time<=10.25*3600 & d_time>=9.75*3600)
         ){
        movementblock[j]<-block
        count<-count+1
        remove<-c(remove,location)
      }
      location<-location+1
      j<-availablelocations[location]
      d_time<-difftime(Data$Date_Time[j],Data$Date_Time[i], unit="secs")
    }
    
   # print(paste("removing",remove))
    if(length(remove)>0) {availablelocations<-availablelocations[-c(remove)]}
  }
  movementblockcount[movementblockcount==0]<-NA
  movement<-cbind(movementblock,movementblockcount)
  return(movement)
}

