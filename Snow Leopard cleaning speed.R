
#--------------------------------
# Project title: Snow leopards
# Script title: Snow Leopard Explore
# Purpose: Exploratory analysis of the data
#--------------------------------

CheckLessThanValue<-function(Data,Variable,Value){
  # - Select data that the is in column Variable with a value of less than equal to Value
  colnumber<-which(colnames(Data) %in% Variable)
  SameLocation<-Data[which(Data[,colnumber]<=Value),]
  # - Prints out the informations about these
  NoAtSameLocation<-dim(SameLocation)[1]
  print(paste("No.of locations where ", Variable, "is less than or equal to ",Value,":",NoAtSameLocation))
  print(SameLocation[,which(colnames(SameLocation) %in% c("LocID","AnimID","Dist_Difference","Time_Difference","Speed_mps"))])
  return(SameLocation$LocID)
}

CheckGreaterThanValue<-function(Data,Variable,Value){
  # - Select data that the is in column Variable with a value of less than equal to Value
  colnumber<-which(colnames(Data) %in% Variable)
  SameLocation<-Data[which(Data[,colnumber]>=Value),]
  # - Prints out the informations about these
  NoAtSameLocation<-dim(SameLocation)[1]
  print(paste("No.of locations where ", Variable, "is greater than or equal to ",Value,":",NoAtSameLocation))
  print(SameLocation[,which(colnames(SameLocation) %in% c("LocID","AnimID","Dist_Difference","Time_Difference","Speed_mps"))])
  return(SameLocation$LocID)
}

DeleteRow<-function(Data,RemoveLocation){
  rownumber<-which(Data$LocID %in% RemoveLocation)
  # - Deletes Row
  Data<-Data[-c(rownumber),]
  # - Sets the displacement, time and speed that it would have been used to calculate to NA 
  Data[rownumber+1,which(names(Data)=="Dist_Difference")]=NA
  Data[rownumber+1,which(names(Data)=="Time_Difference")]=NA
  Data[rownumber+1,which(names(Data)=="Alt_Difference")]=NA
  Data[rownumber+1,which(names(Data)=="Speed_mps")]=NA
  Data[rownumber+1,which(names(Data)=="Bearing")]=NA
  Data[rownumber+1,which(names(Data)=="ChangeAngle")]=NA
  return(Data)
}
 
ResetRow<-function(Data,ResetLocation){
  rownumber<-which(Data$LocID %in% ResetLocation)
  # - Sets the displacement, time and speed that it would have been used to calculate to NA 
  Data[rownumber,which(names(Data)=="Dist_Difference")]=NA
  Data[rownumber,which(names(Data)=="Time_Difference")]=NA
  Data[rownumber+1,which(names(Data)=="Alt_Difference")]=NA
  Data[rownumber,which(names(Data)=="Speed_mps")]=NA
  Data[rownumber+1,which(names(Data)=="Bearing")]=NA
  Data[rownumber+1,which(names(Data)=="ChangeAngle")]=NA
  return(Data)
}

RecalRow<-function(Data,RecalLocation){
  # For each of the values to be reculated loop through
  for(i in 1:length(RecalLocation)){
    rownumber<-which(Data$LocID %in% RecalLocation[i])
    # recalulates all values
    tempalt<-Data$Height[rownumber]-Data$Height[rownumber-1]
    temptime<-difftime(Data$Date_Time[rownumber],Data$Date_Time[rownumber-1], unit="secs")
    tempdist<-Distance.between.GPS.in.meters(Data$Latitude[rownumber-1],Data$Latitude[rownumber],Data$Longitude[rownumber-1],Data$Longitude[rownumber])
    tempbearing<-atan2((Data$Longitude[rownumber]-Data$Longitude[rownumber-1]),(Data$Latitude[rownumber]-Data$Latitude[rownumber-1]))
    tempchangeangle<-tempbearing-Data$Bearing[rownumber-1] #Produces a value -2pi to 2pi needs to be corrected
    if(tempchangeangle>pi){tempchangeangle<-2*pi-tempchangeangle}
    else if(tempchangeangle< (-pi)){tempchangeangle<-2*pi+tempchangeangle}
    # - Sets the displacement, time and speed that it would have been used to calculate to NA 
    Data[rownumber,which(names(Data)=="Dist_Difference")]=tempdist
    Data[rownumber,which(names(Data)=="Time_Difference")]=temptime
    Data[rownumber,which(names(Data)=="Alt_Difference")]=tempalt
    Data[rownumber,which(names(Data)=="Speed_mps")]=Data[rownumber,which(names(Data)=="Dist_Difference")]/Data[rownumber,which(names(Data)=="Time_Difference")]
    Data[rownumber,which(names(Data)=="Bearing")]=tempbearing
    Data[rownumber,which(names(Data)=="ChangeAngle")]=tempchangeangle
  }
  return(Data)
}
 

