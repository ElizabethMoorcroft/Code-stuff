#--------------------------------
# Project title: Snow leopards
# Script title: Snow Leopard Summary of Data
# Purpose: Basic summary of the size of the data
#--------------------------------


SummaryData<-function(Data){
  m<-matrix(ncol=4, nrow=1)
  m[1,1]<-dim(Data)[1] # Size of the data
  m[1,2]<-length(unique(Data$AnimID))
  m[1,3]<-as.character(min(as.POSIXlt(Data$Date_Time),na.rm=T))
  m[1,4]<-as.character(max(as.POSIXlt(Data$Date_Time),na.rm=T))
  #m[1,5]<-min(Data$Latitude,na.rm=T)
  #m[1,6]<-max(Data$Latitude,na.rm=T)
  #m[1,7]<-min(Data$Longitude,na.rm=T)
  #m[1,8]<-max(Data$Longitude,na.rm=T)
  
  return(m)
}

SummaryIndividData<-function(Data){
  m<-matrix(ncol=4, nrow=length(unique(Data$AnimID)))
  animals<-sort(unique(Data$AnimID))
  for(i in 1:length(unique(Data$AnimID))){
    d<-Data[which(Data$AnimID==animals[i]),]
    m[i,1]<-as.character(animals[i])
    m[i,2]<-dim(d)[1] # Size of the data
    m[i,3]<-as.character(min(as.POSIXlt(d$Date_Time),na.rm=T))
    m[i,4]<-as.character(max(as.POSIXlt(d$Date_Time),na.rm=T))
    #m[i,5]<-min(d$Latitude,na.rm=T)
    #m[i,6]<-max(d$Latitude,na.rm=T)
    #m[i,7]<-min(d$Longitude,na.rm=T)
    #m[i,8]<-max(d$Longitude,na.rm=T)
  }
  return(m)
}