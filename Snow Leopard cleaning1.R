#--------------------------------
# Project title: Snow leopards
# Script title: Snow Leopard cleaning
# Purpose: To clean the orginal data
#--------------------------------



#-Directories
DataDir<- "/Volumes/elizabethmoorcroft/Documents/PhD stuff/Snow leopard/Data"
CodeDir<- "/Volumes/elizabethmoorcroft/Documents/PhD stuff/Snow leopard"
SaveDir<- "/Volumes/elizabethmoorcroft/Documents/PhD stuff/Snow leopard/Graphs"


#- Source code
setwd(CodeDir)
source("DistbetweenGPS.R")

#-Load in data
setwd(DataDir)
original<-read.csv("All%20Cat%20Locs%20cleaned%20through%202Aug13.csv")

#-Check load correctly 
dim(original)
head(orginal)

#-varaible names
colnames(original)

#- Remove empty rows
EmptyRemoved<-original[-which(is.na(original$LocID)),]
table(EmptyRemoved[,2],EmptyRemoved[,3])#ID and Name match perfectly

# Unique animal IDs
Animals<-sort(unique(EmptyRemoved$AnimID))

m<-matrix(ncol=8,nrow=1)
colnames(m)<-c("NoOfLocations","NoOfAnimals","MinDate","MaxDate","MinLat","MaxLat","MinLong","MaxLong")
m[1,1]<-dim(EmptyRemoved)[1]
m[1,2]<-length(Animals)
m[1,3]<-as.character(min(as.POSIXlt(EmptyRemoved$Date_Time),na.rm=T))
m[1,4]<-as.character(max(as.POSIXlt(EmptyRemoved$Date_Time),na.rm=T))
m[1,5]<-min(EmptyRemoved$Latitude,na.rm=T)
m[1,6]<-max(EmptyRemoved$Latitude,na.rm=T)
m[1,7]<-min(EmptyRemoved$Longitude,na.rm=T)
m[1,8]<-max(EmptyRemoved$Longitude,na.rm=T)
setwd(DataDir)
write.csv(m,paste("SummaryOfOriginalData",Sys.Date(),".csv",sep=""))

m2<-matrix(ncol=8, nrow=length(Animals))
colnames(m2)<-c("NoOfLocations","AnimalName","MinDate","MaxDate","MinLat","MaxLat","MinLong","MaxLong")
rownames(m2)<-as.character(Animals)
for(i in 1:length(Animals)){
  data<-EmptyRemoved[which(as.character(EmptyRemoved$AnimID)==as.character(Animals[i])),]
  m2[i,1]<-dim(data)[1]
  m2[i,2]<-as.character(unique(data$Name))
  m2[i,3]<-as.character(min(as.POSIXlt(data$Date_Time),na.rm=T))
  m2[i,4]<-as.character(max(as.POSIXlt(data$Date_Time),na.rm=T))
  m2[i,5]<-min(data$Latitude,na.rm=T)
  m2[i,6]<-max(data$Latitude,na.rm=T)
  m2[i,7]<-min(data$Longitude,na.rm=T)
  m2[i,8]<-max(data$Longitude,na.rm=T)
}
m2
setwd(DataDir)
write.csv(m2,paste("SummaryOfOriginalDataByIndividual",Sys.Date(),".csv",sep=""))

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
      "Validated")
SelectedCols<-EmptyRemoved[,which(colnames(EmptyRemoved) %in% variables)]


#--------------------------------


Difference<-data.frame(matrix(ncol=length(variables)+5,nrow=0)) 
#-Time between locations
for(i in 1:length(Animals)){
	print(paste("Animal number",i))
	tempdata<-SelectedCols[which(SelectedCols$AnimID==Animals[i]),]
  tempdata<-tempdata[order(tempdata$Date_Time),]
	lengthtempdata<-dim(tempdata)[1]
	Time_Difference<-vector(length=lengthtempdata)
	Dist_Difference<-vector(length=lengthtempdata)
	Alt_Difference<-vector(length=lengthtempdata)
  Bearing<-vector(length=lengthtempdata)
  ChangeAngle<-vector(length=lengthtempdata)
	tempdata<-cbind(tempdata,Time_Difference,Dist_Difference,Alt_Difference )
	tempdata$Time_Difference[1]<-NA
	tempdata$Dist_Difference[1]<-NA
	tempdata$Alt_Difference[1]<-NA
  tempdata$Bearing[1]<-NA
  tempdata$ChangeAngle[1]<-NA
	for(j in 2:lengthtempdata){
		tempdata$Time_Difference[j]<-difftime(tempdata$Date_Time[j],
											 tempdata$Date_Time[j-1],
											 unit="secs")
		tempdata$Dist_Difference[j]<-Distance.between.GPS.in.meters(tempdata$Latitude[j-1],tempdata$Latitude[j],tempdata$Longitude[j-1],tempdata$Longitude[j])
		tempdata$Alt_Difference[j]<-tempdata$Height[j]-tempdata$Height[j-1]
    tempdata$Bearing[j]<-atan2((tempdata$Longitude[j]-tempdata$Longitude[j-1]),(tempdata$Latitude[j]-tempdata$Latitude[j-1]))
    if(j>2){
      tempdata$ChangeAngle[j]<-tempdata$Bearing[j]-tempdata$Bearing[j-1]
      #if(tempdata$Bearing[j]<0){tempdata$Bearing[j]<-tempdata$Bearing[j]+pi}

    }
	}
	if(i==1){colnames(Difference)<-colnames(tempdata)}
	Difference<-rbind(Difference,tempdata)
}

#Calculate speed
Difference$Speed_mps<-Difference$Dist_Difference/Difference$Time_Difference

#Label males and females
Difference$Sex<-substr(Difference$AnimID,1,1)

#Write clean file to csv
setwd(DataDir)
write.csv(Difference, paste("CleanedData",Sys.Date(),".csv",sep=""),row.names=FALSE)