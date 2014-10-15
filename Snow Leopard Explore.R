#--------------------------------
# Project title: Snow leopards
# Script title: Snow Leopard Explore
# Purpose: Exploratory analysis of the data
#--------------------------------

#-Directories
DataDir<- "/Volumes/elizabethmoorcroft/Documents/PhD stuff/Snow leopard/Data"
CodeDir<- "/Volumes/elizabethmoorcroft/Documents/PhD stuff/Snow leopard"
#-Libraries
library(RColorBrewer)
library(maps)

#-Colours
COL<-c(brewer.pal(n=12, name="Set3"),brewer.pal(n=9, name="Set1"),"black")


#-Read in data
setwd(DataDir)
Data<-read.csv(paste("CleanedData",Sys.Date(),".csv",sep=""))


# ---- Locations on a map

minlat<-min(Data$Latitude)
maxlat<-max(Data$Latitude)
minlong<-min(Data$Longitude)
maxlong<-max(Data$Longitude)


plot(0,0,type="n",
	ylim=c(minlat,maxlat+0.5),xlim=c(minlong,maxlong),
  ylab="Latitude",xlab="Longitude"
	)
legend(y=maxlat+0.5,x=minlong,
       legend=Animals,
       col=COL[1:length(Animals)],
       pch=1:length(Animals),
       cex=0.7
)
for(i in 1:length(Animals)){
	data<-Data[which(Data$AnimID==Animals[i]),]
	points(x=data$Longitude,y=data$Latitude,col=COL[i],pch=i)
}


# ----
#plot(type="n",Data$Dist_Difference,Data$Time_Difference,xlim=c(0,40000),ylim=c(0,50000))

#par(mfrow=c(2,9))
# The distance moved between locations per animal
boxplot(Data$Dist_Difference/Data$Time_Difference~ Data$AnimID, col=COL[1],ylim=c(0,100))
Animals<-unique(Data$AnimID)
plot(type="n",0,0,
	xlim=c(-2000,20000),
	ylim=c(0,0.0010))
for(i in 1:length(Animals)){
	data<-Data[which(Data$AnimID==Animals[i]),]
	#points(data$Dist_Difference,data$Time_Difference,pch=i, col=i)
	points(density(data$Dist_Difference,na.rm=T,adjust=1),type="l",col=COL[as.numeric(data$Sex)+5])
	
}

boxplot(Data$Dist_Difference, by= Data$Sex)

Sex<-unique(Data$Sex)

plot(type="n",0,0,
	xlim=c(-2000,20000),
	ylim=c(0,0.0010))
for(i in 1:length(Sex)){
	data<-Data[which(Data$Sex==Sex[i]),]
	#points(data$Dist_Difference,data$Time_Difference,pch=i, col=i)
	points(density(data$Dist_Difference,na.rm=T,adjust=2),type="l",col=COL[as.numeric(data$Sex)+5])
}



plot(type="n",0,0,
	xlim=c(0,1.5),
	ylim=c(0,15))
for(i in 1:length(Animals)){
	data<-Data[which(Data$AnimID==Animals[i]),]
	#points(data$Dist_Difference,data$Time_Difference,pch=i, col=i)
	points(density(data$Dist_Difference/data$Time_Difference,na.rm=T,adjust=2),
			type="l",
			col=COL[as.numeric(data$Sex)+5])
	print(Animals[i])
	print(density(data$Dist_Difference/data$Time_Difference,na.rm=T,adjust=2))
}

#for(i in 1:length(Animals)){
#	numbers<-which(Data$AnimID==Animals[i] & !is.na(Data$Time_Difference))-1
	#points(data$Dist_Difference,data$Time_Difference,pch=i, col=i)
	#points(density(data$Dist_Difference,na.rm=T,adjust=1),type="l",col=i)
#	points(pc.cr$scores[numbers,1],pc.cr$scores[numbers,2],col=COL[i],pch=i)
#}


?density
plot(density)