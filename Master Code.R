#--------------------------------
# Project title: Snow leopards
# Script title: Master Script
# Purpose: Runs all code
#--------------------------------

#ClearWorkSpace
rm(list = ls())

#-Directories
DataDir<- "/Users/student/Documents/Snow Leopards/Data"
CodeDir<- "/Users/student/Documents/Snow Leopards/Code"
SaveDir<- "/Users/student/Documents/Snow Leopards/Write up /Graphs"

#-Source Functions
setwd(CodeDir)
source("DistbetweenGPS.R")
source("Snow Leopard cleaning.R")
setwd(CodeDir)
source("Snow Leopard DataSummary.R")
source("Snow Leopard cleaning speed.R")
source("Snow Leopard Density Plots.R")
source("Snow Leopard All Locations.R")
source("Snow Leopard Alpha Hull.R")
source("Snow Leopard CalculateCentrePoints.R")
source("Snow Leopard LocationsAndDensities.R")
source("Snow Leopard Exclusion.R")
setwd(CodeDir)
source("Snow Leopard Image angle plot.R")
source("Snow Leopard Circular graphs.R")
source("Snow Leopard HMM form.R")
source("Snow Leopard Clipping Contour.R")



#-Libraries
library(RColorBrewer) 
library(alphahull) #Used for alphahulls
library(fields) #Used for alphahulls
library(fossil)
library(raster)
library(plotKML)  # For google earth
library(plotrix) # For the polar.plot 
library(spatstat)
#library(maps)
library(vioplot)
library(mclust) #For simulation/validation
library(mvtnorm) #For simulation/validation
library(CircStats)
library(GISTools)
#library(betareg)

#-Colours
COL<-c(brewer.pal(n=12, name="Set3"),brewer.pal(n=9, name="Set1"),"black")

####
#### Sections below commented out for speed 
####   - if the cleaning process or variable creation needs to be changed then uncomment 
####
##-Load in data
#setwd(DataDir)
#original<-read.csv("All%20Cat%20Locs%20cleaned%20through%202Aug13.csv")

##-Check load correctly 
#dim(original)
#head(original)

##-varaible names
#colnames(original)

##-Source Cleaning Code
#RemovedRAndC<-RemoveRowsAndCols(original)

##- Saving details of the 
#m<-SummaryData(RemovedRAndC)
#write.csv(m,paste("SummaryOfOriginalData",Sys.Date(),".csv",sep=""))
#m2<-SummaryIndividData(RemovedRAndC)
#write.csv(m2,paste("SummaryOfOriginalDataByIndividual",Sys.Date(),".csv",sep=""))
#setwd(SaveDir)
#write.csv(table(RemovedRAndC$Validated,RemovedRAndC$Collar_ID),"ValidatedvsCollarID.csv")

##-Removes data that doesn't have high enough accuracy
#RemovedUnvalidatedLocations<-RemovedRAndC[which((RemovedRAndC$Nav=="2D" & RemovedRAndC$DOP<=5) |RemovedRAndC$Nav=="3D") ,]

##-Saving details of the 
#m2<-SummaryIndividData(RemovedUnvalidatedLocations)
#write.csv(m2,paste("SummaryOfDataDOPGT3ByIndividual",Sys.Date(),".csv",sep=""))

##-Creates additional variables in the code
#Data<-CreatedVariables(RemovedUnvalidatedLocations)

##-PLots points on google earth
#setwd(SaveDir)
#PlotDataKML(Data, "Test")

##-Sensible distance/time/speed
#CheckLessThanValue(Data,"Dist_Difference",0)
#CheckGreaterThanValue(Data,"Time_Difference",60*60*24*365)
#grthan1month<-CheckGreaterThanValue(Data,"Time_Difference",60*60*24*30)
#Data<-ResetRow(Data,grthan1month)
#grthan1week<-CheckGreaterThanValue(Data,"Time_Difference",60*60*24*7)
#grthan10mps<-CheckGreaterThanValue(Data,"Speed_mps",10)

#setwd(DataDir)
#write.csv(Data,"CurrentData.csv",row.names=FALSE)
####
#### End of commented block 
####

setwd(DataDir)
Data<-read.csv("CurrentData.csv")

## - Only 5hr apart 
Data<-Data[which(Data$Time_Difference<19800 & Data$Time_Difference>=16200),]

pdf("ChangeAngleIndividPolar.pdf")
PlotCircularGraphsIndivid(Data,1,COL,"ChangeAngle",8)
dev.off()
pdf("BearingIndividPolar.pdf")
PlotCircularGraphsIndivid(Data,1,COL,"Bearing",8)
dev.off()

pdf("BearingSex.pdf");PlotCircularGraphsSex(Data,round=1,COLS=COL[4:3],VARIABLE="Bearing",Radius=3); dev.off()
pdf("ChangeAngleSex.pdf");PlotCircularGraphsSex(Data,round=1,COLS=COL[4:3],VARIABLE="ChangeAngle",Radius=3); dev.off()

#Plot showing angle vs distance travelled
setwd(SaveDir)
imageplotangle(0,max(Data$Dist_Difference,na.rm=T), Data, 10, roundangle=20,"AngleVsDistanceTravelled", 10);dev.off()
imageplotangle(0,max(Data$Dist_Difference,na.rm=T), Data[which(Data$Sex=="M"),], 10, roundangle=20,"AngleVsDistanceTravelledMale", 10);dev.off()
imageplotangle(0,max(Data$Dist_Difference,na.rm=T), Data[which(Data$Sex=="F"),], 10, roundangle=20,"AngleVsDistanceTravelledFemale", 10);dev.off()


pdf("AngleTvsAngleT+1.pdf"); imageplotATvsTp1(Data, "ChangeAngle",10); dev.off()
pdf("AngleTvsAngleT+1Males.pdf"); imageplotATvsTp1(Data[which(Data$Sex=="M"),], "ChangeAngle",10); dev.off()
pdf("AngleTvsAngleT+1Females.pdf"); imageplotATvsTp1(Data[which(Data$Sex=="F"),], "ChangeAngle",10); dev.off()

pdf("DistanceTvsDistanceT+1.pdf"); imageplotDTvsTp1(Data, "Dist_Difference",5); dev.off()
pdf("DistanceTvsDistanceT+1Males.pdf"); imageplotDTvsTp1(Data[which(Data$Sex=="M"),], "Dist_Difference",5); dev.off()
pdf("DistanceTvsDistanceT+1Females.pdf"); imageplotDTvsTp1(Data[which(Data$Sex=="F"),], "Dist_Difference",5); dev.off()

pdf("SpeedTvsT+1.pdf"); imageplotSTvsTp1(Data, "Speed_mps",5); dev.off()
pdf("SpeedTvsT+1Male.pdf"); imageplotSTvsTp1(Data[which(Data$Sex=="M"),], "Speed_mps",5); dev.off()
pdf("SpeedTvsT+1Female.pdf"); imageplotSTvsTp1(Data[which(Data$Sex=="F"),], "Speed_mps",5); dev.off()


#pdf("AngleTvsAngleT+1.pdf"); imageplotATvsTp1(Data, "ChangeAngle",5); dev.off()

#- Plots the desities of variables of interest for all, by sex, by individual
setwd(SaveDir)
PlotDensity(Data, Variable="ChangeAngle",  XLAB="Change in Bearing", YMIN=0,YMA=0.35, XMIN=-pi,XMAX=pi,adjust=0.5  )
PlotDensity(Data, Variable="Bearing",  XLAB="Bearing", YMIN=0,YMA=0.35, XMIN=0,XMAX=2*pi,adjust=0.5   )
PlotDensity(Data, Variable="Dist_Difference",  XLAB="Displacement", YMIN=0,YMA=0.0015, XMIN=0,XMAX=20000,Legx=1000,adjust=0.5   )
PlotDensity(Data, Variable="Time_Difference",  XLAB="Length of Time between locations", YMIN=0,YMA=0.010, XMIN=0,XMAX=60*60*24,Legx=1000,adjust=0.5   )
PlotDensity(Data, Variable="Speed_mps",  XLAB="Minimum speed travelled", YMIN=0,YMAX=40, XMIN=0,XMAX=1.2,Legx=0.1,adjust=0.5  )

PlotDensitylog(Data, Variable="Dist_Difference",  XLAB="Log (Displacement)", YMIN=0,YMA=0.4, XMIN=0,XMAX=log(20000),Legx=0,adjust=0.5  )

#PlotDensity(data5hrs, Variable="Dist_Difference", savename="DensityDist5hrs", XLAB="Displacement", YMIN=0,YMA=0.0015, XMIN=0,XMAX=20000,Legx=1000,adjust=0.5   )
#PlotDensity(data5hrs, Variable="Speed_mps",  savename="DensitySpeed5hrs", XLAB="Minimum speed travelled", YMIN=0,YMAX=40, XMIN=0,XMAX=3,Legx=2,adjust=0.5  )

#- Produces descriptive statistics for variables of interest for all, by sex, by individual
#DescriptiveStatTable(Data,"ChangeAngle")
tablemostcommoncontin(Data,"Time_Difference",(60*60),(60*60*24))
DescriptiveStatTable(Data,"Dist_Difference",1)
wilcox.test(Data[which(Data$Sex=="M"),]$Dist_Difference,Data[which(Data$Sex=="F"),]$Dist_Difference,conf.int=TRUE)
ks.test(Data[which(Data$Sex=="M"),]$Dist_Difference,Data[which(Data$Sex=="F"),]$Dist_Difference)
DescriptiveStatTable(Data,"Time_Difference",(60*60))
DescriptiveStatTable(Data,"Speed_mps",1)
wilcox.test(Data[which(Data$Sex=="M"),]$Speed_mps,Data[which(Data$Sex=="F"),]$Speed_mps,conf.int=TRUE)
ks.test(Data[which(Data$Sex=="M"),]$Speed_mps,Data[which(Data$Sex=="F"),]$Speed_mps)

#- Minimum and Maximum latitude/Longitude
MINLAT<-min(Data$Latitude,na.rm=T); MAXLAT<-max(Data$Latitude,na.rm=T);
MINLON<-min(Data$Longitude,na.rm=T);MAXLON<-max(Data$Longitude,na.rm=T);
alphahullindivid(Data, COLS=COL, minlat=MINLAT, maxlat=MAXLAT, minlong=MINLON, maxlong=MAXLON, ALPHA=0.8)
Contoursindivid(Data, COLS=COL, FACTOR=250)
Contours(Data, COL=COL[19], NAME="All", FACTOR=250)

convexhull(Data, COL=COL[19], NAME="All", minlat=MINLAT, maxlat=MAXLAT, minlong=MINLON, maxlong=MAXLON)
ConvexIndivid(Data, COLS=COL, minlat=MINLAT, maxlat=MAXLAT, minlong=MINLON, maxlong=MAXLON)

PlotAllLocations(Data, minlat=MINLAT, maxlat=MAXLAT, minlon=MINLON, maxlon=MAXLON,COLS=COL)
setwd(SaveDir)
#locationsAndDensitiesIndivid(Data, ALPHA=0.05)
#distancelocationstoCentrePointsIndivid(Data,COLS=COL, MINLAT=MINLAT, MAXLAT=MAXLAT, MINLON=MINLON, MAXLON=MAXLON)

#PlotAllLocations(Data[which(abs(Data$ChangeAngle)>(pi-0.1)),], minlat=MINLAT, maxlat=MAXLAT, minlon=MINLON, maxlon=MAXLON,COLS=COL)