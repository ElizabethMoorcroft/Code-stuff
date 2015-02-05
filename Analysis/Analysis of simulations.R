#########################################################################
# Project title: Calculating denisty of animals from number of captures #
# Project: Snow Leopards				                            	#
#                                                                   	#
# Author: Elizabeth Moorcroft                                       	#
# Date created: 3.12.2014                                         		#
#                                                                  	 	#
# Edited by: -			                                                #
# Edited on: -	                                            			#
#                                                                   	#
# Script title: Analysis of simulations        	                    	#
# Script purpose: Calculate the density form the simulation using		#
#                  - REM/gREM									        #
#                  - CMR			                    				#
#				   - SECR												#
#                                                                   	#
#########################################################################
rm(list=ls(all=TRUE)) 

#####################
# Libraries 		#
#####################
library("RColorBrewer")
library("plotrix")
library("Rcapture")
library("secr")
library("adehabitatHR")


#####################
# Directory			#
#####################
DIR_DATA<-"/Users/student/Documents/Snow Leopards/Simulations"
DIR_SAVE<-"/Users/student/Documents/Snow Leopards/Simulations"
DIR_IMG<-"/Users/student/Documents/Snow Leopards/Temp"
DIR_CODE<-"/Users/student/Documents/Snow Leopards/Code/Analysis"


#####################
# Source code		#
#####################
#grem source code
source("/Users/student/Documents/Bats/RAnalysis/R analysis code/calculateProfileWidth.R")

setwd(DIR_CODE)
source("Selecting Functions.R")
source("Calculated Values.R")
source("gREM density.R")
source("CMR density.R")
source("SECR density.R")
source("Loop files.R")
source("Plot Functions.R")
source("Movement.R")

#####################
# Colours 			#
#####################
COLset1=brewer.pal(9,"Set1")
COLset2=brewer.pal(8,"Set2")
COLset3=brewer.pal(12,"Set3")

COLsets=rep(c(COLset1,COLset2,COLset3),4)


#####################
# Load data			#
#####################

#setwd(DIR_DATA)
Name<-"10Dec14,Density5per100km^2,Sex0,NoOfMovementStates2,BlockNumber15,Months5-7,Iterations1-101,"
#movement<-read.csv(paste(Name,"Movement.csv",sep=""))
capture<-read.csv(paste(Name,"Captures.csv",sep=""))
sensors<-read.csv(paste(Name,"Sensors.csv",sep=""))
#settings<-read.csv(paste(Name,"Settings.csv",sep=""))


#####################
# Run folders		#
#####################


#setwd(DIR_IMG)
#pdf("Movement_Male_Months5-7.pdf")
#setwd(DIR_DATA)
#for(i in 2:7){
#    name<-paste("10Dec14,Density5per100km^2,Sex0,NoOfMovementStates",i,",BlockNumber15,Months5-7,Iterations1-101,",sep="")
#    movement(name);
#}
#dev.off()


#setwd(DIR_IMG)
#pdf("Movement_female_Months5-7.pdf")
#setwd(DIR_DATA)
#for(i in 2:7){
#    name<-paste("10Dec14,Density5per100km^2,Sex1,NoOfMovementStates",i,",BlockNumber28,Months5-7,Iterations1-101,",sep="")
#    movement(name);
#}
#dev.off()

#setwd(DIR_IMG)
#pdf("Movement_male_Months2_4.pdf")
#setwd(DIR_DATA)
#for(i in 2:7){
#    name<-paste("10Dec14,Density5per100km^2,Sex1,NoOfMovementStates",i,",BlockNumber25,Months2-4,Iterations1-101,",sep="")
#    movement(name);
#}
#dev.off()

#setwd(DIR_IMG)
#pdf("Movement_female_Months2_4.pdf")
#setwd(DIR_DATA)
#for(i in 2:7){
#    name<-paste("10Dec14,Density5per100km^2,Sex0,NoOfMovementStates",i,",BlockNumber1,Months2-4,Iterations1-101,",sep="")
#    movement(name);
#}
#dev.off()

#####
setwd(DIR_DATA)
grid.number.list<-list(0:6,0:6*2,0:6*3,0:6*4,0:6*5,0:6*6,0:6*7,0:49)
files<-files.calculations(iteration.numbers="1-101",no.of.clusters=as.character(2:7),block.numbers="15",months="5-7",sexes="0",date="10Dec14")
files<-c("10Dec14,Density5per100km^2,Sex0,RandomWalk1,MeanSpeed0.0552,Months5-7,Iterations1-101,",files)
summerF<-calculate.all.files(files,iterations=1:100,percent=100,grid.number=grid.number.list)
files<-files.calculations(iteration.numbers="1-101",no.of.clusters=as.character(2:7),block.numbers="28",months="5-7",sexes="1",date="10Dec14")
files<-c("10Dec14,Density5per100km^2,Sex1,RandomWalk1,MeanSpeed0.0773,Months5-7,Iterations1-101,",files)
summerM<-calculate.all.files(files,iterations=1:100,percent=100,grid.number=grid.number.list)
files<-files.calculations(iteration.numbers="1-101",no.of.clusters=as.character(2:7),block.numbers="1",months="2-4",sexes="0",date="10Dec14")
files<-c("10Dec14,Density5per100km^2,Sex0,RandomWalk1,MeanSpeed0.039,Months2-4,Iterations1-101,",files)
springF<-calculate.all.files(files,iterations=1:100,percent=100,grid.number=grid.number.list)
files<-files.calculations(iteration.numbers="1-101",no.of.clusters=as.character(2:7),block.numbers="25",months="2-4",sexes="1",date="10Dec14")
files<-c("10Dec14,Density5per100km^2,Sex1,RandomWalk1,MeanSpeed0.0606,Months2-4,Iterations1-101,",files)
springM<-calculate.all.files(files,iterations=1:100,percent=100,grid.number=grid.number.list)


setwd(DIR_IMG)
pdf("male and female clusters.pdf")
plot.density.files(summerF,col=COLset1[1:7],sex="F")
plot.density.files(summerM,col=COLset1[1:7],sex="M")
plot.density.files(springF,col=COLset1[1:7],sex="F")
plot.density.files(springM,col=COLset1[1:7],sex="M")
dev.off()



files<-files.calculations(iteration.numbers="1-101",no.of.clusters=c(NA,as.character(2:7)),block.numbers="4",months="8-10",sexes="0",date="10Dec14")
autumnF<-calculate.all.files(files,iterations=1:100,percent=100,grid.number=grid.number.list)
files<-files.calculations(iteration.numbers="1-101",no.of.clusters=c(NA,as.character(2:7)),block.numbers="17",months="8-10",sexes="1",date="10Dec14")
autumnM<-calculate.all.files(files,iterations=1:100,percent=100,grid.number=grid.number.list)

files<-files.calculations(iteration.numbers="1-101",no.of.clusters=c(NA,as.character(2:7)),block.numbers="6",months="11-13",sexes="0",date="10Dec14")
winterF<-calculate.all.files(files,iterations=1:100,percent=100,grid.number=grid.number.list)
files<-files.calculations(iteration.numbers="1-101",no.of.clusters=c(NA,as.character(2:7)),block.numbers="21",months="11-13",sexes="1",date="10Dec14")
winterM<-calculate.all.files(files,iterations=1:100,percent=100,grid.number=grid.number.list)

setwd(DIR_IMG)
pdf("male and female clusters all seasons.pdf")
compare.results(summerF,col=COLset1[1:7],sex="F",ylimits=c(-150,150),grid.number.list=grid.number.list,distance.between.cameras=1)
compare.results(summerM,col=COLset1[1:7],sex="M",ylimits=c(-150,150),grid.number.list=grid.number.list,distance.between.cameras=1)
compare.results(springF,col=COLset1[1:7],sex="F",ylimits=c(-150,150),grid.number.list=grid.number.list,distance.between.cameras=1)
compare.results(springM,col=COLset1[1:7],sex="M",ylimits=c(-150,150),grid.number.list=grid.number.list,distance.between.cameras=1)
compare.results(autumnF,col=COLset1[1:7],sex="F",ylimits=c(-150,150),grid.number.list=grid.number.list,distance.between.cameras=1)
compare.results(autumnM,col=COLset1[1:7],sex="M",ylimits=c(-150,150),grid.number.list=grid.number.list,distance.between.cameras=1)
compare.results(winterF,col=COLset1[1:7],sex="F",ylimits=c(-150,150),grid.number.list=grid.number.list,distance.between.cameras=1)
compare.results(winterM,col=COLset1[1:7],sex="M",ylimits=c(-150,150),grid.number.list=grid.number.list,distance.between.cameras=1)
dev.off()

setwd(DIR_DATA)
files<-files.calculations(iteration.numbers="1-101",no.of.clusters=c(NA,as.character(2:7)),block.numbers="21",months="11-13",sexes="1",date="10Dec14")
secrTwinterM<-calculate.all.files(files,iterations=1:100,percent=100,grid.number=grid.number.list,secr=TRUE)