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
#Name<-"TestForGPS,Density=5per100km^2,NoOfMovementStates=4,BlockNumber=2,Months=(1-3),Iterations=(1-101),"
#movement<-read.csv(paste(Name,"Movement.csv",sep=""))
#capture<-read.csv(paste(Name,"Captures.csv",sep=""))
#sensors<-read.csv(paste(Name,"Sensors.csv",sep=""))
#settings<-read.csv(paste(Name,"Settings.csv",sep=""))


#sensors<-read.csv(paste("8Dec14,Density5per100km^2,Sex0,NoOfMovementStates2,BlockNumber15,Months5-7,Iterations1-101,","Sensors.csv",sep=""))
#settings<-read.csv(paste("8Dec14,Density5per100km^2,Sex0,NoOfMovementStates2,BlockNumber15,Months5-7,Iterations1-101,","Settings.csv",sep=""))
#captures<-read.csv(paste("8Dec14,Density5per100km^2,Sex0,NoOfMovementStates2,BlockNumber15,Months5-7,Iterations1-101,","Captures.csv",sep=""))
#movement<-read.csv(paste("8Dec14,Density5per100km^2,Sex0,NoOfMovementStates2,BlockNumber15,Months5-7,Iterations1-101,","Movement.csv",sep=""))
#####################
# Run folders		#
#####################


setwd(DIR_DATA)
files<-c(	"8Dec14,Density5per100km^2,Sex0,NoOfMovementStates2,BlockNumber15,Months5-7,Iterations1-101,",
            "8Dec14,Density5per100km^2,Sex0,NoOfMovementStates3,BlockNumber15,Months5-7,Iterations1-101,",
            "8Dec14,Density5per100km^2,Sex0,NoOfMovementStates4,BlockNumber15,Months5-7,Iterations1-101,",
            "8Dec14,Density5per100km^2,Sex0,NoOfMovementStates5,BlockNumber15,Months5-7,Iterations1-101,",
            "8Dec14,Density5per100km^2,Sex0,NoOfMovementStates6,BlockNumber15,Months5-7,Iterations1-101,",
            "8Dec14,Density5per100km^2,Sex0,NoOfMovementStates7,BlockNumber15,Months5-7,Iterations1-101,"
)
x<-calculate.all.files(files,iterations=1:100)
plot.density.files(x,col=COLset1[1:3])