#####################################################
#
# Elizabeth Moorcroft 
#
# Snow Leopard WinBUGS Run.R
#
# Libraries needed -  (see Libraries section)
# Source files -  (see source file section)
# Created 14 August 2014
#
# Runs SL data through WinBUGS code
#
#####################################################


####
# Libraries
library(CircStats)
library(R2WinBUGS)
library(R2OpenBUGS)
####


####
# Set working directory
DIR_WORK<-"Z:/Users/student/Documents/Snow Leopards/BUGSstuff"
DIR_CODE<-"Z:/Users/student/Documents/Snow Leopards/BUGSstuff"
####


####
# Source files
setwd(DIR_CODE)
source("SaveInits.R") # Writes initial values and data in correct format to load into winbugs
source("Snow Leopard HMM form2.R") # Saves data in form for HMM/SMM
####


####
# Load data
read.csv() # CREATE FILE
####

####
# Functions
merge.matrix.into.vector<-function(Data,cols){
	x<-as.vector(Data[,cols])
	x <- x[!is.na(x)]
}
####






#####################################################
# Double - No switch
# 	if there is no mechanism to switch between the
# 	 two states then I should be able to stick 
#	 everything together in one massive file and 
# 	 run the data aslong massive data set?! 
#####################################################

test<-Months(Data,0,3)

distance.columns<-seq(1,dim(test[[1]])[2],by=2)
angle.columns<-seq(2,dim(test[[1]])[2],by=2)
vector.distances<-merge.matrix.into.vector(test[[1]],distance.columns)
vector.distance<-log(vector.distances)
vector.angles<-merge.matrix.into.vector(test[[1]],angle.columns)


# How I pick good initial values - try to make grphs look similarish and use those values as inits
par(mfrow=c(2,1))
plot(density(c(rweibull(n=300,shape=3,scale=2.5),rweibull(n=700,shape=7,scale=8))),xlim=c(0,11))
plot(density(log(vector.distances*1000)),xlim=c(0,11))
par(mfrow=c(2,1))
plot(density(vector.angles))
plot(density(c(rwrpcauchy(n=300,location=pi,rho =0.5,rwrpcauchy(n=700,location=0,rho =0.5)))))

# Spliting the data into training and validation data sets
length.data<-length(vector.distances)
ten.percent<-round(length.data/10)
ninety.percent<-length.data-ten.percent
validation.dist<-vector.distances[1:ten.percent]
training.dist<-vector.distances[(ten.percent+1):length.data]
validation.angle<-vector.angles[1:ten.percent]
training.angle<-vector.angles[(ten.percent+1):length.data]

# Initail values
# Remember:
#	Shape = b;	Scale = a
data <- list(npts=ninety.percent, l=training.dist, theta=training.angle)
inits <- function(){
    list(a=c(NA,2.5),b=c(7,3), rho=c(0.5,0.5),mu=c(0,pi),
	eps=5.5,
	idx=idx,
	logit.nu=logit.nu
	)
}

#Save input values
setwd(DIR_WORK)
write(write.inits(inits()),file="DoubleInitsReal.txt")
write(write.inits(data),file="DoubleDataReal.txt")

# Run code
vars.to.save=c("a","b","rho","mu")
results<- bugs(data=data, inits = inits, 
	parameters.to.save=vars.to.save, 
	model.file="Double.txt",
	n.iter=20000,
	debug=TRUE)
