#########################################################################
# Project title: Calculating denisty of animals from number of captures #
# Project: Snow Leopards				                            	#
#                                                                   	#
# Author: Elizabeth Moorcroft                                       	#
# Date created: 9.12.2014                                         		#
#                                                                  	 	#
# Edited by: -			                                                #
# Edited on: -	                                            			#
#                                                                   	#
# Script title: Movement                                                #
# Script purpose: Plots the movement of animals:                        #
#                  -                                                    #
#                  -                                                    #
#				   -                                                    #
#                                                                   	#
#########################################################################

setwd(DIR_DATA)
Name<-"8Dec14,Density5per100km^2,Sex0,NoOfMovementStates2,BlockNumber15,Months5-7,Iterations1-2,"
movement<-read.csv(paste(Name,"Movement.csv",sep=""))
capture<-read.csv(paste(Name,"Captures.csv",sep=""))
sensor<-read.csv(paste(Name,"Sensors.csv",sep=""))





mean.speed<-average.speed(data=movement,length.of.study=length.of.study,number.of.steps=number.of.steps)
mmdm<-mean.maximum.distance.moved(data.capture=capture,data.sensor=sensors)[[1]]
mhrd<-home.range.diameter(movement)
    


ymin<-min(current.movement$y.location,na.rm=TRUE);ymax<-max(current.movement$y.location,na.rm=TRUE);yrange<-ymax-ymin
xmin<-min(current.movement$x.location,na.rm=TRUE);xmax<-max(current.movement$x.location,na.rm=TRUE);xrange<-xmax-xmin
plot(type="l",x=current.movement$x.location,y=current.movement$y.location,col=colour,ylim=c(ymin-yrange/10,ymax+yrange/10))
legend(y=ymax+yrange/10,x=xmax-xrange/10,)

home.range.diameter<-function(data){
    diameter<-0
    for(i in 1:dim(data)[1]){
        for(j in i:dim(data)[1]){
            temp.diameter<-sqrt((data$x.location[i]-data$x.location[j])^2+(data$y.location[i]-data$y.location[j])^2)
            diameter<-max(diameter,temp.diameter)
        }
    }
    return(diameter)
}
mean.home.range.diameter<-function(data){
    animals<-unique(data.capture$animal.number)
    diameter<-rep(0,length(animals))
    for(i in 1:length(animals)){
        diameter[i]<-home.range.diameter(sensors)
    }
    mean.diameter<-mean(diameter)
    return(mean.diameter)

}

home.range.diameter.per.iteration<-function(data,iterations){
    diameters<-rep(NA, length(iterations))
	for(iter in 1:length(iterations)){
		data.for.iter<-iteration.select(data=data,iteration.number=iterations[iter])
		diameters[iter]<-mean.home.range.diameter(data=data.for.iter)
	}
	return(diameters)
}
