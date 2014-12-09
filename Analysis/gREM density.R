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
# Script title: gREM density				   	                    	#
# Script purpose: Calculating REM/gREM density							#
#				   - Calculates average speed							#
#                  - Calculates number of captures in a dataset	        #
#                  - Calculates the no.of caps per iteration			#
#				   - Calculates density of all iterations				#
#                                                                   	#
#########################################################################


# average speed of the animals
average.speed<-function(data,length.of.study,number.of.steps){
	speed<-data[which(data$step.number==(number.of.steps-1)),]$total.distance/length.of.study
	average.speed<-mean(speed)
	return(average.speed)
}
average.speed.per.iteration<-function(data,length.of.study,number.of.steps,iterations){
    speeds<-rep(NA, length(iterations))
	for(iter in 1:length(iterations)){
		data.for.iter<-iteration.select(data=data,iteration.number=iterations[iter])
		speeds[iter]<-average.speed(data=data.for.iter,length.of.study=length.of.study,number.of.steps=number.of.steps)
	}
	return(speeds)
}

# captures the no.of captures and returns this number
number.of.captures<-function(data){
	count<-sum(data$just.entered==1)
	return(count)
}

# For each iteration selects dataset and calculated number of captures
# returns a vector of the number of captures
counts.per.iteration<-function(data,iterations){
	counts<-rep(NA, length(iterations))
	for(iter in 1:length(iterations)){
		data.for.iter<-iteration.select(data=data,iteration.number=iterations[iter])
		counts[iter]<-number.of.captures(data.for.iter)
	}
	return(counts)
}

# Estimates the density from the count data; returns the density estimations
grem<-function(counts,profilewidth,animal.speed,length.of.study,no.sensors){
	density.estimations<-(1/profilewidth)*counts/(animal.speed*length.of.study*no.sensors)
	return(density.estimations)
}

# Run function for all of the above
# Calculates and returns density vector  
density.estimation.grem<-function(data, iterations, cameraWidth, cameraRadi, movement, length.of.study,no.sensors,number.of.steps){
	animal.speed<-average.speed.per.iteration(movement,length.of.study,number.of.steps,iterations=iterations)
	#print(paste("animal.speed:", animal.speed))
	profilewidth<-calcProfileWidth(2*pi ,cameraWidth, cameraRadi)[[1]] # assumes a 360Degree call 
	counts<-counts.per.iteration(data,iterations)
	densitys<-grem(counts=counts,profilewidth,animal.speed,length.of.study,no.sensors)
	return(list(densitys,counts,animal.speed))
}
