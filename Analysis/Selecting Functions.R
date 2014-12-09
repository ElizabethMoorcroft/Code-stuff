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
# Script title: Selecting Functions	        	                    	#
# Script purpose: Selects the relevent subsamples of data:				#
#                  - Iteration									        #
#                  - Individual		                    				#
#				   - Camera												#
#                                                                   	#
#########################################################################


###
# Functions
#select iteration
iteration.select<-function(data,iteration.number){
	new.data<-data[which(data$iteration.number == iteration.number),]
	return(new.data)
}

#select indivdiuals
individual.select<-function(data,animal.number){
	new.data<-data[which(data$animal.number == animal.number),]
	return(new.data)
}

# select camera
sensor.select<-function(data,sensor.number){
	new.data<-data[which(data$sensor.ID %in% sensor.number),]
	return(new.data)
}

# select camera type: Grid or Random
sensor.type.select<-function(data,type){
	new.data<-data[which(data$placement %in% type),]
	return(new.data)
}

# select captures from camera setup type
capture.sensor.placement.select<-function(data.sensor,data.capture,type){
    new.data.sensor<-sensor.type.select(data=data.sensor,type=type)
    sensor.id<-unique(new.data.sensor$sensor.ID)
    new.data.capture<-data.capture[which(data.capture$sensor.ID %in% sensor.id),]
    return(new.data.capture)
}
#Run function to check it works
#x<-capture.sensor.placement.select(data.sensor=sensors,data.capture=capture,type="Random")



