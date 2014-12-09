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
# Script title: Calculated Values	        	                    	#
# Script purpose: Calculate values from the simulation					#
#                  - Time since start							        #
#                  - 				                    				#
#				   - 													#
#                                                                   	#
#########################################################################


# Time since start of simulation
time.of.capture<-function(data,length.of.step){
	data$time.since.start<-(data$step.number+data$time-1)*length.of.step
	return(data)
}

session.number<-function(data,length.of.session,length.of.step){
	data<-time.of.capture(data,length.of.step)
	data$session.number<-ceiling(data$time.since.start/length.of.session)
    data$session.number[which(data$session.number==0)]<-1 # for any animal that starts in the range of the camera
	return(data)
}