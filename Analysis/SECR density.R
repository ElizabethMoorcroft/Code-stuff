#########################################################################
# Project title: Calculating denisty of animals from number of captures #
# Project: Snow Leopards				                            	#
#                                                                   	#
# Author: Elizabeth Moorcroft                                       	#
# Date created: 4.12.2014                                         		#
#                                                                  	 	#
# Edited by: -			                                                #
# Edited on: -	                                            			#
#                                                                   	#
# Script title: CMR density				   	       		             	#
# Script purpose: Calculating REM/gREM density							#
#				   - Calculates capture matrix							#
#                  -                                                    #
#                  -                                                    #
#				   - Calculates density of all iterations				#
#                                                                   	#
#########################################################################



capture.matrix.secr<-function(data,length.of.session,length.of.step){
    
    # only the data when the animal enters the range
    data<-data[which(data$just.entered==1),]
    
    # creates occasion
    data<-session.number(data,length.of.session,length.of.step)
    session.name<-rep("session",length=dim(data)[1])
    
    # new data is session-name, ID, ocassion, sensor
    new.data<-cbind(session.name,data$animal.number,data$session.number,data$sensor.ID)
    new.data<-unique(new.data)
    colnames(new.data)<-c("Session", "ID", "Occasion", "Detector")
    
    
    new.data<-as.data.frame(new.data)
    new.data[,2]<-as.numeric(as.character(new.data[,2]))
    new.data[,3]<-as.numeric(as.character(new.data[,3]))
    new.data[,4]<-as.numeric(as.character(new.data[,4]))+1
    
    return(new.data)
    
}

# gives the sensor matrix in form asked for
sensor.matrix.secr<-function(data){
    
    new.data<-cbind(data$sensor.ID, data$x.location, data$y.location)
    colnames(new.data)<-c("Detector", "x", "y")
    new.data<-as.data.frame(new.data)
    
    return(new.data)
}


estimation.per.iteration<-function(data.capture,data.traps,buffer.width,length.of.session,length.of.step){
    data.capture.matrix<-capture.matrix.secr(data.capture,length.of.session,length.of.step)
    capthist<-make.capthist(captures=data.capture.matrix, traps=data.traps, fmt = "trapID")
    fit<-secr.fit (capthist,buffer=buffer.width,trace=FALSE)
    density<-exp(fit[]$fit$estimate)[1]
    return(density)
}

density.estimation.secr<-function(data.captures,data.traps,iterations,buffer.width,length.of.session,length.of.step){
    sensors.in.format<-read.traps(data =sensor.matrix.secr(sensors))
    densitys<-rep(0,length(iterations))
    for(iter in 1:length(iterations)){
        current.data.capture<-data.captures[which(data.captures$iteration.number==iterations[iter]),]
        densitys[iter]<-estimation.per.iteration(current.data.capture,data.traps=sensors.in.format,buffer.width=buffer.width[iter],length.of.session,length.of.step)
    }
    return(densitys)
}
#Run
#density.estimation.secr(data.captures=capture,data.traps=sensors[which(sensor$placement=="Grid"),],iterations=1:100,buffer.width=cmr[[3]]length.of.session=(60*60*24*10),length.of.step=18000)


