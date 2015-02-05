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



movement<-function(name){
    movement<-read.csv(paste(name,"Movement.csv",sep=""))
    capture<-read.csv(paste(name,"Captures.csv",sep=""))
    sensors<-read.csv(paste(name,"Sensors.csv",sep=""))
    settings<-read.csv(paste(name,"Settings.csv",sep=""))

    sensor.ids<-sensors[which(sensors$placement=="Grid"),]$sensor.ID
    plot.movement(current.movement=movement[which(movement$iteration.number==1),],settings,capture[which(capture$iteration.number==1 & capture$sensor.ID %in% sensor.ids),],sensors[which(sensors$placement=="Grid"),])
}

plot.movement<-function(current.movement,settings,capture,sensors){
    length.of.step<-as.numeric(as.character(settings[which(settings[,1]=="StepLength"),2]))
    length.of.study<-as.numeric(as.character(settings[which(settings[,1]=="LengthMonitoring"),2]))
    number.of.steps<-as.numeric(as.character(settings[which(settings[,1]=="NoSteps"),2]))
    
    mean.speed<-average.speed(data=current.movement,length.of.study=length.of.study,number.of.steps=number.of.steps)
    mmdm<-mean.maximum.distance.moved(data.capture=capture,data.sensor=sensors)[[1]]
    mhrd<-mean.mcp.diameter(data=current.movement,percent=100)[[1]]
    maxwidth<-mean.home.range.diameter(current.movement)
    
    animal.movement<-current.movement[which(current.movement$animal.number==0),]
    
    animal.movement$x.location<-animal.movement$x.location/1000;animal.movement$y.location<-animal.movement$y.location/1000
    ymin<-min(animal.movement$y.location,na.rm=TRUE);ymax<-max(animal.movement$y.location,na.rm=TRUE);yrange<-ymax-ymin
    xmin<-min(animal.movement$x.location,na.rm=TRUE);xmax<-max(animal.movement$x.location,na.rm=TRUE);xrange<-xmax-xmin
    plot(type="l",x=animal.movement$x.location,y=animal.movement$y.location,col=1,ylim=c(ymin-yrange/10,ymax+yrange/10),axes=FALSE);box()
    text(y=ymax+yrange/10,x=xmax-2*xrange/10,paste("average speed",round(mean.speed,2),"m/s"),cex=0.8)
    text(y=ymax+7*yrange/100,x=xmax-2*xrange/10,paste("average mmdm",round(mmdm/1000,2),"km"),cex=0.8)
    text(y=ymax+4*yrange/100,x=xmax-2*xrange/10,paste("average hr diameter",round(mhrd[[1]]/1000,2),"km"),cex=0.8)
    text(y=ymax+1*yrange/100,x=xmax-2*xrange/10,paste("average max width",round(maxwidth[[1]]/1000,2),"km"),cex=0.8)
}




home.range.diameter<-function(data){
    cols<-which(names(data) %in% c("x.location","y.location" ))
    hpts <- chull(data[,cols])
    hpts <- c(hpts, hpts[1])
    new.data<-data[hpts,cols]
    
    temp.diameter<-vector(length=choose(dim(new.data)[1],2))
    count<-1
    for(i in 1:dim(new.data)[1]){
        for(j in i:dim(new.data)[1]){
            temp.diameter[count]<-sqrt((new.data$x.location[i]-new.data$x.location[j])^2+(new.data$y.location[i]-new.data$y.location[j])^2)
            count=count+1
        }
    }
    diameter<-max(temp.diameter)
    return(diameter)
}
mean.home.range.diameter<-function(data){
    animals<-unique(data$animal.number)
    diameter<-rep(0,length=length(animals))
    for(i in 0:length(animals)){
        data.for.animal<-individual.select(data,animals[i])
        diameter[i]<-home.range.diameter(data.for.animal)
    }
    mean.diameter<-mean(diameter)
    return(list(mean.diameter,diameter))
}
home.range.diameter.per.iteration<-function(data,iterations){
    diameters<-vector(mode="list", length=length(iterations))
	for(iter in 1:length(iterations)){
		data.for.iter<-iteration.select(data=data,iteration.number=iterations[iter])
		diameters[[iter]]<-mean.home.range.diameter(data=data.for.iter)
	}
	return(diameters)
}


