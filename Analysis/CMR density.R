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

	# 	T1, T2, T3, ... TN
	#I1  1	 0	 1		 1
	#I2
	#I3
	#...
	#IN
capture.matrix<-function(data,length.of.session,length.of.step){
	data<-session.number(data,length.of.session,length.of.step)
	t<-table(data$animal.number,data$session.number)
    t[which(t>1)]<-1
	return(t)
}

# the capture matrix per iteration
matrix.per.iteration<-function(data,iterations,length.of.session,length.of.step){
    matrices<-vector(mode="list",length=length(iterations))
    for(iter in 1:length(iterations)){
        data.for.iter<-iteration.select(data=data,iteration.number=iterations[iter])
        matrices[[iter]]<-capture.matrix(data=data.for.iter,length.of.session,length.of.step)
    }
    return(matrices)
}

# function for estimation of M0 abundance
abundance.calculation<-function(matrix){
    estimate<-closedp(matrix)$results[1,1] # estimation of M0 abundance
    return(estimate)
}

# For the abundance for all the iterations
abundance<-function(data,iterations,length.of.session,length.of.step){
    matrices<-matrix.per.iteration(data,iterations,length.of.session,length.of.step)
    abundances<-unlist(lapply(matrices,abundance.calculation))
    return(abundances)
}


# Max distnace moved by each individual
max.distance.moved<-function(data){
    
    
    new.data<-data[which(names(data) %in% c("x.location","y.location"))]
    new.data<-unique(new.data)
    all.other.locations <- 1:dim(new.data)[1] ;
    
    if(dim(new.data)[1]>2){
        all.combinations<- combn(all.other.locations,2)
        distance<-rep(0,dim(all.combinations)[2])
        for(combination in 1:dim(all.combinations)[2]){
            i<-all.combinations[1,combination]; j<-all.combinations[2,combination]
            distance[combination]<-sqrt((new.data$x.location[i]-new.data$x.location[j])^2 +(new.data$y.location[i]-new.data$y.location[j])^2)
        }
    
        max.distance<-max(distance)
    }else{max.distance<-0}
    return(max.distance)
}

# Mean maximum distance moved for all individuals in one iteration
mean.maximum.distance.moved<-function(data.capture,data.sensor){
    animals<-unique(data.capture$animal.number)
    distance<-rep(0,length(animals))
    for(i in 1:length(animals)){
        sensors.ids<-data.capture[which(data.capture$animal.number %in% animals[i]),]$sensor.ID
        sensors<-data.sensor[which(data.sensor$sensor.ID %in% sensors.ids),]
        distance[i]<-max.distance.moved(sensors)
    }
   mean.max.distance<-mean(distance)
   return(list(mean.max.distance,distance))
}

mmdm.per.iteration<-function(data.capture,data.sensor,iterations){
    mmdm<-rep(0,length(iterations))
    for(iter in 1:length(iterations)){
        capture.data.for.iter<-iteration.select(data=data.capture,iteration.number=iterations[iter])
        mmdm[iter]<-mean.maximum.distance.moved(data.capture=capture.data.for.iter,data.sensor=data.sensor)[[1]]
    }
    return(mmdm)
}

density.estimation.crm<-function(data.capture, data.sensor, iterations, grid.width, length.of.session=(60*60*24*10),length.of.step=18000){
    # Area of grid
    area.of.grid<-grid.width^2
    # Area of additional space (r=average displacement): r*gw+r*gh+(pi*(r/2)^2)
    buffer.width<-mmdm.per.iteration(data.capture=data.capture,data.sensor=data.sensor,iterations)
    
    # 2 options for  Total area
    buffer.area.mmdm<-4*buffer.width*grid.width+pi*(buffer.width)^2
    total.area.mmdm<-area.of.grid+buffer.area.mmdm
    buffer.area.mmdm.over2<-2*buffer.width*grid.width+pi*(buffer.width/2)^2
    total.area.mmdm.over2<-area.of.grid+buffer.area.mmdm.over2
    
    # Calculation of abundance
    abundance<-abundance(data.capture,iterations,length.of.session,length.of.step)
    
    # Transformation of abundance to density
    density.mmdm<-abundance/total.area.mmdm
    density.mmdm.over2<-abundance/total.area.mmdm.over2
    
    # List two sets of results
    mmdm<-list(density.mmdm,abundance,buffer.width)
    mmdm.over2<-list(density.mmdm.over2,abundance,buffer.width/2)
    
    #returns values
	return(list(mmdm,mmdm.over2))
}


# Run the code
#x<-capture.sensor.placement.select(data.sensor=sensors,data.capture=capture,type="Grid")
#matrix<-matrix.per.iteration(data=x,1,length.of.session=(60*60*24*10),length.of.step=18000)
#abundance(data=x,iterations=1,length.of.session=(60*60*24*10),length.of.step=18000)
#closedp(matrix[[1]])
#cmr<-density.estimation.crm(data.capture=x, data.sensor=sensors, iterations=1:100, grid.width=40000, length.of.session=(60*60*24*10),length.of.step=18000)