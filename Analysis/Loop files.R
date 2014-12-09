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
# Script title: Loop files		   	                 				   	#
# Script purpose: Finds all the relevnat files and loops through them	#
#				   - loops through all files and the                    #
#                  -                                                    #
#                                                                   	#
#########################################################################

#finds files
find.files<-function(iteration.numbers,no.of.clusters,block.numbers,months){
	
	all.files<-list.files() # collects all file names
    all.files<-all.files[grep("8Dec14",all.files)]
	caps<-all.files[grep("Captures.csv",all.files)] # selects the captures files
	if(!is.na(iteration.numbers)){iteration<-caps[grep(iteration.numbers,caps)]} else{iteration<-caps}
	if(!is.na(iteration.numbers)){blocks<-iteration[grep(paste("BlockNumber=",block.numbers),iteration)]} else{blocks<-iteration}
	if(!is.na(iteration.numbers)){clusters<-blocks[grep(paste("NoOfMovementStates=",no.of.clusters),blocks)]} else{clusters<-blocks}
	if(!is.na(iteration.numbers)){months<-clusters[grep(months,clusters)]}else{months<-clusters}
	
	names.of.simulations<-unlist(strsplit(months,"Captures.csv"))
	
	return(names.of.simulations)
}


density.estimation<-function(capture.used,iterations,movement,sensor.used,grid.width,length.of.session,length.of.step,length.of.study,cameraWidth,cameraRadi,number.of.steps){
    
    # density estimation results
    print("grem");print(dim(sensor.used)[1])
    grem.results<-density.estimation.grem(  data=capture.used,
                                            iterations=iterations,
                                            cameraWidth=cameraWidth,
                                            cameraRadi=cameraRadi,
                                            movement=movement,
                                            length.of.study=length.of.study,
                                            no.sensors=dim(sensor.used)[1],
                                            number.of.steps=number.of.steps)
    
    
    print("cmr")
    cmr.results<-density.estimation.crm(    data.capture=capture.used,
                                            data.sensor=sensor.used,
                                            iterations=iterations,
                                            grid.width=grid.width,
                                            length.of.session=length.of.session,
                                            length.of.step=length.of.step)
                                            
    cmr.mmdm.results<-cmr.results[[1]]
    cmr.mmdm.over2.results<-cmr.results[[2]]
    
    print("secr")
    #secr.results<-density.estimation.secr(  data.captures=capture.used,
    #                                        data.traps=sensor.used,
    #                                        iterations=iterations,
    #                                        buffer.width=cmr.results[[3]],
    #                                        length.of.session=(60*60*24*10),
    #                                        length.of.step=18000)
    
    
    return(list(grem.results,cmr.mmdm.results,cmr.mmdm.over2.results))#,secr.results
}



calculate.all.files<-function(files,iterations,length.of.session=(60*60*24*10),cameras=1){ # camera = {interval, no.of.cameras(sq.number)}
	
	l.files<-length(files)
	list.of.results<-vector(mode="list",length=l.files)
	
	for(i in 1:l.files){
		print(paste(i,"/",l.files))
		file<-files[i]
		settings <- read.csv(paste(file,"Settings.csv",sep=""),header=TRUE)
		capture  <- read.csv(paste(file,"Captures.csv",sep=""),header=TRUE)
		movement <- read.csv(paste(file,"Movement.csv",sep=""),header=TRUE)
        sensors <- read.csv(paste(file,"Sensors.csv",sep=""),header=TRUE)
        
        capture.used<-capture.sensor.placement.select(data.sensor=sensors,data.capture=capture,type="Grid")
        sensor.used<-sensor.type.select(data=sensors,type="Grid")
        
        
        # parameters
        grid.width<-as.numeric(as.character(settings[which(settings[,1]=="GridWidth"),2]))
        length.of.step<-as.numeric(as.character(settings[which(settings[,1]=="StepLength"),2]))
        length.of.study<-as.numeric(as.character(settings[which(settings[,1]=="LengthMonitoring"),2]))
        number.of.steps<-as.numeric(as.character(settings[which(settings[,1]=="NoSteps"),2]))
        camera.radii<-mean(sensor.used$radius)
        camera.width<-mean(sensor.used$half.width.angle)
        
        results<-density.estimation(capture.used=capture.used,
                                    iterations=iterations,
                                    movement=movement,
                                    sensor.used=sensor.used,
                                    grid.width=grid.width,
                                    length.of.session=length.of.session,
                                    length.of.step=length.of.step,
                                    length.of.study=length.of.study,
                                    cameraWidth=camera.width*2,
                                    cameraRadi=camera.radii,
                                    number.of.steps=number.of.steps
                                    )
        
		list.of.results[[i]]<-results
		
	}
	return(list.of.results)
}

