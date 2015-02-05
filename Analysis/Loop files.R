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
files.calculations<-function(iteration.numbers,no.of.clusters,block.numbers,months,sexes,date){
    list.of.files<-lapply(no.of.clusters,function(x){
        find.files(iteration.numbers=iteration.numbers,no.of.clusters=x,block.numbers=block.numbers,months=months,sexes=sexes,date=date)
        }
    )
    return(unlist(list.of.files))
}

find.files<-function(iteration.numbers,no.of.clusters,block.numbers,months,sexes,date){#"8Dec14"
	
	all.files<-list.files() # collects all file names
    all.files<-all.files[grep(date,all.files)]
	caps<-all.files[grep("Captures.csv",all.files)] # selects the captures files
	if(!is.na(iteration.numbers)){iteration<-caps[grep(paste("Iterations",iteration.numbers,sep=""),caps)]} else{iteration<-caps}
	if(!is.na(iteration.numbers)){sex<-iteration[grep(paste("Sex",sexes,sep=""),iteration)]} else{sex<-iteration}
    if(!is.na(iteration.numbers)){month.data<-sex[grep(months,sex)]}else{months<-sex}
    if(!is.na(no.of.clusters)){
        if(!is.na(iteration.numbers)){blocks<-month.data[grep(paste("BlockNumber",block.numbers,sep=""),month.data)]} else{blocks<-month.data}
        if(!is.na(iteration.numbers)){clusters<-blocks[grep(paste("NoOfMovementStates",no.of.clusters,sep=""),blocks)]} else{clusters<-blocks}
    } else{
        print(month.data)
        if(!is.na(iteration.numbers)){clusters<-month.data[grep("MeanSpeed",month.data)]} else{clusters<-month.data}
    }
    
	names.of.simulations<-unlist(strsplit(clusters,"Captures.csv"))
	
	return(names.of.simulations)
}


density.estimation<-function(capture.used,iterations,movement,sensor.used,grid.width,length.of.session,length.of.step,length.of.study,cameraWidth,cameraRadi,number.of.steps,percent,home.range.width,secr){
    
    
    
    
    # density estimation results
    #print("grem");print(dim(sensor.used)[1])
    grem.results<-density.estimation.grem(  data=capture.used,
                                            iterations=iterations,
                                            cameraWidth=cameraWidth,
                                            cameraRadi=cameraRadi,
                                            movement=movement,
                                            length.of.study=length.of.study,
                                            no.sensors=dim(sensor.used)[1],
                                            number.of.steps=number.of.steps)
    
    
    #print("cmr")

    
    cmr.results<-density.estimation.crm(    data.capture=capture.used,
                                            data.sensor=sensor.used,
                                            data.movement=movement,
                                            iterations=iterations,
                                            grid.width=grid.width,
                                            length.of.session=length.of.session,
                                            length.of.step=length.of.step,
                                            percent=percent,
                                            home.range.width=home.range.width)
                                            

    
    return.values<-list(grem.results)
    for(p in 1:length(cmr.results)){
        return.values[[p+1]]<-cmr.results[[p]]
    }
    
    if(secr==TRUE){
        print("secr")
        print(cmr.results[[4]][[3]])
        secr.results<-density.estimation.secr(  data.captures=capture.used,
                                            data.traps=sensor.used,
                                            iterations=iterations,
                                            buffer.width=cmr.results[[4]][[3]],
                                            length.of.session=(60*60*24*10),
                                            length.of.step=18000)
        return.values[[length(cmr.results)+1]]<-secr.results
    }
    
    return(return.values)#,secr.results
}



calculate.all.files<-function(files,iterations,length.of.session=(60*60*24*10),cameras=1,percent, grid.numbers,secr=FALSE){ # camera = {interval, no.of.cameras(sq.number)}
	
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
        
        list.of.results.j<-vector(mode="list",length=length(grid.numbers))
        
        home.range.width<-rep(0,length(percent))
        temp.home.range.width<-mean.mcp.diameter.per.iteration(data=movement,percent=percent,iterations=1)
        for(p in 1:length(percent)){
            #print(temp.home.range.width[[1]])
            home.range.width[p]<-temp.home.range.width[[1]][[1]][[p]]
            #print(home.range.width[p])
        }
        #print(home.range.width)
        
        
        for(j in 1:length(grid.numbers)){
            
            sensor.used.j<-sensor.used[which(sensor.used$grid.row %in% grid.numbers[[j]] & sensor.used$grid.column %in% grid.numbers[[j]]),]
            sensor.numbers<-sensor.used.j$sensor.ID
            capture.used.j<-capture[which(capture$sensor.ID %in% sensor.numbers),]
            
            print(paste("camera config: ",j, "/",length(grid.numbers)))
            
            results<-density.estimation(capture.used=capture.used.j,
                                    iterations=iterations,
                                    movement=movement,
                                    sensor.used=sensor.used.j,
                                    grid.width=grid.width,
                                    length.of.session=length.of.session,
                                    length.of.step=length.of.step,
                                    length.of.study=length.of.study,
                                    cameraWidth=camera.width*2,
                                    cameraRadi=camera.radii,
                                    number.of.steps=number.of.steps,
                                    percent=percent,
                                    home.range.width=home.range.width,
                                    secr=secr
                                    )
            list.of.results.j[[j]]<-results
        }
		list.of.results[[i]]<-list.of.results.j
		
	}
	return(list.of.results)
}

