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
# Script title: Plot Functions                                          #
# Script purpose: Plots the results of the density estimation and       #
#                 capture numbers:                                      #
#                  -                                                    #
#                  -                                                    #
#				   -                                                    #
#                                                                   	#
#########################################################################




####### plots
plot.counts.files<-function(list.of.results){
	
	
	plot(0,type="n",ylim=c(2000,4000),xlim=c(0.5,length(list.of.results)+0.5),axes=FALSE,xlab="Number of clusters",ylab="Number of captures",main="")
	box()
	abline(h=0)
	for(i in 1:length(list.of.results)){
        
		boxplot(x=list.of.results[[i]][[2]],at=i,add=TRUE,axes=FALSE,col="grey")
		text(x=i,y=2010,round(list.of.results[[i]][[3]],4))
	}
	mtext(at=1:length(list.of.results),c("M2","F2","F3","F4"),side=1)
	
}

plot.density.files<-function(list.of.results,col,sex,ylimits,grid.number.list,distance.between.cameras,no.of.cameras){
	
    points.between.cameras<-grid.number.list[2]-grid.number.list[1]
    new.distance.between.cameras<-points.between.cameras*distance.between.cameras
    
	#col=c("blue","green","red")
    density.true<-5*10^-8
    plot(0,type="n",
        ylim=ylimits,xlim=c(0.5,length(list.of.results)+0.5),
        axes=FALSE,
        xlab="Number of clusters",ylab="Percentage error",
        main=paste("Number of cameras: ",(no.of.cameras)^2,", distance between cameras: ",new.distance.between.cameras,"km",sep="")
        )
	box()
	abline(h=0,lty=2,col="grey")
	for(i in 1:length(list.of.results)){
        temp.data<-list.of.results[[i]]
        no.models<-length(temp.data)
        width.of.box<-1/no.models
        for(j in 1:no.models){
            density.est<-temp.data[[j]][[1]]
            percentage.error<-(density.est-density.true)/density.true*100
            boxplot(x=percentage.error,at=i+(j-(no.models+1)/2)/(no.models+2),add=TRUE,axes=FALSE,col=col[j],boxwex=width.of.box)
        }
		text(x=i,y=ylimits[1]+20,paste("mean speed: ",round(mean(temp.data[[1]][[3]]),4),"m/s",sep=""),cex=0.45)
		text(x=i,y=ylimits[1]+17,paste("mean mmdm: ",round(mean(temp.data[[2]][[3]]/1000),1),"km",sep=""),cex=0.45)
        text(x=i,y=ylimits[1]+14,paste("mean mhrd: ",round(mean(temp.data[[5]][[3]]/1000),1)*2,"km",sep=""),cex=0.45)
        text(x=i,y=ylimits[1]+11,paste("mean max width: ",round(mean(temp.data[[4]][[3]]/1000),1)*2,"km",sep=""),cex=0.45)
	}
	mtext(at=1:length(list.of.results),paste(sex,c("rw","2","3","4","5","6","7"),sep=""),side=1)
    mtext(at=seq(from=ylimits[1],to=ylimits[2],by=round((ylimits[2]-ylimits[1])/6)),seq(from=ylimits[1],to=ylimits[2],by=round((ylimits[2]-ylimits[1])/6)),side=2)
    
    legend(x=length(list.of.results)-1,y=ylimits[2]-50,fill=col,legend=c("gREM","CMR, buffer:mmdm","CMR, buffer:mmdm/2","CMR, buffer:max diameter/2","CMR, buffer:HR/2"),cex=0.7)
}

compare.results<-function(files,col,sex,ylimits=c(-100,200),grid.number.list,distance.between.cameras){
    
    no.of.subs<-length(grid.number.list)
    print(no.of.subs)
    for(k in 1:no.of.subs){
        list.of.results<-lapply(files,function(x){x[[k]]})
        plot.density.files(list.of.results,col,sex,ylimits=ylimits,grid.number.list=grid.number.list[[k]],distance.between.cameras=distance.between.cameras,no.of.cameras=length(grid.number.list[[k]]))

    }
}


compare.setup.results<-function(files,col,sex,ylimits=c(-100,200),grid.number.list,distance.between.cameras){
    
    no.of.subs<-length(grid.number.list)
    print(no.of.subs)
    for(k in 1:no.of.subs){
        list.of.results<-lapply(files,function(x){x[[k]]})
        plot.density.files(list.of.results,col,sex,ylimits=ylimits,grid.number.list=grid.number.list[[k]],distance.between.cameras=distance.between.cameras,no.of.cameras=length(grid.number.list[[k]]))
        
    }
}