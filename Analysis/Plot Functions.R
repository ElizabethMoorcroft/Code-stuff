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

plot.density.files<-function(list.of.results,col){
	
	#col=c("blue","green","red")
    density.true<-5*10^-8
    plot(0,type="n",
        ylim=c(-40,40),xlim=c(0.5,length(list.of.results)+0.5),
        axes=FALSE,
        xlab="Number of clusters",ylab="Percentage error",main=NULL)
	box()
	abline(h=0,lty=2,col="grey")
	for(i in 1:length(list.of.results)){
        for(j in 1:3){
            density.est<-list.of.results[[i]][[j]][[1]]
            percentage.error<-(density.est-density.true)/density.true*100
            boxplot(x=percentage.error,at=i+(j-2)/4,add=TRUE,axes=FALSE,col=col[j],boxwex=0.4)
        }
		text(x=i,y=-35,paste("mean speed: ",round(list.of.results[[i]][[1]][[3]],4),"m/s",sep=""),cex=0.45)
		text(x=i,y=-38,paste("mean mmdm: ",round(mean(list.of.results[[i]][[2]][[3]]/1000),1),"km",sep=""),cex=0.45)
	}
	mtext(at=1:length(list.of.results),c("F2","F3","F4","F5","F6","F7"),side=1)
    mtext(at=seq(from=-40,to=40,by=20),seq(from=-40,to=40,by=20),side=2)
    
    legend(x=length(list.of.results)-1,y=40,fill=col,legend=c("gREM","CMR, buffer:mmdm","CMR, buffer:mmdm/2"),cex=0.7)
}

#setwd(DIR_IMG)
#pdf("grem results clusters 2,3,4.pdf")
#plot.counts.files(x)
#plot.density.files(x)
#dev.off()
