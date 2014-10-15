#--------------------------------
# Project title: Snow leopards
#
# Elizabeth Moorcroft: Created XXX, Modified: 18 August 2014
# 
# Script title: Snow Leopard Alpha plots
# Purpose: Creates plots of snow leopard land use
#--------------------------------

####
# Libries
library("spatstat") # for the owin function
####


####
# Load data
Data<-read.csv("CurrentData.csv")
row.numbers<-Months(Data,6,3)[[3]]

####

####
# Source file
setwd(CodeDir)
source("Owin2Polygon.R") # for the owin2SP function


####
# Changes the points into a convex hull shape
points.to.object<-function(points){
   chull.obj <- chull(x=points[,1],y=points[,2])
    temp2<-points[c(chull.obj,chull.obj[1]),1:2]
  	temp2<-temp2[-dim(temp2)[1],]
  	temp3<-temp2[dim(temp2)[1]:1,]
  
  # Changes the pairs of points into a shape using owin
  w<-0
  try(w <- owin(poly=list(x=temp3[,1],y=temp3[,2])))
  if(is.numeric(w)){
  	try(w <- owin(poly=list(x=rev(temp3[,1]),y=rev(temp3[,2]))))
  }
  if(is.numeric(w)){
  	print("Error: Will not become shape"); break;
  }
	
	return(w)	
}

####
# Indenting the shape, returns new shape
indent.shape<-function(shape, indent.value){
   new.shape<-erosion(shape,indent.value)
   if(is.empty(new.shape)){
     print("Shape is now empty")
   }
   return(new.shape)
}


####
# Size and Number of points within the shape
size.and.points<-function(shape,points){
   shape.format<-owin2SP(shape)
   counts<-poly.counts(SpatialPoints(points),shape.format);
   area<-poly.areas(shape.format)
   return(list(counts=counts, area=area))	
}

####
# Calculations for %age changes
change.in.numbers<-function(total.area, total.counts, new.values){
	percentage.area.in.shape<-new.values$area/total.area
	precentage.counts.in.shape<-new.values$counts/total.counts; 
	results<-matrix(ncol=2,nrow=1,c(percentage.area.in.shape,precentage.counts.in.shape)*100)
	return(results)
}

####
update.plot<-function(shape, points, indent.value, results,total.counts,total.area,Col){
	  shape<-indent.shape(shape,indent.value)
      #plot(shape,col=Col,border=Col,add=TRUE)
	  
	  number.counts.and.area<-size.and.points(shape,points)
	  indent.results<-change.in.numbers(total.counts=total.counts,total.area=total.area,number.counts.and.area)
	  results<-rbind(results, indent.results)
	return(list(results,shape))
}


####
# Plot
plot.erosion.of.convex.hull<-function(points, index.divider=500,no.levels=10){
	
	#Colours used in plots
	COL1 = brewer.pal(9, "BuGn")[1]
	COL2 = brewer.pal(9, "BuGn")[9]
	Col = colorRampPalette(c(COL1,COL2))(no.levels)
	
	min.x.value<-min(points[,1],na.rm=T); max.x.value<-max(points[,1],na.rm=T)
	min.y.value<-min(points[,2],na.rm=T); max.y.value<-max(points[,2],na.rm=T)

	#par(mfrow=c(1,1))
	#plot(0,type="n",
  #     xlim=c(min.x.value,max.x.value),ylim=c(min.y.value,max.y.value),
  #     xlab="",ylab=""
	)
	
	shape<-points.to.object(points)
	if(is.empty(shape)){print("Error: Shape is empty at start of plot"); break;}
    #plot(shape,col=Col[1],border=Col[no.levels],main="",add=T) # Adds to plot
	
	# total number of points and size
	total.counts<-dim(points)[1]
	total.area<-poly.areas(owin2SP(shape))
	
	orig.size.points<-size.and.points(shape,points)
	new.results<-change.in.numbers(total.area, total.counts, orig.size.points)
	results<-matrix(ncol=2,nrow=1,c(100,100)); colnames(results)<-c("%age.area","%age.points")
	results<-rbind(results,new.results)
	
	indent<-max(floor(min(range(as.rectangle(shape)$x),range(as.rectangle(shape)$y))/index.divider),1)
	print(paste("indent =",indent))
	for(i in 1:(no.levels-1)){	
		#print(paste(i, "/",(no.levels-1)))
	  indent.value<-indent*i
      result.new<-try(
      				update.plot(shape=shape, points=points,
      				 indent.value=indent.value, results=results ,
      				 total.counts=total.counts,total.area=total.area,
      				 Col=Col[i+1])
      				,silent=TRUE)
      if(is.list(result.new)){results<-result.new[[1]];shape<-result.new[[2]]} else {break;}
  }
  return(results)
}

