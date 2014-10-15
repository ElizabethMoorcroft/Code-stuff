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
      plot(shape,col=Col,border=Col,add=TRUE)
	  
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

	par(mfrow=c(1,1))
	plot(0,type="n",
       xlim=c(min.x.value,max.x.value),ylim=c(min.y.value,max.y.value),
       xlab="",ylab=""
	)
	
	shape<-points.to.object(points)
	if(is.empty(shape)){print("Error: Shape is empty at start of plot"); break;}
    plot(shape,col=Col[1],border=Col[no.levels],main="",add=T) # Adds to plot
	
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
##################



# Values for all point to 100% 
R<- plot.erosion.of.convex.hull(points, index.divider=54000,no.levels=100)

# Plots the % of area against the percentage of the area remaining in the centre
# To show whether there are large amounts of points near the edge of the convex hull or at the centre
plot(0,xlim=c(0,100),ylim=c(0,100),type="n")
points(R[,1],type="l",col=1); points(R[,2],type="l",col=2)
legend(10,10,c("%age of area","% of points"),col=c(1,2),lty=c(1,1))
plot(x=R[,1],y=R[,2],xlab="Centre %age of area",ylab="%age of points in area",type="l")


#pdf(paste("ErosionProcess","Contour",".pdf",sep=""))
#dev.off()

# Runs through each item on list of chain of locations
results.list<-list()
for(i in 1:length(row.numbers)){
	points<-Data[row.numbers[[i]],which(colnames(Data)=="UTM_X_Zone" |colnames(Data)=="UTM_Y_Zone")]
	results.list[[i]]<-plot.erosion.of.convex.hull(points, index.divider=1000000,no.levels=10000)
}

####
# Simulates values from the expected movement where:
# 	prob.capture = g*exp(-d^2/(2*sigma^2))
# To see whether the real movement follows the pattern 
# which Efford/Broaders lay out as a method of animal movement 
# for SECR

# Sets seed and creates a random stream of numbers
max.distance=1000; number.sample.at.dist=100
total.sample = max.distance*number.sample.at.dist
set.seed(1); random.stream<-sample(size=total.sample,x=total.sample*10^9,replace=FALSE)

number.on.stream<-0
sim.values<-matrix(ncol=2,nrow=0)

for(distance in 1:max.distance){
	for(j in 1:number.sample.at.dist){
		number.on.stream<-number.on.stream+1
		set.seed(random.stream[number.on.stream])
		# Creates a second stream of random numbers
		random.stream.two<-sample(size=51,x=51*10^9,replace=FALSE)
		# random decides of capture given probability of capture = g*exp(-distance^2/(2*sigma^2))
		set.seed(random.stream.two[01]); pass<-rbinom(1,1,1*exp(-distance^2/(2*100^2)))
		if(pass==1){
			# Given that an event would have been captured and that 
			# the total distance from the centre is: distance
			# Calculate an x,y position and add to results 
			set.seed(random.stream.two[11]); x<-runif(1,min=0,max=distance); 
			set.seed(random.stream.two[21]); x<-x*(rbinom(1,1,0.5)*2-1)
			set.seed(random.stream.two[31]); y<-sqrt((distance^2)-(x^2)); 
			set.seed(random.stream.two[41]); y<-y*(rbinom(1,1,0.5)*2-1)
			set.seed(random.stream.two[51]); order<-rbinom(1,1,0.5);
			if(order==1){vector<-c(x,y)}else{vector<-c(y,x)}
			random.values<-matrix(ncol=2,vector)
			sim.values<-rbind(sim.values,random.values)
		}
	}
}

# Run simulated values through the convex hull program and plot the points 
Sim<-plot.erosion.of.convex.hull(sim.values, index.divider=10000,no.levels=50)
points(sim.values)

###
# Plots the % of the area v.s. % of the points, for:
# 	- all the data
#	- parity 
#	- simulated SECR movement
plot(0,xlim=c(0,100),ylim=c(0,100),type="n",xlab="Centre %age of area",ylab="%age of points in area")
for(i in 1:length(row.numbers)){
	print(paste("Shape number",i))
	R<-results.list[[i]]
	points(x=R[,1],y=R[,2],type="l",lwd=2)
}
abline(a=0,b=1,col="red",lty=2,lwd=2) # plots the line of parity
points(x=Sim[,1],y=Sim[,2],type="l",col="green",lty=2,lwd=2) # plots the simulated
legend(x=0,y=100,col=c("black","red","green"),lty=c(1,2,2), legend=c("Data","Parity","SECR"),cex=0.8) 