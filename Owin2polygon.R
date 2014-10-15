
library(spatstat)
#temp<-unique(Data[,which(names(Data) %in% c("UTM_X_Zone","UTM_Y_Zone"))])

#temp<-as.data.frame(matrix(ncol=2,nrow=10000,runif(20000)))
#colnames(temp)<-c("UTM_X_Zone","UTM_Y_Zone")
#l<-dim(temp)[1]
#temp<-temp[sample(1:l, l, replace=F),]
#temp<-temp*1000
#ahull.obj <- ahull(x=temp$UTM_X_Zone,y=temp$UTM_Y_Zone, alpha = 1500)
    # creates an alpha hull object

#ahull.obj$ashape.obj
#test<-ah2sp(ahull.obj$ashape.obj)
#test<-ashape_to_SPLDF(ahull.obj$ashape.obj)
#test<-ahull_to_SPLDF(ahull.obj)
#erosion(test,0.2)

ashape_to_SPLDF <- function(x, proj4string=NA)
  {
	if(class(x) != 'ashape')
		stop('this function only works with `ashape` class objects')
	
	# convert ashape edges to DF
	x.as.df <- as.data.frame(x$edges)
	
	# convert each edge to a line segment
	l.list <- list()
	for(i in 1:nrow(x.as.df))
		{
		# extract line start and end points as 1x2 matrices
		p1 <- cbind(x.as.df$x1[i], x.as.df$y1[i])
		p2 <- cbind(x.as.df$x2[i], x.as.df$y2[i])
		# row-bind into 2x3 matrix
		l.list[[i]] <- Line(rbind(p1, p2))
		}
		print(nrow(x.as.df))
	# promote to Lines class, then to SpatialLines class
	l <- Lines(l.list,ID="a")
	print(l)
	# copy over CRS data from original point data
	l.spl <- SpatialLines(list(l))
	print("finshed l.spl")
	# promote to SpatialLinesDataFrame, required for export to GRASS / OGR
	l.spldf <- SpatialLinesDataFrame(l.spl, data=data.frame(id=1), match.ID=FALSE)
	
	return(l.spldf)
	}


##https://stat.ethz.ch/pipermail/r-sig-geo/2009-May/005781.html
owin2Polygons <- function(x, id="1") {
  #print(" Enter owin2poly")
  stopifnot(is.owin(x))
  x <- as.polygonal(x)
 # print(x)
  closering <- function(df) { df[c(seq(nrow(df)), 1), ] }
  #print(closering)
  #print(" finish closering")
  pieces <- lapply(x$bdry,
                   function(p) {
                     Polygon(coords=closering(cbind(p$x,p$y)),
                             hole=is.hole.xypolygon(p))  })
  #print(" finish pieces")
  #print(pieces)
  z <- Polygons(pieces, id)
  #print(" finish z")
  return(z)
}

tess2SP <- function(x) {
  stopifnot(is.tess(x))
  y <- tiles(x)
  nam <- names(y)
  z <- list()
  for(i in seq(y))
    z[[i]] <- owin2Polygons(y[[i]], nam[i])
  return(SpatialPolygons(z))
}

owin2SP <- function(x) {
 # print("Enter owin2sp")
  stopifnot(is.owin(x))
  y <- owin2Polygons(x)
  #print("owin2polycal")
  z <- SpatialPolygons(list(y))
  #print("spatpolcal")
  return(z)
}