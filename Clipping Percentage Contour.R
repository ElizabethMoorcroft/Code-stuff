library("GISTools")

#IDS<-c("M1","M3","M4","M7","M8","M9","M10","F2","F3","F5","F6","F7","F8","F9")
plot(x=0,y=0, xlim=c(0,100), ylim=c(0,1),ylab="Percentage of captures",xlab="Area used",type="n")
#abline(a=0,b=1)
listresults<-vector(mode="list")
par(mfrow=c(2,1))

#for(j in 1:length(IDS)){
for(j in 1){
  print(paste("J:",j))
  animal <- Data[which(Data$AnimID ==IDS[j]),]
  strtdate<-min(as.Date(animal[,5]))+250
  plus1mnth<-strtdate+30*1
  animal<-animal[which(as.Date(animal[,5]) <= plus1mnth),]

  temp<-unique(animal[,which(names(animal) %in% c("UTM_X_Zone","UTM_Y_Zone"))])
  chull.obj <- chull(x=temp$UTM_X_Zone,y=temp$UTM_Y_Zone)
  temp2<-temp[c(chull.obj,chull.obj[1]),1:2]
  temp2<-temp2[-dim(temp2)[1],]
  temp3<-temp2[dim(temp2)[1]:1,]
  w<-0
  try(w <- owin(poly=list(x=temp3[,1],y=temp3[,2])))
  if(is.numeric(w)){(w <- owin(poly=list(x=rev(temp3[,1]),y=rev(temp3[,2]))))}
  

  #erosion(w,0.1)
  #Shapes<-Allshapes[[j]]
  #temp<-unique(Data[which(Data$AnimID==IDS[j]),which(names(Data) %in% c("UTM_X_Zone","UTM_Y_Zone"))])
  Matrix<-matrix(ncol=2,nrow=1000,0)
  
  #Numberofshape<-which(unlist(lapply(Shapes, class))=="owin")
  Numberofshape<-1
  TOTALAREA<-0
  TOTALCNTS<-0
  for(sh in 1:length(Numberofshape)){
    #print(paste("SH:",sh))
     # w<-Shapes[[Numberofshape[sh]]]
      TOTALCNTS<-poly.counts(SpatialPoints(temp),owin2SP(w))
      TOTALAREA<-poly.areas(owin2SP(w))
      for(i in 0:999){
        #print(paste("I:",i))
        x<-erosion(w,10*i)
       # print(" Erosion cal")
        if(is.empty(x)){break;}
        xasShape<-owin2SP(x)
        #plot(add=T,x)
        #print(" CONVERT cal")
        CNTS<-poly.counts(SpatialPoints(temp),xasShape) #library(GISTools)
        #print(" CNTS cal")

        AREA<-poly.areas(xasShape)
        #print(" AREA cal")
        #print(paste(100-100*CNTS/ALL, 100-100*poly.areas(owin2SP(x))/AREA))
        Matrix[i+1,1]<-Matrix[i+1,1]+100*CNTS/TOTALCNTS
        Matrix[i+1,2]<-Matrix[i+1,2]+100*AREA/TOTALAREA 
        if(100-100*CNTS/ALL>99.9){print("In break");break;}
        #print(paste(" End I: ",i))
      }
  #points(temp[,2:1])
  secplot<-Matrix
  for(i in 1:999){
    secplot[i,1]<-Matrix[i,1]-Matrix[i+1,1]
  }
  secplot <- secplot[!duplicated(secplot),]
  #points(x=Matrix[,2],y=Matrix[,1],col=j,type="l")
  #points(x=secplot[,2],y=secplot[,1],col=j,type="l")
  #lines(smooth.spline(x=secplot[,2],y=secplot[,1], df=10), col = j)
  #plot(smooth.spline(x=secplot[,2],y=secplot[,1], df=10), col = j,type="l")
  #plot(w, border = j); box(); points(temp,col=j)
  }
  listresults[[j]]<-Matrix
}

#pdf(paste("ErosionProcess","Contour",".pdf",sep=""))
  # To make sure 1 graph 
  par(mfrow=c(1,1))
  plot(0,type="n",xlim=c(4770965,4797759),ylim=c(615500.5,681850.8)
       ,xlab="",ylab="")
    # Load in data
    shape<-w
    if(is.empty(shape)){break;}
    points<-unique(animal[,which(names(animal) %in% c("UTM_X_Zone","UTM_Y_Zone"))])
    YelToRed = brewer.pal(9, "YlOrRd")
    
    # counts
    allpoints<-dim(points)[1]
    area<-poly.areas(owin2SP(shape))
    examplecounts<-matrix(ncol=2,nrow=9)
    examplecounts[1,]<-c(100,100)
    colnames(examplecounts)<-c("%InnerArea","%Counts")
    
    # Start plot
    plot(shape,col=YelToRed[1],border=YelToRed[9],main="",add=T)

    for(i in 1:8){
      indent<-floor(min(max(as.rectangle(shape)$x)-min(as.rectangle(shape)$x),max(as.rectangle(shape)$y)-min(as.rectangle(shape)$y))/32)
      shape<-erosion(shape,i*indent)
      if(is.empty(shape)){break;}
      plot(shape,col=YelToRed[i+1],border=YelToRed[i+1],add=TRUE)
      CNTS<-poly.counts(SpatialPoints(points),owin2SP(shape))
      AREA<-poly.areas(owin2SP(shape))
      examplecounts[i+1,]<-c(round(100*AREA/area),round(100*CNTS/allpoints))
  }
  points(points,pch=".")

#dev.off()