CalcalateArcs<-function(x,y,radi,angle){
    xvals<-c(x+radi*sin(angle))
    yvals<-c(y+radi*cos(angle))
    return(list(x=xvals,y=yvals))
}
duplicatescal<-function(y){
  #dups<-which(duplicated(y)|duplicated(y)[length(y):1])
  #return(dups)
  numbers<-which(table(y)>1)
  #print(numbers)
  if(length(numbers)>0){
      names<-as.numeric(names(table(y))[numbers])
      #print(names)
      index<-which(y %in% names)
      }
  else{index=NA}
  return(index)
}
switchduplicates<-function(data){
  #Are there any duplicates - records the index as dups
  dups<-duplicatescal(sapply(data,function(x) x[[4]]))
  #print(dups)
  # records the number that has been switched
  switched<-c(sapply(data,function(x) x[[4]])[dups[1]])
  
  while(length(dups)>=1 & !is.na(dups[1])){
       
      index<-dups[length(dups)]
     # print(paste("number index",index))
      end1<-data[[index]][[3]];end2<-data[[index]][[4]]
      data[[index]][[3]]<-end2;data[[index]][[4]]<-end1;
      data[[index]][[2]]<-rev(data[[index]][[2]]);data[[index]][[1]]<-rev(data[[index]][[1]])
      #print(paste("values post switch",data[[index]][[3]],data[[index]][[4]]))

      # check for more duplicates
      ends<-sapply(data,function(x) x[[4]])
      dups<-duplicatescal(ends)
      #print("dups",length(dups)/2)
      #print("switched");print(switched)
      if(length(dups)>0 & !is.na(dups[1])){
        ends<-sapply(data,function(x) x[[4]])[dups]
        delete<-which(dups %in% switched)
       # print("ends");print(ends)
       # print("delete");print(delete)
        if(length(delete)>0){dups<-dups[-delete][1]}
        #print("dups");print(dups)
        switched<-c(switched,dups[1])
      }
     # print("end")
  }
  if(any(table(sapply(data,function(x) x[[4]]))==2) | any(table(sapply(data,function(x) x[[3]]))==2)){"Print ERROR is duplicates"}
  return(data)
}
createorder<-function(listofoutput){
  order<-c()
  NoCirc<-dim(alphahull$arcs)[1]
  # Looks at the order of the output list
  for(j in 1:NoCirc){
    current<-sapply(listofoutput,function(x) x[[4]])[j]
    temporder<-which(sapply(listofoutput,function(x) x[[3]]) %in% sapply(listofoutput,function(x) x[[4]])[j])
    #threetofour<-which(sapply(listofoutput,function(x) x[[3]]) %in% current)
    #fourtofour<-which(sapply(listofoutput,function(x) x[[4]]) %in% current)
    #if(!is.na(threetofour) & !is.na(fourtofour)){
    #  if(threetofour==fourtofour){temporder<-threetofour}
    #  else if(threetofour==current){temporder<-fourtofour}
    #  else{temporder<-threetofour}
    #}
    order<-c(order,temporder)
    print(paste(j,temporder))
  }
  return(order)
}
Findingloops<- function(order,availablenumbers){
    #Inital vals 
    start<-availablenumbers[1]
    nextnumber<-order[availablenumbers[1]]
    current<-c(start,nextnumber)
    delete<-c(which(availablenumbers==nextnumber), which(availablenumbers==start))
    availablenumbers<-availablenumbers[-delete]
    #print(availablenumbers)
    #print(paste("nextnumber",nextnumber,"start",start))
    while(nextnumber!=start & length(availablenumbers)>0){ 
      nextnumber<-order[nextnumber]
      current<-c(current,nextnumber)
      delete<-which(availablenumbers==nextnumber)
      if(length(delete)>0){availablenumbers<-availablenumbers[-delete]}
      #print(length(availablenumbers))
      #print(paste("nextnumber",nextnumber))
    }
    #print(paste("Length an (FL)",length(availablenumbers)))
    return(list(availablenumbers,current))
}
numberingshape<-function(order,listofoutput){
    availablenumbers<-order
    count<-1
    shapes<-vector(mode="list")
    #print(length(order))
    while(length(availablenumbers)>0){
      temp<-Findingloops(order,availablenumbers)
      availablenumbers<-temp[[1]]
      current<-unique(temp[[2]])
      tempshape<-vector(mode="list",length=length(current))
      index<-which(!is.na(current))
      for(i in index){
        listofoutput[[current[i]]][[5]]=count
        tempshape[[i]]<-listofoutput[[current[i]]]
      }
      shapes[[count]]<-tempshape
      count = count+1
    }

    return(shapes)
}
distancebetween<-function(x1,x2,y1,y2){
  delta<-sqrt((x1-x2)^2+(y1-y2)^2)
  return(delta)
}
outlineshape<-function(arcsinshape){
    xvals<-arcsinshape[[1]][[1]]
    yvals<-arcsinshape[[1]][[2]]
    if(length(arcsinshape)>1){
    for(i in 2:length(arcsinshape)){  
      xs<-arcsinshape[[i]][[1]]; ys<-arcsinshape[[i]][[2]]
      if(length(xs)>0){
        firstx<-xvals[1]; firsty<-yvals[1]
        lastx<-xvals[length(xvals)]; lasty<-yvals[length(yvals)]
        
        delta1<-distancebetween(xs[1],lastx,ys[1],lasty)
        delta2<-distancebetween(rev(xs)[1],lastx,rev(ys)[1],lasty)
        delta3<-distancebetween(xs[1],firstx,ys[1],firsty)
        delta4<-distancebetween(rev(xs)[1],firstx,rev(ys)[1],firsty)
        Minimum<-min(c(delta1,delta2,delta3,delta4))
        
        if(Minimum==delta1 |Minimum==delta2 ){
            if(delta1<=delta2){
              xvals<-c(xvals,arcsinshape[[i]][[1]])
              yvals<-c(yvals,arcsinshape[[i]][[2]])
            }
            else{
              xvals<-c(xvals,rev(arcsinshape[[i]][[1]]))
              yvals<-c(yvals,rev(arcsinshape[[i]][[2]]))
            }
        }
        else{
            if(delta3<delta4){
              xvals<-c(rev(arcsinshape[[i]][[1]]),xvals)
              yvals<-c(rev(arcsinshape[[i]][[2]]),yvals)
            }
            else{
              xvals<-c(arcsinshape[[i]][[1]],xvals)
              yvals<-c(arcsinshape[[i]][[2]],yvals)
            }  
        }
      }
    }
  }
  return(list(xvals, yvals))
}
Numberofshapes<-function(listofoutput){
    print(length(listofoutput[[1]]))
    numbers<-sapply(listofoutput, function(x) x[[5]])
    Nuofshapes<-max(numbers)
    return (Nuofshapes)
}
selectshapesid<-function(count,listofoutput){
  numbers<-sapply(listofoutput, function(x) x[[5]])
  ids<-which(numbers == count)
  return(ids)
}
selectshape<-function(count,listofoutput){
    index<-selectshapesid(count,listofoutput)
    listshape<-vector(mode="list",length=length(index))
    for(indexiterator in 1:length(index)){
    #  print(paste("indexiterator ",indexiterator ))
      temp<-listofoutput[[index[indexiterator]]]
      listshape[[indexiterator]]<-temp
    }
    return(listshape)
}
createowin<-function(xvals,yvals){
    obj<-0
    if(length(xvals)>1){ 
      #print("in if")
      try(obj<-owin(poly=list(x=xvals,y=yvals)))
      if(class(obj)!="owin"){try(obj<-owin(poly=list(x=rev(xvals),y=rev(yvals))))}
    }
    else{obj<-SpatialPoints(cbind(xvals,yvals))}
    
    return(obj)
}
TooClose<-function(x,y){
  delta<-vector(length=(length(x)))
  delta[1]<-sqrt((x[length(x)]-x[1])^2+(y[length(x)]-y[1])^2)
  for(i in 2:length(x)){  
    delta[i]<-sqrt((x[i-1]-x[i])^2+(y[i-1]-y[i])^2)
    }
  #print(delta)
  diff<-min(as.numeric(names(table(delta))[table(delta)==max(table(delta))]))[1]/2
  deletethesenumbers<-which(delta<diff)
  #print(diff)
  if(length(deletethesenumbers)==(length(x))){deletethesenumbers<-NA}
  return(deletethesenumbers)
}

IDS<-c("M1","M3","M4","M8","M9","M10","F2","F3","F5","F6","F7","F8","F9")


Hulls<-vector(mode="list")
ALPHA<-2000

CalculateAlphaShape<-function(Data,ID,ALPHA){
  temp<-unique(Data[which(Data$AnimID==ID),which(names(Data) %in% c("UTM_X_Zone","UTM_Y_Zone"))])
  colnames(temp)<-c("UTM_X_Zone","UTM_Y_Zone")
  l<-dim(temp)[1]
  set.seed(1)
  temp<-temp[sample(1:l, l, replace=F),]

  alphahull <- ahull(x=temp$UTM_X_Zone,y=temp$UTM_Y_Zone, alpha = ALPHA)
  print("finsihed alpha hulls")    
  listofoutput<-vector(mode="list")
  NoCirc<-dim(alphahull$arcs)[1]
  
   for(i in 1:NoCirc){ 
      Aval<- atan2(alphahull$arcs[i,4],alphahull$arcs[i,5])
      Mxaval<-Aval+alphahull$arcs[i,6]
      Mnaval<-Aval-alphahull$arcs[i,6] 
      output<-seq(Mnaval,Mxaval,by=0.5)
      calarcs<-CalcalateArcs(x=alphahull$arcs[i,1],y=alphahull$arcs[i,2],radi=alphahull$arcs[i,3],angle=output)
      temp<-vector(mode="list")
      temp[[1]]<-calarcs$x
      temp[[2]]<-calarcs$y
      temp[[3]]<-alphahull$arcs[i,7]
      temp[[4]]<-alphahull$arcs[i,8]
      listofoutput[[i]]<-temp
    }
  
  listminusdups<-switchduplicates(listofoutput)
  order<-createorder(listminusdups)
  shapelist<-numberingshape(order,listminusdups) #return(listofoutput)  
  # Looks at the order of the output list
  numshapes<-length(shapelist)
  owinlist<-vector(mode="list")
  #break;
  for(shape in 1:numshapes){
    print(paste("shape",shape,"/",numshapes ))
    tempshapes<-shapelist[[shape]]
    shapeoutline<-outlineshape(tempshapes)
    #plot(shapeoutline[[1]],shapeoutline[[2]])
    if(length(shapeoutline[[1]])>1){
      delete<-TooClose(shapeoutline[[1]],shapeoutline[[2]])
      if(!is.na(delete[1])){
        owinshape<-createowin(shapeoutline[[1]][-delete],shapeoutline[[2]][-delete])
      }else{ 
        owinshape<-createowin(shapeoutline[[1]],shapeoutline[[2]])
      }
    }else{
       owinshape<-createowin(shapeoutline[[1]],shapeoutline[[2]])}
      owinlist[[shape]]<-owinshape
  }
  
  return (owinlist)
}

Allshapes<-vector(mode="list",length=length(IDS))
for(id in 1:length(IDS)){
# for(id in 1){
  print(paste("ID", id, "/", length(IDS)))
  temp<-CalculateAlphaShape(Data,IDS[id],ALPHA)
  print(temp)
  Allshapes[[id]]<-temp
}

for(i in 1:length(IDS)){
 for(j in 1:length(Allshapes[[i]])) {
    if(is.numeric(Allshapes[[i]][[j]])==TRUE) {
      print(paste("I:",i,"J:",j))
    }
  }
}

#pdf(paste("ErosionProcess",ALPHA,".pdf",sep=""))
  # To make sure 1 graph 
  #par(mfrow=c(1,1))
  #plot(0,type="n",xlim=c(4770965,4797759),ylim=c(615500.5,681850.8)
  #     ,xlab="",ylab="")
#  for(ShapeNumber in 1:9){
#    print(ShapeNumber)
    # Load in data
#    shape<-Allshapes[[1]][[ShapeNumber]]
#    if(is.empty(shape)){break;}
#    points<-unique(Data[which(Data$AnimID==IDS[1]),which(names(Data) %in% c("UTM_X_Zone","UTM_Y_Zone"))])
#    YelToRed = brewer.pal(9, "YlOrRd")
    # counts
#    allpoints<-dim(points)[1]
#    area<-poly.areas(owin2SP(shape))
#    examplecounts<-matrix(ncol=2,nrow=9)
#    examplecounts[1,]<-c(100,100)
#    colnames(examplecounts)<-c("%InnerArea","%Counts")
#    # Start plot
#    #plot(shape,col=YelToRed[1],border=YelToRed[9],main="",add=T)
#    plot(shape,border=YelToRed[9],main="",add=T)
#
#    #for(i in 1:8){
#    #  indent<-floor(min(max(as.rectangle(shape)$x)-min(as.rectangle(shape)$x),max(as.rectangle(shape)$y)-min(as.rectangle(shape)$y))/32)
#    #  shape<-erosion(shape,i*indent)
#    #  if(is.empty(shape)){break;}
#    #  CNTS<-poly.counts(SpatialPoints(points),owin2SP(shape))
#    ##  plot(shape,col=YelToRed[i+1],border=YelToRed[i+1],add=TRUE)
#    #  AREA<-poly.areas(owin2SP(shape))
#    #  examplecounts[i+1,]<-c(round(100*AREA/area),round(100*CNTS/allpoints))
#    #}
#  }
#  #points(points,pch=".")
#
#dev.off()

#temppnts<-round(points/1000)*1000

#pdf("TrueDensityM1.pdf")
#filled.contour(table(temppnts[,1],temppnts[,2]),
#               x=sort(unique(temppnts[,1])),
#               y=sort(unique(temppnts[,2])),
#               axes=TRUE,
#               color.palette=colorRampPalette(brewer.pal(9, "YlOrRd"),space="rgb"),
#               plot.axes={points(points,pch=".")
#                          axis(1)
#                          axis(2)}
#               )
#dev.off()
