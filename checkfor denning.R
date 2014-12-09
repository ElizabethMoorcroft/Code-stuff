blocks.with.classifcated.wanted<-function(data,classification.no,min.no.of.occurance){
  t<-table(data$block_number,data$classification.of.cluster)
  column.names<-as.numeric(colnames(t))
  row.names<-as.factor(row.names(t))
  column<-which(column.names==classification.no)
  list.block.numbers<-row.names[which(t[,column]>=min.no.of.occurance)]
  
  return(list.block.numbers)
}


select.block.of.movement<-function(block,data){
  selected.data<-data[which(data$block_number==block),]
  return(selected.data)
}

plot.of.classsifcation<-function(data,classification.no){
  data<-data[which(!is.na(data$UTM_X_Zone)),]
  plot(data$UTM_X_Zone,data$UTM_Y_Zone,type="l")
  rows<-which(data$classification.of.cluster==classification.no)
  for(i in 1:length(rows)){
    x<-data[c(rows[i]-1),]
    x1<-data[c(rows[i]),]
    arrows(x0=x$UTM_X_Zone,y0=x$UTM_Y_Zone,x1=x1$UTM_X_Zone,y1=x1$UTM_Y_Zone,col="red",code=2,length=0.1) # code 2 puts arrow head at x1
  }
}

plot.all.classification.wanted<-function(data,classification.no,min.no.of.occurance){
  list.of.blocks<-blocks.with.classifcated.wanted(data,classification.no,min.no.of.occurance)
  l.list.of.blocks<-length(list.of.blocks)
  sq.root<-ceiling(sqrt(l.list.of.blocks))
  par(mfrow=c(sq.root,sq.root))
  for(i in list.of.blocks){
    temp.data<-select.block.of.movement(block=i,data)
    plot.of.classsifcation(temp.data,classification.no)
  }
}


plot.of.all.classsifcation<-function(data){
  data<-data[which(!is.na(data$UTM_X_Zone)),]
  plot(data$UTM_X_Zone,data$UTM_Y_Zone,type="n")
  COL<-c(brewer.pal(n=9, name="Set1")[1:8],"black",brewer.pal(n=12, name="Set3")[9])
  for(i in 2:dim(data)[1]){
    x<-data[i-1,]
    x1<-data[i,]
    class<-data[i,]$classification.of.cluster
    if(is.na(class)){class=10}
    print(class)
    arrows(x0=x$UTM_X_Zone,y0=x$UTM_Y_Zone,x1=x1$UTM_X_Zone,y1=x1$UTM_Y_Zone,col=COL[class],code=2,length=0.01) # code 2 puts arrow head at x1
  }
}

plot.all.classification<-function(data,classification.no,min.no.of.occurance){
  list.of.blocks<-blocks.with.classifcated.wanted(data,classification.no,min.no.of.occurance)
  l.list.of.blocks<-length(list.of.blocks)
  sq.root<-ceiling(sqrt(l.list.of.blocks))
  par(mfrow=c(sq.root,sq.root))
  for(i in list.of.blocks){
    temp.data<-select.block.of.movement(block=i,data)
    plot.of.all.classsifcation(temp.data)
  }
}




identifing.classification.of.interest<-function(cluster,i,theta.min,theta.max,displ.min,displ.max){
  classification.no<-which(cluster$mean[1,]>displ.min & cluster$mean[1,]<displ.max & 
                            cluster$mean[2,]>theta.min & cluster$mean[2,]<theta.max)
  if(length(classification.no)>1){print(paste("error too many classifcations in cluster"),i);quit()}
  
  return(classification.no)
}


testing.for.cluster<-function(cluster.list,data.list,theta.min=2.5,theta.max=pi,displ.min=1500,displ.max=3000,min.no.of.occurance=5){
  l.cluster.list<-length(cluster.list)
  for(i in 1:l.cluster.list){
    class.number<-identifing.classification.of.interest(cluster.list[[i]],i,theta.min,theta.max,displ.min,displ.max)
    if(length(class.number)==0){print(paste("There is no cluster in list number",i))}else{
    pdf(paste("MovementForClassification",i,".pdf",sep=""))
    plot.all.classification.wanted(data.list[[i]],classification.no=class.number,min.no.of.occurance)
    dev.off()
    }
  }
}

testing.all.cluster<-function(cluster.list,data.list,theta.min=2.5,theta.max=pi,displ.min=1500,displ.max=3000,min.no.of.occurance=5){
  l.cluster.list<-length(cluster.list)
  for(i in 1:l.cluster.list){
    class.number<-identifing.classification.of.interest(cluster.list[[i]],i,theta.min,theta.max,displ.min,displ.max)
    if(length(class.number)==0){print(paste("There is no cluster in list number",i))}else{
      pdf(paste("MovementForClassification",i,".pdf",sep=""))
      plot.all.classification(data.list[[i]],classification.no=class.number,min.no.of.occurance)
      dev.off()
    }
  }
}

testing.for.cluster(cluster.list=class.summer.25[[1]],data.list=class.summer.25[[3]])
  


