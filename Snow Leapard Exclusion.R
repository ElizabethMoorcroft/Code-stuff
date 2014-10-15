exculsion<-function(Data,exclusion){
  l<-dim(exclusion)[1]
  print(l)
    m<-SummaryData(Data)
  
  for(i in 1:l){
    print(paste(i,"/",l))
    if(exclusion[i,3]=="equal"){
      Data<-equal(Data,exclusion[i,1],exclusion[i,2])
    } else 
    if(exclusion[i,3]=="GreaterThan"){
      Data<-GreaterThan(Data,exclusion[i,1],exclusion[i,2])
    } else
    if(exclusion[i,3]=="LessThan"){
      Data<-LessThan(Data,exclusion[i,1],exclusion[i,2])
    } else {print("Error")}
    
    print(dim(Data)[1])
    m<-rbind(m,SummaryData(Data))
    
  }
  
  write.csv(m,"Exclusion.csv")
  return(Data )
}

equal<-function(Data,exclusionvar,exclusionval){
  colnumber<-which(names(Data) %in% exclusionvar)
  Data<-Data[which(Data[,colnumber] == exclusionval),]
  print(length(Data)[1])

  return(Data )
}

GreaterThan<-function(Data,exclusionvar,exclusionval){
  colnumber<-which(names(Data) %in% exclusionvar)
  Data<-Data[which(Data[,colnumber] > as.numeric(exclusionval)),]
  return(Data )

}

LessThan<-function(Data,exclusionvar,exclusionval){
  colnumber<-which(names(Data) %in% exclusionvar)
  Data<-Data[which(Data[,colnumber] < as.numeric(exclusionval)),]
  return(Data )
}