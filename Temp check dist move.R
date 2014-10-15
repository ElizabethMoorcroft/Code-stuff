setwd(SaveDir)
par(mfrow=c(1,1))
for(i in 1:length(unique(Data$AnimID))){
  name<-paste("blocks",unique(Data$AnimID)[i],".pdf",sep="")
  pdf(name)
  plot(0,xlim=c(0,10),ylim=c(0,0.6),type="n")
  tempdata<-b[[i]]
  for(j in 1:length(tempdata)){
    dist.data<-tempdata[[j]]$block_dist.difference
    if(sum(!is.na(dist.data))>30){
      points(density(log(dist.data),na.rm=T),col="red",type="l")
    }
  }
  dev.off()
}


par(mfrow=c(3,2))

for(i in 1:length(unique(Data$AnimID))){
  loc.id.tmp<-b[[i]][which(unlist(lapply(b[[i]],length))>=30)]
  if( length(loc.id.tmp)>2){
    print(unique(Data$AnimID)[i])

  
  dist.tmp<-lapply(loc.id.tmp,function(x){Data[which(Data$LocID %in% x),]$Dist_Difference})

  plot(xlim=c(0,10),0,type="n",ylim=c(0,1))
  plot.log.density<-function(c){points(type="l",density(log(c),na.rm=T))}
  lapply(dist.tmp,plot.log.density)

  no.records<-length(loc.id.tmp)

  for(i in 1:(no.records-1)){for(j in 2:no.records){
    no.tests<-choose(no.records,2)
    #print(0.05/no.tests)
    if(i!=j){
      p.value<-ks.test(dist.tmp[i][[1]],dist.tmp[j][[1]])$p.value
      #if(p.value<0.05/no.tests){print(p.value)}
    }
  }}
}}

plot(xlim=c(0,5000),0,type="n",ylim=c(0,0.1))
plot.density<-function(c){points(type="l",density(c,na.rm=T))}
lapply(dist.tmp,plot.density)