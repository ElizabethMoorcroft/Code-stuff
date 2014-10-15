## Running the langrock code with analysis

# Source code
setwd(CodeDir); source("Langrock individ HMM Code Changed.R")
setwd(CodeDir); source("Langrock run functions.R")
setwd(CodeDir); source("Snow Leopard HMM form2.R")

# Select data
Start2month=0
noofmonths=6
x<-Months(Data,Start2month,Noofmonths=noofmonths)
AnID<-sort(unique(Data$AnimID))

# Runs the individuals through Langrock algorithm to get set of parameters
LangrockParamters<-LangrockIndividuals(x,AnID)

#### Anlaysis starts ####

par(mfrow=c(2,6))
for(i in 1:13){
  print(i)
  if(!is.null(listofindivid[[i]])){
    listofindivid[[i]][[1]]-> hmm
    listofindivid[[i]][[2]]-> hsmm
    meanweibull1<-gamma(1+(1/hmm$a[1]))*hmm$b[1]
    meanweibull2<-gamma(1+(1/hmm$a[2]))*hmm$b[2]
    if(i==9){print(paste("meanweibull1",meanweibull1,"meanweibull2",meanweibull2))}
    if(meanweibull1<meanweibull2 && meanweibull2!=Inf){order<-c(1,2)}else{order<-c(2,1)}
    state1<-hmm$Gamma[2,1]/(1-hmm$Gamma[1,1]+hmm$Gamma[2,1])
    state2<-1-state1
    plot(density(
        log(rweibull(10000,shape=hmm$a[order[1]],scale=hmm$b[order[1]])*1000)
            
            ,adjust=5,from=0.1)
         ,type="l",col="black",lwd=2,lty=2,xlim=c(0,10),ylim=c(0,2),
         main=AnID[i])
    points(density(
        log(rweibull(10000,shape=hmm$a[order[2]],scale=hmm$b[order[2]])*1000)
            ,adjust=5,from=0.1)
           ,type="l",col="black",lwd=2,lty=1)
    meanweibull1<-gamma(1+(1/hsmm$a[1]))*hsmm$b[1]
    meanweibull2<-gamma(1+(1/hsmm$a[2]))*hsmm$b[2]
    if(i==9){print(paste("meanweibull1",meanweibull1,"meanweibull2",meanweibull2))}
    if(meanweibull1<meanweibull2 && meanweibull2!=Inf){order<-c(1,2)}else{order<-c(2,1)}    
    points(density(
          log(rweibull(10000,shape=hsmm$a[order[1]],scale=hsmm$b[order[1]])*1000)
              ,adjust=5,from=0.1)
           ,type="l",col="red",lwd=2,lty=2)
    points(density(
          log(rweibull(10000,shape=hsmm$a[order[2]],scale=hsmm$b[order[2]])*1000)
              ,adjust=5,from=0.1)
           ,type="l",col="red",lwd=2,lty=1)
  }
}

setwd(SaveDir)
pdf(paste("movement2monthstart",Start2month,".pdf",sep=""))
par(mfrow=c(3,4))
for(i in 1:13){
  print(i)
  if(!is.null(listofindivid[[i]])){hmm
    listofindivid[[i]][[1]]-> hmm
    listofindivid[[i]][[2]]-> hsmm
    meanweibull1<-gamma(1+(1/hmm$a[1]))*hmm$b[1]
    meanweibull2<-gamma(1+(1/hmm$a[2]))*hmm$b[2]
    if(i==9){print(paste("meanweibull1",meanweibull1,"meanweibull2",meanweibull2))}
    if(meanweibull1<meanweibull2 && meanweibull2!=Inf){order<-c(1,2)}else{order<-c(2,1)}
    state1<-hmm$Gamma[2,1]/(1-hmm$Gamma[1,1]+hmm$Gamma[2,1])
    state2<-1-state1
    if(hmm$b[order[2]]<100){
      movement<-rweibull(100000*state1,shape=hmm$a[order[1]],scale=hmm$b[order[1]])
      movement<-c(rweibull(100000*state2,shape=hmm$a[order[2]],scale=hmm$b[order[2]]),movement)
    } else{
      movement<-rweibull(100000*state1,shape=hmm$a[order[1]],scale=hmm$b[order[1]])
      movement<-c(rweibull(100000*state2,shape=1,scale=10),movement)
    }
    
      plot(density(
        log(movement*1000)            
            ,adjust=0.5,from=0.1)
         ,type="l",col="black",lty=2,xlim=c(0,10),ylim=c(0,0.5),
         main=AnID[i],lwd=2,
         sub=paste("N =",sum(!is.na(x[,i*2-1]))),
          xlab="log displacement")
                                   
    points(density(log(x[,i*2-1]*1000),na.rm=T),col=2,type="l",lwd=2)
    legend(legend=c("real","modelled"),x=0,y=0.5,col=c(2,1),lty=c(1,2),cex=0.8)
  }
}
dev.off()

pdf("InidividalModelledDisaplcement.pdf")
plot(0,0,type="n",xlab="displacement (km)",ylab="Density",xlim=c(0,10),ylim=c(0,1))
for(i in 1:(dim(x)[2]/2)){
  values<-listofindivid[[i]]
  if(!is.null(values[1])){
    points(density(rweibull(10000,values$a[1],values$b[1]),adjust=5,from=0.1),type="l",col=COL[i],lwd=2,lty=2)
    points(density(rweibull(10000,values$a[2],values$b[2]),adjust=5,from=0.1),type="l",col=COL[i],lwd=2,lty=1)
  }
}
legend(x=2,y=1,col=COL,lty=1,lwd=2,legend=sort(unique(Data$AnimID)),cex=0.8)
dev.off()

pdf("InidividalModelledAngle.pdf")
plot(0,0,type="n",xlab="Turn angle (radians)",ylab="density",xlim=c(-pi,pi),ylim=c(0,1))
for(i in 1:(dim(x)[2]/2)){
  values<-listofindivid[[i]]
   if(!is.null(values)){
     TEMP<-rwrpcauchy(100000,values$co[1],rho=values$kappa[1])
    for(j in 1:100000){if(TEMP[j]>pi){TEMP[j]<-TEMP[j]-2*pi} }
    points(density(TEMP,adjust=1,from=-pi+0.3,to=pi-0.3),type="l",col=COL[i],lwd=2,lty=2)
    TEMP<-rwrpcauchy(100000,values$co[2],rho=values$kappa[2])
    for(j in 1:100000){if(TEMP[j]>pi){TEMP[j]<-TEMP[j]-2*pi} }
    points(density(TEMP,adjust=1,from=-pi+0.3,to=pi-0.3),type="l",col=COL[i],lwd=2,lty=1)
   }
}
legend(x=2,y=1,col=COL,lty=1,lwd=2,legend=sort(unique(Data$AnimID)),cex=0.8)

dev.off()