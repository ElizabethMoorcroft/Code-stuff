## PLots for langrock individuals

PlotAll<-function(listofindivid){
  for(i in 1:length(listofindivid)){
    dev.new()
    print(i)
    if(length(listofindivid[[i]])>2){
      listofindivid[[i]][[1]]-> hmm
      listofindivid[[i]][[2]]-> hsmm
      listofindivid[[i]][[4]][,1]-> actualdis
      alldisplacement(hmm)-> modeldishmm
      alldisplacement(hsmm)-> modeldishsmm
      
      plot(density(log(actualdis*1000),na.rm=T),main=listofindivid[[i]][[3]],ylim=c(0,0.4),xlim=c(0,10),xlab="",lwd=2)
      points(density(log(modeldishmm*1000)),type="l",col="red",lwd=2)
      points(density(log(modeldishsmm*1000)),type="l",col="green",lwd=2)
      legend(0,0.4, legend=c("Data","HMM","HSMM"),cex=0.6,col=c("black","red","green"),lty=c(1,1,1))
      text(5,0.4, paste("Number points:",sum(!is.na(results[[i]][[4]][,1])) ))
    }
    else{ # Puts in a blank plot if langrock did not converge
      plot(0,0,
           ylim=c(0,0.4),xlim=c(0,10),
           type="n",
           main=listofindivid[[i]][[1]],
           sub=paste("Not converged; N = ",listofindivid[[i]][[2]],sep=""))
    text(5,0.4, paste("Number points:",sum(!is.na(results[[i]][[4]][,1])) ))

    }
  }
}


PlotAllangle<-function(listofindivid){
  par(mfrow=c(11,5))
  for(i in 1:length(listofindivid)){
    print(i)
    if(length(listofindivid[[i]])>2){
      listofindivid[[i]][[1]]-> hmm
      listofindivid[[i]][[2]]-> hsmm
      listofindivid[[i]][[4]][,2]-> actualang
      allangle(hmm)-> modelanghmm
      allangle(hsmm)-> modelanghsmm
      
      PlotCircularGraphs(actualang,1,"black",F,4)
      PlotCircularGraphs(modelanghmm,1,"red",T,4)
      PlotCircularGraphs(modelanghsmm,1,"green",T,4)
      box()
      
      #legend(0,0.4, legend=c("Data","HMM","HSMM"),cex=0.6,col=c("black","red","green"),lty=c(1,1,1))
    }
    else{ # Puts in a blank plot if langrock did not converge
      plot(0,0,
           ylim=c(0,0.4),xlim=c(0,10),
           type="n",
           main=listofindivid[[i]][[1]],
           sub=paste("Not converged; N = ",listofindivid[[i]][[2]],sep=""))
    }
  }
}


displacement<-function(length,a,b){rweibull(length,shape=a,scale=b)}
turnangle<-function(length, co,kappa){rwrpcauchy(length,location=co,rho=kappa)}
#timestate1<-function(Gamma){state1<-Gamma[2,1]/(1-Gamma[1,1]+Gamma[2,1])}

alldisplacement<-function(model){
  dis<-displacement(length=round(model$delta[1]*10000),a=model$a[1],b=model$b[1])
  dis<-c(dis,displacement(length=round(model$delta[2]*10000),a=model$a[2],b=model$b[2]))
  return(dis);
  }

allangle<-function(model){
  ang<-turnangle(length=round(model$delta[1]*10000),co=model$co[1],kappa=model$kappa[1])
  ang<-c(ang,turnangle(length=round(model$delta[2]*10000),co=model$co[2],kappa=model$kappa[2]))
  ang[ang>pi]<-ang[ang>pi]-2*pi
  return(ang);
  }


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