source("Snow Leopard HMM form2.R")
source("Langrock individ HMM Code Changed run.R")
source("Langrock individ HMM Code Changed real data 28Aug.R")
source("simulated movement.R")




split<-Months(data=Data,min=4,Noofmonths=1,no.of.miss.occasions=25,minumum.no.locations=40)
list.animals<-unlist(lapply((split[[2]]),function(x){x[[1]]}))
animal.in.split<-unique(list.animals)
internally.consistant<-rep(NA,length(animal.in.split))
par(mfrow=c(3,3))
for(i in 1:length(animal.in.split)){
  columns<-which(list.animals==animal.in.split[i])
  no.tests<-choose(length(columns),2)
  if(sum(list.animals==animal.in.split[i])>=2){
    sig.diff<-rep(NA,no.tests)
    print(animal.in.split[i])
    plot(0,type="n",xlim=c(0,5),ylim=c(0,1),main=animal.in.split[i])
    counter<-0
    for(k in 1:sum(list.animals==animal.in.split[i])){
      points(density(split[[1]][,c(columns[k])*2-1],na.rm=T),col=k,type="l")
      if(k>=2){
        for(j in (k-1):1){
          counter<-counter+1
          pvalue<-ks.test(split[[1]][,c(columns[k])*2-1],split[[1]][,c(columns[j])*2-1])$p.value
          pvalue.angle<-ks.test(split[[1]][,c(columns[k])*2],split[[1]][,c(columns[j])*2])$p.value
          if(pvalue<(0.05/no.tests) | pvalue.angle<(0.05/no.tests)){
            sig.diff[counter]<-"sig.diff"
            #print(paste("distance",pvalue,"angle",pvalue.angle))
          } else{sig.diff[counter]<-"not.sig.diff"}
        }
      }
    }
    print(sig.diff)
    if(any(sig.diff=="sig.diff",na.rm=T)){internally.consistant[i]<-NA}else{internally.consistant[i]<-animal.in.split[i]}
  }
}

internally.consistant <- internally.consistant[!is.na(internally.consistant)]
parameters<-list(
 "Shape"=c(3,5),
  "Scale"=c(3,0.5),
  "Rho"=c(0.2,0.2),
  "Mu"=c(0.1,3),
  "State"=c(0.7,0.7),
  "StateHSMM"=c(1,1,0.5,0.8),
  "stepm"=35,
  "m"=c(30,30)
)

results.list.hmm<-list(); results.list.hsmm<-list()
simulated.movement<-as.data.frame(matrix(ncol=8,nrow=0))
names(simulated.movement)<-c("UTM_X_Zone","UTM_Y_Zone","Date_Time","Dist_Difference","ChangeAngle","True_State","AnimID","Sex")

for(i in 1:length(internally.consistant)){
  block.no<-which(list.animals==internally.consistant[i])
  columns<-sort(c(block.no*2,block.no*2-1))
  movement.matrix<-split[[1]][,columns]
  
  
  r<-run.all.individuals(movement.matrix,no.of.5hr.blocks=length(block.no),parameters)
  hmm<-list(r$HMM);hsmm<-list(r$HSMM)
  results.list.hmm<-c(results.list.hmm,hmm)
  results.list.hsmm<-c(results.list.hsmm,hsmm)
  
  codes<-unlist(lapply(hmm ,function(y){unlist(lapply(y,function(x){if(is.null(x)){NA} else{x$code}}))}))
  
  set.seed(1)
  hmm.id<-sample(which(codes==1),1)
  print(hmm.id)
  Sex<-unique(unlist(lapply(split[[2]][block.no],function(x){x[2]})))
  AnimID<-unique(unlist(lapply(split[[2]][block.no],function(x){x[1]})))
  
  parameters.new<-list(
    "SHAPE"=hmm[[1]][[hmm.id]]$a,
    "SCALE"=hmm[[1]][[hmm.id]]$b,
    "RHO"=hmm[[1]][[hmm.id]]$kappa,
    "MU"=hmm[[1]][[hmm.id]]$co,
    "STATE"=c(hmm[[1]][[hmm.id]]$Gamma[1,1],hmm[[1]][[hmm.id]]$Gamma[2,2]),
    "STATEHSMM"=c(1,1,0.5,0.8)
  )
  
  move<-path.of.animal(number.of.steps=24*(3*30)/5,seed = 1,parameters=parameters.new)
  new.move<-cbind(move,AnimID,Sex)
  simulated.movement<-rbind(simulated.movement,new.move)
}

par(mfrow=c(3,2))
for(i in internally.consistant){
  
  dist.current<-as.numeric(as.character(simulated.movement[which(simulated.movement$AnimID==i),]$Dist_Difference))
  plot(density(dist.current,na.rm=T,from=0,to=15),ylim=c(0,0.5))
  columns<-which(list.animals==i)
  print(paste(i," significant p-value <",0.05/length(columns)))
  for(k in 1:length(columns)){
    points(density(split[[1]][,c(columns[k])*2-1],na.rm=T,from=0,to=15),type="l",col="red")
    pvalue<-ks.test(split[[1]][,c(columns[k])*2-1],dist.current)$p.value
    print(pvalue); if(pvalue<(0.05/length(columns))){print("*")}
  }
}

par(mfrow=c(3,2))
for(i in internally.consistant){
  
  dist.current<-as.numeric(as.character(simulated.movement[which(simulated.movement$AnimID==i),]$Dist_Difference))
  plot(density(log(dist.current*1000),na.rm=T,from=0,to=10),ylim=c(0,0.5))
  columns<-which(list.animals==i)
  print(paste(i," significant p-value <",0.05/length(columns)))
  for(k in 1:length(columns)){
    real.data<-split[[1]][,c(columns[k])*2-1]
    points(density(log(real.data*1000),na.rm=T,from=0,to=10),type="l",col="red")
    pvalue<-ks.test(real.data,dist.current)$p.value
    print(pvalue); if(pvalue<(0.05/length(columns))){print("*")}
  }
}


points(density(c(rweibull(700,shape=5,scale=3),rweibull(700,shape=2,scale=0.5)),from=0,to=15),type="l",col="green")


Validate<-function(data,min,Noofmonths,no.of.miss.occasions=1,minumum.no.locations=25){
  
}