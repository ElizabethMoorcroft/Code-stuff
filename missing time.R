source('~/Documents/Snow Leopards/Code/Snow Leopard HMM form2.R')

#test<-runif(10000,0,1439)
#plot(density(test,from=0,to=1439),type="l",col="green",ylim=c(0,1*10^-3))
test<-as.POSIXlt(Data[unlist(m[[3]]),]$Date_Time)
test<-test$hour*60+test$min
pdf("missing_times.pdf")
  plot(density(test,from=0,to=1439),type="l",col="green",ylim=c(0,1*10^-3))
  m<-Months(Data,min=0,Noofmonths=12,no.of.miss.occasions=432,minumum.no.locations=2)
  t<-unlist(lapply(m[[4]],function(y){lapply(y,function(x){as.POSIXlt(x)$hour*60+as.POSIXlt(x)$min})}))
  points(density(t,from=0,to=1439),type="l",col="blue")
  ks.p.value<-ks.test(t,test)$p.value
  print(paste(i,"th p.value",ks.p.value))
  legend(x=0,y=4*10^-4,col=c("green","blue"),lty=c(1,1),legend=c("missing","recorded"))
dev.off()

for(i in 0:11){
  m<-Months(Data,min=i,Noofmonths=3,no.of.miss.occasions=432,minumum.no.locations=2)
  t<-unlist(lapply(m[[4]],function(y){lapply(y,function(x){as.POSIXlt(x)$hour*60+as.POSIXlt(x)$min})}))
  points(density(t,from=0,to=1439),type="l")
  ks.p.value<-ks.test(t,test)$p.value
  print(paste(i,"th p.value",ks.p.value))
}

pdf("Missing_Locations.pdf")
l<-length(unlist(m[[5]]))
plot(Data$UTM_X_Zone,Data$UTM_Y_Zone)
points(x=unlist(m[[5]])[1:(l/2)*2-1]
       ,y=unlist(m[[5]])[(1:(l/2))*2]
       ,col="green")
dev.off()

