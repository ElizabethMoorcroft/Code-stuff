
x<-100
reg<-lm(data=Data[which(Data$Dist_Difference>x),],formula=log(Dist_Difference)~Sex)
sigma<-summary(reg)$sigma
female<-exp(reg$coef[1]+rnorm(30000,mean=0,sd=sigma))
male<-exp(reg$coef[1]+reg$coef[2]+rnorm(30000,mean=0,sd=sigma))
pdf(paste("Regression fitted distance >",x,".pdf",sep=""))
plot(density(female),col=COL[4],xlim=c(0,20000),xlab="Distance",main=NA,sub="Based on Displacement>100m",lty=2)
points(density(male),col=COL[3],type="l",lty=2)
points(density(Data[which(Data$Dist_Difference>x & Data$Sex=="M"),]$Dist_Difference,na.rm=T),col=COL[3],type="l")
points(density(Data[which(Data$Dist_Difference>x & Data$Sex=="F"),]$Dist_Difference,na.rm=T),col=COL[4],type="l")
legend(x=7500,y=4*(10^-4),
       legend=c(paste("Intercept",round(reg$coef[1],3)),
                paste("Male",round(reg$coef[2],3)),
                paste("Sigma",round(summary(reg)$sigma,3)))
      )
dev.off()


pdf(paste("QQplot- Regression fitted distance >",x,".pdf",sep=""))
par(mfrow=c(2,2))
plot(reg)
dev.off()




x<-100
reg<-lm(data=Data[which(Data$Dist_Difference>s),],formula=log(Dist_Difference)~Sex)
sigma<-summary(reg)$sigma
female<-exp(reg$coef[1]+rnorm(30000,mean=0,sd=sigma))
male<-exp(reg$coef[1]+reg$coef[2]+rnorm(30000,mean=0,sd=sigma))
pdf(paste("Regression fitted distance >",x,".pdf",sep=""))
plot(density(female),col=COL[4],xlim=c(0,20000),xlab="Distance")
points(density(male),col=COL[3],type="l")
points(density(Data[which(Data$Dist_Difference>x & Data$Sex=="M"),]$Dist_Difference,na.rm=T),col=COL[3],type="l",lty=2)
points(density(Data[which(Data$Dist_Difference>x & Data$Sex=="F"),]$Dist_Difference,na.rm=T),col=COL[4],type="l",lty=2)
legend(x=7500,y=4*(10^-4),
       legend=c(paste("Intercept",round(reg$coef[1],3)),
                paste("Male",round(reg$coef[2],3)),
                paste("Sigma",round(summary(reg)$sigma,3)))
      )
dev.off()



reg<-lm(data=Data[which(Data$Dist_Difference>100),],formula=log(sqrt(Speed_mps))~Sex)
par(mfrow=c(2,2));plot(reg)
sigma<-summary(reg)$sigma
female<-exp(reg$coef[1]+rnorm(30000,mean=0,sd=sigma))^2
male<-exp(reg$coef[1]+reg$coef[2]+rnorm(30000,mean=0,sd=sigma))^2
plot(density(female),col=COL[4])
points(density(male),col=COL[3],type="l")
points(density(Data[which(Data$Dist_Difference>100 & Data$Sex=="M"),]$Speed_mps,na.rm=T),col=COL[3],type="l",lty=2)
points(density(Data[which(Data$Dist_Difference>100 & Data$Sex=="F"),]$Speed_mps,na.rm=T),col=COL[4],type="l",lty=2)

reg<-lm(data=Data[which(Data$Dist_Difference>100 & Data$Time_Difference<60*60*24),],formula=log(Speed_mps)~Sex+Time_Difference)
par(mfrow=c(2,2));plot(reg)
sigma<-summary(reg)$sigma
female<-exp(reg$coef[1]+rnorm(30000,mean=0,sd=sigma))^2
male<-exp(reg$coef[1]+reg$coef[2]+rnorm(30000,mean=0,sd=sigma))^2
plot(density(female),col=COL[4])
points(density(male),col=COL[3],type="l")
points(density(Data[which(Data$Dist_Difference>100 & Data$Sex=="M"),]$Speed_mps,na.rm=T),col=COL[3],type="l",lty=2)
points(density(Data[which(Data$Dist_Difference>100 & Data$Sex=="F"),]$Speed_mps,na.rm=T),col=COL[4],type="l",lty=2)


