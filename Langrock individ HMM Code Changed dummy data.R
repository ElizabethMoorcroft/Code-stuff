###


####
# Set working directory
DIR_WORK<-"/Users/student/Documents/Snow Leopards/BUGSstuff"
DIR_CODE<-"/Users/student/Documents/Snow Leopards/Code"
DIR_DATA<-"/Users/student/Documents/Snow Leopards/Data"
####

####
# Source file
setwd(DIR_CODE)
source("Snow Leopard HMM form2.R") # for the owin2SP function

####
# Load data
setwd(DIR_DATA)
Data<-read.csv("CurrentData.csv") # CREATE FILE
selected.5.hour.blocks<-Months(Data,6,3)
####
no.of.cols<-dim(selected.5.hour.blocks[[1]])[2]
movement.matrix<-selected.5.hour.blocks[[1]]
max.displacement<-max(movement.matrix[,c((1:(no.of.cols/2))*2-1)],na.rm=T)
min.displacement<-min(movement.matrix[,c((1:(no.of.cols/2))*2-1)],na.rm=T)
plot(type="n",0,xlim=c(0,max.displacement),ylim=c(0,2))
for(i in 1:(no.of.cols/2)){
  column<-(i*2)-1
  points(type="l",col="red",
         density(movement.matrix[,column],na.rm=T))
}


### Create dummy data
N=10000
Shape<-c(1.8,1)
Scale<-c(3,1)
Rho<-c(0.5,0.5)
Mu<-c(0.1,3)
State<-c(0.3,0.5)
CurrentState<-1
x<-c();y<-c()

for(i in 1:N){
  if(CurrentState==1){
    shape=Shape[1];scale=Scale[1];mu=Mu[1];rho=Rho[1]; state=(1-State[1])
  }else{
    shape=Shape[2];scale=Scale[2];mu=Mu[2];rho=Rho[2]; state=State[2]
  }
  
  x<-c(x,rweibull(n=1, shape=shape, scale = scale))
  y<-c(y,rwrpcauchy(n=1,location=mu,rho =rho))
  CurrentState<-rbinom(1,1,state)
}

points(density(x),type="l")



## this code has two parts: 1.) ESTIMATION OF (INDIVIDUAL-SPECIFIC) HMM and 2.) ESTIMATION OF (INDIVIDUAL-SPECIFIC) HSMM
## observations ("obs") need to be given in an n x 2 matrix, with first column giving the step lengths and second column giving the turning angles

## for illustration purposes the data stored in the file "obs.txt" (in the supplementary material to the paper) may be used: 
## read.table(".../obs.txt",header=T)->obs
## this file comprises the observed step lengths and turning angles for one of the bison

library(CircStats)
library(boot)


######## 1.) fit hidden Markov movement model (Weibull & wrapped Cauchy) ########

## function that transforms each of the (possibly constrained) parameters to the real line
move.HMM.pn2pw <- function(a,b,kappa,gamma,co){
  ta <- log(a)
  tb <- log(b)
  tkappa <- logit(kappa)
  tgamma <- logit(gamma)
  tco <- c(log((co[1])/(2*pi-co[1])),log((pi+co[2])/(pi-co[2])))
  parvect <- c(ta,tb,tkappa,tgamma,tco)
  return(parvect)
}

## inverse transformation back to the natural parameter space
move.HMM.pw2pn <- function(parvect){
  epar <- exp(parvect)
  a <- epar[1:2]
  b <- epar[3:4]
  kappa <- inv.logit(parvect[5:6])
  gamma <- inv.logit(parvect[7:8])  
  Gamma <- matrix(rep(NA,4),nrow=2) 
  Gamma[1,1] <- gamma[1]; Gamma[1,2] <- 1-gamma[1]; Gamma[2,2] <- gamma[2]; Gamma[2,1] <- 1-gamma[2] 
  delta <- solve(t(diag(2)-Gamma+1),c(1,1))             
  co <- c(2*pi*inv.logit(parvect[9]),pi*(epar[10]-1)/(epar[10]+1))
  return(list(a=a,b=b,kappa=kappa,Gamma=Gamma,delta=delta,co=co))
}                 

## function that computes minus the log-likelihood
move.HMM.mllk <- function(parvect,obs){
  n <- dim(obs)[1]
  lpn <- move.HMM.pw2pn(parvect)
  #print(lpn)
  allprobs <- matrix(rep(NA,2*n),nrow=n)
  for (k in 1:n){
    if (is.na(obs[k,1])) {
      allprobs[k,] <- c(1,1)
    }
    if (!is.na(obs[k,1])) {
      for (j in 1:2){
        angle.prob <- ifelse(is.na(obs[k,2]),1,dwrpcauchy(obs[k,2],mu=lpn$co[j],rho=lpn$kappa[j]))
        allprobs[k,j] <- dweibull(obs[k,1],shape=lpn$a[j],scale=lpn$b[j])*angle.prob  
      } # j index
    } # non-miss check
  } # k index
  foo <- lpn$delta 
  lscale <- 0
  for (i in 1:n){
    foo <- foo%*%lpn$Gamma*allprobs[i,]  
    sumfoo <- sum(foo)
    lscale <- lscale+log(sumfoo)
    foo <- foo/sumfoo
  }
  mllk <- -lscale
  #print(allprobs)
  mllk
}


## function that runs the numerical maximization of the above likelihood function and returns the results
move.HMM.mle <- function(obs,a0,b0,kappa0,gamma0,co0){
  parvect0 <- move.HMM.pn2pw(a0,b0,kappa0,gamma0,co0)
  mod <- nlm(move.HMM.mllk,parvect0,obs,print.level=0,stepmax=stepm)
  mllk <- mod$minimum
  pn <- move.HMM.pw2pn(mod$estimate)
  list(a=pn$a,b=pn$b,kappa=pn$kappa,Gamma=pn$Gamma,delta=pn$delta,co=pn$co,mllk=mllk)
}


listofindivid<-vector(mode="list")

####


### Create dummy data
N=5000
Shape<-c(1.8,1)
Scale<-c(3,1)
Rho<-c(0.5,0.5)
Mu<-c(0.1,3)
State<-c(0.3,0.5)
CurrentState<-1
x<-c();y<-c()

for(i in 1:N){
	if(CurrentState==1){
		shape=Shape[1];scale=Scale[1];mu=Mu[1];rho=Rho[1]; state=(1-State[1])
	}else{
		shape=Shape[2];scale=Scale[2];mu=Mu[2];rho=Rho[2]; state=State[2]
	}
	
	x<-c(x,rweibull(n=1, shape=shape, scale = scale))
	y<-c(y,rwrpcauchy(n=1,location=mu,rho =rho))
	CurrentState<-rbinom(1,1,state)
}

tempdata<-cbind(x,y)
# move.HMM.mle(obs=tempdata,a0=Scale,b0=Shape,kappa0=Rho,gamma0=State,co0=Mu)->moveHMM
move.HMM.mle(obs=tempdata,a0=c(1,0.5),b0=c(1,1),kappa0=c(0.8,0.9),gamma0=c(0.9,0.2),co0=c(0.5,0.6))->moveHMM

time.in.state.1<-(moveHMM$Gamma[2])/(1-moveHMM$Gamma[1]+moveHMM$Gamma[2])
No.simulations<-1000
sim.distance<-c(rweibull(No.simulations*time.in.state.1,Shape=moveHMM$a[1],Scale=moveHMM$b[1]),
				rweibull(No.simulations*(1-time.in.state.1),Shape=moveHMM$a[2],Scale=moveHMM$b[2]))
sim.angle<-c(rwrpcauchy(n=No.simulations*time.in.state.1,location=moveHMM$co[1],rho =moveHMM$kappa[1]),
				rwrpcauchy(n=No.simulations*(1-time.in.state.1),location=moveHMM$co[2],rho =moveHMM$kappa[2]))

par(mfrow=c(2,2))
plot(density(tempdata[,1]),xlim=c(0,12))
plot(density(sim.distance),xlim=c(0,12))
plot(density(tempdata[,2]),xlim=c(0,2*pi))
plot(density(sim.angle),xlim=c(0,2*pi))



######## 2.) fit hidden semi-Markov movement model (Weibull & wrapped Cauchy & negative binomial) ########

## function that derives the t.p.m. of the HMM that represents the HSMM (see Langrock and Zucchini, 2011) 
gen.Gamma <- function(m,p){
  Gamma <- diag(sum(m))*0
  ## state aggregate 1
  Gamma[1,m[1]+1] <- dnbinom(0,size=p[1],prob=p[3]); Gamma[1,2] <- 1-Gamma[1,m[1]+1]
  for (i in 2:(m[1]-1)){
    cc<-rep(1,sum(m))
    for (k in 1:(i-1)) {cc[k] <- Gamma[k,k+1]}
    dd<-prod(cc)
    if (dd>1e-12) Gamma[i,m[1]+1] <- dnbinom(i-1,size=p[1],prob=p[3])/dd
    if (dd<1e-12) Gamma[i,m[1]+1] <- 1
    Gamma[i,i+1] <- 1-Gamma[i,m[1]+1]
  } 
  cc<-rep(1,sum(m))
  for (k in 1:(m[1]-1)){cc[k] <- Gamma[k,k+1]}
  dd<-prod(cc)
  if (dd>1e-12) Gamma[m[1],m[1]+1] <- dnbinom(m[1]-1,size=p[1],prob=p[3])/dd
  if (dd<1e-12) Gamma[m[1],m[1]+1] <- 1
  Gamma[m[1],m[1]] <- 1-Gamma[m[1],m[1]+1] 
  
  ## state aggregate 2
  Gamma[m[1]+1,1] <- dnbinom(0,size=p[2],prob=p[4]); Gamma[m[1]+1,m[1]+2] <- 1-Gamma[m[1]+1,1]
  for (i in 2:(m[2]-1)){
    cc <- rep(1,sum(m))
    for (k in 1:(i-1)) {cc[k] <- Gamma[m[1]+k,m[1]+k+1]}
    dd <- prod(cc)
    if (dd>1e-12) Gamma[m[1]+i,1] <- dnbinom(i-1,size=p[2],prob=p[4])/dd
    if (dd<1e-12) Gamma[m[1]+i,1] <- 1
    Gamma[m[1]+i,m[1]+i+1] <- 1-Gamma[m[1]+i,1]
  }
  cc<-rep(1,sum(m))
  for (k in 1:(m[2]-1)) {cc[k] <- Gamma[m[1]+k,m[1]+k+1]}
  dd<-prod(cc)
  if (dd>1e-12) Gamma[m[1]+m[2],1] <- dnbinom(m[2]-1,size=p[2],prob=p[4])/dd
  if (dd<1e-12) Gamma[m[1]+m[2],1] <- 1
  Gamma[m[1]+m[2],m[1]+m[2]] <- 1-Gamma[m[1]+m[2],1] 
  Gamma 
}

## function that transforms each of the (possibly constrained) parameters to the real line
move.HSMM.pn2pw <- function(a,b,kappa,gam,co){
  ta <- log(a)
  tb <- log(b)
  tkappa <- logit(kappa)
  tgam <- c(log(gam[1:2]),logit(gam[3:4]))
  tco <- c(log((co[1])/(2*pi-co[1])),log((pi+co[2])/(pi-co[2])))
  parvect <- c(ta,tb,tkappa,tgam,tco)
  return(parvect)
}

## inverse transformation back to the natural parameter space
move.HSMM.pw2pn <- function(parvect){
epar <- exp(parvect)
a <- epar[1:2]
b <- epar[3:4]
kappa <- inv.logit(parvect[5:6])
gam <- c(epar[7:8],inv.logit(parvect[9:10]))    
co <- c(2*pi*inv.logit(parvect[11]),pi*(epar[12]-1)/(epar[12]+1))
return(list(a=a,b=b,kappa=kappa,gam=gam,co=co))
}

## function that computes minus the log-likelihood of the HSMM
move.HSMM.mllk <- function(parvect,obs,ini){
  #print(paste("move.hsmm.mle:",ini))
   n <- dim(obs)[1]
   lpn <- move.HSMM.pw2pn(parvect)
   allprobs <- matrix(rep(NA,(m[1]+m[2])*n),nrow=n)
   gamma <- gen.Gamma(m,lpn$gam)
   if (ini==1) {delta <- rep(1/(sum(m)),sum(m))} # if invertibility problems
   if (ini==0) {delta <- solve(t(diag(sum(m))-gamma+1),rep(1,sum(m)))}
   for (k in 1:n){
      if (is.na(obs[k,1])) {
      allprobs[k,] <- rep(1,m[1]+m[2])
      }
      if (!is.na(obs[k,1])) {
        for (j in 1:2){
        angle.prob <- ifelse(is.na(obs[k,2]),1,dwrpcauchy(obs[k,2],mu=lpn$co[j],rho=lpn$kappa[j]))
        allprobs[k,((j-1)*m[1]+1):((j-1)*m[1]+m[j])] <- rep(dweibull(obs[k,1],shape=lpn$a[j],scale=lpn$b[j])*angle.prob,m[j])   
        } # j index
      } # non-miss check
   } # for each ovbervation (k loop)
   foo <- delta  
   lscale <- 0
   for (i in 1:n){
      foo <- foo%*%gamma*allprobs[i,]  
      sumfoo <- sum(foo)
      lscale <- lscale+log(sumfoo)
      foo <- foo/sumfoo
   }
   mllk <- -lscale
   mllk
}

## function that runs the numerical maximization of the above likelihood function and returns the results
move.HSMM.mle <- function(obs,a0,b0,kappa0,gam0,co0,ini){
  parvect0 <- move.HSMM.pn2pw(a0,b0,kappa0,gam0,co0)
 # print(ini)
  mod <- nlm(move.HSMM.mllk,parvect0,obs,ini,print.level=0,stepmax=stepm)
  mllk <- mod$minimum
  pn <- move.HSMM.pw2pn(mod$estimate)
  gamma <- gen.Gamma(m,pn$gam)
  delta <- solve(t(diag(sum(m))-gamma+1),rep(1,sum(m)))
  list(a=pn$a,b=pn$b,kappa=pn$kappa,gam=pn$gam,delta=c(sum(delta[1:m[1]]),1-sum(delta[1:m[1]])),co=pn$co,mllk=mllk)
}



### Create dummy data
N=5000
Shape<-c(1.8,1)
Scale<-c(3,1)
Rho<-c(0.5,0.5)
Mu<-c(0.1,3)
State<-c(1,1,0.5,0.8)
CurrentState<-1; time.in.current.state<-rbinom(1,size=State[1],prob=State[2])
x<-c();y<-c()
m<-c(30,30)

for(i in 1:N){
	if(time.in.current.state==0){
		if(CurrentState==1){
			time.in.current.state<-rbinom(1,size=State[1],prob=State[3])
			CurrentState=0
		}else{
			time.in.current.state<-rbinom(1,size=State[2],prob=State[4])
			CurrentState=1
		}
	}
	if(CurrentState==1){
		shape=Shape[1];scale=Scale[1];mu=Mu[1];rho=Rho[1];
	}else{
		shape=Shape[2];scale=Scale[2];mu=Mu[2];rho=Rho[2];
	}
	
	x<-c(x,rweibull(n=1, shape=shape, scale = scale))
	y<-c(y,rwrpcauchy(n=1,location=mu,rho =rho))
}

tempdata2<-cbind(x,y)
stepm <- 35
ini <- 1
move.HSMM.mle(tempdata,a0=Scale,b0=Shape,kappa0=Rho,gam0=State,co0=Mu,ini=ini) -> moveHSMM

time.in.state.1<-moveHSMM$delta[1]
No.simulations<-1000
sim.distance<-c(rweibull(n=No.simulations*time.in.state.1,shape=moveHSMM$a[1],scale=moveHSMM$b[1]),
				rweibull(n=No.simulations*(1-time.in.state.1),shape=moveHSMM$a[2],scale=moveHSMM$b[2]))
sim.angle<-c(rwrpcauchy(n=No.simulations*time.in.state.1,location=moveHSMM$co[1],rho =moveHSMM$kappa[1]),
				rwrpcauchy(n=No.simulations*(1-time.in.state.1),location=moveHSMM$co[2],rho =moveHSMM$kappa[2]))

par(mfrow=c(2,2))
plot(density(tempdata[,1]),xlim=c(0,12))
points(density(sim.distance),col="red",type="l")
plot(density(tempdata[,2]),xlim=c(0,2*pi))
points(density(sim.angle),col="red",type="l")


## then maximize assuming stationarity
ini=0
stepm <- 5
move.HSMM.mle(tempdata,a0=moveHSMM$a ,b0=moveHSMM$b,kappa0=moveHSMM$kappa,gam0=moveHSMM$gam,co0=moveHSMM$co,ini=ini) -> moveHSMM
moveHSMM

time.in.state.1<-moveHSMM$delta[1]
No.simulations<-1000
sim.distance<-c(rweibull(n=No.simulations*time.in.state.1,shape=moveHSMM$a[1],scale=moveHSMM$b[1]),
				rweibull(n=No.simulations*(1-time.in.state.1),shape=moveHSMM$a[2],scale=moveHSMM$b[2]))
sim.angle<-c(rwrpcauchy(n=No.simulations*time.in.state.1,location=moveHSMM$co[1],rho =moveHSMM$kappa[1]),
				rwrpcauchy(n=No.simulations*(1-time.in.state.1),location=moveHSMM$co[2],rho =moveHSMM$kappa[2]))


plot(density(tempdata[,1]),xlim=c(0,12))
points(density(sim.distance),col="red",type="l")
plot(density(tempdata[,2]),xlim=c(0,2*pi))
points(density(sim.angle),col="red",type="l")





## initial parameter values used in the numerical maximization (several different combinations should be tried in order to ensure hitting the global maximum)
#a0 <- moveHMM$a  
#b0 <- moveHMM$b    
#kappa0 <- moveHMM$kappa    
#co0 <- moveHMM$co  
#gam0 <- c(1,1,moveHMM$Gamma[1,2],moveHMM$Gamma[2,1]) # neg binom = c(size1 size2, prob1, prob2)
#m <- c(30,30)

## first maximize not assuming stationarity (otherwise errors are likely, affects only the initial distribution)
#stepm <- 35
#ini <- 1
#move.HSMM.mle(x[,3:4],a0,b0,kappa0,gam0,co0) -> moveHSMM

## then maximize assuming stationarity
#ini=0
#moveHSMM$a -> a0 
#moveHSMM$b -> b0 
#moveHSMM$kappa -> kappa0
#moveHSMM$gam -> gam0
#moveHSMM$co -> co0
#stepm <- 5
#move.HSMM.mle(x[,3:4],a0,b0,kappa0,gam0,co0) -> moveHSMM
#moveHSMM

