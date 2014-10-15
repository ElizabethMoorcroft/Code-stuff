## Estimation of hierarchical HSMM 
## observations ("OBS") need to be given in an n x (2*p) matrix, with p being the number of individuals, 
## and the observed step lengths and turning angles for individual i given in columns 2*(i-1)+1 and 2*(i-1)+2, respectively


library(CircStats)
library(boot)
               
               
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
  ta <- c(a[1:2],log(a[3:4]))
  tb <- log(b)
  tkappa <- logit(kappa)
  tgam <- c(log(gam[1:2]),log(gam[3:4]/(1-gam[3:4])))
  tco <- c(log((co[1])/(2*pi-co[1])),log((pi+co[2])/(pi-co[2])))
  parvect <- c(ta,tb,tkappa,tgam,tco)
  return(parvect)
}
            
## inverse transformation back to the natural parameter space            
move.HSMM.pw2pn <- function(parvect){
  epar <- exp(parvect)
  a <- c(parvect[1:2],epar[3:4])
  b <- epar[5:6]
  kappa <- inv.logit(parvect[7:8])
  gam <- c(exp(parvect[9:10]),exp(parvect[11:12])/(exp(parvect[11:12])+1))    
  co <- c(2*pi*inv.logit(parvect[13]),pi*(exp(parvect[14])-1)/(exp(parvect[14])+1))
  return(list(a=a,b=b,kappa=kappa,gam=gam,co=co))
}

## function that computes minus the log-likelihood
move.HSMM.mllk <- function(parvect,OBS){
  lpn <- move.HSMM.pw2pn(parvect)
  gamma <- gen.Gamma(m,lpn$gam)
  delta <- solve(t(diag(sum(m))-gamma+1),rep(1,sum(m)))
  mllk.all <- 0
  K=25
  a1 <- seq(re1min,re1max,length=K)
  a2 <- seq(re2min,re2max,length=K)
  a.m1 <- (a1[-1]+a1[-K])*0.5   
  a.m2 <- (a2[-1]+a2[-K])*0.5   
  am <- rbind(exp(a.m1),exp(a.m2)) 
  n.ind<-dim(OBS)[2]/2 # !! I added this line !!
  for (ani in 1:n.ind){ 
    print(paste("ani loop: ",ani,"/",n.ind,sep=""))
    obs <- OBS[,((ani-1)*2+1):((ani-1)*2+2)]
    n <- max(which(!is.na(obs[,1])))
    obs <- obs[1:n,]
    allprobs <- matrix(rep(NA,sum(m)*n),nrow=n)
    one.animal <- matrix(rep(NA,(K-1)^2*2),ncol=2)
    for (j1 in 1:(K-1)){
      #print(paste("j1 loop: ",j1,"/",K-1,sep=""))
      for (j2 in 1:(K-1)){  
        #print(paste("j2 loop: ",j2,"/",K-1,sep=""))
        for (k in 1:n){# for each entry for animal ani
            #print(paste("k loop: ",k,"/",n,sep=""))
            if (is.na(obs[k,1])) {allprobs[k,] <- rep(1,sum(m)) }
            if (!is.na(obs[k,1])) {
              #dwrpchauchy(theta, mu,  rho) - caluclates the density function at theta, for the cauchy distributed in mu and rho
              angle.prob <- ifelse(is.na(obs[k,2]),1,dwrpcauchy(obs[k,2],mu=lpn$co[1],rho=lpn$kappa[1]))
              #dweibull(x,shape, scale)
              allprobs[k,1:m[1]] <- rep(dweibull(obs[k,1],shape=lpn$b[1],scale=am[1,j1])*angle.prob,m[1])
              angle.prob <- ifelse(is.na(obs[k,2]),1,dwrpcauchy(obs[k,2],mu=lpn$co[2],rho=lpn$kappa[2]))
              allprobs[k,(m[1]+1):sum(m)] <- rep(dweibull(obs[k,1],shape=lpn$b[2],scale=am[2,j2])*angle.prob,m[2]) 
            }  
        } #End of k   
        foo <- delta  
        lscale <- 0
      for (i in 1:n){
          foo <- foo%*%gamma*allprobs[i,]  
          sumfoo <- sum(foo)
          lscale <- lscale+log(sumfoo)
          foo <- foo/sumfoo
      }
      llk <- lscale
      stan.fac1 <- pnorm(max(a1),lpn$a[1],lpn$a[3])-pnorm(min(a1),lpn$a[1],lpn$a[3])
      stan.fac2 <- pnorm(max(a2),lpn$a[2],lpn$a[4])-pnorm(min(a2),lpn$a[2],lpn$a[4])
      r <- (j1-1)*(K-1)+1+(j2-1)
      one.animal[r,1] <- llk
      one.animal[r,2] <- stan.fac1^(-1)*stan.fac2^(-1)*
        (pnorm(a1[j1+1],lpn$a[1],lpn$a[3])-pnorm(a1[j1],lpn$a[1],lpn$a[3]))*
        (pnorm(a2[j2+1],lpn$a[2],lpn$a[4])-pnorm(a2[j2],lpn$a[2],lpn$a[4]))
      }
    }                                       
    ma <- max(one.animal[,1]) 
    for (goo in 1:dim(one.animal)[1]) {
    		ifelse(ma-one.animal[goo,1]<700,one.animal[goo,1]<-exp(one.animal[goo,1]-ma),one.animal[goo,1]<-0)
    	}  ### to avoid underflow
    mllk.all <- mllk.all-(log(sum(one.animal[,1]*one.animal[,2]))+ma)  
  }
  return(mllk.all)
}

iterationcounter<-1
## function that runs the numerical maximization of the above likelihood function and returns the results
move.HSMM.mle <- function(OBS,a0,b0,kappa0,gamma0,co0){
  print("Start function")
  parvect0 <- move.HSMM.pn2pw(a0,b0,kappa0,gamma0,co0)
  print("parvect0 - complete ")
  mod <- nlm(move.HSMM.mllk,parvect0,OBS,print.level=2,hessian=TRUE,stepmax=stepm,iterlim=4000) ## hessian=TRUE only for confidence intervals 
  mllk <- mod$minimum
  print(paste("Code for NLM",mod$code))
  pn <- move.HSMM.pw2pn(mod$estimate)
  list(a=pn$a,b=pn$b,kappa=pn$kappa,H=mod$hessian,gamma=pn$gam,co=pn$co,mllk=mllk)
}



### Create dummy data
N=25;anim=20
Shape<-c(1.8,1)
Scale.Distrubtion<-c(0.1,0.01,0.5,0.5)
Rho<-c(0.5,0.5)
Mu<-c(0.1,3)
State<-c(1,1,0.5,0.8)
CurrentState<-1; time.in.current.state<-rbinom(1,size=State[1],prob=State[2])
scales.used<-matrix(ncol=2,nrow=0)

sim.data<-matrix(ncol=0,nrow=N)
m<-c(30,30)
set.seed(1)
for(animal in 1:anim){
	x<-c();y<-c()
	Scale<-c(rlnorm(1,Scale.Distrubtion[1],Scale.Distrubtion[3]),rlnorm(1,Scale.Distrubtion[2],Scale.Distrubtion[4]))
	scales.used<-rbind(scales.used,Scale)
	print(paste("Scale",Scale))
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
	sim.data<-cbind(x,y,sim.data)
}
	

## initial parameter values to be used in the numerical maximization (several different combinations should be tried in order to ensure hitting the global maximum)
#a0 <- c(0.8,0.9,exp(2.2),exp(-3)) # The mean and SD of the log normal distribution of the scale weibull parameter
#b0 <- c(0.1,0.5) # Weibull shape parameters
#kappa0 <- c(exp(-1.38),exp(-0.4)) # wrapped Cauchy concentration parameters
#co0 <- c(exp(-0.94),exp(1.1)) # wrapped Cauchy mean parameters
#gamma0 <- c(0.39,3.75,0.99,0.99)  # negative binomial state dwell-time distribution parameters

## choose range over which to integrate the random effects' distributions
re1min <- 0.001
re1max <- 0.5
re2min <- 0.1
re2max <- 1

## size of state aggregates
m <- c(20,10)

## run the numerical maximization
stepm <- 20
move.HSMM.mle(sim.data,a0=Scale.Distrubtion,b0=Shape,kappa0=Rho,gamma0=State,co0=Mu) -> moveHSMM
moveHSMM

sim.scale.state1<-rlnorm(10000,moveHSMM$a[1],moveHSMM$a[3])
sim.scale.state2<-rlnorm(10000,moveHSMM$a[2],moveHSMM$a[4])


par(mfrow=c(2,1))
plot(density(sim.scale.state1))
for(i in 1:anim){abline(v=scales.used[i,1],col="red")}
points(density(rlnorm(10000,Scale.Distrubtion[1],Scale.Distrubtion[3]),col="green")
plot(density(sim.scale.state2))
for(i in 1:anim){abline(v=scales.used[i,2],col="red")}
points(density(rlnorm(10000,Scale.Distrubtion[2],Scale.Distrubtion[4]),col="green")


new.window()
sim.distance<-c(rweibull(n=No.simulations*time.in.state.1,shape=moveHSMM$a[1],scale=moveHSMM$b[1]),
				rweibull(n=No.simulations*(1-time.in.state.1),shape=moveHSMM$a[2],scale=moveHSMM$b[2]))
sim.angle<-c(rwrpcauchy(n=No.simulations*time.in.state.1,location=moveHSMM$co[1],rho =moveHSMM$kappa[1]),
				rwrpcauchy(n=No.simulations*(1-time.in.state.1),location=moveHSMM$co[2],rho =moveHSMM$kappa[2]))