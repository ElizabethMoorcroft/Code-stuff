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
for (ani in 1:n.ind){  
obs <- OBS[,((ani-1)*2+1):((ani-1)*2+2)]
n <- max(which(!is.na(obs[,1])))
obs <- obs[1:n,]
allprobs <- matrix(rep(NA,sum(m)*n),nrow=n)
 one.animal <- matrix(rep(NA,(K-1)^2*2),ncol=2)
 for (j1 in 1:(K-1)){                 
 for (j2 in 1:(K-1)){                
 for (k in 1:n){
if (is.na(obs[k,1])) {
allprobs[k,] <- rep(1,sum(m))
}
if (!is.na(obs[k,1])) {
angle.prob <- ifelse(is.na(obs[k,2]),1,dwrpcauchy(obs[k,2],mu=lpn$co[1],rho=lpn$kappa[1]))
allprobs[k,1:m[1]] <- rep(dweibull(obs[k,1],shape=lpn$b[1],scale=am[1,j1])*angle.prob,m[1])
angle.prob <- ifelse(is.na(obs[k,2]),1,dwrpcauchy(obs[k,2],mu=lpn$co[2],rho=lpn$kappa[2]))
allprobs[k,(m[1]+1):sum(m)] <- rep(dweibull(obs[k,1],shape=lpn$b[2],scale=am[2,j2])*angle.prob,m[2]) 
 }  
 }  
 foo <- delta  
 lscale <- 0
 for (i in 1:n)
  {
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
  }}                                       
  ma <- max(one.animal[,1]) 
  for (goo in 1:dim(one.animal)[1]) {ifelse(ma-one.animal[goo,1]<700,one.animal[goo,1]<-exp(one.animal[goo,1]-ma),one.animal[goo,1]<-0)}  ### to avoid underflow
  mllk.all <- mllk.all-(log(sum(one.animal[,1]*one.animal[,2]))+ma)  
  } 
 return(mllk.all)
}
 
## function that runs the numerical maximization of the above likelihood function and returns the results
move.HSMM.mle <- function(OBS,a0,b0,kappa0,gamma0,co0){
parvect0 <- move.HSMM.pn2pw(a0,b0,kappa0,gamma0,co0)
mod <- nlm(move.HSMM.mllk,parvect0,OBS,print.level=2,hessian=TRUE,stepmax=stepm,iterlim=4000) ## hessian=TRUE only for confidence intervals 
mllk <- mod$minimum
pn <- move.HSMM.pw2pn(mod$estimate)
list(a=pn$a,b=pn$b,kappa=pn$kappa,H=mod$hessian,gamma=pn$gam,co=pn$co,mllk=mllk)
}

## initial parameter values to be used in the numerical maximization (several different combinations should be tried in order to ensure hitting the global maximum)
a0 <- c(log(0.08),log(0.46),0.11,0.05) # associated with the random effects distributions 
b0 <- c(0.8,0.85) # Weibull shape parameters
kappa0 <- c(0.2,0.4) # wrapped Cauchy concentration parameters
co0 <- c(pi,0) # wrapped Cauchy mean parameters
gamma0 <- c(0.39,3.75,0.14,0.59)  # negative binomial state dwell-time distribution parameters

## choose range over which to integrate the random effects' distributions
re1min <- log(0.05)
re1max <- log(0.12)
re2min <- log(0.39)
re2max <- log(0.59)

## size of state aggregates
m <- c(20,10)

## run the numerical maximization
stepm <- 500
move.HSMM.mle(OBS,a0,b0,kappa0,gamma0,co0) -> moveHSMM
moveHSMM
