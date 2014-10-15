## this code has two parts: 1.) ESTIMATION OF (INDIVIDUAL-SPECIFIC) HMM and 2.) ESTIMATION OF (INDIVIDUAL-SPECIFIC) HSMM
## observations ("obs") need to be given in an n x 2 matrix, with first column giving the step lengths and second column giving the turning angles

## for illustration purposes the data stored in the file "obs.txt" (in the supplementary material to the paper) may be used: 
## read.table(".../obs.txt",header=T)->obs
## this file comprises the observed step lengths and turning angles for one of the bison



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
        # I added the tryCatch statement, to suppress the produced NaN warning statement
        tryCatch(allprobs[k,j] <- dweibull(obs[k,1],shape=lpn$a[j],scale=lpn$b[j])*angle.prob, warning = function(c) allprobs[k,j] <-NaN)
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
  if(is.na(mllk)){mllk<-10^200} ## I added this line, to stop error "replaced NA/INF with max. value" have manunally replaced NAs in code
  mllk
}


## function that runs the numerical maximization of the above likelihood function and returns the results
move.HMM.mle <- function(obs,a0,b0,kappa0,gamma0,co0,stepm){
  parvect0 <- move.HMM.pn2pw(a0,b0,kappa0,gamma0,co0)
  mod <-nlm(move.HMM.mllk,parvect0,obs,print.level=0,stepmax=stepm)
  mllk <- mod$minimum
  code <- mod$code
  pn <- move.HMM.pw2pn(mod$estimate)
  list(a=pn$a,b=pn$b,kappa=pn$kappa,Gamma=pn$Gamma,delta=pn$delta,co=pn$co,mllk=mllk,code=code)
}





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
move.HSMM.mllk <- function(parvect,obs,ini,m){
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
        # I added the tryCatch statement, to suppress the produced NaN warning statement
        tryCatch(allprobs[k,((j-1)*m[1]+1):((j-1)*m[1]+m[j])] <- rep(dweibull(obs[k,1],shape=lpn$a[j],scale=lpn$b[j])*angle.prob,m[j])  , warning = function(c) allprobs[k,((j-1)*m[1]+1):((j-1)*m[1]+m[j])] <-rep(NaN,m[j])) 
        } # j index
      } # non-miss check
   } # for each ovbervation (k loop)
   foo <- delta  
   lscale <- 0
   for (i in 1:n){
      foo <- foo%*%gamma*allprobs[i,]  
      sumfoo <- sum(foo)
      # I added the tryCatch statement, to suppress the produced NaN warning statement
      tryCatch(lscale <- lscale+log(sumfoo), warning = function(c) lscale <-NaN)
      foo <- foo/sumfoo
   }
   mllk <- -lscale
   if(is.na(mllk) | is.infinite(mllk)){mllk<-10^200} ## I added this line, to stop error "replaced NA/INF with max. value" have manunally replaced NAs in code
   mllk
}

## function that runs the numerical maximization of the above likelihood function and returns the results
move.HSMM.mle <- function(obs,a0,b0,kappa0,gam0,co0,ini,stepm,m){
  parvect0 <- move.HSMM.pn2pw(a0,b0,kappa0,gam0,co0)
  mod <- nlm(move.HSMM.mllk,parvect0,obs,ini,print.level=0,stepmax=stepm,m=m)
  mllk <- mod$minimum
 code <- mod$code
  pn <- move.HSMM.pw2pn(mod$estimate)
  gamma <- gen.Gamma(m,pn$gam)
  delta <- solve(t(diag(sum(m))-gamma+1),rep(1,sum(m)))
  list(a=pn$a,b=pn$b,kappa=pn$kappa,gam=pn$gam,delta=c(sum(delta[1:m[1]]),1-sum(delta[1:m[1]])),co=pn$co,mllk=mllk,code=code)
}