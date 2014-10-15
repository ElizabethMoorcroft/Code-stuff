# Run the individual movement

LangrockIndividuals<-function(x,animallist){
  listofindivid<-vector(mode="list")
  for(i in 1:(dim(x)[2]/2)){
    print(paste("i: ",i,"/",dim(x)[2]/2))
  
    # Intial values
    setwd(CodeDir); source("Langrock initial values.R")
  
    # Select animal i's data
    tempdata<-x[,c((i*2)-1,i*2)]
    
    tempvector<-vector(mode="list",length=2)
    tempvector[[1]]<-animallist[i];tempvector[[2]]<-length(!is.na(tempdata[1]));
    #Runs HMM and HSMM with error catching, returns blank if there if fails
    try(  tempvector<-HMMandHSMM(tempdata,animallist[i]))
    listofindivid[[i]]<-tempvector
  }
  return(listofindivid);
}  

HMMandHSMM<-function(tempdata,ID){
  # Run individual HMM
    a0<-c(0.8,0.9) # Weibull shape parameters
    b0<-c(0.1,0.5) # Weibull scale parameters
    kappa0<-c(0.2,0.3) # wrapped Cauchy concentration parameters
    co0<-c(pi,0) # wrapped Cauchy mean parameters
    gamma0<-c(0.7,0.5) # diagonal entries of the transition probability matrix
    move.HMM.mle(tempdata,a0,b0,kappa0,gamma0,co0)->moveHMM
    
    print("run hmm mle")
    ## initial parameter values used in the numerical maximization (several different combinations should be tried in order to ensure hitting the global maximum)
    a0 <- moveHMM$a  
    b0 <- moveHMM$b    
    kappa0 <- moveHMM$kappa    
    co0 <- moveHMM$co  
    gam0 <- c(1,1,moveHMM$Gamma[1,2],moveHMM$Gamma[2,1])
    
    ini<-1
    ## first maximize not assuming stationarity (otherwise errors are likely, affects only the initial distribution)
    move.HSMM.mle(tempdata,a0,b0,kappa0,gam0,co0,ini=ini) -> moveHSMM
    print("run hsmm mle")
    ## then maximize assuming stationarity
    moveHSMM$a -> a0 
    moveHSMM$b -> b0 
    moveHSMM$kappa -> kappa0
    moveHSMM$gam -> gam0
    moveHSMM$co -> co0
    stepm <- 5
    ini<-0
    move.HSMM.mle(tempdata,a0,b0,kappa0,gam0,co0,ini=ini) -> moveHSMM
    moveHSMM
    tempvector<-vector(mode="list",length=4)
    tempvector[[1]]<-moveHMM;tempvector[[2]]<-moveHSMM;tempvector[[3]]<-ID; tempvector[[4]]<-tempdata
    return(tempvector);
}