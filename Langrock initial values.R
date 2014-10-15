## Source values for Langrock analysis
  ## initial parameter values to be used in the numerical maximization (several different combinations should be tried in order to ensure hitting the global maximum)
  a0<-c(0.8,1.3) # Weibull shape parameters
  b0<-c(0.02,1.3) # Weibull scale parameters
  kappa0<-c(0.1,0.2) # wrapped Cauchy concentration parameters
  co0<-c(2.2,0.7) # wrapped Cauchy mean parameters
  gamma0<-c(0.4,0.7) # diagonal entries of the transition probability matrix

  ## run the numerical maximization
  stepm <- 35
  
  m <- c(30,30)
