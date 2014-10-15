

####
## Functions
# Selecting a training data set vs validation data
select.train<-function(percent.train,no.of.5hr.blocks,seed){
  no.of.blocks.train<-floor(no.of.5hr.blocks*percent.train/100)
  no.of.blocks.validation<-no.of.5hr.blocks-no.of.blocks.train
  
  all.blocks<-1:no.of.5hr.blocks
  
  set.seed(seed)
  train.block.numbers<-sample(x=all.blocks,size=no.of.blocks.train,replace=FALSE)
  validation.block.numbers<-all.blocks[-train.block.numbers]
  
  return(list(train.block.numbers,validation.block.numbers))
}

# splits the data into training and validation data 
split.data<-function(data,block.numbers){
  movement.matrix<-data[[1]]
  individual.sex<-data[[2]]
  row.numbers<-data[[3]]
  
  training.blocks<-block.numbers[[1]];training.column.numbers<-unlist(lapply(training.blocks,function(x){c(x*2-1,x*2)}))
  validation.blocks<-block.numbers[[2]];validation.column.numbers<-unlist(lapply(validation.blocks,function(x){c(x*2-1,x*2)}))
  
  training.data<-list("train.movement.matrix" = movement.matrix[,training.column.numbers],
                      "train.individual.sex" = individual.sex[training.blocks],
                      "train.row.numbers" = row.numbers[training.blocks]
                      )

  validation.data<-list("valid.movement.matrix" = movement.matrix[,validation.column.numbers],
                        "valid.individual.sex" = individual.sex[validation.blocks],
                        "valid.row.numbers" = row.numbers[validation.blocks]
                      )
  return(list(training.data,validation.data))
}

# Runs the langrock code on the 
run.all.individuals<-function(movement.matrix,no.of.5hr.blocks,parameters){
  
  #Inital values
  Shape <-parameters$Shape
  Scale <-parameters$Scale
  Rho <-parameters$Rho
  Mu <-parameters$Mu
  State <-parameters$State
  StateHSMM <-parameters$StateHSMM 
  
  stepm <- parameters$stepm
  m <-parameters$m
  # lists to save results
  results.of.HMM<-list()
  results.of.HSMM.ini1<-list()
  results.of.HSMM.ini0<-list()
  
  for(i in 1:no.of.5hr.blocks){
    print(paste("i:",i,"/",no.of.5hr.blocks))
    columns<-c((i*2-1),(i*2))
    try(move.HMM.mle(obs=movement.matrix[,columns],a0=Scale,b0=Shape,kappa0=Rho,gamma0=State,co0=Mu,stepm=stepm)->results.of.HMM[[i]])
    ini=1    
    try(move.HSMM.mle(obs=movement.matrix[,columns],a0=Scale,b0=Shape,kappa0=Rho,gam0=StateHSMM,co0=Mu,ini=ini,stepm=stepm,m=m)->moveHSMM)
    moveHSMM->results.of.HSMM.ini1[[i]]
    ini=0
    try(move.HSMM.mle(movement.matrix[,columns],a0=moveHSMM$a ,b0=moveHSMM$b,kappa0=moveHSMM$kappa,gam0=moveHSMM$gam,co0=moveHSMM$co,ini=ini,stepm=stepm,m=m) -> results.of.HSMM.ini0[[i]])  
  }
  
  return(list(  "HMM"=results.of.HMM,
                "HSMM.ini1"=results.of.HSMM.ini1,
                "HSMM"=results.of.HSMM.ini0
              ))
}

# input for data is the HMM, HSMM data for all individuals
# selects the values for the parametr of interst and works out the density for plotting. 
select.values<-function(data,no.of.parameter){ 
  values<-list();density<-list()
  length.parameter<-length(unlist(data[[1]][no.of.parameter]))
  for(parameter.no in 1:length.parameter){
    vals<-unlist(lapply(data, function(x) {unlist(x[no.of.parameter][1])[parameter.no]}))
    values[[parameter.no]]<-vals
    density[[parameter.no]]<-density(vals)
  }
  return(list("values"=values,"density"=density))
}

############ RUN SIMULATED MOVEMENT
##
#parameters<-list(
# "Shape"=c(2,2),
#  "Scale"=c(5,0.1),
#  "Rho"=c(0.2,0.2),
#  "Mu"=c(0.1,3),
#  "State"=c(0.2,0.5),
#  "StateHSMM"=c(1,1,0.5,0.8),
#  "stepm"=35,
#  "m"=c(30,30)
#)
#current.state=1
#N=10000
#movement.matrix<-matrix(ncol=2,nrow=N)
#for(i in 1:N){
#  if(current.state==1){
#    shape<-parameters$Shape[1]
#    scale<-parameters$Scale[1]
#    probs<-parameters$State[1]
#    mu<-parameters$Mu[1]
#    rho<-parameters$Rho[1]
#  }
#  else{
#    shape<-parameters$Shape[2]
#    scale<-parameters$Scale[2]
#    probs<-parameters$State[2]
#    mu<-parameters$Mu[2]
#    rho<-parameters$Rho[2]
#  }
#  
#  movement.matrix[i,1]<-rweibull(n=1,shape=shape,scale=scale)
#  movement.matrix[i,2]<-rwrpcauchy(n=1,location=mu,rho =rho)
#  
#  current.state<-rbinom(1,1,probs)
#}

#set.seed(1)
#inits<-parameters
#inits$Shape<- inits$Shape + rnorm(2,0,2)
#inits$Scale<- inits$Scale + rnorm(2,0,2)
#inits$Mu<- inits$Mu + rnorm(2,0,1)
#inits$Rho<- inits$Rho + runif(2,0,0.5)
#inits$State<-inits$State + runif(2,0,0.5)
#inits

#test.data<-run.all.individuals(movement.matrix,1,inits)
#test.data[[1]][[1]]
