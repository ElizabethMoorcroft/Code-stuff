#--------------------------------
# Project title: Snow leopards
# Script title: Snow Leopard Polar Plota
# Purpose: To produce circular graphs
#--------------------------------

PlotCircularAll<-function(Data,round,COL,Radius){
  t<-table(round(Data,round))/length(Data)
  polar.plot(as.vector(t*100),as.numeric(names(t))*180/pi,main=NULL,lwd=1,line.col=COL,rp.type="polygon",radial.lim=c(0,Radius))
}

PlotCircularGraphsSex<-function(Data,round,COLS,VARIABLE,Radius){
    op<-par(oma=c(1, 1, 1, 1) + 0.1)
    d<-Data[which(Data$Sex =="F"),which(names(Data) == VARIABLE)]
    d<-d[which( !is.na(d))]
    PlotCircularGraphs(d,round,COLS[1],F,Radius)
    
    d<-Data[which(Data$Sex =="M"),which(names(Data) == VARIABLE)]
    d<-d[which( !is.na(d))]
    PlotCircularGraphs(d,round,COLS[2],T,Radius)

    par(op) # Leave the last plot
    op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)
    legend(x=0.8,y=1,c("Female","Male"),col=COLS[1:2],lty=1,lwd=2)
}

PlotCircularGraphs<-function(Data,round,COL,ADD,Radius){
  data<-Data[!is.na(Data)]
  t<-table(round(data,round))/length(data)
  polar.plot(as.vector(t*100),as.numeric(names(t))*180/pi,main=NULL,lwd=1,line.col=COL,rp.type="polygon",add=ADD,radial.lim=c(0,Radius),radial.labels="")
}

PlotCircularGraphsIndivid<-function(Data,round,COLS,VARIABLE,Radius){
  anid<-sort(unique(Data$AnimID))
  #PlotCircularGraphs(Data,round,COLS[length(anid)+1],F)
  
  for(i in 1:length(anid)){
    d<-Data[which(Data$AnimID ==anid[i]),which(names(Data) == VARIABLE)]
    d<-d[which( !is.na(d))]
    if(i>1){
      PlotCircularGraphs(d,round,COLS[i],T,Radius)
    }else{
      op<-par(oma=c(1, 1, 1, 1) + 0.1)
      PlotCircularGraphs(d,round,COLS[i],F,Radius)}
  }
  par(op) # Leave the last plot
  op <- par(usr=c(0,1,0,1), # Reset the coordinates
          xpd=NA)
  legend(x=0.8,y=1,anid,col=COLS[1:length(anid)],lty=1,lwd=2)
}