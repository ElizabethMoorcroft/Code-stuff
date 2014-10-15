
dis0 <-which(Data$Dist_Difference==0)

plot(0,0,type="n",
  ylim=c(minlat,maxlat+0.5),xlim=c(minlong,maxlong),
  ylab="Latitude",xlab="Longitude"
	)
legend(y=maxlat+0.5,x=minlong,
       legend=Animals,
       col=COL[1:length(Animals)],
       pch=1:length(Animals),
       cex=0.7
)
for(i in 1:length(dis0)){
  data<-Data[c(dis0[i]-1,dis0[i]),]
  c<-which( Animals %in% unique(data$AnimID) )
  print(paste(i, c, unique(data$AnimID)))
	points(x=data$Longitude,y=data$Latitude,col=COL[c],pch=c)
}

#---


timegtw <-which(Data$Time_Difference>24*60*60*7)

plot(0,0,type="n",
  ylim=c(minlat,maxlat+0.5),xlim=c(minlong,maxlong),
  ylab="Latitude",xlab="Longitude"
  )
legend(y=maxlat+0.5,x=minlong,
       legend=Animals,
       col=COL[1:length(Animals)],
       pch=1:length(Animals),
       cex=0.7
)
for(i in 1:length(timegtw)){
  data<-Data[c(timegtw[i]-1,timegtw[i]),]
  c<-which( Animals %in% unique(data$AnimID) )
  print(paste(i, c, unique(data$AnimID)))
	points(x=data$Longitude,y=data$Latitude,col=COL[c],type="l")
}