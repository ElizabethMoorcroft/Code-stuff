#--------------------------------
# Project title: Snow leopards
# Script title: Snow Leopard cleaning collars
# Purpose: To look at the collar data 
#--------------------------------

#-Directories
DataDir<- "/Volumes/elizabethmoorcroft/Documents/PhD stuff/Snow leopard/Data"
CodeDir<- "/Volumes/elizabethmoorcroft/Documents/PhD stuff/Snow leopard"
SaveDir<- "/Volumes/elizabethmoorcroft/Documents/PhD stuff/Snow leopard/Graphs"

#-Libraries
library(RColorBrewer)
library(maps)

#-Colours
COL<-c(brewer.pal(n=12, name="Set3"),brewer.pal(n=9, name="Set1"),"black")

#-Read in data
setwd(DataDir)
Data<-read.csv(paste("CleanedData",Sys.Date(),".csv",sep=""))
#Data<-read.csv(paste("CleanedData","2013-12-",".csv",sep=""))

#List of animal IDs
Animals<-unique(Data$AnimID)

setwd(SaveDir)
pdf("CollarUsage.pdf")
plot(as.factor(Data$AnimID),
    as.Date(Data$Date_Time),
    sub="All Collars ",
    ylab="Date",
    xlab="Animal",
    xaxt = "n",
    type="n"
)
for(k in 1:length(Animals)){
  d_individ<-Data[which(Data$AnimID==Animals[k]),]
  print(paste(Animals[k],",",k,"=", dim(d_individ)[1]))
  points(as.factor(d_individ$AnimID),
         as.Date(d_individ$Date_Time),
        pch=16,
        col=COL[k]
        )
}
axis(side=1,
  at=as.numeric(unique(Data$AnimID)),
  labels =unique(Data$AnimID),
  las=2  
)
box()
dev.off()


# Collar ID 
# Is Collar_ID "0" an ID or a lack of ID? All other collars have a 4-5 digit number
# Is collar "0" on multiple animals at once? #Yes
collarIDs<-unique(Data$Collar_ID)
for(i in 1:length(collarIDs)){
  data<-Data[which(Data$Collar_ID==collarIDs[i]),]
  t<-table(data$AnimID)
  l<-length(t[which(t>0)])
  n<-names(t[which(t>0)])
  m<-matrix(ncol=2,nrow=l)
  
  #plots of collar usage
  setwd(SaveDir)
  pdf(paste("CollarUsage",collarIDs[i],".pdf",sep=""))
  plot(as.factor(data$AnimID),
       as.Date(data$Date_Time),
       sub=paste("Collar ID:", collarIDs[i]),
       ylab="Date",
       xlab="Animal",
       xaxt = "n",
       #pch=16,
       #col=COL[which(Animals %in% data$AnimID )]
       type="n"
  )
  for(k in 1:l){
    d_individ<-data[which(data$AnimID==n[k]),]
    points(as.factor(d_individ$AnimID),
          as.Date(d_individ$Date_Time),
          pch=16,
          col=COL[which( Animals %in% n[k])])
  }
  axis(side=1,
       at=as.numeric(unique(data$AnimID)),
       labels =unique(data$AnimID))
  box()
  dev.off()
  # Calculate the min and maximum collar dates
  for(j in 1:l){
    m[j,1]<-min(as.Date(Difference[which(Difference$Collar_ID==collarIDs[i] & Difference$AnimID==n[j]),]$Date_Time))
    m[j,2]<-max(as.Date(Difference[which(Difference$Collar_ID==collarIDs[i] & Difference$AnimID==n[j]),]$Date_Time))
  }
}