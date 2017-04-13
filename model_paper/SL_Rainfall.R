setwd('/Users/Yang/Downloads')
index = list.files()
table = as.data.frame(matrix(rep(NA,72*26),ncol=72))
table[1,]<-as.numeric(read.table('[T+]data.tsv'))
for (i in 1:25){
  temp <- (paste('[T+]data (',i,').tsv',sep=""))
  table[i+1,]<-as.numeric(read.table(temp))
  rm(temp)
}
days <- c(31,  28,  31, 30,  31,  30,  31,  31,  30,  31,  30,  31,
          31,  28,  31, 30,  31,  30,  31,  31,  30,  31,  30,  31,
          31,  28,  31, 30,  31,  30,  31,  31,  30,  31,  30,  31,
          31,  29,  31, 30,  31,  30,  31,  31,  30,  31,  30,  31,
          31,  28,  31, 30,  31,  30,  31,  31,  30,  31,  30,  31,
          31,  28,  31, 30,  31,  30,  31,  31,  30,  31,  30,  31)
total <- as.data.frame(matrix(rep(NA,72*26),ncol=72))
for (i in 1:26){
  total[i,] <- table[i,]*days
}
row.names(total) <- district
write.csv(total,"SL_rainfall_district.csv",sep=" ")

#categorization
L_EPI <- c("Colombo","Kurunegala","Gampaha","Hambantota","Kalutara")
S_EPI <- c("Anuradhapura","Moneragala","Trincomalee")
M <- c("Matara","Polonnaruwa")
END <- c("Ratnapura","Kegalle","Galle")

par(mar=c(2,2,2,1))
par(mfrow=c(5,1))
for (i in 1:5){
  plot(as.numeric(total[which(row.names(total)==L_EPI[i]),]),type='l',lwd=2,ylim=c(0,650),main=L_EPI[i],xaxt='n')
  axis(1,at=c(0,12,24,36,48,60,72),labels=c(2009,2010,2011,2012,2013,2014,2015))
}

par(mfrow=c(3,1))
for (i in 1:3){
  plot(as.numeric(total[which(row.names(total)==S_EPI[i]),]),type='l',lwd=2,ylim=c(0,650),main=S_EPI[i],xaxt='n')
  axis(1,at=c(0,12,24,36,48,60,72),labels=c(2009,2010,2011,2012,2013,2014,2015))
}

par(mfrow=c(2,1))
for (i in 1:2){
  plot(as.numeric(total[which(row.names(total)==M[i]),]),type='l',lwd=2,ylim=c(0,650),main=M[i],xaxt='n')
  axis(1,at=c(0,12,24,36,48,60,72),labels=c(2009,2010,2011,2012,2013,2014,2015))
}

par(mfrow=c(3,1))
for (i in 1:3){
  plot(as.numeric(total[which(row.names(total)==END[i]),]),type='l',lwd=2,ylim=c(0,650),main=END[i],xaxt='n')
  axis(1,at=c(0,12,24,36,48,60,72),labels=c(2009,2010,2011,2012,2013,2014,2015))
}
