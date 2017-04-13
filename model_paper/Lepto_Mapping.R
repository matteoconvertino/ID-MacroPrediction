require(rgdal)
require(SDMTools)
require(raster)
require(reshape)
require(slam)
require(GISTools)
require(rgeos)
require(ggplot2)
year <- c(rep(2009,12),rep(2010,12),rep(2011,12),rep(2012,12),rep(2013,12))
month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
month <- rep(month,5)
tags <- paste(year,month)
setwd('/Users/Yang/Google Drive/Lepto/LKA_adm')
province <- readOGR(".","LKA_adm1")
plot(province)
setwd('/Users/Yang/Google Drive/Lepto')
pre <- read.csv('LKA_pre.csv')
pre <- pre[,(397-59):397]
shades <- auto.shading(pre, n = 9, cutter = rangeCuts, cols = brewer.pal(9, "Blues"))
setwd('/Users/Yang/Google Drive/Lepto')
case <- read.csv('SriLankaMonthly200914.csv',header=F)
rownames(case) <- case[,1]
case <- case[,-1]
case <- case[,1:60]
province_0 <- gCentroid(province,byid=T)

base <- seq(0.5,2.5,2/10)
mark <- seq(0,1,0.1)
temp <- melt(case)[,2]
temp <- temp[temp!=0]
quantile(temp,probs=mark)

convert <- function(vec){
  n <- length(vec)
  output <- rep(NA,n)
  for (i in 1:n){
    if(vec[i]==0){
      output[i]<-0
    } else if(vec[i]<=1){
      output[i]<-base[1]
    }else if(vec[i]<=2&&vec[i]>1){
      output[i]<-base[2]
    }else if(vec[i]<=3&&vec[i]>2){
      output[i]<-base[3]
    }else if(vec[i]<=5&&vec[i]>3){
      output[i]<-base[4]
    }else if(vec[i]<=7&&vec[i]>5){
      output[i]<-base[5]
    }else if(vec[i]<=9&&vec[i]>7){
      output[i]<-base[6]
    }else if(vec[i]<=13&&vec[i]>9){
      output[i]<-base[7]
    }else if(vec[i]<=19&&vec[i]>13){
      output[i]<-base[8]
    }else if(vec[i]<=27&&vec[i]>19){
      output[i]<-base[9]
    }else if(vec[i]<=42&&vec[i]>27){
      output[i]<-base[10]
    }else if(vec[i]<=745&&vec[i]>42){
      output[i]<-base[11]
    }else next
  }
  return(output)
}

setwd('/Users/Yang/Google Drive/Lepto/Maps/pre_case/EPS')
for (i in 1:60){
setEPS()
name <- paste(i,'-',tags[i],'.eps',sep="")
postscript(name)
size <- convert(case[,i])
choropleth(province,pre[,i],shades)
points(y=province_0$y,x=province_0$x,pch=16,cex=size,col='red')
main <- paste("Sri Lanka", tags[i])
title(main)
choro.legend(82.05,9,shades,cex=0.8,x.intersp=0,bty='n')
points(82,9.5,pch=16,cex=0.5,col='red')
text(82.6,9.5,'Lowest 10%, Inc',cex=0.8)
points(82,9.3,pch=16,cex=1.5,col='red')
text(82.6,9.3,'Middle 10%, Inc',cex=0.8)
points(82,9.1,pch=16,cex=2.5,col='red')
text(82.6,9.1,'Highest 10%, Inc',cex=0.8)
dev.off()}

setwd('/Users/Yang/Google Drive/Lepto/Maps/pre_case/PNG')
for (i in 1:60){
  png(paste(i,'-',tags[i],'.png',sep=''))
  size <- convert(case[,i])
  choropleth(province,pre[,i],shades)
  points(y=province_0$y,x=province_0$x,pch=16,cex=size,col='red')
  main <- paste("Sri Lanka", tags[i])
  title(main)
  choro.legend(82.05,9,shades,cex=0.8,x.intersp=0,bty='n')
  points(82,9.5,pch=16,cex=0.5,col='red')
  text(82.6,9.5,'Lowest 10%, Inc',cex=0.8)
  points(82,9.3,pch=16,cex=1.5,col='red')
  text(82.6,9.3,'Middle 10%, Inc',cex=0.8)
  points(82,9.1,pch=16,cex=2.5,col='red')
  text(82.6,9.1,'Highest 10%, Inc',cex=0.8)
  dev.off()}

seasonal_pre <- matrix(rep(NA,25*12),ncol=12)
seasonal_pre <- as.data.frame(seasonal_pre)
mark <- c(1,1+12,1+12*2,1+12*3,1+12*4)
for (i in 1:12){
  seasonal_pre[,i]<- rowMeans(pre[,(mark+(i-1))])
}
seasonal_case <- matrix(rep(NA,25*12),ncol=12)
seasonal_case <- as.data.frame(seasonal_case)
for (i in 1:12){
  seasonal_case[,i]<- rowMeans(case[,(mark+(i-1))])
}

base <- seq(0.5,2.5,2/10)
mark <- seq(0,1,0.1)
temp <- melt(seasonal_case)[,2]
temp <- temp[temp!=0]
bound <- quantile(temp,probs=mark)

convert <- function(vec,bound){
  n <- length(vec)
  output <- rep(NA,n)
  for (i in 1:n){
    if(vec[i]==0){
      output[i]<-0
    } else if(vec[i]<=bound[1]){
      output[i]<-base[1]
    }else if(vec[i]<=bound[2]&&vec[i]>bound[1]){
      output[i]<-base[2]
    }else if(vec[i]<=bound[3]&&vec[i]>bound[2]){
      output[i]<-base[3]
    }else if(vec[i]<=bound[4]&&vec[i]>bound[3]){
      output[i]<-base[4]
    }else if(vec[i]<=bound[5]&&vec[i]>bound[4]){
      output[i]<-base[5]
    }else if(vec[i]<=bound[6]&&vec[i]>bound[5]){
      output[i]<-base[6]
    }else if(vec[i]<=bound[7]&&vec[i]>bound[6]){
      output[i]<-base[7]
    }else if(vec[i]<=bound[8]&&vec[i]>bound[7]){
      output[i]<-base[8]
    }else if(vec[i]<=bound[9]&&vec[i]>bound[8]){
      output[i]<-base[9]
    }else if(vec[i]<=bound[10]&&vec[i]>bound[9]){
      output[i]<-base[10]
    }else if(vec[i]<=bound[11]&&vec[i]>bound[10]){
      output[i]<-base[11]
    }else next
  }
  return(output)
}

setwd('/Users/Yang/Google Drive/Lepto/Maps/pre_case/season_EPS')
for (i in 1:12){
  setEPS()
  name <- paste(i,'-',month[i],'.eps',sep="")
  postscript(name)
  size <- convert(seasonal_case[,i],bound)
  choropleth(province,seasonal_pre[,i],shades)
  points(y=province_0$y,x=province_0$x,pch=16,cex=size,col='red')
  main <- paste("Sri Lanka", month[i])
  title(main)
  choro.legend(82.05,9,shades,cex=0.8,x.intersp=0,bty='n')
  points(82,9.5,pch=16,cex=0.5,col='red')
  text(82.6,9.5,'Lowest 10%, Inc',cex=0.8)
  points(82,9.3,pch=16,cex=1.5,col='red')
  text(82.6,9.3,'Middle 10%, Inc',cex=0.8)
  points(82,9.1,pch=16,cex=2.5,col='red')
  text(82.6,9.1,'Highest 10%, Inc',cex=0.8)
  dev.off()}

setwd('/Users/Yang/Google Drive/Lepto/Maps/pre_case/season_PNG')
for (i in 1:12){
  png(paste(i,'-',month[i],'.png',sep=''))
  size <- convert(seasonal_case[,i],bound)
  choropleth(province,seasonal_pre[,i],shades)
  points(y=province_0$y,x=province_0$x,pch=16,cex=size,col='red')
  main <- paste("Sri Lanka", month[i])
  title(main)
  choro.legend(82.05,9,shades,cex=0.8,x.intersp=0,bty='n')
  points(82,9.5,pch=16,cex=0.5,col='red')
  text(82.6,9.5,'Lowest 10%, Inc',cex=0.8)
  points(82,9.3,pch=16,cex=1.5,col='red')
  text(82.6,9.3,'Middle 10%, Inc',cex=0.8)
  points(82,9.1,pch=16,cex=2.5,col='red')
  text(82.6,9.1,'Highest 10%, Inc',cex=0.8)
  dev.off()}

setwd('/Users/Yang/Google Drive/Lepto/Maps/pre_case/PNG')
my_command <- 'convert -delay 30 -loop 0 *.png  pre_case_ts.gif'
system(my_command)


setwd('/Users/Yang/Google Drive/Lepto/Maps/pre_case/season_PNG')
my_command <- 'convert -delay 30 -loop 0 *.png  pre_case_season_ts.gif'
system(my_command)
