setwd('/Users/Yang/Downloads')
cases = read.csv('cases.csv',header=F)
rownames(cases) <- cases[,1]
cases <- cases [,2:313]
colnames(cases) <- c(1:312)
weeks <- c(1,11,21,31,41,52)
require(slam)
results <- matrix(rep(NA,6*26),ncol=6)
for (j in 1:6){
  temp <- matrix(rep(NA,6*26),ncol=6)
  for (i in 1:6){
    temp[,i]<-cases[,(weeks[j]+(i-1)*52)]
  }
  results[,j] <- row_means(temp)
}
rownames(results)<-rownames(cases)
write.csv(results,'six-point-cases.csv')
