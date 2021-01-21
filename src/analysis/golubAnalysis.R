setwd('..')
setwd('..')

datadir = "data/cleaned"
df1 = read.table(file=paste(datadir,"golub1",sep='/'))
df2 = read.table(file=paste(datadir,"golub2",sep='/'))

g1 = 3:22
g2 = 23:36
t.stat = apply(X=df1, MARGIN=1, FUN=function(X){t.test(as.numeric(X[g1]),as.numeric(X[g2]))$statistic})
df = apply(X=df1, MARGIN=1, FUN=function(X){t.test(as.numeric(X[g1]),as.numeric(X[g2]))$parameter})
p.value = 2*(1 - pnorm(abs(t.stat)))
#check for the variance of two groups if they are equal or differnt
#Apply quantile transformation to transform all t statistics to normal z-scores


p = seq(0,1,by=0.01)
hist(p.value, freq=F)
lines(p, dunif(p), lwd=2, col='red')

x = seq(-8,8,by=0.1)
hist(t.stat, breaks=x, freq=F) 
lines(x, dnorm(x), lwd=2, col='red')

g1 = 3:29
g2 = 30:40
t.stat = apply(X=df2, MARGIN=1, FUN=function(X){t.test(as.numeric(X[g1]),as.numeric(X[g2]))$statistic})
p.value = 2*(1 - pnorm(abs(t.stat)))

p = seq(0,1,by=0.01)
hist(p.value, freq=F)
lines(p, dunif(p), lwd=2, col='red')

x = seq(-8,8,by=0.1)
hist(t.stat, breaks=x, freq=F) 
lines(x, dnorm(x), lwd=2, col='red')




