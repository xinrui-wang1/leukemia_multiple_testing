setwd('..')
setwd('..')

datadir = "data/cleaned"
df1 = read.table(file=paste(datadir,"golub1",sep='/'))
df2 = read.table(file=paste(datadir,"golub2",sep='/'))

#test set
g1 = 3:22 #ALL
g2 = 23:36 #AML
t.stat = apply(X=df1, MARGIN=1, FUN=function(X){t.test(as.numeric(X[g1]),as.numeric(X[g2]))$statistic})
df = apply(X=df1, MARGIN=1, FUN=function(X){t.test(as.numeric(X[g1]),as.numeric(X[g2]))$parameter})
p.value = 2*(1 - pnorm(abs(t.stat)))

#p values histogram
p = seq(0,1,by=0.01)
hist(p.value, freq=F)
lines(p, dunif(p), lwd=2, col='red')
#t statistics histograms
x = seq(-8,8,by=0.1)
hist(t.stat, breaks=x, freq=F) 
lines(x, dnorm(x), lwd=2, col='red')

#training set
g1 = 3:29 #ALL
g2 = 30:40 #AML 
t.stat = apply(X=df2, MARGIN=1, FUN=function(X){t.test(as.numeric(X[g1]),as.numeric(X[g2]))$statistic})
df = apply(X=df2, MARGIN=1, FUN=function(X){t.test(as.numeric(X[g1]),as.numeric(X[g2]))$parameter})
#get positive and negative t-stats, orderNorm to normal distribution, attach negative sign to negative ones
#get p.values from t-statistic
p.value = apply(X=df2, MARGIN=1, FUN=function(X){t.test(as.numeric(X[g1]),as.numeric(X[g2]))$p.value}) #p.values are missing signs
#p.value = 2*(1 - pnorm(abs(t.stat))) #not correct, cant apply normal distribution to t-stat
#p values histogram
p = seq(0,1,by=0.01)
hist(p.value, freq=F)
lines(p, dunif(p), lwd=2, col='red')
#t statistics histogram
x = seq(-8,8,by=0.5)
hist(t.stat, breaks=x, freq=F) 
lines(x, dnorm(x), lwd=2, col='red')
#df histogram
hist(df)

#every t-statistic has a different number of df, transformation should be different for each
#different t's have diff df, need to compare to different distributions
#if this works then next we can simulate the null proportion
#map t-density to gaussian z distribution
#make sure areas are equal for each statistic
#how to find value of z such that t-statistic
#area under the density = 1 - pt(t, df=df) = 1 sided p-value
#area under the z curve = 1 - pnorm(z)
#pt(t, df) = pnorm(z); z = qnorm(pt(t, df))
plot(ecdf(t.stat), main=' Empirical cumulative distribution function')
abline(0,1,lwd=2,col='red')
qqnorm(t.stat, main=' QQ plot for t-statistics')
abline(0,1,lwd=2,col="red")
#transform to z scores
areas = 1 - pt(t.stat, df=df) #area to the right of each t statistic (with given degrees of freedom)
z.scores = qnorm(areas) #get respective z scores from each area
qqnorm(z.scores, main=' QQ plot for z scores') #qq plot for normalcy
abline(0,1,lwd=2,col='red')
dhist(z.scores, freq=F)
lines(x, dnorm(x), lwd=2,col='red')
hist(t.stat, freq=F)
lines(x, dnorm(x), lwd=2,col='red')
#cdf calculates area under density function up to certain point


# Expected number of p-values less than a threshold
m = length(train.p.value)
p.expected = p*m
p.obtained = rep(0, length(p))
for(i in 1:length(p)) {
  p.obtained[i] = sum(train.p.value < p[i])
}
plot(p, p.obtained, col='black', type='l', lwd=2, ylab='# p-values')
lines(p, p.expected, type='l', col='red', lwd=2)

# Expected number of t-values greater than a threshold
t.expected = m*2*(1-pnorm(abs(x)))
t.obtained = rep(0, length(x))
for(i in 1:length(x)) {
  t.obtained[i] = sum(abs(t.stat) > abs(x[i]))
}
plot(x, t.obtained, col='black', type='l', lwd=2, ylab='# t-stat')
lines(x, t.expected, type='l', col='red', lwd=2)

# Plot of false discovery rate
plot(p, p.expected/p.obtained, col='black', type='l', lwd=2, ylab='FDR')
plot(x, t.expected/t.obtained, col='black', type='l', lwd=2, ylab='FDR')

#estimation of p0
#p0 should be the fraction of genes that don't correspond to AML (H0)
#need histogram of z scores
#take an interval around 0, find scaling factor that makes it fit
#f(z) = p0 * f0(z) + (1-p0)fA(z)
#what scaling factor makes the brown curve match the histogram
#