library(matrixStats)
library(data.table)
library(bestNormalize)
setwd('/Users/jacob/documents/github/dsc180b-capstone/')
datadir = "data/cleaned"
train = read.csv(file=paste(datadir,"train.csv",sep='/'))
test = read.csv(file=paste(datadir,"test.csv",sep='/'))
actual = read.csv("data/raw/actual.csv")
combined = read.csv(file=paste(datadir,"golub_combined.csv",sep='/'))

#get boolean lists for each data set of which patients had which cancer
ALLtrainBool = (actual$cancer == 'ALL')[0:(ncol(train)-2)]
AMLtrainBool = !ALLtrainBool

#index the data with the boolean lists to get the ALL and AML data
ALLtrain = train[3:ncol(train)][ALLtrainBool]
AMLtrain = train[3:ncol(train)][AMLtrainBool]

t.stat = apply(X=train, MARGIN=1, FUN=function(X){t.test(as.numeric(X[ALLtrainBool]),as.numeric(X[AMLtrainBool]))$statistic})
df = apply(X=train, MARGIN=1, FUN=function(X){t.test(as.numeric(X[ALLtrainBool]),as.numeric(X[AMLtrainBool]))$parameter})
p.value = 2*(1 - pnorm(abs(t.stat)))


alpha = 0.2
m = length(p.value)
p.value = sort(p.value)
kBH = alpha * (1:m)/m
which(p.value <= kBH)
plot(p.value)
lines(kBH, lty=2)
plot(p.value[1:10])
lines(kBH, lty=2)
s = sample(1:m, 1000)
Rho = cor(t(combined[s,]))
rho = Rho[which(lower.tri(R))]
hist(rho, breaks=50)
x = seq(-8,8,0.1)
hist(t.stat, breaks=x)
lines(x, m*0.1*dt(x, df=32), lty=1, col=2)
p0 = 1
P0 = p.value
Phat = ecdf(p.value)(p.value)
FDRhat = p0 * P0 / pmax(Phat,1/m)
plot(p.value[1:10], m*Phat[1:10], "b", ylim=c(0,10))
lines(p.value[1:10],m*P0[1:10], lty=2)
legend(0,10,c("m*Phat","m*P0"),lty=c(1,2))
plot(p.value[1:10], FDRhat[1:10],"b")
abline(a=alpha0, b=0, lty=2)



#check for the variance of two groups if they are equal or different
#Apply quantile transformation to transform all t statistics to normal z-scores

plot(df)
p = seq(0,1,by=0.01)
hist(p.value, freq=F)
lines(p, dunif(p), lwd=2, col='red')

x = seq(-8,8,by=0.1)
hist(t.stat, breaks=x, freq=F) 
lines(x, dnorm(x), lwd=2, col='red')

#read more on order norm
#every t-statistic has a different number of df, transformation should be different for each
#if this works then next we can simulate the null proportion
orderNorm_obj = orderNorm(t.stat)
new_t.stat = predict(orderNorm_obj)
x = seq(-8,8,by=0.1)
hist(new_t.stat, breaks=x, freq=F) 
lines(x, dnorm(x), lwd=2, col='red')

#turn each group into a matrix and calculate row-wise variance of each gene
ALLtrainVars = rowVars(as.matrix(ALLtrain))
AMLtrainVars = rowVars(as.matrix(AMLtrain))
#find the difference in variance between each respective gene by group
varDiffs = ALLtrainVars - AMLtrainVars
varRatios = ALLtrainVars / AMLtrainVars
plot(varDiffs)
plot(varRatios)

#t test on each gene by group
t.stat = apply(X=transformed, MARGIN=1, FUN=function(X){t.test(as.numeric(X[ALLtrainBool]),as.numeric(X[AMLtrainBool]))$statistic})
#degrees of freedom by gene
df = apply(X=transformed, MARGIN=1, FUN=function(X){t.test(as.numeric(X[ALLtrainBool]),as.numeric(X[AMLtrainBool]))$parameter})
p.value = 2*(1 - pnorm(abs(t.stat)))

plot(df)
p = seq(0,1,by=0.01)
hist(p.value, freq=F)
lines(p, dunif(p), lwd=2, col='red')

x = seq(-8,8,by=0.1)
hist(t.stat, breaks=x, freq=F) 
lines(x, dnorm(x), lwd=2, col='red')
