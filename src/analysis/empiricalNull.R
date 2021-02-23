source('golubAnalysis.R')

df1 <- read.table('data/cleaned/golub2')
g1 = 3:22 #ALL patients
g2 = 23:36 #AML patients
t.stat = apply(X=df1, MARGIN=1, FUN=function(X){t.test(as.numeric(X[g1]),as.numeric(X[g2]))$statistic})
df = apply(X=df1, MARGIN=1, FUN=function(X){t.test(as.numeric(X[g1]),as.numeric(X[g2]))$parameter})
p.value = apply(X=df1, MARGIN=1, FUN=function(X){t.test(as.numeric(X[g1]),as.numeric(X[g2]))$p.value})
train_list = list(t.stat, df, p.value)
names(train_list) = c('t.stat','df','p.value')

areas = pt(t.stat, df = df) #area to the right of each t statistic (with given degrees of freedom)
z.scores = qnorm(areas) #respective z scores for each area (t statistic density)

p = seq(0,1,by=0.01)
hist(p.value, freq=F)
lines(p, dunif(p), lwd=2, col='red')

x = seq(-8,8,by=0.1)
hist(t.stat, 100, prob=T) 
lines(x, dnorm(x), lwd=2, col='red')

x = seq(-5,5,by=0.1)
hist(z.scores, 100, prob=T) 
lines(x, dnorm(x), lwd=2, col='red')

z.index = c()
interval = 0.1
for (i in 1:length(z.scores)) {
  if (z.scores[i] > -interval & z.scores[i] < interval)
    z.index <- append(z.index, i)
}
intervalZ <- z.scores[z.index]

z.density <- density(z.scores)
f = c()
for (z in sort(intervalZ)) {
  temp  <- approx(z.density$x, z.density$y, xout=z)$y
  f <- append(f, temp)
}
hist(z.scores, 100, prob=T)
lines(z.density, col='red')

plot(sort(intervalZ), log(f), ylim=c(-1.3, -0.8))
lines(sort(intervalZ), log(dnorm(sort(intervalZ))), col='red')

p0 = exp(mean(log(f) - log(dnorm(sort(intervalZ)))))
p0
