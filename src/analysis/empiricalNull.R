df1 <- read.table('data/cleaned/golub1')
g1 = 3:29 #ALL patients
g2 = 30:40 #AML patients
t.stat = apply(X=df1, MARGIN=1, FUN=function(X){t.test(as.numeric(X[g1]),as.numeric(X[g2]))$statistic})
df = apply(X=df1, MARGIN=1, FUN=function(X){t.test(as.numeric(X[g1]),as.numeric(X[g2]))$parameter})
p.value = apply(X=df1, MARGIN=1, FUN=function(X){t.test(as.numeric(X[g1]),as.numeric(X[g2]))$p.value})
train_list = list(t.stat, df, p.value)
names(train_list) = c('t.stat','df','p.value')

areas = 1 - pt(t.stat, df = df) #area to the right of each t statistic (with given degrees of freedom)
z.scores = qnorm(areas) #respective z scores for each area (t statistic density)

p = seq(0,1,by=0.01)
hist(p.value, freq=F)
lines(p, dunif(p), lwd=2, col='red')

x = seq(-8,8,by=0.1)
hist(t.stat, breaks=x, freq=F) 
lines(x, dnorm(x), lwd=2, col='red')

x = seq(-5,5,by=0.1)
hist(z.scores, breaks=x, freq=F) 
lines(x, dnorm(x), lwd=2, col='red')

z.index = c()
for (i in 1:length(z.scores)) {
  if (z.scores[i] > -1 & z.scores[i] < 1)
    z.index <- append(z.index, i)
}
intervalZ <- z.scores[z.index]


z.density <- density(z.scores)
f0 = c()
for (z in intervalZ) {
  temp  <- approx(z.density$x, z.density$y, xout=z)$y
  f0 <- append(f0, temp)
}

plot(intervalZ, log(f0))
lines(sort(intervalZ), log(dnorm(sort(intervalZ))), col='red')

p0 = exp(mean(log(abs(log(f0)-log(dnorm(intervalZ))))))
p0
