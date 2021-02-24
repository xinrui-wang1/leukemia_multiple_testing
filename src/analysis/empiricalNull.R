source("src/analysis/GolubAnalysis.R")
#Load dataset
df1 <- read.table('data/cleaned/golub1')
df2 <- read.table('data/cleaned/golub2')

estimate_p0 <- function(z.scores) {
  #Estimate p0 to fit the empirical null distribution
  z.index = c()
  interval = 0.2
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
  
  p0 = exp(mean(log(f) - log(dnorm(sort(intervalZ)))))
  return(p0)
}

estimate_p0(transform_data(df1, FALSE))
estimate_p0(transform_data(df2, TRUE))



t.stat = test_stats(df1)$t.stat
p.value = test_stats(df1)$p.value
z.scores = transform_data(df1,FALSE)

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
interval = 0.2
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

plot(sort(intervalZ), log(f), ylim=c(-1.5, -0.7))
lines(sort(intervalZ), log(dnorm(sort(intervalZ))), col='red')

p0 = exp(mean(log(f) - log(dnorm(sort(intervalZ)))))
p0
