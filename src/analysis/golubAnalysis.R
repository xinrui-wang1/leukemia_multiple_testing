train_stats <- function(data) {
  #find the t statistics, degrees of freedom, and p values of the training set
  g1 = 3:29 #ALL patients
  g2 = 30:40 #AML patients
  t.stat = apply(X=data, MARGIN=1, FUN=function(X){t.test(as.numeric(X[g1]),as.numeric(X[g2]))$statistic})
  df = apply(X=data, MARGIN=1, FUN=function(X){t.test(as.numeric(X[g1]),as.numeric(X[g2]))$parameter})
  p.value = apply(X=data, MARGIN=1, FUN=function(X){t.test(as.numeric(X[g1]),as.numeric(X[g2]))$p.value})
  train_list = list(t.stat, df, p.value)
  names(train_list) = c('t.stat','df','p.value')
  return(train_list)
}

test_stats <- function(data) {
  #find the t statistics, degrees of freedom, and p values of the test set
  g1 = 3:22 #ALL
  g2 = 23:36 #AML
  t.stat = apply(X=data, MARGIN=1, FUN=function(X){t.test(as.numeric(X[g1]),as.numeric(X[g2]))$statistic})
  df = apply(X=data, MARGIN=1, FUN=function(X){t.test(as.numeric(X[g1]),as.numeric(X[g2]))$parameter})
  p.value = apply(X=df2, MARGIN=1, FUN=function(X){t.test(as.numeric(X[g1]),as.numeric(X[g2]))$p.value})
  test_list = list(t.stat, df, p.value)
  names(test_list) = c('t.stat','df','p.value')
  return(test_list)
}

transform_data <- function(data, train) { #takes data as input, outputs the z-scores
  if (train == TRUE) {
    stats = train_stats(data)
  }
  else {
    stats = test_stats(data)
  }
  t.stat = stats$t.stat
  df = stats$df
  areas = 1 - pt(t.stat, df = df) #area to the right of each t statistic (with given degrees of freedom)
  z.scores = qnorm(areas) #respective z scores for each area (t statistic density)
  return(z.scores)
}

qq_plot <- function(data, outdir, train, transformed = FALSE) {
  #create qq plots, if transformed = TRUE then create plot for z scores
  if (transformed == TRUE) {
    jpeg(paste(outdir,'qq_plot_transformed.jpg',sep='/'))
    z.scores = transform_data(data, train)
    qqnorm(z.scores, main=' QQ plot for transformed z scores')
    abline(0,1,lwd=2,col='red')
  }
  else {
    if (train==TRUE) {
      jpeg(paste(outdir, 'qq_plot_train', sep='/'))
      x = train_stats(data)$t.stat
      qqnorm(x, main= ' QQ plot for t statistics')
      abline(0,1,lwd=2,col='red')
    }
    else {
      jpeg(paste(outdir, 'qq_plot_test', sep='/'))
      x = test_stats(data)$t.stat
      qqnorm(x, main= ' QQ plot for t statistics')
      abline(0,1,lwd=2,col='red')
    }
  }
  
  dev.off()
}

hist_p <- function(data, outdir, train) {
  #create histogram for the p-value
  if (train == TRUE) {
    jpeg(paste(outdir,'train_tstat_hist.jpg',sep='/'))
    p.value = train_stats(data)$p.value
  }
  else {
    jpeg(paste(outdir,'test_tstat_hist.jpg',sep='/'))
    p.value = test_stats(data)$p.value
  }
  
  p = seq(0,1,by=0.01)
  hist(p.value, freq=F)
  lines(p, dunif(p), lwd=2, col='red')
  
  dev.off()
}

hist_tstat <- function(data,outdir,train) {
  #create histogram for tstat
  if (train == TRUE) {
    jpeg(paste(outdir,'train_tstat_hist.jpg',sep='/'))
    t.stat = train_stats(data)$t.stat
  }
  else {
    jpeg(paste(outdir,'test_tstat_hist.jpg',sep='/'))
    t.stat = test_stats(data)$t.stat
  }
  x = seq(-5,5,by=0.1)
  hist(t.stat, breaks=x, freq=F) 
  lines(x, dnorm(x), lwd=2, col='red')
  
  dev.off()
}

generate_plots_golub <- function(data, outdir, train) {
  #generate plots and save in output directory
  hist_tstat(data, outdir,train)
  hist_p(data, outdir, train)
  qq_plot(data, outdir, train)
  qq_plot(data, outdir, train, transformed=TRUE)
}
