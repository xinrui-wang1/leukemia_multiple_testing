setwd('..')
setwd('..')

datadir = "data/raw/"
outpath = "data/cleaned"
outdir = "data/out"

clean_data <- function(filename){
  df <- read.table(file=paste(datadir,filename,sep='/'), skip=1, nrows=100000)
  df <- subset(df,select=c(V3,V6))
  names(df)[names(df) == "V3"] <- "marker_name"
  names(df)[names(df) == "V6"] <- "p_value"
  
  #create histogram for the p-values
  jpeg(paste(outdir,'alzheimer_pval_hist.jpg',sep = '/'))
  
  hist(df$p_value)
  dev.off()
 
  write.table(df, paste(outpath,filename,sep='/'),
              row.names = TRUE, col.names = TRUE) 
}

clean_data("alzheimers1.txt")

df <- read.table(file=paste(outpath,"alzheimers1.txt",sep='/'))

hist(df$p_value)

