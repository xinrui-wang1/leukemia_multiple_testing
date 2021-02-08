#etl.R contains functions used to extract data from different datasets
source("src/cleaning/GolubCleaning.R")
clean_data <- function(data, datadir, outpath=NA) {
  data <- read.table(datadir)
}

get_data <- function(data, datadir, outpath=NA) {
  # access the selected data set
  data <- read.table(datadir)
  
  if(is.na(outpath)) {
    return(data)
  }
  else {
    write(data, outpath, sep = "\t")
  }
}

