# DSC180B-Capstone-Project
- data download: in the command line enter Rscript run-data.R data
- analysis: in the command line enter Rscript run-data.R analysis
  - the resulting graphs will be in the data/out folder 

- data: contains the raw and cleaned versions of the datasets we're working with. Also will hold the graphs from analysis
- src: contains the analysis, cleaning, and data etl scripts.
  - analysis: golubAnalysis.R contains the script we used to do tests and generate plots
  - cleaning: golubCleaning.R contains the script we used to clean the raw datasets found in data/raw
  - data: etl.R contains the scirpt to extract the data for run-data.R
