#Load libraries
library(data.table)
library(magrittr)

#Load data and aggregate
setwd("/Users/Desmond/Desktop/Work/503 Project/AllData")
temp = list.files(pattern = "*.csv")
allfiles = lapply(temp,fread)



