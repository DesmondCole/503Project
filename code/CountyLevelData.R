#Load libraries
library(data.table)
library(magrittr)

#Load data and aggregate
setwd("/Users/Desmond/Desktop/Work/503 Project/AllData")
temp = list.files(pattern = "*.csv")
allfiles = lapply(temp,fread)

#Unique IDs
allfiles[[1]] = allfiles[[1]] %>%
  .[]


#County-Level
countydata = allfiles[[1]] %>%
  .[,]


#State-Level






#Time series
library(dtwclust)

