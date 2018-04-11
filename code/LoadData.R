#Load relevant libraries
library(plyr)
library(dplyr)
library(data.table)

#This Pulls the initial csv files exported from Kaggle. 
#Probably unnecessary to include the root code used to generate the csv files themselves.
setwd("/Users/Desmond/Desktop/Work/503 Project")

#Pull 2015 data
setwd("./2015")
temp = list.files(pattern = "*.csv")
allfiles = lapply(temp,fread)
allfiles = lapply(allfiles, function(x) x = x[,-1])

#Data.table merge
require(data.table)
keep = union(names(allfiles[[1]]),names(allfiles[[2]]))
master2015 = allfiles[[1]][allfiles[[2]],mget(keep),on="consecutive_number"]
for (i in 3:7){
  keep = union(names(master2015),names(allfiles[[i]]))
  master2015 = master2015[allfiles[[i]],mget(keep),on="consecutive_number",allow.cartesian=TRUE]
}


#Merge/combine data
master2015 = join(allfiles[[1]],allfiles[[2]],by="consecutive_number",type='left',match='all')
for(i in 3:7){
  master2015 = join(master2015,allfiles[[i]],by="consecutive_number",type='left',match='all')
  master2015 = master2015[,!duplicated(colnames(master2015),fromLast=TRUE)]
}
