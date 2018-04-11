#Load relevant libraries
library(plyr)
library(dplyr)
library(data.table)

#This Pulls the initial csv files exported from Kaggle. 
#Probably unnecessary to include the root code used to generate the csv files themselves.
setwd("/Users/Desmond/Desktop/Work/503 Project")
datasetnames = c("accident.csv",
                 "cevent.csv",
                 "damage.csv",
                 "distract.csv",
                 "drimpair.csv",
                 "factor.csv",
                 "maneuver.csv",
                 "nmcrash.csv",
                 "nmimpair.csv",
                 "nmprior.csv",
                 "parkwork.csv",
                 "pbtype.csv",
                 "person.csv",
                 "safetyeq.csv",
                 "vehicle.csv",
                 "vevent.csv",
                 "vindecode.csv",
                 "violatn.csv",
                 "vision.csv",
                 "vsoe.csv")

#Pull 2015 data
setwd("./2015")
temp = list.files(pattern = "*.csv")
files_2015 = lapply(temp,fread)
files_2015 = lapply(files_2015, function(x) x = cbind(Year=rep("2015",nrow(x)), x[,-1]))

#Pull2016 data
setwd("../2016")
temp = list.files(pattern = "*.csv")
files_2016 = lapply(temp,fread)
files_2016 = lapply(files_2016, function(x) x = cbind(Year=rep("2016",nrow(x)),x[,-1]))

allfiles = list()

for(i in 1:20){
  allfiles[[i]] = rbind(files_2015[[i]],files_2016[[i]])
}
names(allfiles) = datasetnames

setwd("/Users/Desmond/Desktop/Work/503 Project/AllData")
for(i in 1:length(allfiles)){
  write.csv(allfiles[[i]],file=names(allfiles)[i],row.names=FALSE)
}
zip("/Users/Desmond/Documents/GitHub/503Project/data/AllData",datasetnames)

