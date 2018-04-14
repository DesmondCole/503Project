#Load libraries
library(data.table)
library(magrittr)
library(ggplot2)
library(dtwclust)

#Load data and aggregate
setwd("/Users/Desmond/Desktop/Work/503 Project/AllData")
temp = list.files(pattern = "*.csv")
allfiles = lapply(temp,fread)
names(allfiles) = temp

setwd("/Users/Desmond/Documents/GitHub/503Project")
TimeData =  allfiles[[1]] %>%
  .[,`:=` (date_of_crash = as.Date(timestamp_of_crash))] %>%
  .[hour_of_crash != 99,
    .(fatalaccidents = .N),
    by=.(date_of_crash,hour_of_crash,day_of_week,
         FIPS,state_name,Population)] %>% 
  .[,week_of_crash := week(date_of_crash)]
 

#Nationwide Daily
NationalData = TimeData[,.(fatalaccidents=sum(fatalaccidents)),by=.(hour_of_crash)]
NationalPlot = ggplot(data=NationalData,aes(x=hour_of_crash,y=fatalaccidents)) + 
  geom_line() + 
  labs(x="Time of Day",y="Fatal Accidents") + 
  ggtitle("Accidents by Time of Day - Nationwide") + 
  scale_y_continuous(limits=c(1000,4000))
ggsave("./report/NationalDayTrends",plot=NationalPlot,device="png")


#State-level Daily - Plot
TotalAccidents_State = TimeData[,.(totalaccidents=sum(fatalaccidents)),
                          by = .(state_name)]
StateData = TimeData %>%
  .[TotalAccidents_State,on="state_name"] %>%
  .[,.(fatalaccidents = sum(fatalaccidents)/totalaccidents),
                     by = .(hour_of_crash,state_name,totalaccidents)]
StatePlot = ggplot(data=StateData,
                   aes(x=hour_of_crash,y=fatalaccidents,group=state_name)) +
  geom_line() + 
  labs(x = "Time of Day",y="Proportion of Total Fatal Accidents") + 
  ggtitle("Accidents by Time of Day - State-by-State")

#State-level Daily - Clustering
StateData = TimeData %>%
  .[TotalAccidents,on="state_name"] %>%
  .[,.(fatalaccidents = sum(fatalaccidents)/totalaccidents),
    by = .(week_of_crash,state_name,totalaccidents)]

StateTimeSeries = dcast(StateData,state_name ~ week_of_crash,value.var="fatalaccidents")
StateTimeSeries[is.na(StateTimeSeries)] = 0

stateclust = tsclust(StateTimeSeries[,-1],type="partitional",
                     k=2:6,preproc=zscore,distance="sbd",
                     centroid="shape")


#Nationwide Weekly
NationalData = TimeData[,.(fatalaccidents=sum(fatalaccidents)),by=.(hour_of_crash)]
NationalPlot = ggplot(data=NationalData,aes(x=hour_of_crash,y=fatalaccidents)) + 
  geom_line() + 
  labs(x="Time of Day",y="Fatal Accidents") + 
  ggtitle("Accidents by Time of Day - Nationwide") + 
  scale_y_continuous(limits=c(1000,4000))
ggsave("./report/NationalDayTrends",plot=NationalPlot,device="png")


#State-level Weekly - Plot


#State-level Weekly - Clustering








