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
 

#Nationwide Hourly
NationalData = TimeData[,.(fatalaccidents=sum(fatalaccidents)),by=.(hour_of_crash)] %>%
  .[order(hour_of_crash)]
NationalPlot = ggplot(data=NationalData,aes(x=hour_of_crash,y=fatalaccidents)) + 
  geom_line() + 
  labs(x="Time of Day",y="Fatal Accidents") + 
  ggtitle("Accidents by Time of Day - Nationwide") + 
  scale_y_continuous(limits=c(1000,4000))
ggsave("./report/NationalDayTrends.png",plot=NationalPlot,device="png")

NationalTraffic = data.table(Hour=seq(0,23,1),
                             Share = c(.0081,.0052,
                                       .0047,.0057,
                                       .0099,.023,
                                       .0489,.0679,
                                       .0629,.0531,
                                       .0509,.0538,
                                       .0560,.0574,
                                       .0635,.0733,
                                       .0804,.0775,
                                       .0579,.0437,
                                       .0338,.0280,
                                       .0205,.0138)) %>%
  .[,CarsOnRoad := Share * 264000000]

NationalData = cbind(NationalData,CarsOnRoad = NationalTraffic$CarsOnRoad) %>%
  .[,WeightedAccidents := 1000*fatalaccidents/CarsOnRoad]
WeightedNationalPlot = ggplot(data=NationalData,aes(x=hour_of_crash,y=WeightedAccidents)) + 
  geom_line() + 
  labs(x="Time of Day",y="Fatal Accidents per Thousand Vehicles") + 
  ggtitle("Accidents by Time of Day - Nationwide")
ggsave("./report/WeightedNationalDayTrends.png",plot=WeightedNationalPlot,
       device="png",width=9.47,height=7.9,units='in')

#State-level Hourly - Plot
TotalAccidents_State = TimeData[,.(totalaccidents=sum(fatalaccidents)),
                          by = .(state_name)]
StateData = TimeData %>%
  .[TotalAccidents_State,on="state_name"] %>%
  .[,.(fatalaccidents = sum(fatalaccidents)),
                     by = .(hour_of_crash,state_name)]
StatePlot = ggplot(data=StateData,
                   aes(x=hour_of_crash,y=fatalaccidents,group=state_name)) +
  geom_line() + 
  labs(x = "Time of Day",y="Total Fatal Accidents") + 
  ggtitle("Fatal Accidents by Time of Day - State-by-State")
StatePlot

StateTraffic = fread("/Users/Desmond/Documents/GitHub/503Project/data/carsperstate.csv") %>%
  .[,c("state_name","Cars Per State"),with=FALSE]

StateData = StateData[StateTraffic,on="state_name"] %>%
  .[state_name != "Dist. of Col.",Hour := hour_of_crash] %>%
  .[NationalTraffic,on="Hour"] %>%
  .[,CarsPerHourPerState := Share*`Cars Per State`] %>%
  .[,weightedaccidents := 1000*fatalaccidents/CarsPerHourPerState] %>%
  .[,c("Hour","state_name","weightedaccidents")]

WeightedStatePlot = ggplot(data=StateData,
                   aes(x=Hour,y=weightedaccidents,group=state_name)) +
  geom_line() + 
  labs(x = "Time of Day",y="Fatal Accidents per Thousand Vehicles") + 
  ggtitle("Fatal Accidents by Time of Day - State-by-State")
WeightedStatePlot
ggsave("./report/WeightedStatePlot.png",plot=WeightedStatePlot,
       device="png",width=9.47,height=7.9,units='in')


#State-level Hourly - Clustering
StateTimeSeries = dcast(StateData,state_name ~ Hour,value.var="weightedaccidents")
StateTimeSeries[is.na(StateTimeSeries)] = 0
stateclust = tsclust(StateTimeSeries[,-1],type="partitional",
                     k=2:4,preproc=zscore,distance="sbd",
                     centroid="shape")
ClusterPlot_Hourly = plot(stateclust[[2]],"centroids")
ClusterAssignments = cbind(StateTimeSeries$state_name,stateclust[[2]]@cluster)
ggsave("./report/StateClusterPlot_Hourly.png",plot=ClusterPlot_Hourly,
       device='png',width=11.3,height=6.87,units='in')





#Nationwide Weekly
NationalWeekly = TimeData[,.(fatalaccidents=sum(fatalaccidents)),by=.(week_of_crash)] %>%
  .[]
  NationalData = TimeData[,.(fatalaccidents=sum(fatalaccidents)),by=.(hour_of_crash)] %>%
  .[order(hour_of_crash)]



#State-level Weekly - Clustering
StateData_Weekly = TimeData %>%
  .[TotalAccidents_State,on="state_name"] %>%
  .[,.(fatalaccidents = sum(fatalaccidents)/totalaccidents),
    by = .(week_of_crash,state_name,totalaccidents)]

StateTimeSeries = dcast(StateData,state_name ~ week_of_crash,value.var="fatalaccidents")
StateTimeSeries[is.na(StateTimeSeries)] = 0

stateclust = tsclust(StateTimeSeries[,-1],type="partitional",
                     k=2:6,preproc=zscore,distance="sbd",
                     centroid="shape")








