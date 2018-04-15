library(data.table)
library(magrittr)
library(ggplot2)
library(dtwclust)

#Load data
setwd("/Users/Desmond/Desktop/Work/503 Project/AllData")
temp = list.files(pattern = "*.csv")
allfiles = lapply(temp,fread)
names(allfiles) = temp

setwd("/Users/Desmond/Documents/GitHub/503Project")


#Generate Maker Data
Makers = c("GM","Ford","Toyota",
           "FCA/Chrysler","Honda",
           "Nissan","Hyundai-Kia","Subaru",
           "Volkswagen","Daimler",
           "BMW","Mazda","Mitsubishi","Volvo","Land Rover")

RelMarketNumbers = c(45276000,
                     38649600,
                     36762000,
                     33237600,
                     23839200,
                     22545600,
                     20776800,
                     8857200,
                     8012400,
                     7682400,
                     5689200,
                     4567200,
                     1412400,
                     1122000,
                     1069200)

MakerData = data.table(Maker = seq(1:15),Makers = Makers,MarketNumbers = RelMarketNumbers)



#Generate Aggregate Classification Dataset
keepVars = c("uniqueID","number_of_drunk_drivers","police_reported_drug_involvement","driver_distracted_by_name",
             "impairment_at_time_of_crash_driver_name","previous_recorded_crashes","previous_recorded_suspensions_and_revocations",
             "previous_dwi_convictions","restraint_system_helmet_use_name","fire_occurrence","number_of_fatalities",
             "hit_and_run","travel_speed","speed_limit","speeding_related","atmospheric_conditions_1_name","FIPS","Population",
             "timestamp_of_crash","hour_of_crash","day_of_week","state_name")

ClassificationData = allfiles[[1]] %>%
  .[,intersect(keepVars,names(allfiles[[1]])),with=FALSE] %>%
  .[,`:=` (date_of_crash = as.Date(timestamp_of_crash))]





for (i in c(4,5,13)){
  tokeep = intersect(keepVars,names(allfiles[[i]]))
  temp = allfiles[[i]][,tokeep,with=FALSE]
  ClassificationData = ClassificationData[temp,on="uniqueID",allow.cartesian=TRUE]
}

VehicleData = allfiles[[15]][!(vehicle_configuration_name %in% 
                                 c("Truck Tractor/Semi-Trailer",
                                   "Truck Tractor (Bobtail, i.e., Tractor Only, No Trailer)",
                                   "Truck Tractor/Double",
                                   "Truck Tractor/Triple",
                                   "Truck More than 10,000 lbs., Cannot Classify",
                                   "Bus (Seats for More Than 15 Occupants, Including Driver, 2010-Later",
                                   "Single-Unit Truck (3 or More axles)",
                                   "Single-Unit Truck (2 axles and GVWR more\nthan 10,000 lbs."))] %>%
  .[(previous_dwi_convictions < 99 & previous_recorded_crashes < 98 & previous_recorded_suspensions_and_revocations < 99),] %>%
  .[(travel_speed < 300 & speed_limit < 98),] %>%
  .[,`:=` (Maker = 1*(vehicle_make_name %in% c("Chevrolet","Saturn","Pontiac","Oldsmobile","Saab","Buick/Opel","Cadillac","GMC")) + 
             2*(vehicle_make_name %in% c("Ford","Lincoln","Mercury")) + 
             3*(vehicle_make_name %in% c("Toyota","Daihatsu","Lexus","Scion (Since 2012)")) + 
             4*(vehicle_make_name %in% c("Fiat","Chrysler","Dodge","Jeep/Kaiser-Jeep/Willys Jeep","Plymouth")) + 
             5*(vehicle_make_name %in% c("Datsun/Nissan","Infiniti","Bluebird")) + 
             6*(vehicle_make_name %in% c("Honda","Acura")) + 
             7*(vehicle_make_name %in% c("Hyundai","Kia")) + 
             8*(vehicle_make_name %in% c("Subaru")) + 
             9*(vehicle_make_name %in% c("Audi","Bentley","Porsche","Ducati","Volkswagen")) + 
             10*(vehicle_make_name %in% c("Mercedes-Benz","Smart (Since 2010)")) + 
             11*(vehicle_make_name %in% c("BMW")) + 
             12*(vehicle_make_name %in% c("Mazda")) + 
             13*(vehicle_make_name %in% c("Mitsubishi")) + 
             14*(vehicle_make_name %in% c("Volvo")) + 
             15*(vehicle_make_name %in% c("Land Rover","Jaguar")))] %>%
  .[Maker != 0,] %>%
  .[,c(intersect(keepVars,names(allfiles[[15]])),"Maker"),with=FALSE] %>%
  .[MakerData,on="Maker"]

AltVehicleData = 

ClassificationData = unique(ClassificationData)

ClassificationData = ClassificationData[VehicleData,on="uniqueID"]

SummaryData = ClassificationData[,.(deathspermillion = 1000000*sum(number_of_fatalities)/MarketNumbers),
                      by=.(Makers,MarketNumbers)] %>%
  .[order(-deathspermillion)]


CarPlot = ggplot(SummaryData,aes(x=reorder(Makers,-deathspermillion))) +
  geom_bar(aes(weight=deathspermillion),fill="dark green") + 
  labs(y = "Fatalities per Million Cars",x = "Manufacturer") + 
  ggtitle("Ranking of Manufacturer by Fatal Accidents")
CarPlot
ggsave("./report/ManufacturerRankingPlot",plot=CarPlot,device="png",width=15.2,height=7.69,units="in")


#Automaker Analysis. Even with observed differences among car manufacturers, it is unclear 
#whether or not unobservables account for the majority of the differences. To explore this further, 
#will use multi-class boosting and weighting techniques to perform classification of car type based on behavioral characteristics.
library(maboost)
library(DMwR)


ClassificationData = ClassificationData[,`:=` (Distracted = factor(driver_distracted_by_name != "Not Distracted"),
                                               Drugs = factor(police_reported_drug_involvement == "Yes (Drugs Involved)"),
                                               NoRestraint = factor(restraint_system_helmet_use_name == "None Used"),
                                               MultiFatality = factor((number_of_fatalities > 1)),
                                               Fire = factor(fire_occurrence == "Yes"),
                                               HitAndRun = factor(hit_and_run == "Yes"),
                                               Speeding = factor(speeding_related),
                                               Makers = factor(Makers))]

ModelVars = c("Distracted","Drugs","NoRestraint","MultiFatality","Fire","HitAndRun",
              "Speeding","Makers","previous_recorded_crashes","previous_dwi_convictions",
              "previous_recorded_suspensions_and_revocations")

AnalysisData = ClassificationData[,ModelVars,with=FALSE]

#Binary rebalancing and Classification
BinClassData = SMOTE(MultiFatality ~ ., data=AnalysisData)
basicmodel = glm(MultiFatality ~ ., data=BinClassData,family="binomial")


#Multiclass Rebalancing and Classification
MakerModel = maboost(Makers ~ .,data=AnalysisData,C50tree=TRUE)
MakerModel = maboost(Makers ~ .,data=AnalysisData,C50tree=TRUE,C5.0Control(minCases=10,CF=.4))



#Data for MDS
MDSData = ClassificationData[,`:=` (Distracted = 1*(driver_distracted_by_name != "Not Distracted"),
                                               Drugs = 1*(police_reported_drug_involvement == "Yes (Drugs Involved)"),
                                               NoRestraint = 1*(restraint_system_helmet_use_name == "None Used"),
                                               MultiFatality = 1*(number_of_fatalities > 1),
                                               Fire = 1*(fire_occurrence == "Yes"),
                                               HitAndRun = 1*(hit_and_run == "Yes"),
                                               Speeding = 1*(!(speeding_related %in% c("No","Unknown"))),
                                               Makers = factor(Makers))] %>%
  .[,c(ModelVars,"MarketNumbers"),with=FALSE] %>%
  .[,.(Distracted = sum(Distracted)/MarketNumbers,Drugs = sum(Drugs)/MarketNumbers,
       NoRestraint = sum(NoRestraint)/MarketNumbers,MultiFataity = sum(MultiFatality)/MarketNumbers,
       Fire = sum(Fire)/MarketNumbers,MarketNumbers,HitAndRun = sum(HitAndRun)/MarketNumbers,MarketNumbers,Speeding = sum(Speeding)/MarketNumbers),
    by=.(Makers,MarketNumbers)]

MDSData = ClassificationData[,`:=` (Distracted = 1*(driver_distracted_by_name != "Not Distracted"),
                                    Drugs = 1*(police_reported_drug_involvement == "Yes (Drugs Involved)"),
                                    NoRestraint = 1*(restraint_system_helmet_use_name == "None Used"),
                                    MultiFatality = 1*(number_of_fatalities > 1),
                                    Fire = 1*(fire_occurrence == "Yes"),
                                    HitAndRun = 1*(hit_and_run == "Yes"),
                                    Speeding = 1*(!(speeding_related %in% c("No","Unknown"))),
                                    Makers = factor(Makers))] %>%
  .[,c(ModelVars,"MarketNumbers"),with=FALSE] %>%
  .[,.(Distracted = sum(Distracted),Drugs = sum(Drugs),
       NoRestraint = sum(NoRestraint),MultiFataity = sum(MultiFatality),
       Fire = sum(Fire),HitAndRun = sum(HitAndRun),MarketNumbers,Speeding = sum(Speeding)),
    by=.(Makers,MarketNumbers)]


Makers = MDSData$Makers

MDSData = MDSData[,-c("Makers","MarketNumbers"),with=FALSE]

distancemat = dist(MDSData)
mdsresults = cmdscale(distancemat)
