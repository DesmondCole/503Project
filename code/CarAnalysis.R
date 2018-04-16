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
             "timestamp_of_crash","hour_of_crash","day_of_week","state_name","type_of_intersection")

ClassificationData = allfiles[[1]] %>%
  .[,intersect(keepVars,names(allfiles[[1]])),with=FALSE] %>%
  .[,`:=` (date_of_crash = as.Date(timestamp_of_crash))]

for (i in c(4,5,13)){
  tokeep = intersect(keepVars,names(allfiles[[i]]))
  temp = allfiles[[i]][,tokeep,with=FALSE]
  ClassificationData = ClassificationData[temp,on="uniqueID",allow.cartesian=TRUE]
}

ManufacturerData = allfiles[[15]][!(vehicle_configuration_name %in% 
                                      c("Truck Tractor/Semi-Trailer",
                                        "Truck Tractor (Bobtail, i.e., Tractor Only, No Trailer)",
                                        "Truck Tractor/Double",
                                        "Truck Tractor/Triple",
                                        "Truck More than 10,000 lbs., Cannot Classify",
                                        "Bus (Seats for More Than 15 Occupants, Including Driver, 2010-Later",
                                        "Single-Unit Truck (3 or More axles)",
                                        "Single-Unit Truck (2 axles and GVWR more\nthan 10,000 lbs."))] %>%
  .[(previous_dwi_convictions < 99 & previous_recorded_crashes < 98 & previous_recorded_suspensions_and_revocations < 99),] %>%
  .[(travel_speed < 998 & speed_limit < 98),] %>%
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

ClassificationData = unique(ClassificationData)

ClassificationData = ClassificationData[ManufacturerData,on="uniqueID"]

ClassificationData = unique(ClassificationData)

ClassificationData = ClassificationData[,`:=` (Distracted = factor(driver_distracted_by_name != "Not Distracted"),
                                               Drugs = factor(police_reported_drug_involvement == "Yes (Drugs Involved)"),
                                               NoRestraint = factor(restraint_system_helmet_use_name == "None Used"),
                                               MultiFatality = factor((number_of_fatalities > 1)),
                                               Fire = factor(fire_occurrence == "Yes"),
                                               HitAndRun = factor(hit_and_run == "Yes"),
                                               Speeding = factor(speeding_related),
                                               Makers = factor(Makers))]


SummaryData = ClassificationData[,.(deathspermillion = 1000000*sum(number_of_fatalities)/MarketNumbers),
                      by=.(Makers,MarketNumbers)] %>%
  .[order(-deathspermillion)]


CarPlot = ggplot(SummaryData,aes(x=reorder(Makers,-deathspermillion))) +
  geom_bar(aes(weight=deathspermillion),fill="dark green") + 
  labs(y = "Fatalities per Million Cars",x = "Manufacturer") + 
  ggtitle("") + 
  theme(axis.text.x = element_text(size=10))
CarPlot
ggsave("./report/ManufacturerRankingPlot.png",plot=CarPlot,device="png",width=15.2,height=7.69,units="in")



#MDS for Automaker and Vehicle Type
ModelVars = c("Distracted","Drugs","NoRestraint","MultiFatality","Fire","HitAndRun",
              "Speeding","Makers","previous_recorded_crashes","previous_dwi_convictions",
              "previous_recorded_suspensions_and_revocations")


CarTypeData = allfiles[[17]][(uniqueID %in% ClassificationData$uniqueID),] %>% 
  .[,CarType := 1*(grepl("Luxury",standard_segmentation) & 
                     !grepl("Non Luxury",standard_segmentation) &
                     !grepl("Non-Luxury",standard_segmentation)) + 
                2*(grepl("Non Luxury", standard_segmentation) | 
                     grepl("Non-Luxury",standard_segmentation)),] %>% 
  .[,CarType2 := 10*(grepl("SUV",standard_segmentation)) + 
      20*(grepl("CUV",standard_segmentation)) + 
      30*(grepl("Traditional",standard_segmentation)) + 
      40*(grepl("Pickup",standard_segmentation)) + 
      50*(grepl("Van",standard_segmentation)) + 
      60*(grepl("Sport",standard_segmentation)) + 
      70*(grepl("Prestige",standard_segmentation)) + 
      80*(grepl("Exotic",standard_segmentation))] %>%
  .[,c("uniqueID","CarType","CarType2"),with=FALSE] %>%
  .[,CarTypeCode := (CarType + CarType2)]

CarTypeName = data.table(Name = c("Luxury SUV","Non Luxury SUV",
                                  "Luxury CUV","Non Luxury CUV",
                                  "Luxury Traditional","Non Luxury Traditional",
                                  "Luxury Pickup","Non Luxury Pickup",
                                  "Luxury Van","Non Luxury Van",
                                  "Luxury Sport","Non Luxury Sport",
                                  "Luxury Prestige","Luxury Exotic","Other"),
                         CarTypeCode = c(11,12,21,22,31,32,41,42,51,52,61,62,71,81,0))

CarTypeData = CarTypeData[CarTypeName,on="CarTypeCode"] %>%
  .[,c("uniqueID","Name"),with=FALSE]

MDSData_Maker = ClassificationData[,`:=` (Distracted = 1*(driver_distracted_by_name != "Not Distracted"),
                                    Drugs = 1*(police_reported_drug_involvement == "Yes (Drugs Involved)"),
                                    NoRestraint = 1*(restraint_system_helmet_use_name == "None Used"),
                                    MultiFatality = 1*(number_of_fatalities > 1),
                                    Fire = 1*(fire_occurrence == "Yes"),
                                    HitAndRun = 1*(hit_and_run == "Yes"),
                                    Speeding = 1*(!(speeding_related %in% c("No","Unknown"))),
                                    Makers = factor(Makers))] %>%
  .[,ModelVars,with=FALSE] %>%
  .[,.(Distracted = sum(Distracted)/.N,Drugs = sum(Drugs)/.N,
       NoRestraint = sum(NoRestraint)/.N,MultiFataity = sum(MultiFatality)/.N,
       Fire = sum(Fire)/.N,HitAndRun = sum(HitAndRun)/.N,Speeding = sum(Speeding)/.N),
    by=.(Makers)]

Makers = MDSData_Maker$Makers

MDSData_Maker = MDSData_Maker[,-c("Makers"),with=FALSE]

distancemat = dist(MDSData_Maker)
mdsresults = cmdscale(distancemat)
mdsresults = data.frame(mdsresults,Makers)
MDSPlot = ggplot(data=mdsresults,aes(x=X1,y=X2,label=Makers)) + 
  geom_point(shape=21,fill=NA,colour=NA) + 
  geom_text_repel(aes(label=Makers),size=4.5) + 
  labs(x="Dim. 1",y="Dim. 2") + 
  scale_x_continuous(limits=c(-.05,.12))
MDSPlot
ggsave("./report/ManufacturerMDSPlot.png",plot=MDSPlot,device="png",
       width=9.32,height=8.38,units="in")




MDSData_Type = unique(ClassificationData[CarTypeData,on="uniqueID",allow.cartesian=TRUE])



MDSData_Type = MDSData_Type[,`:=` (Distracted = 1*(driver_distracted_by_name != "Not Distracted"),
                                         Drugs = 1*(police_reported_drug_involvement == "Yes (Drugs Involved)"),
                                         NoRestraint = 1*(restraint_system_helmet_use_name == "None Used"),
                                         MultiFatality = 1*(number_of_fatalities > 1),
                                         Fire = 1*(fire_occurrence == "Yes"),
                                         HitAndRun = 1*(hit_and_run == "Yes"),
                                         Speeding = 1*(!(speeding_related %in% c("No","Unknown"))),
                                         Name = factor(Name))] %>%
  .[,c(ModelVars,"Name"),with=FALSE] %>%
  .[,.(Distracted = sum(Distracted)/.N,Drugs = sum(Drugs)/.N,
       NoRestraint = sum(NoRestraint)/.N,MultiFataity = sum(MultiFatality)/.N,
       Fire = sum(Fire)/.N,HitAndRun = sum(HitAndRun)/.N,Speeding = sum(Speeding)/.N),
    by=.(Name)] %>%
  .[Name != "Luxury Van",] %>%
  .[!(Name %in% c("Luxury Exotic","Luxury Prestige","Luxury Pickup"))]

Types = MDSData_Type$Name

MDSData_Type = MDSData_Type[,-c("Name"),with=FALSE]



distancemat = dist(MDSData_Type,method="manhattan")
mdsresults_type = cmdscale(distancemat)
mdsresults_type = data.frame(mdsresults_type,Types)
MDSPlot_type = ggplot(data=mdsresults_type,aes(x=X1,y=X2,label=Types)) + 
  geom_point(shape=21,fill=NA,colour=NA) + 
  geom_text_repel(aes(label=Types),size=4.5,segment.alpha=0) + 
  labs(x="Dim. 1",y="Dim. 2") + 
  scale_x_continuous(limits=c(-.15,.20))
MDSPlot_type

ggsave("./report/VehicleTypeMDSPlot.png",plot=MDSPlot_type,device="png",
       width=9.32,height=8.38,units="in")


#Automaker Analysis. Even with observed differences among car manufacturers, it is unclear 
#whether or not unobservables account for the majority of the differences. To explore this further, 
#will use multi-class boosting and weighting techniques to perform classification of car type based on behavioral characteristics.
library(maboost)
library(DMwR)

ModelVars = c("Distracted","Drugs","NoRestraint","MultiFatality","HitAndRun",
              "Speeding","previous_recorded_crashes","previous_dwi_convictions",
              "previous_recorded_suspensions_and_revocations","travel_speed","Makers",
              "DrunkDrivers","Incl_Weather","Intersection","date_of_crash","hour_of_crash")


AnalysisData = ClassificationData[,`:=` (Distracted = factor(1*(driver_distracted_by_name != "Not Distracted")),
                                   Drugs = factor(1*(police_reported_drug_involvement == "Yes (Drugs Involved)")),
                                   DrunkDrivers = factor(1*(number_of_drunk_drivers > 0)),
                                   NoRestraint = factor(1*(restraint_system_helmet_use_name == "None Used")),
                                   MultiFatality = factor(1*(number_of_fatalities > 1)),
                                   Fire = factor(1*(fire_occurrence == "Yes")),
                                   HitAndRun = factor(1*(hit_and_run == "Yes")),
                                   Speeding = factor(1*(!(speeding_related %in% c("No","Unknown")))),
                                   Incl_Weather = factor(1*(!(atmospheric_conditions_1_name %in% c("Clear",
                                                                                            "Not Reported",
                                                                                            "Unknown")))),
                                   Intersection = factor(1*(!(type_of_intersection %in% c("Not an Intersection","Unknown","Not Reported"))))),] %>%
  .[,ModelVars,with=FALSE]

write.csv(AnalysisData,file = "./data/DataForClassification.csv",row.names=FALSE)


library(adabag)
library(e1071)
#Risk of multi-fatality accident
set.seed(10)
testinds = sample(1:nrow(AnalysisData),size=.20*nrow(AnalysisData))
AnalysisData_Test = AnalysisData[testinds,]
AnalysisData_Train = AnalysisData[-testinds,] %>% .[,-c("date_of_crash"),with=FALSE]
AnalysisData_Train_Rebal = SMOTE(MultiFatality ~ ., data=AnalysisData_Train)

#Logistic Regression
logitmodel_multi = glm(MultiFatality ~ ., 
                       data=AnalysisData_Train_Rebal,family="binomial")
logitprobs = predict(logitmodel_multi,newdata=AnalysisData_Test,type="response")
logitpreds = round(logitprobs)
logitaccuracy = mean(logitpreds == AnalysisData_Test$MultiFatality)
logitaccuracy

OtherCoeffs

#Automaker odds ratios
OddsRatios = data.frame(`Odds Ratios` = logitmodel_multi$coefficients)
OddsRatios = cbind(OddsRatios,Names = as.character(row.names(OddsRatios)))

OtherCoeffs = copy(OddsRatios)


OddsRatios = data.table(OddsRatios) %>%
  .[(11:24),]


Automaker = c("Daimler","FCA/Chrysler","Ford","GM",
          "Honda","Hyundai-Kia","Land Rover",
          "Mazda","Mitsubishi","Nissan",
          "Subaru","Toyota","Volkswagen",
          "Volvo")

OddsRatios = cbind(OddsRatios,Automaker) %>%
  .[,-c("Names"),with=FALSE] %>%
  .[order(-Odds.Ratios)]
library(xtable)
result = xtable(OddsRatios,caption="Automaker Odds Ratios")

OtherCoeffs = data.table(OtherCoeffs) %>%
  .[-c(1,11:24),]

Coeffs = c("Distracted","On Drugs","No Restraint","Hit and Run","Speeding",
           "Previous Recorded Crashes","Previous DWI Convictions",
           "Previous Recorded Suspensions And Revocations","Travel Speed","Drunk",
           "Inclement Weather","Intersection","Hour of Crash")

OtherCoeffs = data.table(OddsRatio = OtherCoeffs$Odds.Ratios,Variable = Coeffs)
library(xtable)
Othercoeffsresult = xtable(OtherCoeffs,caption="Predictor Odds Ratios")

#SVM
svmmodel_multi = svm(MultiFatality ~ .,data=AnalysisData_Train_Rebal,cross=5)
svmmodel_testerror_multi = predict(svmmodel_multi,newdata=AnalysisData_Test[,-c("MultiFatality"),with=FALSE])
svmaccuracy = mean(svmmodel_testerror_multi == AnalysisData_Test$MultiFatality)
svmaccuracy

#SVM (using Rminer)
svmmodelrminer = fit(MultiFatality ~ ., model="svm",data=AnalysisData_Train_Rebal,C=1,cross=5)
svm.imp = Importance(svmmodelrminer,data=AnalysisData_Train_Rebal)
SVMImpData = data.table(Imp = svm.imp$imp,Var = names(AnalysisData_Train_Rebal)) %>%
  .[Var != "MultiFatality",] %>%
  as.data.frame()
SVMImpData$Var = gsub("_","\n",SVMImpData$Var)
SVMIMPPlot = ggplot(data=SVMImpData,aes(x=reorder(Var,-Imp))) + 
  geom_bar(aes(weight=Imp),fill="dark green") + 
  labs(x = "Variable",y="Importance") + 
  ggtitle("") + 
  theme(axis.text.x = element_text(size=10))
ggsave("./report/ImportancePlot_SVM.png",SVMIMPPlot,device="png",
       width=12.3,height=6.21,units="in")




#Adaboost
adamodel_multi = boosting(MultiFatality ~ ., data=AnalysisData_Train_Rebal)
adamodelaccuracy = predict(adamodel_multi,newdata=AnalysisData_Test)
adamodelaccuracy_rate = mean(adamodelaccuracy$class == AnalysisData_Test$MultiFatality)
adamodelaccuracy_rate
ADAImpData = data.frame(Imp = adamodel_multi$importance)
ADAImpData = data.frame(ADAImpData,Var = row.names(ADAImpData))
row.names(ADAImpData) = NULL
levels(ADAImpData$Var) = gsub("_","\n",levels(ADAImpData$Var))
ADAIMPPlot = ggplot(data=ADAImpData,aes(x=reorder(Var,-Imp))) + 
  geom_bar(aes(weight=Imp),fill="dark green") + 
  labs(x = "Variable",y="Importance") + 
  theme(axis.text.x = element_text(size=10)) + 
  ggtitle("")
ggsave("./report/ImportancePlot_ADABoost.png",ADAIMPPlot,device="png",
       width=12.3,height=6.21,units="in")




Accuracy = data.frame(t(c(logitaccuracy,svmaccuracy,adamodelaccuracy_rate)))
names(Accuracy) = c("Logistic Regression","Supp. Vec. Machine","AdaBoost")
row.names(Accuracy) = "Accuracy"
library(xtable)
result = xtable(Accuracy,caption="Test Set Prediction Accuracy")


#Risk of Hit and Run

# Environmental Conditions
EnvironmentalData = allfiles[[1]] %>%
  .[atmospheric_conditions_1_name != "Unknown",] %>%
  .[,.(fatalaccidents = .N),by=.(FIPS,atmospheric_conditions_1_name)] %>%
  .[FIPS %in% c("17031"),]

SimpleEnvironPlot = ggplot(data=EnvironmentalData,
                           aes(x = reorder(atmospheric_conditions_1_name,-fatalaccidents))) + 
  geom_bar(aes(weight=fatalaccidents),fill="dark green") + 
  labs(x = "Atmospheric Conditions",y="Fatal Accidents")
ggsave("./report/Environmental.png",SimpleEnvironPlot,device="png",
       width=9.56,height=5.26,units='in')





