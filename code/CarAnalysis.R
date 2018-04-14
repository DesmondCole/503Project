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

#Aggregate by make
MakeData = allfiles[[15]][!(vehicle_configuration_name %in% 
                              c("Truck Tractor/Semi-Trailer",
                                "Truck Tractor (Bobtail, i.e., Tractor Only, No Trailer)",
                                "Truck Tractor/Double",
                                "Truck Tractor/Triple",
                                "Truck More than 10,000 lbs., Cannot Classify",
                                "Bus (Seats for More Than 15 Occupants, Including Driver, 2010-Later",
                                "Single-Unit Truck (3 or More axles)",
                                "Single-Unit Truck (2 axles and GVWR more\nthan 10,000 lbs."))
                          ,.(fatalaccidents = .N),by=vehicle_make_name]

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

MakeData = MakeData[,`:=` (Maker = 1*(vehicle_make_name %in% c("Chevrolet","Saturn","Pontiac","Oldsmobile","Saab","Buick/Opel","Cadillac","GMC")) + 
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
  .[Maker != 0, .(accidents = sum(fatalaccidents)),by=Maker]

MakeData = MakeData[MakerData,on="Maker"] %>%
  .[,`:=` (shareaccidents = 1000000*accidents/MarketNumbers)] %>%
  .[order(-shareaccidents)]


CarPlot = ggplot(MakeData,aes(x=reorder(Makers,-shareaccidents))) +
  geom_bar(aes(weight=shareaccidents),fill="dark green") + 
  labs(y = "Fatal Accidents per Million Cars",x = "Manufacturer") + 
  ggtitle("Ranking of Manufacturer by Fatal Accidents")
CarPlot
ggsave("./report/ManufacturerRankingPlot",plot=CarPlot,device="png",width=15.2,height=7.69,units="in")
