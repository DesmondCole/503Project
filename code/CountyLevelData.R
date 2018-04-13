library(data.table)
library(magrittr)
library(plyr)
accidentdata = fread("/Users/Desmond/Desktop/Work/503 Project/AllData/accident.csv") %>%
  .[,`:=` (FIPS = state_number*1000 + county),]

censusdata = fread("/Users/Desmond/Desktop/Work/503 Project/cc-est2016-alldata.csv") %>% 
  .[,`:=` (FIPS = as.factor(1000*STATE + COUNTY))] %>%
  .[,`:=` (Year = 2015 * (YEAR==8) + 2016*(YEAR==9))] %>%
  .[((Year==2015 | Year==2016) & AGEGRP == 0),.(Population=as.numeric(TOT_POP)),by=.(FIPS,Year)]
  
accidentdata = join(accidentdata,censusdata,by = c("Year","FIPS"),type="left")

