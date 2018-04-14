library(maps)
library(ggmap)
library(tidyr)
library(dplyr)

setwd("C:/Users/Teerth/Downloads/503 Project Data")

accident <- read.csv("accident.csv", header = TRUE)

US <- get_map(location = "US", zoom = 4, scale = 2, maptype = "roadmap")

ggmap(US) + 
  geom_point(aes(x = longitude, y = latitude), colour = "blue", 
  alpha = 0.2, size = .6, data = accident)

ggmap(US) + 
  geom_density2d(aes(x = longitude, y = latitude), 
                 alpha = .4, size = .6, data = accident) +
  stat_density2d(aes(x = longitude, y = latitude), colour = "blue", 
                 alpha = 1, size = .5, data = accident)

zip <- accident %>% group_by(FIPS) %>% mutate(totals = n())

zip1 <- zip %>% ungroup() %>% mutate(prop = totals/n())

library(choroplethr)
library(choroplethrMaps)
data(county.regions)

zip1$FIPS <- as.character(zip1$FIPS)

county.regions$county.fips.character

county <- left_join(zip, county.regions, by = c("FIPS" = "county.fips.character"))

county_map <- county_choropleth(county, legend = "Proportions")
