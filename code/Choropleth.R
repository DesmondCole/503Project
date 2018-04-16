install.packages(c("rgeos", "gpclib", "maptools", "sp"))

library(rgeos)
library(maptools)
library(gpclib)
library(ggplot2)

accidents <- read.csv("accident.csv", header = TRUE)

setwd("C:/Users/Teerth/Documents/GitHub/503Project/data")
statepop <- read.csv("census-state-populations.csv", header = TRUE)

usstate <- readShapeSpatial("C:/Users/Teerth/Documents/GitHub/503Project/code/usSHP/cb_2017_us_state_20m")

joined <- left_join(accidents, statepop, by = "state")

usstate <- fortify(usstate)

usstate$id <- toupper(usstate$id)

states <- joined %>% group_by(state) %>% 
  mutate(totals = n(), prop = (1000 * totals)/pop_est_2014)

sumstate <- states %>% summarise(prop = mean(prop))

id <- as.tbl(unique(usstate$id)[2:52])


sumstate2 <- data.frame(state = sumstate$state, id = id, prop = sumstate$prop)

merge <- merge(usstate, sumstate2, by = "id")
ordermerge <- merge[order(merge$order), ]

library(plyr)

distcenters <- ddply(usstate, .(id), summarize, clat = mean(lat), clong = mean(long))

ggplot() +
  geom_polygon(data = ordermerge, 
               aes(x = long, y = lat, group = group, fill = prop), 
               color = "black", size = 0.25) + 
  coord_map() +
  scale_fill_distiller(name="Percent", palette = "Purples")+
  ylim(23,50) + 
  xlim(-125, -67)+
  xlab("") +
  ylab("")
#  geom_text(data = distcenters, aes(x = clong, y = clat, label = id, size = .2))
