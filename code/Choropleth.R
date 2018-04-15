install.packages(c("rgeos", "gpclib", "maptools", "sp"))

library(rgeos)
library(maptools)
library(gpclib)

uscounty <- readShapeSpatial("C:/Users/Teerth/Documents/GitHub/503Project/code/usSHP/UScounties")
plot(uscounty)

library(ggplot2)

uscounty <- fortify(uscounty)

uscounty$id <- toupper(uscounty$id)

ggplot() + geom_map(data = zip1, aes(map_id = county,fill = prop), 
                    map = uscounty) + expand_limits(x = uscounty$long, y = uscounty$lat)

uscounty
zip1
