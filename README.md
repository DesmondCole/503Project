# 503Project

Data:
2015-2016 US Traffic Fatality Records, published by NHTSA. Included in the dataset are various tables with information regarding the circumstances of vehicle crashes involving at least one fatality (where the death occurred within 30 days of the crash). The included information describes the circumstances of the crash, including environmental conditions, driver/occupant descriptions (including age and drug/alcohol test results), location, time of day, make/model of involved vehicles, and EMS response time. 
https://www.kaggle.com/usdot/nhtsa-traffic-fatalities

Keep in mind:
- The consecutive number uniquely identifies an accident in a given year. Thus, a true unique accident identifier in the datasets in the zip file is the year-consecnumber combination.


To include in Analysis:
- Map of traffic fatalities by county, (ideally) weighted by population.
- Plot of traffic fatalities by time of day / time of week / season (to see periods with relatively high-fatality incidence).
- (potentially) time series clustering of weekly fatal traffic accidents by state, if returning decent results.
- damage areas that may correlated with high fatality, aka weak spot of car structure 
- safety equippment of motorists, possible clustering??? to see how effective to protect people, or see any difference with different time/ season/road condition
- Classification of car model by death rate (would have to incorporate counts of cars of that model sold)
- Classification of deadliest road types (e.g. intersections, traffic circles)
- Prediction


Variables to include in final dataset (smart to build indicators off of these. For example, whether or not driver's vision was obscured, whether or not there was at least one drunk driver, etc.):
- Location/Timing (state, consec number, city, county, day/month/year/hour/minute,lat/long for heatmap)
- Number of fatalities
- Driver conditions (disability, BAC, drugs, distraction, etc.)
- Road characteristics
- atmospheric/environmental conditions (weather, vision, etc.)


Variables I think we can ignore:
- Everything from following:
  - violatn.csv
  -
