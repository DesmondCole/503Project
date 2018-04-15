setwd("C:/Users/Yunbin/UM Google/Winter 2018/STATS 503/Project")
library(readr)
library(dplyr)
library(tree)
library(rpart)
library(heuristica)
library(caret)
library(stargazer)

DataForClassification <- read_csv("DataForClassification.csv")

working = DataForClassification 

set.seed(415)

# categorical variable
working$Distracted = factor(working$Distracted)
working$Drugs = factor(working$Drugs)
working$NoRestraint = factor(working$NoRestraint)
working$MultiFatality = factor(working$MultiFatality)
working$Fire = factor(working$Fire)
working$HitAndRun = factor(working$HitAndRun)
working$Speeding = factor(working$Speeding)
working$DrunkDrivers = factor(working$DrunkDrivers)
working$Incl_Weather = factor(working$Incl_Weather)
working$Intersection = factor(working$Intersection)

# decode time 
decode_time = function(x){
  if (x <= 7){
    return(0)
  } else if ((x <= 18)) {
    return(1)
  } else {
    return(2)
  }
}
 
working$time = sapply(working$hour_of_crash, decode_time)
working = na.omit(working)

working_data = working %>%
  # remove missing values for speed
  filter(travel_speed != 997) %>%
  # remove column makers
  select(-Makers, -date_of_crash) %>%
  # remove missing value for hour
  filter(hour_of_crash <= 24) %>%
  mutate(time = factor(time)) %>%
  select(-hour_of_crash)
 

run = working_data %>%
  filter(HitAndRun == 1)
norun = working_data %>%
  filter(HitAndRun == 0)

run.index = sample(1:dim(run)[1], 10000, replace = TRUE)
run.new = run[run.index, ]

not.run.index = sample(1:dim(norun)[1], 40000, replace = FALSE)
norun.new = norun[not.run.index, ]

new.data = rbind(run.new, norun.new)

test.index = sample(dim(new.data)[1], as.integer(dim(new.data)[1]*0.2))
test_set = new.data[test.index, ]
train_set = new.data[-test.index, ]


logit = glm(HitAndRun ~ Distracted + Drugs + MultiFatality + Speeding +
              previous_recorded_crashes + previous_dwi_convictions + previous_recorded_suspensions_and_revocations +
              DrunkDrivers + time, data=train_set, family='binomial')
summary(logit)

logit.prob = predict(logit, test_set, type="response")
logit.pred = rep(0, dim(test_set)[1])
logit.pred[logit.prob >= 0.5] = 1

confusion_matrix = confusionMatrix(as.factor(logit.pred), test_set$HitAndRun)





