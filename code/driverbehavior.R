library(dplyr)
library(gbm)
library(knncat)
library(rpart)

setwd("C:/Users/Teerth/Downloads/503 Project Data")

accidents <- read.csv("accident.csv", header = TRUE)
impairment <- read.csv("nmimpair.csv", header = TRUE)
distract <- read.csv("distract.csv", header = TRUE)

impair <- left_join(accidents, impairment, by = "uniqueID")
impairdistract <- left_join(impair, distract, by = "uniqueID")
impairdistract$condition_impairment_at_time_of_crash_non_motorist_name <- 
    as.factor(impair$condition_impairment_at_time_of_crash_non_motorist_name)
impairdistract$condition_impairment_at_time_of_crash_non_motorist[is.na(impairdistract$condition_impairment_at_time_of_crash_non_motorist_name)] <- 0

fatal <- impairdistract %>% mutate(serious = ifelse(number_of_fatalities > 1, 1, 0))

fatal$serious <- as.factor(fatal$serious)

fatal1 <- fatal %>% select(functional_system_name, light_condition_name, atmospheric_conditions_1,
                           atmospheric_conditions_2, atmospheric_conditions,
                           related_factors_crash_level_1, related_factors_crash_level_2,
                           related_factors_crash_level_3,
                           condition_impairment_at_time_of_crash_non_motorist, serious,
                           driver_distracted_by)

fatal1 <- fatal1 %>% filter(atmospheric_conditions_1_name != "Severe Crosswinds")

train_index <- sample(1:nrow(fatal1), nrow(fatal1) * .8)
train_fatal <- fatal1[train_index, ]
test_fatal <- fatal1[-train_index, ]

boundary_plot <- function(df, classifier, predict_function, resolution = 500, ...) {
  colnames(df) = c("Var1", "Var2", "Class")
  classifier_obj <- classifier(df)
  v1 = seq(min(df[,1]), max(df[,1]), length=resolution)
  v2 = seq(min(df[,2]), max(df[,2]), length=resolution)
  Grid = expand.grid(Var1 = v1, Var2 = v2)
  Grid$class = predict_function(classifier_obj, Grid, ...)
  ggplot(data=df, aes(x=Var1, y=Var2, color=Class)) +
    geom_contour(data=Grid, aes(z=as.numeric(class)),
                 color="black",size=.5)+
    geom_point(size=2,aes(color=Class, shape=Class))
}

fatal_logit <- glm(serious ~ ., data = train_fatal, family = "binomial")

logit_prediction <- (predict(fatal_logit, test_fatal, type = "response")  > 0.5) * 1
logit_prediction[is.na(logit_prediction)] <- 0
mean(logit_prediction != test_fatal$serious)

fatal_logit

logit_prediction

fatal_gbm_bern <- gbm(serious ~ ., data = train_fatal, distribution = "bernoulli")
fatal_gbm_huber <- gbm(serious ~ ., data = train_fatal, distribution = "huberized")
fatal_gbm_ada <- gbm(serious ~ ., data = train_fatal, distribution = "adaboost")

gbm_bern_prediction <- (predict(fatal_gbm_bern, test_fatal, type = "response")  > 0.5) * 1

fatal_gbm_bern
fatal_gbm_huber
fatal_gbm_ada

summary(fatal_gbm_ada)

knncat(train_fatal, test_fatal)

fatal_tree <- rpart(serious ~ ., data = train_fatal, method = "class")

tree_predict = predict(fatal_tree ,test_fatal, type="class")
mean(tree_predict != test_fatal$serious)

