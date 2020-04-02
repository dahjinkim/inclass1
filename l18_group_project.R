# set working directory
setwd("C:/Users/oyina/src/senior_2019-2020/spring_2020/political_data_science/group_work")

# load in data
senateData<-read.csv("CandidateLevel.csv")

library(tidyverse)
library(readr)

senateData_ReM = senateData %>%
  mutate(IncumbentWon = (Incumbent == 1 & VotePercentage >= 50)) %>%
  filter(Incumbent == 1)

# subset into test and train
senateTrain = senateData_ReM[senateData_ReM$cycle != 2016,]
senateTest = senateData_ReM[senateData_ReM$cycle == 2016,]

# linear prob model
ProjectModelTrain<-glm(IncumbentWon~pvi*Republican+PercentageRaised+GenericBallotSept, data=senateTrain, family="binomial")
summary(ProjectModelTrain)
result = predict(ProjectModelTrain, senateTest, type="response")
result
# confusion matrix
# lin_result = (exp(result))/(1+exp(result)) # sigmoidal
# lin_result # sigmoidal
binary_result = (result > 0.5)*1

conf_mat = table(binary_result, senateTest$IncumbentWon)
conf_mat
# binary_result FALSE TRUE
# 0     3    2
# 1     3   21
precision = conf_mat[2,2]/(conf_mat[2,2]+conf_mat[2,1])
recall = conf_mat[2,2]/(conf_mat[2,2]+conf_mat[1,2])
precision # 0.875
recall # 0.9130435


# k nearest neighbors
library(class)
incumWonTrain<-senateTrain[,c("pvi", "Republican", "PercentageRaised", "GenericBallotSept")]
incumWonTest<-senateTest[,c("pvi", "Republican", "PercentageRaised", "GenericBallotSept")]
mod1_knn<-knn(train=incumWonTrain, test=incumWonTest, cl=senateTrain$IncumbentWon, k=5)
conf_mat = table(mod1_knn, senateTest$IncumbentWon)
conf_mat
# mod1_knn FALSE TRUE
# FALSE     1    2
# TRUE      5   21
precision = conf_mat[2,2]/(conf_mat[2,2]+conf_mat[2,1])
recall = conf_mat[2,2]/(conf_mat[2,2]+conf_mat[1,2])
precision # 0.8076923
recall # 0.9130435


# random forest
library(randomForest)
equation<-as.formula("IncumbentWon~pvi*Republican+PercentageRaised+GenericBallotSept")
senateTrain$IncumbentWon<-as.factor(senateTrain$IncumbentWon)
mod1_forest<-randomForest(equation, data=senateTrain, 
                          ntree=100, mtry=4)
result = predict(ProjectModelTrain, senateTest)

# confusion matrix
binary_result = (result > 0.5)*1
conf_mat = table(binary_result, senateTest$IncumbentWon)
conf_mat
# binary_result FALSE TRUE
# 0     4    3
# 1     2   20
precision = conf_mat[2,2]/(conf_mat[2,2]+conf_mat[2,1])
recall = conf_mat[2,2]/(conf_mat[2,2]+conf_mat[1,2])
precision #0.9090909
recall #0.8695652
