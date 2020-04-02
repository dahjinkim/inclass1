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

# confusion matrix
binary_result = (result > 0.5)*1
conf_mat = table(binary_result, senateTest$IncumbentWon)
conf_mat
precision = conf_mat[2,2]/(conf_mat[2,2]+conf_mat[2,1])
recall = conf_mat[2,2]/(conf_mat[2,2]+conf_mat[1,2])
precision
recall



# k nearest neighbors

result = predict(ProjectModelTrain, senateTest, type="response")

# confusion matrix
binary_result = (result > 0.5)*1
conf_mat = table(binary_result, senateTest$IncumbentWon)
conf_mat
precision = conf_mat[2,2]/(conf_mat[2,2]+conf_mat[2,1])
recall = conf_mat[2,2]/(conf_mat[2,2]+conf_mat[1,2])
precision
recall


# random forest
library(rpart)
equation<-as.formula("IncumbentWon~pvi*Republican+PercentageRaised+GenericBallotSept")
# tree_mod1<-rpart(equation, data=senateTrain)
# result = predict(tree_mod1, senateTest)

library(randomForest)
senateTrain$IncumbentWon<-as.factor(senateTrain$IncumbentWon) # Leave as continuous for regression
mod1_forest<-randomForest(equation, data=senateTest, 
                          ntree=201, mtry=2)
mod1_forest # This confusion matrix is "out of bag"

# confusion matrix
conf_mat = mod1_forest$confusion
precision = conf_mat[2,2]/(conf_mat[2,2]+conf_mat[2,1])
recall = conf_mat[2,2]/(conf_mat[2,2]+conf_mat[1,2])
precision
recall