rm(list = ls())

library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)

Data <- read.csv("Processed_Data_v2.csv",stringsAsFactors = TRUE)

Data <- Data[-1]

dir <- sort(sample(c(1:nrow(Data)),size = 0.7*nrow(Data)))

Train_Data <- Data[dir,2:16]
Test_Data <- Data[-dir,2:16]
Train_Class <- as.integer(Data[dir,1]) - 1
Test_Class <- as.integer(Data[-dir,1])

Model <- xgboost(data = data.matrix(Train_Data),
                 label = Train_Class,
                 eta = 0.01,
                 max_depth = 14, 
                 nround=50, 
                 subsample = 0.75,
                 colsample_bytree = 1,
                 seed = 1,
                 eval_metric = "mlogloss",
                 objective = "multi:softprob",
                 nthread = 3,
                 num_class = 3)
result <- predict(Model,data.matrix(Test_Data),reshape = TRUE)
result <- as.data.frame(result)
colnames(result) = levels(Data[-dir,1])

pred <- apply(result, 1, which.max)
cm <- table(Test_Class,pred)
error_rate <- 1 - sum(diag(cm))/length(Test_Class)

xgb.save(Model,"XGboost_Model")
