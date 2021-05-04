

## remove all objects
rm(list=ls())


#install.packages('randomForest')
#install.packages('ggplot2')
#install.packages('cowplot')
library(randomForest)
library(ggplot2)

library(cowplot)
#theme_set(theme_cowplot())


#Load 
filename <- file.choose()
lbs1 <- read.csv(filename, na.strings = "?")

#shows the first six rows of data
head(lbs1)

#str() function describe the structure of the data, 
str(lbs1)

#summary
summary(lbs1)

#omit missing value
lbs<-na.omit(lbs1)

# Split inyo Train and Validation sets
# Training set : Validation Set = 70: 30 (random)
#set.seed(100)
idx<- sort(sample(nrow(lbs),as.integer(.70*nrow(lbs))))

#Store 70% in the "training" dataset
TrainSet<-lbs[idx,]
#Store the rest in the "test" dataset
ValidSet<-lbs[-idx,] 

# test, training
index <- seq (1,nrow(lbs),by=10)
ValidSet<-lbs[index,]
TrainSet<-lbs[-index,]

summary(TrainSet)
summary(ValidSet)


# Create a Random Forest model with default parameters
# (cnt to be predicted by the data in all of the other colums)
model <- randomForest(cnt~ .,data = TrainSet,  ntree = 1000,importance = TRUE)
model


#error rate (ntree)
oob.error.data <-data.frame(
  Trees=rep(1:nrow(model$err.rate), times=4),
  Type =rep(c("OOB", "high", "low", "normal"), each = nrow(model$err.rate)),
  Error =c(model$err.rate[,"OOB"],
           model$err.rate[,"high"],
           model$err.rate[,"low"],
           model$err.rate[,"normal"])
)
oob.error.data
getOption("max.print")
# 1000

ggplot(data=TrainSet, aes(x=Trees, y=Error)) + geom_line(aes(color=Type))


# Using For loop to identify the right mtry for model
a=c()
i=6
for (i in 4:9) {
  model2 <- randomForest(cnt ~ ., data = TrainSet, ntree = 1000, mtry = i, importance = TRUE)
  predValid <- predict(model2, ValidSet, type = "class")
  a[i-3] = mean(predValid == ValidSet$cnt)
}
a

plot(4:9,a)
# mtry = 8

# Fine tuning parameters of Random Forest model (ntree=1000, mtry=8)
model3 <- randomForest(cnt~ ., data = TrainSet, ntree = 1000, mtry = 8, importance = TRUE)
model3

# Predicting on train set
predTrain <- predict(model3, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$cnt)  

# Predicting on Validation set
predValid <- predict(model3, ValidSet, type = "class")

# Checking classification accuracy
mean(predValid == ValidSet$cnt)                    
table(predValid,ValidSet$cnt)


# To check important variables
importance(model3)        
varImpPlot(model3)   
