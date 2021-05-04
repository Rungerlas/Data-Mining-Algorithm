#First name: Ruohuan 
#Last name: Xu
#ID: 10453903

rm(list = ls())

filename<-file.choose() 
imputdata<-  read.csv(filename)
N_imputdata<-na.omit(imputdata)
index<-sort(sample(nrow(N_imputdata),round(.25*nrow(N_imputdata))))

training<-N_imputdata[-index,]
test<-N_imputdata[index,]

features <- N_imputdata[-11][-1]
labels <- N_imputdata[,11]

training$Class <- as.character(training$Class)
training$Class <- as.factor(training$Class)

#install.packages('randomForest')
#install.packages('caret', dependencies = TRUE)
library(randomForest)
library(caret)

fit <- randomForest( Class~., data=training, importance=TRUE)
importance(fit)
print(fit)
varImpPlot(fit)
Prediction <- predict(fit, test)
table(actual=test[,4],Prediction)

radomforest1 <- randomForest(features, as.factor(labels))
print(radomforest1)
importance(radomforest1)

plot(radomforest1)
varImpPlot(radomforest1)

varimp <- varImp(radomforest1)
varImpPlot(radomforest1)
varimp[order(varimp$Overall, decreasing = TRUE), ,drop = FALSE]

