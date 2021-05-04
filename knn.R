#First name: Ruohuan 
#Last name: Xu
#ID: 10453903

rm(list=ls())

filename <- file.choose()
imputdata<- read.csv(filename, na.strings="?")
View(imputdata)

deletemissingrows <- na.omit(imputdata)
deletemissingrows

deletemissingrows$Class<- factor(deletemissingrows$Class, levels = c("2", "4"))
is.factor(deletemissingrows$Class)

library(kknn)

index <-sort(sample(nrow(deletemissingrows),as.integer(.70*nrow(deletemissingrows))))
trainingdata<-deletemissingrows[index,]
test<-deletemissingrows[-index,]

#k=3
knn1 <-kknn(formula=as.factor(Class)~., trainingdata[,-1], test, k=3, kernel = "triangular" )

fit <- fitted(knn1)
table(test$Class,fit)

wrongdatarate =sum(fit!=test$Class)/length(test$Class)
print(paste("Right rate:",(1-wrongdatarate)*100))

#k=5
knn2 <-kknn(formula=as.factor(Class)~., trainingdata, test, k=5, kernel = "triangular" )

fit <- fitted(knn2)
table(test$Class,fit)

wrongdatarate =sum(fit!=test$Class)/length(test$Class)
print(paste("Right rate:",(1-wrongdatarate)*100))

#k=10
knn3 <-kknn(formula=as.factor(Class)~., trainingdata, test, k=10, kernel = "triangular" )

fit <- fitted(knn3)
table(test$Class,fit)

wrongdatarate =sum(fit!=test$Class)/length(test$Class)
print(paste("Right rate:",(1-wrongdatarate)*100))

