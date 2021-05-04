
rm(list=ls())
install.packages("neuralnet")
library("neuralnet")
install.packages("NeuralNetTools")
library('NeuralNetTools')

csvfile<-file.choose()

bc2<-read.csv(csvfile,na.string="?")
bc2<-bc2[,-1]
index<-seq(from=1, to=nrow(bc2), by=6)
test<-bc2[index,]
training<-bc2[-index,]

tree<-neuralnet( cnt~t1+t2+hum+wind_speed+is_holiday+is_weekend+is_cloudy+is_raining+is_snowing+is_spring+is_summer+is_fall+is_winter+is_night+hour,
                 data=training,hidden = 9,rep =1, stepmax = 5e+06, threshold = 0.01)

neuralweights(tree)
plot(tree)
sum(tree$weights[[1]][[1]][1,])
sum(tree$weights[[1]][[1]][2,])
sum(tree$weights[[1]][[1]][3,])
sum(tree$weights[[1]][[1]][4,])
sum(tree$weights[[1]][[1]][5,])
sum(tree$weights[[1]][[1]][6,])
sum(tree$weights[[1]][[1]][7,])
sum(tree$weights[[1]][[1]][8,])
sum(tree$weights[[1]][[1]][9,])
sum(tree$weights[[1]][[1]][10,])
sum(tree$weights[[1]][[1]][11,])
sum(tree$weights[[1]][[1]][12,])
sum(tree$weights[[1]][[1]][13,])
sum(tree$weights[[1]][[1]][14,])
sum(tree$weights[[1]][[1]][15,])
sum(tree$weights[[1]][[1]][16,])

net_results<-predict(tree,test)
cm<-table(test$cnt,apply(net_results,1,which.max))
rate<-sum(cm[1,1]+cm[2,2]+cm[3,3])/sum(cm)
rate
