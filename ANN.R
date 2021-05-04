#First name: Ruohuan 
#Last name: Xu
#ID: 10453903

rm(list = ls())

filename<-file.choose() 
imputdata<-  read.csv(filename,, na.strings = "?")

summary(imputdata)
mean(imputdata$F2)
mean(imputdata$F6)
apply(imputdata[,c(-1,-11)],2,mean)
N_imputdata<-na.omit(imputdata)

index <- seq (1,nrow(N_imputdata),by=5)
test<-N_imputdata[index,]
training<-N_imputdata[-index,]

install.packages('neuralnet')
library("neuralnet")
net1 <- neuralnet(Class~F1+F2+F3+F4+F5+F6+F7+F8+F9,training, hidden=5,threshold=0.1)

plot(net1)
net2 <-compute(net1, test[,c(-1,-11)]) 
Ann=as.numeric(net2$net.result)

round_Ann<-round(Ann)
Ann2<-ifelse(Ann<2.5,2,4)

table(Actual=test$Class,round_Ann)
table(Actual=test$Class,Ann2)

wrong<- (test$Class!=Ann2)
wrong_rate<-sum(wrong)/length(wrong)
wrong_rate

