# First name: Ruohuan
# Last name: Xu
# ID : 10453903

rm(list=ls())

filename <- file.choose()
imputdata<- read.csv(filename,colClasses=c("Sample"="character",
                                                "F1"="factor","F2"="factor","F3"="factor",
                                                "F4"="factor","F5"="factor","F6"="factor",
                                                "F7"="factor","F8"="factor","F9"="factor",
                                                "Class"="factor"))

index <- sort(sample(nrow(imputdata), round(.25*nrow(imputdata))))
training <- imputdata[-index,]
testing <- imputdata[index,]

#install.packages('C50')
library('C50')

C50_list <- C5.0( Class~.,data=training[,-1] )
summary(C50_list)
plot(C50_list)
Predict<-predict( C50_list ,testing , type="class" )
table(actual=testing[,11],C50=Predict)

wrong <- (testing[,11]!=Predict)
wrongrate<-sum(wrong)/length(testing[,11])
wrongrate

