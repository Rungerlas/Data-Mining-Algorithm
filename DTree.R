#First name: Ruohuan
#Last name: Xu
#Student ID:10453903

rm(list = ls())

#reading the file
filename <- file.choose()
imputdata <- read.csv(filename,colClasses = c("Sample" = "character",
                                              "F1"="factor","F2"="factor","F3"="factor","F4"="factor","F5"="factor","F6"="factor","F7"="factor","F8"="factor","F9"="factor","Class"="factor"))
View(imputdata)

#install packages
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("RColorBrewer")

library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

dev.off()

#create CART
index <- sort(sample(nrow(imputdata),round(.25*nrow(imputdata))))
trainning <- imputdata[-index,]
testing <- imputdata[index,]

CART_class <- rpart(Class~.,data = trainning[,-1])
rpart.plot(CART_class)

CART_predict <- predict(CART_class,testing,type="class")
table(Actual = testing[,11],CART = CART_predict)

CART_predict2 <- predict(CART_class,testing)
str(CART_predict2)

prp(CART_class)
fancyRpartPlot(CART_class)

#error rate
CART_predict <- predict(CART_class,testing,type="class")
CART_error <- sum(testing[,11]!=CART_predict)
CART_errorrate <- CART_error/length(testing[,11])
CART_errorrate
