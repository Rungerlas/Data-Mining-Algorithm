### Clear the environment
rm(list=ls())
dev.off()

### Load data
bcwd <- read.csv("/Users/zhangjiaqian/Desktop/CS513/final/Processed_Data_V2.csv", na.strings = "?")

### import plotting tools ###
library(rpart)
library(rpart.plot)  		
library(rattle)           
library(RColorBrewer) 

### Set up testing and training data ###
index<-seq(from=1, to=nrow(bcwd), by=6)
training<-bcwd[-index,]
testing<-bcwd[index,]

### Plot Tree ###
DTree<-rpart(as.factor(cnt)~.,data=training[,-1], method="class", parms=list(split="gini"))
fancyRpartPlot(DTree)

### Test data prediction ###
pred<-predict(DTree ,testing[,-1], type="class")

### Freq table###
table(actual=testing$cnt,pred)
View(table(actual=testing$cnt,pred))

### Calculate Error Rate ###
wrong<-(testing$cnt!=pred)
rate<-sum(wrong)/length(wrong)
rate
### Accuracy ###
print(paste0('The accuracy percentage is: ', (1-rate)*100))

