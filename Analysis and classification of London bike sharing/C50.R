### Clear the environment
rm(list=ls())
dev.off()

### Load data
bcwd <- read.csv("/Users/zhangjiaqian/Desktop/CS513/final/Processed_Data_v2.csv", na.strings = "?")

### import packages ###
library('C50')

### Remove the rows with missing values
bcwd_new <- na.omit(bcwd)

### Inital training and test data
index <- seq (1,nrow(bcwd_new),by=6)
test<-bcwd_new[index,]
training<-bcwd_new[-index,]

### Grow the tree
mytree <- C5.0(factor(cnt)~., data =training[,-1])

### Plot the tree
plot(mytree)

### Predict the result for test data
prediction<-predict(mytree ,test[,-1], type="class")

### Use the table to interpret the resutls
table(actual=test$cnt,prediction)

### Evaluate the tree by erro rate
wrong<- (test$cnt!=prediction)
rate<-sum(wrong)/length(wrong)

rate
### Accuracy ###
print(paste0('The accuracy percentage is: ', (1-rate)*100))
