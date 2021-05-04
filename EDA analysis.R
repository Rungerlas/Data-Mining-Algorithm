#First name: Ruohuan 
#Last name: Xu
#ID: 10453903

rm(list = ls())

imputdata <- read.csv("D://chrome//breast-cancer-wisconsin.data.csv", header = TRUE, na.strings = '?')
imputdata

#I.	Summarizing each column (e.g. min, max, mean )

summary(imputdata$F1)
summary(imputdata$F2)
summary(imputdata$F3)
summary(imputdata$F4)
summary(imputdata$F5)
summary(imputdata$F6)
summary(imputdata$F7)
summary(imputdata$F8)
summary(imputdata$F9)

#II.	Identifying missing values

imputdata[is.na(imputdata)]
is.na(imputdata)
sum(is.na(imputdata))

#III.	Replacing the missing values with the ¡°mean¡± of the column.

material <- imputdata$F6
replaced <- mean(imputdata$F6, na.rm = TRUE)
str(replaced)
imputdata[is.na(imputdata$F6)] <- replaced

#IV.	Displaying the frequency table of ¡°Class¡± vs. F6

table(Class = imputdata$Class, Sitem = imputdata$F6)
pairs(imputdata[c(2:5,11)],main = "table", pch = 21 , bg= c("blue","green")[factor(imputdata$Class)])

#V.	Displaying the scatter plot of F1 to F6, one pair at a time

plot(imputdata$F1~imputdata$F2)
plot(imputdata$F1~imputdata$F3)
plot(imputdata$F1~imputdata$F4)
plot(imputdata$F1~imputdata$F5)
plot(imputdata$F1~imputdata$F6)
plot(imputdata$F2~imputdata$F3)
plot(imputdata$F2~imputdata$F4)
plot(imputdata$F2~imputdata$F5)
plot(imputdata$F2~imputdata$F6)
plot(imputdata$F3~imputdata$F4)
plot(imputdata$F3~imputdata$F5)
plot(imputdata$F3~imputdata$F6)
plot(imputdata$F4~imputdata$F5)
plot(imputdata$F4~imputdata$F6)
plot(imputdata$F5~imputdata$F6)

#VI.	Show histogram box plot for columns F7 to F9

hist(imputdata$F7, main = "Histogram F7", xlab = "columns F7")
hist(imputdata$F8, main = "Histogram F8", xlab = "columns F8")
hist(imputdata$F9, main = "Histogram F9", xlab = "columns F9")

boxplot(imputdata$F7, main = "Box plot F7", xlab = "columns F7")
boxplot(imputdata$F8, main = "Box plot F8", xlab = "columns F8")
boxplot(imputdata$F9, main = "Box plot F9", xlab = "columns F9")

#2- Delete all the objects from your R- environment. Reload the ¡°breast-cancer-wisconsin.data.csv¡± from canvas into R. Remove any row with a missing value in any of the columns.

rm(list = ls())

imputdata <- read.csv("D://chrome//breast-cancer-wisconsin.data.csv", header = TRUE, na.strings = '?')

imputdata2 <- na.omit(imputdata)
imputdata2
