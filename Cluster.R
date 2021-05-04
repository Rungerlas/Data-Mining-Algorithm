#First name: Ruohuan 
#Last name: Xu
#ID: 10453903

rm(list=ls())

filename<-file.choose() 
imputdata<-read.csv(filename,na.strings = c("?"))
View(imputdata)

imputdata1<-na.omit(imputdata)
imputdata2<-na.omit(imputdata)

#Cluster 1
imputdata1_dist<-dist(imputdata1[,-c(1,11)])
result1<-hclust(imputdata1_dist)
hclust_1<-cutree(result1,2)
hclust_1
table(hclust_1,imputdata1[,11])

hcl <- hclust_1*2
wrong<- (hcl!=imputdata1[,11] )
wrongrate<-sum(wrong)/length(wrong)
wrongrate

#Cluster 2
imputdata2_Kmean<- kmeans(imputdata2[,-c(1,11)],2,nstart = 10)
imputdata2_Kmean$cluster
table(imputdata2_Kmean$cluster,imputdata2[,11])

km <- imputdata2_Kmean$cluster*2
wrong2<- (km !=imputdata2[,11] )
wrongrate2<-sum(wrong2)/length(wrong2)
wrongrate2

