rm(list=ls())

filename<-file.choose() 
imputdata<-  read.csv(filename,na.strings = c("?"))
imputdata2 <-na.omit(imputdata)
imputdata3 <- imputdata2[,2:17]
summary(imputdata3)
#index<-seq(1,nrow(imputdata3),by=5)
index<-sort(sample(nrow(imputdata3),round(.25*nrow(imputdata3))))
Test_data<-imputdata3[index,]

#pkgs <- c("factoextra",  "NbClust")
#install.packages(pkgs)
#install.packages('fpc')
library(factoextra)
library(NbClust)
library(cluster)
library(fpc)
library(neuralnet)

imputdata3$cnt = as.numeric(factor(imputdata3$cnt,
                                   levels = c('low','normal','high'),
                                   labels = c('0','1','2')))
Test_data$cnt = as.numeric(factor(Test_data$cnt,
                                   levels = c('low','normal','high'),
                                   labels = c('0','1','2')))

df <- scale(imputdata3_training)
head(df)
fviz_nbclust(df, kmeans, method = "silhouette", k.max=20)+
  labs(subtitle = "Silhouette method")
fviz_nbclust(df, kmeans, method = "wss", k.max=20) +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
fviz_nbclust(df, kmeans, nstart = 50,  method = "gap_stat", nboot = 50, k.max=20)+
  labs(subtitle = "Gap statistic method")

#?fviz_nbclust()
set.seed(123)
#?kmeans()
head(imputdata3_training)
kmeans_3<- kmeans(imputdata3,4,iter.max = 10,nstart = 100)
kmeans_3$centers
kmeans_3$cluster
head(kmeans_3$cluster)

kmeans_3$centers
fviz_cluster(kmeans_3,imputdata3)
table(imputdata3$cnt,kmeans_3$cluster)

write.csv(kmeans_3$centers,'/Users/ruohuanxu/Downloads/CCs.csv')
plot(imputdata3_training,col=kmeans_3)

print(kmeans_3$cluster)
cluster_1 = kmeans_3$cluster

fitted(kmeans_3)

predict.kmeans <- function(object, newdata){
  centers <- object$centers
  n_centers <- nrow(centers)
  dist_mat <- as.matrix(dist(rbind(centers, newdata)))
  dist_mat <- dist_mat[-seq(n_centers), seq(n_centers)]
  max.col(-dist_mat)
}

imputdata3 <- scale(imputdata3)
plotcluster(imputdata3, kmeans_3$cluster)

predict <- predict.kmeans(kmeans_3,Test_data)
result <- kmeans_3$cluster[index]
result <- unname(result)
wrong <- sum(predict == result,na.rm = TRUE)
rate <- wrong/length(predict)
sum1 <- sum(wrong,na.rm = TRUE)
wrongrate <-rate
wrongrate


