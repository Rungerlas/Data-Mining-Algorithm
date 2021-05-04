rm(list = ls())

library(factoextra)
library(NbClust)
library("class")

Data <- read.csv("Processed_Data_v2.csv", stringsAsFactors = TRUE)
Data <- Data[-1]
True_Class <- Data$cnt
Data <- Data[-1]
d <- dist(Data)
True_Class <- factor(x = True_Class, levels = c("low","normal","high"))

dir <- sort(sample(c(1:nrow(Data)),size = 0.7*nrow(Data)))

creat_new_data <- function(Data,tree,TrueClass)
{
  new_data <- Data
  new_data$clusterlabel <- tree
  new_data$ClusterOne <- c(0)
  new_data$ClusterTwo <- c(0)
  new_data$ClusterThree <- c(0)
  new_data <- change_flag_variable(new_data)
  new_data <- new_data[-16]
  new_data$Class <- TrueClass
  return(new_data)
}
  
change_flag_variable <- function(x)
{
  for(i in 1:nrow(x))
  {
    if(x$clusterlabel[i] == 1)
      x$ClusterOne[i] <- 1
    if(x$clusterlabel[i] == 2)
      x$ClusterTwo[i] <- 1
    if(x$clusterlabel[i] == 3)
      x$ClusterThree[i] <- 1
  }
  return(x)
}

knn_model_creation <- function(Data,dir)
{
  max_accuracy <- 0
  True_Class <- Data[-dir,19]
  i <- 1
  for(i in 1:100)
  {
    model <- knn(train = Data[dir,-19], test = Data[-dir,-19], cl = Data[dir,19], k = i)
    cm <- table(factor(x = True_Class, levels = c("low","normal","high")),model)
    accuracy <- sum(diag(cm))/length(Data[-dir,19])
    if(accuracy > max_accuracy)
    {
      max_model <- model
      max_accuracy <- accuracy
      k <- i
    }
  }
  
  model_accur_k <- list(max_model,max_accuracy,k)
  return(model_accur_k)
}

#using ward.D2 method
ward <- hclust(d,method = "ward.D2")
ward_tree <- cutree(ward,k = 3)
ward_data <- creat_new_data(Data,ward_tree,True_Class)
ward_list <- knn_model_creation(ward_data,dir)
error_rate_ward <- 1 - as.numeric(ward_list[2])
ward_k <- as.numeric(ward_list[3])

#using single method
sing <- hclust(d,method = "single")
sing_tree <- cutree(sing,k = 3)
sing_data <- creat_new_data(Data,sing_tree,True_Class)
sing_list <- knn_model_creation(sing_data,dir)
error_rate_single <- 1 - as.numeric(sing_list[2])
single_k <- as.numeric(sing_list[3])

#using complete method
com <- hcut(d,k=3,hc_method = "complete")
fviz_cluster(com,Data)
com_tree <- cutree(com,k = 3)
com_data <- creat_new_data(Data,com_tree,True_Class)
com_list <- knn_model_creation(com_data,dir)
error_rate_complete <- 1 - as.numeric(com_list[2])
complete_k <- as.numeric(com_list[3])
                   
#using average method
ave <- hcut(d,k=3,hc_method = "average")
fviz_cluster(ave,Data)
ave_tree <- cutree(ave,k = 3)
ave_data <- creat_new_data(Data,ave_tree,True_Class)
ave_list <- knn_model_creation(ave_data,dir)
error_rate_average <- 1 - as.numeric(ave_list[2])
average_k <- as.numeric(ave_list[3])

#using mcquitty method
mcq <- hclust(d,method = "mcquitty")
mcq_tree <- cutree(mcq,k = 3)
mcq_data <- creat_new_data(Data,mcq_tree,True_Class)
mcq_list <- knn_model_creation(mcq_data,dir)
error_rate_mcquitty <- 1 - as.numeric(mcq_list[2])
mcquitty_k <- as.numeric(mcq_list[3])

#using median method
med <- hclust(d,method = "median")
med_tree <- cutree(med,k = 3)
med_data <- creat_new_data(Data,med_tree,True_Class)
med_list <- knn_model_creation(med_data,dir)
error_rate_median <- 1 - as.numeric(med_list[2])
median_k <- as.numeric(med_list[3])

#using centroid method
cen <- hclust(d,method = "centroid")
cen_tree <- cutree(cen,k = 3)
cen_data <- creat_new_data(Data,cen_tree,True_Class)
cen_list <- knn_model_creation(cen_data,dir)
error_rate_centroid <- 1 - as.numeric(cen_list[2])
centroid_k <- as.numeric(cen_list[3])

#only use knn
max_accuracy_knn <- 0
i <- 1
for(i in 1:100)
{
  model <- knn(train = Data[dir,], test = Data[-dir,], cl = True_Class[dir], k = i)
  cm <- table(factor(x = True_Class[-dir], levels = c("low","normal","high")),model)
  accuracy <- sum(diag(cm))/nrow(Data[-dir,])
  if(accuracy > max_accuracy_knn)
  {
    max_model_knn <- model
    max_accuracy_knn <- accuracy
    k_knn <- i
  }
}
error_rate_knn <- 1 - max_accuracy_knn

output <- data.frame(error_rate_ward,ward_k,error_rate_average,average_k,error_rate_centroid,centroid_k,error_rate_complete,complete_k,error_rate_mcquitty,mcquitty_k,error_rate_single,single_k,error_rate_knn,k_knn)

write.csv(output,"hclust_result.csv")

