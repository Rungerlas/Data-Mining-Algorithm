rm(list = ls())

Data <- read.csv(file = "london_merged.csv", stringsAsFactors = FALSE)

summary(Data)

# 1 means low, 2 means noraml, 3 means high
judge_level <- function(x)
{
  Qu <- quantile(x)
  First_quantile <- Qu[2]
  Median <- Qu[3]
  Thrid_quantile <- Qu[4]
  
  for(i in 1:NROW(x))
  {
    if(x[i] <= First_quantile)
      x[i] = 1
    else if(First_quantile < x[i] & x[i] <= Thrid_quantile)
      x[i] = 2
    else
      x[i] = 3
  }
  return(x)
}

Chi_Squared_test <- function(x)
{
  p_value <- table(c(1:(ncol(x))-1),c(1:(ncol(x))-1))
  for(i in 2:ncol(x))
  {
    for(j in 2:ncol(x))
    {
      result <- chisq.test(x[,i],x[,j])
      p_value[i-1,j-1] <- result$p.value
    }
  }
  return(p_value)
}

normalize <- function(x)
{
  
  x <- (x-min(x))/(max(x)-min(x))
  return(x)
}

weather_code_process <- function(x,y)
{
  for(i in 1:nrow(x))
  {
    if(x$weather_code[i] >= 2)
      y$is_cloudy[i] = 1
    if(x$weather_code[i] >=7 & x$weather_code[i] < 26)
      y$is_raining[i] = 1
    if(x$weather_code[i] >= 26)
      y$is_snowing[i] = 1
  }
  
  return(y)
}

season_process <- function(x,y)
{
  for(i in 1:nrow(x))
  {
    if(x$season[i] == 0)
      y$is_spring[i] = 1
    if(x$season[i] == 1)
      y$is_summer[i] = 1
    if(x$season[i] == 2)
      y$is_fall[i] = 1
    if(x$season[i] == 3)
      y$is_winter[i] = 1
  }
  return(y)
}

time_process <- function(x,y)
{
  for(i in 1:nrow(x))
  {
    hour <- as.numeric(format(strptime(x$timestamp[i],"%Y-%m-%d %H:%M:%S",tz="") ,format = "%H"))
    y$hour[i] <- hour
    if(!is.na(hour))
      if(hour > 17||hour < 5)
        y$is_night[i] <- 1
  }
  return(y)
}

New_Data <- Data
New_Data <- New_Data[-10]
New_Data <- New_Data[,-1]
New_Data <- New_Data[,-6]
New_Data$is_cloudy <- c(0)
New_Data$is_raining <- c(0)
New_Data$is_snowing <- c(0)
New_Data$is_spring <- c(0)
New_Data$is_summer <- c(0)
New_Data$is_fall <- c(0)
New_Data$is_winter <- c(0)
New_Data$is_night <- c(0)
New_Data$hour <- c(0)
New_Data$cnt <- judge_level(New_Data$cnt)
New_Data$cnt <- factor(New_Data$cnt,levels = c(1,2,3), labels = c("low","normal","high"))
New_Data <- weather_code_process(Data,New_Data)
New_Data <- season_process(Data,New_Data)
New_Data <- time_process(Data,New_Data)

analyse_independent <- as.table(Chi_Squared_test(New_Data))
library(gplots)
balloonplot(t(analyse_independent),main = "Chi_Squared_test",label = FALSE, show.margins = FALSE)

analyse_independent_origin <- as.table(Chi_Squared_test(Data))
balloonplot(t(analyse_independent),main = "Chi_Squared_test_before_process",label = FALSE, show.margins = FALSE)


New_Data$t1 <- normalize(New_Data$t1)
New_Data$t2 <- normalize(New_Data$t2)
New_Data$hum <- normalize(New_Data$hum)
New_Data$wind_speed <- normalize(New_Data$wind_speed)
New_Data$hour <- normalize(New_Data$hour)

write.csv(New_Data,"Processed_Data_v2.csv")

