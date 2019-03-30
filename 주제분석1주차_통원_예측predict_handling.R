setwd("C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data")
setwd("C:/Users/Jungwoo Lim/Desktop")
final<-read.csv("final_data4.csv",header=T)

library(dplyr)
daily15_temp_data<-read.csv("daily15_temperature.csv",header=T)
daily15_windSpeed_data<-read.csv("daily15_windSpeed.csv",header=T)
daily15_humidity_data<-read.csv("daily15_humidity.csv",header=T)



daily15_temp_data$X<-as.character(daily15_temp_data$X)
daily15_windSpeed_data$X<-as.character(daily15_windSpeed_data$X)

daily15<-full_join(daily15_temp_data,daily15_windSpeed_data,by=c("X"="X"))
daily15<-cbind(daily15,daily15_humidity_data)
str(daily15_temp_data)
names(daily15)
daily15_predict<-subset(daily15,select=c(X,predicted.x,predicted.y,predicted))
daily15_predict<-na.omit(daily15_predict)

write.csv(daily15_predict,file="C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data/predict_weather.csv")
