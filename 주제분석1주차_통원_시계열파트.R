getwd()
setwd("C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data")


data<-read.csv("final_weather.csv",header=T)


install.packages("forecast")
library(forecast)
install.packages("dplyr")
library(dplyr)
install.packages("stringr")
library("stringr")
install.packages("xts")
library("xts")

####Data handling####
sum(is.na(data$temp))/nrow(data)
sum(is.na(data$rainfall))/nrow(data)
sum(is.na(data$windSpeed))/nrow(data)
sum(is.na(data$windDirection))/nrow(data)
sum(is.na(data$humidity))/nrow(data)
sum(is.na(data$steamPressure))/nrow(data)
sum(is.na(data$dewpointTemp))/nrow(data)
sum(is.na(data$spotAtmospherePressure))/nrow(data)
sum(is.na(data$sealevelAtmospherePressure))/nrow(data)
sum(is.na(data$sunshine))/nrow(data)
sum(is.na(data$solarRadiation))/nrow(data)
sum(is.na(data$snowCover))/nrow(data)
sum(is.na(data$X3hrSnowCover))/nrow(data)
sum(is.na(data$totalCloudObs))/nrow(data)
sum(is.na(data$visibility))/nrow(data)
sum(is.na(data$groundStateCode))/nrow(data)
sum(is.na(data$domesticStateCode))/nrow(data)
sum(is.na(data$groundSurfaceTemp))/nrow(data)
sum(is.na(data$earthTemp_5cm))/nrow(data)
sum(is.na(data$earthTemp_10cm))/nrow(data)
sum(is.na(data$earthTemp_20cm))/nrow(data)
sum(is.na(data$earthTemp_30cm))/nrow(data)
sum(is.na(data$최저운고.100m..))/nrow(data)
#NA 50% 이상인건 지워버리자
data$rainfall<-NULL
data$snowCover<-NULL
data$X3hrSnowCover<-NULL
data$groundStateCode<-NULL
data$domesticStateCode<-NULL
data$X<-NULL

dataf<-data
dataf<-subset(data,subset=is.na(data$temp)!=TRUE)

str(dataf)

#변수들 중에 date가 10일미만이면 0, 20일미만이면 1, 31일미만이면 2로 코딩해주자!
#5일단위도 넣어보았다!
#1일단위도 넣어보았다!
names(dataf)
temp<-data.frame(cbind(dataf$year,dataf$month,dataf$date,dataf$temp))
names(temp)<-c("year","month","date","temp")

temp$date_fix<-ifelse(temp$date<10,0,ifelse(temp$date<20,1,2))
temp$date_fix2<-ifelse(temp$date<5,0,
                            ifelse(temp$date<10,1,
                                   ifelse(temp$data.Date<15,2,
                                          ifelse(temp$date<20,3,
                                                 ifelse(temp$date<25,4,5)))))



temp$data.YMD<-paste(temp$data.Year,temp$data.Month,temp$data.Date_fix,sep = "-")
temp$data.YMD2<-paste(temp$data.Year,temp$data.Month,temp$data.Date_fix2,sep = "-")
temp$data.YMD3<-paste(temp$data.Year,temp$data.Month,temp$data.Date,sep = "-")
meanTemperature<-temp %>% group_by(data.YMD) %>% summarise(mean.temp=mean(data.temp))
meanTemperature2<-temp %>% group_by(data.YMD2) %>% summarise(mean.temp=mean(data.temp))
meanTemperature3<-temp %>% group_by(data.YMD3) %>% summarise(mean.temp=mean(data.temp))
meanTemperature<-na.omit(meanTemperature)
meanTemperature2<-na.omit(meanTemperature2)
meanTemperature3<-na.omit(meanTemperature3)
####Transform Data to TimeSeries####
ts_meanTemp<-as.xts(meanTemperature3$mean.temp,as.Date(meanTemperature3$data.YMD3, order.by=as.POSIXct(meanTemperature3$data.YMD3)))
names(ts_meanTemp)<-"Temperature"


class(ts_meanTemp)
start(ts_meanTemp);end(ts_meanTemp)
frequency(temp) #cycle of ts

plot(temp,ylab="Temperature") #plotting time series
plot(aggregate(AirPassengers,FUN=var),ylab="Passengers") #trend

dat<-log(AirPassengers) #stabilizing variance
plot(dat,ylab="Passengers")

#ARIMA
par(mfrow=c(1,2))
Acf(diff(data$temp),lag=50);Pacf(diff(data$temp),lag=50)
par(mfrow=c(1,1))

#SARIMA
s.diff<-diff(diff(data$temp),lag=12)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(s.diff);title("Both Trend & Seasonality Diff")
Acf(s.diff,lag=50);Pacf(s.diff,lag=50)

fit0<-auto.arima(dat);fit0 #AIC=-483.4 ARIMA(0,1,1)(0,1,1)[12]
fit1<-arima(dat, order=c(1,1,0), seasonal=list(order=c(1,0,0), period=12)) #aic=-469.89
fit2<-arima(dat, order=c(0,1,1), seasonal=list(order=c(1,0,0),period=12)) #aic = -470.39

