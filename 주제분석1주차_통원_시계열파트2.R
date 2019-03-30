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
install.packages("tidyr")
library(tidyr)
install.packages("itsmr")
library("itsmr")
install.packages("Metrics")
library("Metrics")
install.packages("forecast")
library(forecast)

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
write.csv(dataf,file="C:/Users/Jungwoo Lim/Desktop/weather.csv")
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



temp$data.YMD<-paste(temp$year,temp$month,temp$date_fix,sep = "-")
temp$data.YMD2<-paste(temp$year,temp$month,temp$date_fix2,sep = "-")
temp$data.YMD3<-paste(temp$year,temp$month,temp$date,sep = "-")
temp$data.YMD4<-paste(temp$year,temp$month,sep = "-")
meanTemperature<-temp %>% group_by(data.YMD) %>% summarise(mean.temp=mean(temp))
meanTemperature2<-temp %>% group_by(data.YMD2) %>% summarise(mean.temp=mean(temp))
meanTemperature3<-temp %>% group_by(data.YMD3) %>% summarise(mean.temp=mean(temp))
meanTemperature4<-temp %>% group_by(data.YMD4) %>% summarise(mean.temp=mean(temp))

meanTemperature<-na.omit(meanTemperature)
meanTemperature2<-na.omit(meanTemperature2)
meanTemperature3<-na.omit(meanTemperature3)
meanTemperature4<-na.omit(meanTemperature4)

mean_temp<-str_split(meanTemperature4$data.YMD4,"-",n=2)
df <- data.frame(matrix(unlist(mean_temp), nrow=41, byrow=T))
mean_temp2<-str_split(meanTemperature3$data.YMD3,"-",n=3)
df2<- data.frame(matrix(unlist(mean_temp2),nrow=1218,byrow=T))

mean_temp_final<-cbind(df,meanTemperature4$mean.temp)
mean_temp_final<-mean_temp_final[-c(37:41),]
mean_temp_final3<-cbind(df2,meanTemperature3$mean.temp)


names(mean_temp_final)<-c("Year","Month","Temp")
write.csv(mean_temp_final,file="C:/Users/Jungwoo Lim/Desktop/weather2.csv")
names(mean_temp_final3)<-c("Year","Month","Date","Temp")

temp


#################################Monthly Average!&Daily Average!#############################################
####TimeSeries DataHandling####
mean_temp_final2<-ts(mean_temp_final$Temp,frequency=12,start=c(2015,1),end=c(2017,12))
mean_temp_final4<-ts(mean_temp_final3$Temp,frequency=365,start=c(2015,1,1),end=c(2018,5,2))


####Transform Data to TimeSeries####
#Monthly
par(mfrow=c(2,1))
class(mean_temp_final2)
start(mean_temp_final2);end(mean_temp_final2)
frequency(mean_temp_final2) #cycle of ts
plot(mean_temp_final2,ylab="Temperature") #plotting time series
plot(aggregate(mean_temp_final2,FUN=mean)) #trend
plot(aggregate(mean_temp_final2,FUN=var)) #variance가 점점 커지구 mean이 작아진다

#Daily
par(mfrow=c(2,1))
class(mean_temp_final4)
start(mean_temp_final4);end(mean_temp_final4)
frequency(mean_temp_final4) #cycle of ts
plot(mean_temp_final4,ylab="Temperature") #plotting time series
plot(aggregate(mean_temp_final4,FUN=mean)) #trend
plot(aggregate(mean_temp_final4,FUN=var)) #variance가 점점 커지구 mean이 작아진다

datsi<-mean_temp_final2
datsi2<-mean_temp_final4

####Eliminating Trend and Seasonality####

####Differencing####
#Monthly
plot(diff(datsi)) #only trend
plot(diff(datsi,lag=12)) #only seasonality
plot(diff(diff(datsi,lag=12))) #both (seasonality first)
plot(diff(diff(datsi),lag=12)) #both (trend first)
test(diff(diff(datsi,lag=12)))#추세와 계절성이 잘 제거되었다. 
test((diff(datsi,lag=12)))#계절성만 제거한게 훨씬 더 iid를 만족한다. 

#Daily
plot(diff(datsi2)) #only trend
plot(diff(datsi2,lag=365)) #only seasonality
plot(diff(diff(datsi2,lag=365))) #both (seasonality first)
plot(diff(diff(datsi2),lag=365)) #both (trend first)
test(diff(diff(datsi2,lag=365)))#그나마 seasonality를 먼저 제거한게 약한 iid를 만족



####Classical Decomposition####
#Monthly
dd<-decompose(datsi)
plot(dd)
random<-na.omit(dd$random) 
library(itsmr) # for test iid
test(random)#추세와 계절성이 매우 잘 지워졌다.

#Daily
dd<-decompose(datsi2)
plot(dd)
random<-na.omit(dd$random) 
library(itsmr) # for test iid
test(random)#추세와 계절성이 매우 잘 제거되지 못했다. ㅠㅠ


####ARIMA####
#Monthly
library(forecast)
par(mfrow=c(1,2))
Acf(diff(datsi),lag=50);Pacf(diff(datsi),lag=50)
par(mfrow=c(1,1))
#ARIMA(p,d,q)는 ARIMA(1,1,1)
#그치만 그래도 SARIMA 까지 해보자

#Daily
par(mfrow=c(1,2))
Acf(diff(datsi2),lag=1000);Pacf(diff(datsi2),lag=1000)
par(mfrow=c(1,1))
#ACF그래프에서 주기적으로 값이 커진다. ARIMA를 적용할 수 없음! 계절성을 잡아야한다!




####SARIMA####
#Monthly
s.diff<-diff(diff(datsi),lag=12)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(s.diff);title("Both Trend & Seasonality Diff")
Acf(s.diff,lag=50);Pacf(s.diff,lag=50)
#ACF: 1에서 단절
#PACF: 1에서 단절
#SARIMA(1,1,1)(0,0,0)

fit0M<-auto.arima(datsi);fit0M #ARIMA(0,0,0)(0,1,0)[12] 
test(residuals(fit0M))#iid가정 만족

#Daily
s.diff<-diff(diff(diff(datsi2,lag=365)))
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(s.diff);title("Both Trend & Seasonality Diff")
Acf(s.diff,lag=1000);Pacf(s.diff,lag=1000)
#ACF그래프가 1에서 단절되고 PACF그래프가 지수적으로 감소하니 
#ARIMA(P,D,Q)는 ARIMA(1,1,0)
#ARIMA(p,d,q)는 ARIMA(0,1,0)
#SARIMA(1,1,0)(1,0,0)
library(itsmr)
fit0D<-auto.arima(datsi2);fit0D #ARIMA(2,0,1)(0,1,0)[365] 
test(residuals(fit0D))
a<-data.frame(datsi2)
####OUT OF SAMPLE FORECAST####
#Monthly
train_m<-ts(datsi[1:24],start=c(2015,1),frequency = 12)
test_m<-ts(datsi[25:36],start=c(2017,1),frequency = 12)

#Daily
train_d<-ts(datsi2[1:731],start=c(2015,1),frequency=365)##2015~2016년까지
test_d<-ts(datsi2[732:1100],start=c(2017,6),frequency = 365)

library(forecast)

detach("package:itsmr")


install.packages("Metrics")
library("Metrics")

#Monthly_Model Selection
#auto.arima
fit0M<-auto.arima(train_m);fit0M#AIC: 175.95
prd0M<-forecast(fit0M,h=24)$mean#SARIMA(0,0,0)(0,1,0)
rmse(test_m,prd0M) #rmse: 11.63024

#fit1M
fit1m<-arima(train_m, order=c(1,1,1));fit1m#AIC:171.02
prd1m<-forecast(fit1m,h=24)$mean
rmse(test,prd1m) 

#fit2M
fit2m<-arima(train_m, order=c(0,1,1));fit2m#AIC:169.02
prd2m<-forecast(fit2m,h=24)$mean
rmse(test,prd2m)

#fit3M
fit3m<-arima(train_m, order=c(0,1,0));fit3m#AIC:168.04
prd3m<-forecast(fit3m,h=24)$mean
rmse(test,prd3m) 

#fit4M
fit4m<-arima(train_m, order=c(0,0,0),seasonal=list(order=c(0,1,0)));fit4m#AIC:39.37
prd4m<-forecast(fit4m,h=24)$mean
rmse(test,prd4m) 

#Forecasting-fit4m으로!
f0<-forecast(fit4m,h=12)
plot(f0)
par(mfrow=c(1,1))
f0
x<-data.frame(f0)
rownames(x)<-c( "Jan 2018", "Oct 2018", "Nov 2018", "Dec 2018", "Feb 2018", "Mar 2018", "Apr 2018", "May 2018", "Jun 2018","Jul 2018", "Aug 2018", "Sep 2018")
x
rownames(x)<-c(1,10,11,12,2,3,4,5,6,7,8,9)


#Daily_Model Selection
library(itsmr)
#auto.arima
fit0D<-auto.arima(train_d);fit0D#AIC:4263
prd0D<-forecast(fit0D,h=12)$fit0D#SARIMA(1,1,2)(0,1,0)[365] 
rmse(test,prd0D)

#fit1D
fit1d<-arima(train_d,order=c(1,1,0), seasonal=list(order=c(1,0,0),period=15));fit1d
prd1d<-forecast(fit1d,h=12)$mean
rmse(test,prd1d)

#fit2D
fit2d<-arima(train_d,order=c(2,0,1), seasonal=list(order=c(0,1,0),period=15));fit2d
prd2d<-forecast(fit2d,h=12)$mean
rmse(test,prd2d)

#fit3D
fit3d<-arima(train_d,order=c(2,0,1), seasonal=list(order=c(0,1,0),period=15));fit3d
prd3d<-forecast(fit3d,h=12)$mean
rmse(test,prd3d)

#fit4D
fit4d<-arima(train_d,order=c(1,1,0), seasonal=list(order=c(1,0,0),period=15));fit4d#AIC:4010.54
prd3d<-forecast(fit4d,h=12)$mean
rmse(test,prd3d)

#

#Forecasting-auto A


final_prediction<-x
write.csv(final_prediction,file="C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data/predict_month2.csv")



