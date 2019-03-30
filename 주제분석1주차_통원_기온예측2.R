#################################Monthly Average!&Daily Average!#############################################
####TimeSeries DataExploring####
mean_temp_final100<-ts(mean_temp_final$Temp,frequency=36,start=c(2015,1,5),end=c(2018,5,5))
mean_temp_final200<-ts(mean_temp_final2$Temp,frequency=365,start=c(2015,1,1),end=c(2018,5,2))
mean_temp_final300<-ts(mean_temp_final3$Temp,frequency=12,start=c(2015,1),end=c(2018,5))

str(mean_temp_final100)
str(mean_temp_final200)
str(mean_temp_final300)

plot(mean_temp_final100)
plot(aggregate(mean_temp_final100,FUN=mean))#mean down!
plot(aggregate(mean_temp_final100,FUN=var))#variance up!

plot(mean_temp_final200)
plot(aggregate(mean_temp_final200,FUN=mean))#mean down!
plot(aggregate(mean_temp_final200,FUN=var))#variance up

plot(mean_temp_final300)
plot(aggregate(mean_temp_final300,FUN=mean))#mean down!
plot(aggregate(mean_temp_final300,FUN=var))#variance up

#########auto.arima##############
    
#Daily_15days
#####Standardize Variance!######
log_mean_humidity_final100<-log(mean_humidity_final100)
log_mean_humidity_final200<-log(mean_humidity_final200)
log_mean_humidity_final300<-log(mean_humidity_final300)


###이거 어떻게 할지 논의해야겠다. ㅠㅠ 
#########auto.arima##############

#Daily_15days
#분산 안정화 시킨거
plot(log_mean_humidity_final100)
test(log_mean_humidity_final100)
auto.arima(log_mean_humidity_final100)#ARIMA(1,0,0)(1,1,0)[36] #AIC: -83.5

lr<-log_mean_humidity_final100

lr110.110<-Arima(lr, order=c(1,1,0),seasonal = list(order=c(0,0,1), period=36),method="ML");AIC(lr110.110)#AIC:-110.4294
lr111.110<-Arima(lr, order=c(1,1,1),seasonal = list(order=c(0,0,1), period=36),method="ML");AIC(lr111.110)#AIC:-128.9215
lr110.100<-Arima(lr, order=c(1,1,0),seasonal = list(order=c(1,0,0), period=36),method="ML");AIC(lr110.100)#AIC:-112.1172
lr111.111<-Arima(lr, order=c(1,1,1),seasonal = list(order=c(1,1,1), period=36),method="ML");AIC(lr111.111)#AIC:-75.17033
lr100.211<-Arima(lr, order=c(1,0,0),seasonal = list(order=c(2,1,1), period=36),method="ML");AIC(lr100.211)#AIC:-79.76123


test(lr111.110$residuals)#iid success!


flr111.110<-forecast::forecast(lr111.110, h=36)
plot(flr111.110)
flr111.110

exp.xflr111.110<-exp(flr111.110$x)
exp.mflr111.110<-exp(flr111.110$mean)
index(exp.xflr111.110)

library(zoo)
hw=HoltWinters(exp.xflr111.110)
p<-predict(hw,25,prediction.interval = TRUE)
all <- merge(actual=as.zoo(exp.xflr111.110), predicted=as.zoo(p))
head(all)

library(lubridate)
install.packages("dygraphs")
library(dygraphs)

all.xts <- xts(all, date_decimal(index(all)))
dygraph(all.xts, "time series dygraph") %>%
  dySeries("actual", label = "Actual") %>%
  dySeries(c("lwr", "fit", "upr"), label = "Predicted")

all.xts
daily15_humidity<-data.frame(all.xts)
write.csv(daily15_humidity,file="C:/Users/Jungwoo Lim/Desktop/daily15_humidity.csv")



#모델선택과정은 밑으로 가세요


#Monthly
#분산 안정화 안시킨거
plot(mean_temp_final300)
test(mean_temp_final300)
auto.arima(mean_temp_final300)#ARIMA(0,0,0)(0,1,0)[12], AIC:98.46
lr3<-mean_temp_final300
lr000.010<-Arima(lr3, order=c(0,0,0),seasonal = list(order=c(0,1,0), period=12),method="ML");AIC(lr000.010)#AIC:98.45572
lr010.010<-Arima(lr3, order=c(0,1,0),seasonal = list(order=c(0,1,0), period=12),method="ML");AIC(lr010.010)#AIC:105.1143
lr110.010<-Arima(lr3, order=c(1,1,0),seasonal = list(order=c(0,1,0), period=12),method="ML");AIC(lr110.010)#AIC:104.5494
lr010.110<-Arima(lr3, order=c(0,1,0),seasonal = list(order=c(1,1,0), period=12),method="ML");AIC(lr010.110)#AIC:106.0366
lr110.000<-Arima(lr3, order=c(0,0,0),seasonal = list(order=c(1,1,1), period=12),method="ML");AIC(lr110.000)#AIC:99.2997



flr000.010<-forecast::forecast(lr000.010,h=25)
plot(flr000.010)
monthly<-data.frame(flr000.010)
test(lr000.010$residuals)#iid나름만족!



write.csv(monthly,file="C:/Users/Jungwoo Lim/Desktop/monthly_temp.csv")
write.csv(daily15,file="C:/Users/Jungwoo Lim/Desktop/daily15_temp.csv")






##############################################################################################
################# train data vs test data >> rmse 비교 >> 최적합의 모델 선택 #################
##############################################################################################
#daily15 model 선택
plot(mean_temp_final100)
lr<-mean_temp_final100
train<-(lr[1:88])
test<-(lr[89:113])
ts.train<-ts(train)
ts.test<-ts(test)
auto.arima(lr)

#auto.arima(lr)의 결과로 나온 모델(ARIMA(2,1,0)(0,1,1)[36])을 기준으로 후보군 모델들을 설정하고, 트레이닝 데이터 이용해 SARIMA modeling.

train.200.011<-Arima(ts.train, order=c(2,0,0),seasonal = list(order=c(0,1,1), period=36),method="ML");AIC(train.200.011)#AIC:700.8021
train.210.011<-Arima(ts.train, order=c(2,1,0),seasonal = list(order=c(0,1,1), period=36),method="ML");AIC(train.210.011)#AIC:707.958
train.210.001<-Arima(ts.train, order=c(2,1,0),seasonal = list(order=c(0,0,1), period=36),method="ML");AIC(train.210.001)#AIC:1079.579
train.210.111<-Arima(ts.train, order=c(2,1,0),seasonal = list(order=c(1,1,1), period=36),method="ML");AIC(train.210.111)#AIC:709.9437
train.210.110<-Arima(ts.train, order=c(2,1,0),seasonal = list(order=c(1,1,0), period=36),method="ML");AIC(train.210.110)#AIC:708.2355
train.110.011<-Arima(ts.train, order=c(1,1,0),seasonal = list(order=c(0,1,1), period=36),method="ML");AIC(train.110.011)#AIC:711.436


f.train.200.011<-forecast::forecast(train.200.011, h=72) #train data로 만든 모델로 예측값
f.train.210.011<-forecast::forecast(train.210.011, h=72) #train data로 만든 모델로 예측값
f.train.210.001<-forecast::forecast(train.210.001, h=72) #train data로 만든 모델로 예측값
f.train.210.110<-forecast::forecast(train.210.110, h=72) #train data로 만든 모델로 예측값
f.train.110.011<-forecast::forecast(train.110.011, h=72) #train data로 만든 모델로 예측값


AIC.train<-c(AIC(train.200.011),AIC(train.210.011),AIC(train.210.001),AIC(train.210.110),AIC(train.110.011))
AIC.train #train.200.011 >> best model

#test data로 미리 빼놓았던 실제 데이터와 train data로 만들었던 여러 후보군 모델들로 도출해낸 예측값을 이용해 RMSE 측정
rmse(f.train.200.011$mean, test) #15.96727
rmse(f.train.210.011$mean, test) #15.95714
rmse(f.train.210.001$mean, test) #14.45432 >> best model
rmse(f.train.210.110$mean, test) #15.96111
rmse(f.train.110.011$mean, test) #16.00199 

plot(f.train.412.012) #AIC와 rmse를 모두 비교해본 결과 train.200.011 모델이 최적합 모델이었다.




#monthly 모델선택
plot(mean_temp_final300)
lr<-mean_temp_final300
train<-(lr[1:28])
test<-(lr[29:40])
ts.train<-ts(train)
ts.test<-ts(test)
auto.arima(lr)
#auto.arima(lr)의 결과로 나온 모델(ARIMA(2,1,0)(0,1,1)[36])을 기준으로 후보군 모델들을 설정하고, 트레이닝 데이터 이용해 SARIMA modeling.

train.000.010<-Arima(ts.train, order=c(0,0,0),seasonal = list(order=c(0,1,0), period=12),method="ML");AIC(train.000.010)#AIC:49.6849
train.010.010<-Arima(ts.train, order=c(0,1,0),seasonal = list(order=c(0,1,0), period=12),method="ML");AIC(train.010.010)#AIC:52.37072
train.110.010<-Arima(ts.train, order=c(1,1,0),seasonal = list(order=c(0,1,0), period=12),method="ML");AIC(train.110.010)#AIC:54.12538
train.010.110<-Arima(ts.train, order=c(0,1,0),seasonal = list(order=c(1,1,0), period=12),method="ML");AIC(train.010.110)#AIC:52.18324
train.110.000<-Arima(ts.train, order=c(0,0,0),seasonal = list(order=c(1,1,1), period=12),method="ML");AIC(train.110.000)#AIC:50.88942


f.train.000.010<-forecast::forecast(train.000.010, h=24) #train data로 만든 모델로 예측값
f.train.010.010<-forecast::forecast(train.010.010, h=24) #train data로 만든 모델로 예측값
f.train.110.010<-forecast::forecast(train.110.010, h=24) #train data로 만든 모델로 예측값
f.train.010.110<-forecast::forecast(train.010.110, h=24) #train data로 만든 모델로 예측값
f.train.110.000<-forecast::forecast(train.110.000, h=24) #train data로 만든 모델로 예측값


AIC.train<-c(AIC(train.000.010),AIC(train.010.010),AIC(train.110.010),AIC(train.010.110),AIC(train.110.000))
AIC.train #train.000.010 >> best model

#test data로 미리 빼놓았던 실제 데이터와 train data로 만들었던 여러 후보군 모델들로 도출해낸 예측값을 이용해 RMSE 측정
rmse(f.train.000.010$mean, test) #1.529104
rmse(f.train.010.010$mean, test) #1.415202
rmse(f.train.110.010$mean, test) #1.387894 >> best model
rmse(f.train.010.110$mean, test) #1.785915
rmse(f.train.110.000$mean, test) #1.561379 

plot(f.train.000.010) #AIC와 rmse를 모두 비교해본 결과 train.000.010 모델이 최적합 모델이었다.



