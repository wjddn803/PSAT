#################################Monthly Average!&Daily Average!#############################################
####TimeSeries DataExploring####
mean_humidity_final100<-ts(mean_humidity_final$humidity,frequency=36,start=c(2015,1,5),end=c(2018,5,5))

str(mean_humidity_final100)

plot(mean_humidity_final100)
plot(aggregate(mean_humidity_final100,FUN=mean))#mean down!
plot(aggregate(mean_humidity_final100,FUN=var))#variance up!

#########auto.arima##############

#Daily_15days
#분산 안정화 시킨거
mean_humidity

plot(log_mean_humidity_final100)
test(log_mean_humidity_final100)
auto.arima(log_mean_humidity_final100)#ARIMA(1,0,0)(1,1,0)[36] #AIC:-83.57

lr<-log_mean_humidity_final100


lr100.011<-Arima(lr, order=c(1,0,0),seasonal = list(order=c(0,1,1), period=36),method="ML",transform.pars = FALSE);AIC(lr100.011)#AIC:-83.50474
lr110.011<-Arima(lr, order=c(1,1,0),seasonal = list(order=c(0,1,1), period=36),method="ML",transform.pars = FALSE);AIC(lr110.011)#AIC:-54.36789
lr110.001<-Arima(lr, order=c(1,1,0),seasonal = list(order=c(0,0,1), period=36),method="ML",transform.pars = FALSE);AIC(lr110.001)#AIC:-110.4069
lr110.211<-Arima(lr, order=c(1,1,0),seasonal = list(order=c(2,1,1), period=36),method="ML",transform.pars = FALSE);AIC(lr110.211)#AIC:-52.47119
lr110.110<-Arima(lr, order=c(1,1,0),seasonal = list(order=c(1,1,0), period=36),method="ML",transform.pars = FALSE);AIC(lr110.110)#AIC:-53.77205


test(lr110.001$residuals)#autoarimma가 가장 작지만 iid만족하게 하는건 이게 더 좋음


flr110.001<-forecast::forecast(lr110.001, h=72)
plot(flr110.001)
flr110.001
daily15<-data.frame(flr110.001)

exp.xflr110.001<-exp(flr110.001$x)
exp.mflr110.001<-exp(flr110.001$mean)

par(mfrow=c(1,1))

all<-merge(actual=as.zoo(exp.xflr110.001),predicted=as.zoo(exp.mflr110.001))
all.xts <- xts(all, date_decimal(index(all)))
dygraph(all.xts,"Humidity") %>% 
  dySeries("actual", label="Actual") %>% 
  dySeries("predicted", label="Predicted")

all.xts
daily15_humidity<-data.frame(all.xts)
write.csv(daily15_humidity,file="C:/Users/Jungwoo Lim/Desktop/daily15_humidity.csv")



#모델선택과정은 밑으로 가세요


#Monthly
#분산 안정화 안시킨거
plot(mean_humidity_final300)
test(mean_humidity_final300)
auto.arima(mean_humidity_final300)#ARIMA(0,0,0)(0,1,0)[12], AIC:98.46
lr3<-mean_humidity_final300
lr000.010<-Arima(lr3, order=c(0,0,0),seasonal = list(order=c(0,1,0), period=12),method="ML");AIC(lr000.010)#AIC:98.45572
lr010.010<-Arima(lr3, order=c(0,1,0),seasonal = list(order=c(0,1,0), period=12),method="ML");AIC(lr010.010)#AIC:105.1143
lr110.010<-Arima(lr3, order=c(1,1,0),seasonal = list(order=c(0,1,0), period=12),method="ML");AIC(lr110.010)#AIC:104.5494
lr010.110<-Arima(lr3, order=c(0,1,0),seasonal = list(order=c(1,1,0), period=12),method="ML");AIC(lr010.110)#AIC:106.0366
lr110.000<-Arima(lr3, order=c(0,0,0),seasonal = list(order=c(1,1,1), period=12),method="ML");AIC(lr110.000)#AIC:99.2997



flr000.010<-forecast::forecast(lr000.010,h=25)
plot(flr000.010)
monthly<-data.frame(flr000.010)
test(lr000.010$residuals)#iid나름만족!



write.csv(monthly,file="C:/Users/Jungwoo Lim/Desktop/monthly_humidity.csv")
write.csv(daily15,file="C:/Users/Jungwoo Lim/Desktop/daily15_humidity.csv")






##############################################################################################
################# train data vs test data >> rmse 비교 >> 최적합의 모델 선택 #################
##############################################################################################
#daily15 model 선택
plot(mean_humidity_final100)
lr<-log_mean_humidity_final100
train<-(lr[1:88])
test<-(lr[89:112])
ts.train<-ts(train)
ts.test<-ts(test)
auto.arima(lr)

#auto.arima(lr)의 결과로 나온 모델(ARIMAARIMA(1,0,0)(1,1,0)[36])을 기준으로 후보군 모델들을 설정하고, 트레이닝 데이터 이용해 SARIMA modeling.

train.100.011<-Arima(ts.train, order=c(1,0,0),seasonal = list(order=c(0,1,1), period=36),method="ML");AIC(train.100.011)#AIC:-57.51601
train.110.011<-Arima(ts.train, order=c(1,1,0),seasonal = list(order=c(0,1,1), period=36),method="ML");AIC(train.110.011)#AIC:-34.43623
train.110.001<-Arima(ts.train, order=c(1,1,0),seasonal = list(order=c(0,0,1), period=36),method="ML");AIC(train.110.001)#AIC:-82.93137
train.110.111<-Arima(ts.train, order=c(1,1,0),seasonal = list(order=c(1,1,1), period=36),method="ML");AIC(train.110.111)#AIC:-32.55299
train.110.110<-Arima(ts.train, order=c(1,1,0),seasonal = list(order=c(1,1,0), period=36),method="ML");AIC(train.110.110)#AIC:-34.55326



f.train.100.011<-forecast::forecast(train.100.011, h=72) #train data로 만든 모델로 예측값
f.train.110.011<-forecast::forecast(train.110.011, h=72) #train data로 만든 모델로 예측값
f.train.110.001<-forecast::forecast(train.110.001, h=72) #train data로 만든 모델로 예측값
f.train.110.110<-forecast::forecast(train.110.110, h=72) #train data로 만든 모델로 예측값




#test data로 미리 빼놓았던 실제 데이터와 train data로 만들었던 여러 후보군 모델들로 도출해낸 예측값을 이용해 RMSE 측정
rmse(f.train.100.011$mean, test) #0.2122776
rmse(f.train.110.011$mean, test) #0.3578357
rmse(f.train.110.001$mean, test) #0.2620267
rmse(f.train.110.110$mean, test) #0.338953


plot(f.train.412.012) #AIC와 rmse를 모두 비교해본 결과 train.200.011 모델이 최적합 모델이었다.



###############################################################################################
###################################ARCH, GARCH 모델 적합해보자!################################
###############################################################################################






