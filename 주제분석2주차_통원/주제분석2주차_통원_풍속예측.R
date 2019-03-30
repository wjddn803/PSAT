#################################Monthly Average!&Daily Average!#############################################
####TimeSeries DataExploring####
mean_windSpeed_final100<-ts(mean_windSpeed_final$windSpeed,frequency=36,start=c(2015,1,5),end=c(2018,4,25))

str(mean_windSpeed_final100)

plot(mean_windSpeed_final100)
plot(aggregate(mean_windSpeed_final100,FUN=mean))#mean down!
plot(aggregate(mean_windSpeed_final100,FUN=var))#variance down!

#####Standardize Variance!######
log_mean_windSpeed_final100<-log(mean_windSpeed_final100)



###이거 어떻게 할지 논의해야겠다. ㅠㅠ 
#########auto.arima##############

#Daily_15days
#분산 안정화 시킨거
plot(log_mean_windSpeed_final100)
test(log_mean_windSpeed_final100)
auto.arima(log_mean_windSpeed_final100)#ARIMA(0,1,2)(0,0,1)[36] #AIC: -143.15

lr<-log_mean_windSpeed_final100

lr012.011<-Arima(lr, order=c(0,1,2),seasonal = list(order=c(0,1,1), period=36),method="ML");AIC(lr012.011)#AIC:-68.88448
lr011.011<-Arima(lr, order=c(0,1,1),seasonal = list(order=c(0,1,1), period=36),method="ML");AIC(lr011.011)#AIC:-64.58517
lr112.111<-Arima(lr, order=c(1,1,2),seasonal = list(order=c(1,1,1), period=36),method="ML");AIC(lr112.111)#AIC:-66.93466
lr012.101<-Arima(lr, order=c(0,1,2),seasonal = list(order=c(1,0,1), period=36),method="ML");AIC(lr012.101)#AIC:-141.3604
lr012.100<-Arima(lr, order=c(0,1,2),seasonal = list(order=c(1,0,0), period=36),method="ML");AIC(lr012.100)#AIC:-143.3404


test(lr012.100$residuals)#iid success!

par(mfrow=c(1,1))
flr012.100<-forecast::forecast(lr012.100, h=72)
plot(flr012.100)
flr112.001

exp.xflr012.100<-exp(flr012.100$x)
exp.mflr012.100<-exp(flr012.100$mean)

exp.xflr112.001
exp.mflr112.001

all<-merge(actual=as.zoo(exp.xflr012.100),predicted=as.zoo(exp.mflr012.100))
all.xts <- xts(all, date_decimal(index(all)))
dygraph(all.xts,"windSpeed") %>% 
  dySeries("actual", label="Actual") %>% 
  dySeries("predicted", label="Predicted")

all.xts
daily15_windSpeed<-data.frame(all.xts)
write.csv(daily15_windSpeed,file="C:/Users/Jungwoo Lim/Desktop/daily15_windSpeed.csv")


#분산 안정화 안시킨거
plot(mean_windSpeed_final100)
test(mean_windSpeed_final100)
auto.arima(mean_windSpeed_final100)#ARIMA(1,1,1)(1,0,1)[36] #AIC: 49.31

lr<-mean_windSpeed_final100

lr111.011<-Arima(lr, order=c(1,1,1),seasonal = list(order=c(0,1,1), period=36),method="ML",transform.pars = FALSE);AIC(lr111.011)#AIC:63.06451
lr111.111<-Arima(lr, order=c(1,1,1),seasonal = list(order=c(1,1,1), period=36),method="ML",transform.pars = FALSE);AIC(lr111.111)#AIC:64.9759
lr211.101<-Arima(lr, order=c(2,1,1),seasonal = list(order=c(1,0,1), period=36),method="ML",transform.pars = FALSE);AIC(lr211.101)#AIC:49.052
lr210.101<-Arima(lr, order=c(2,1,0),seasonal = list(order=c(1,0,1), period=36),method="ML",transform.pars = FALSE);AIC(lr210.101)#AIC:53.96552
lr210.111<-Arima(lr, order=c(2,1,0),seasonal = list(order=c(1,1,1), period=36),method="ML",transform.pars = FALSE);AIC(lr210.111)#AIC:70.17931
lr211.110<-Arima(lr, order=c(2,1,1),seasonal = list(order=c(1,1,0), period=36),method="ML",transform.pars = FALSE);AIC(lr211.110)#AIC:60.99044


test(lr210.101$residuals)#iid success!


flr210.101<-forecast::forecast(lr210.101, h=72)
plot(flr210.101)
flr210.101



xflr210.101<-exp(flr210.101$x)
mflr210.101<-exp(flr210.101$mean)


all<-merge(actual=as.zoo(xflr210.101),predicted=as.zoo(mflr210.101))
all.xts <- xts(all, date_decimal(index(all)))
dygraph(all.xts,"windSpeed") %>% 
  dySeries("actual", label="Actual") %>% 
  dySeries("predicted", label="Predicted")

all.xts
daily15_windSpeed<-data.frame(all.xts)
write.csv(daily15_windSpeed,file="C:/Users/Jungwoo Lim/Desktop/daily15_windSpeed.csv")


#모델선택과정은 밑으로 가세요





##############################################################################################
################# train data vs test data >> rmse 비교 >> 최적합의 모델 선택 #################
##############################################################################################
#daily15 model 선택
plot(log_mean_windSpeed_final100)
lr<-log_mean_windSpeed_final100
train<-(lr[1:88])
test<-(lr[89:113])
ts.train<-ts(train)
ts.test<-ts(test)
auto.arima(lr)

#auto.arima(lr)의 결과로 나온 모델(ARIMA(0,1,2)(0,0,1)[36])을 기준으로 후보군 모델들을 설정하고, 트레이닝 데이터 이용해 SARIMA modeling.

train.012.011<-Arima(ts.train, order=c(0,1,2),seasonal = list(order=c(0,1,1), period=36),method="ML");AIC(train.012.011)#AIC:-55.16111
train.011.011<-Arima(ts.train, order=c(0,1,1),seasonal = list(order=c(0,1,1), period=36),method="ML");AIC(train.011.011)#AIC:-55.24331
train.112.111<-Arima(ts.train, order=c(1,1,2),seasonal = list(order=c(1,1,1), period=36),method="ML");AIC(train.112.111)#AIC:-53.02047
train.012.101<-Arima(ts.train, order=c(0,1,2),seasonal = list(order=c(1,0,1), period=36),method="ML");AIC(train.012.101)#AIC:-118.1133
train.012.100<-Arima(ts.train, order=c(0,1,2),seasonal = list(order=c(1,0,0), period=36),method="ML");AIC(train.012.100)#AIC:-119.1834


f.train.012.011<-forecast::forecast(train.012.011, h=72) #train data로 만든 모델로 예측값
f.train.011.011<-forecast::forecast(train.011.011, h=72) #train data로 만든 모델로 예측값
f.train.112.111<-forecast::forecast(train.112.111, h=72) #train data로 만든 모델로 예측값
f.train.012.101<-forecast::forecast(train.012.101, h=72) #train data로 만든 모델로 예측값
f.train.012.100<-forecast::forecast(train.012.100, h=72) #train data로 만든 모델로 예측값


AIC.train<-c(AIC(train.012.011),AIC(train.011.011),AIC(train.112.111),AIC(train.012.101),AIC(train.012.100))
AIC.train #train.200.011 >> best model

#test data로 미리 빼놓았던 실제 데이터와 train data로 만들었던 여러 후보군 모델들로 도출해낸 예측값을 이용해 RMSE 측정

exp.xflr012.100<-exp(flr012.100$x)
exp.mflr012.100<-exp(flr012.100$mean)

exp.train.mflr012.011<-exp(f.train.012.011$mean)
exp.train.mflr011.011<-exp(f.train.011.011$mean)
exp.train.mflr112.111<-exp(f.train.112.111$mean)
exp.train.mflr012.101<-exp(f.train.012.101$mean)
exp.train.mflr012.100<-exp(f.train.012.100$mean)

rmse(exp.train.mflr012.011, test) #15.96727
rmse(exp.train.mflr011.011, test) #15.95714
rmse(exp.train.mflr112.111, test) #14.45432 >> best model
rmse(exp.train.mflr012.101, test) #15.96111
rmse(exp.train.mflr012.100, test) #16.00199 

plot(f.train.412.012) #AIC와 rmse를 모두 비교해본 결과 train.200.011 모델이 최적합 모델이었다.



###############################################################################################
###################################ARCH, GARCH 모델 적합해보자!################################
###############################################################################################


