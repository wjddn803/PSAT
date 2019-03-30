################################Daily Average!#############################################
####TimeSeries DataExploring####
mean_temp_final100<-ts(mean_temp_final$Temp,frequency=36,start=c(2015,1,5),end=c(2018,5,5))
str(mean_temp_final100)
plot(mean_temp_final100)
plot(aggregate(mean_temp_final100,FUN=mean))#mean down!
plot(aggregate(mean_temp_final100,FUN=var))#variance up!

#Daily_15days
#분산 안정화 안시킨거
plot(mean_temp_final100)
test(mean_temp_final100)
auto.arima(mean_temp_final100)#ARIMA(2,1,0)(0,1,1)[36]  #AIC:337.84 ->best model

lr<-mean_temp_final100

lr200.011<-Arima(lr, order=c(2,0,0),seasonal = list(order=c(0,1,1), period=36),method="ML",transform.pars = FALSE);AIC(lr200.011)#AIC:339.2004
lr210.011<-Arima(lr, order=c(2,1,0),seasonal = list(order=c(0,1,1), period=36),method="ML",transform.pars = FALSE);AIC(lr210.011)#AIC:337.8438
lr210.001<-Arima(lr, order=c(2,1,0),seasonal = list(order=c(0,0,1), period=36),method="ML",transform.pars = FALSE);AIC(lr210.001)#AIC:531.0877
lr210.111<-Arima(lr, order=c(2,1,0),seasonal = list(order=c(1,1,1), period=36),method="ML",transform.pars = FALSE);AIC(lr210.111)#AIC:338.5983
lr210.110<-Arima(lr, order=c(2,1,0),seasonal = list(order=c(1,1,0), period=36),method="ML",transform.pars = FALSE);AIC(lr210.110)#AIC:338.5699
lr110.011<-Arima(lr, order=c(1,1,0),seasonal = list(order=c(0,1,1), period=36),method="ML",transform.pars = FALSE);AIC(lr110.011)#AIC:351.6744

test(lr210.011$residuals)#iid나름만족!

par(mfrow=c(1,1))
flr210.011<-forecast::forecast(lr210.011, h=72)
plot(flr210.011)
flr210.011
daily15_temp<-data.frame(flr210.011)

library(dygraphs)
xflr210.011<-flr210.011$x
mflr210.011<-flr210.011$mean
index(xflr210.011)
library(xts)
library(lubridate)
all<-merge(actual=as.zoo(xflr210.011),predicted=as.zoo(mflr210.011))
all.xts <- xts(all, date_decimal(index(all)))
dygraph(all.xts,"Temperature") %>% 
  dySeries("actual", label="Actual") %>% 
  dySeries("predicted", label="Predicted")

all.xts
daily15_temperature<-data.frame(all.xts)
write.csv(daily15_temperature,file="C:/Users/Jungwoo Lim/Desktop/daily15_temperature.csv")


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

train.200.011<-Arima(ts.train, order=c(2,0,0),seasonal = list(order=c(0,1,1), period=36),method="ML");AIC(train.200.011)#AIC:216.0987
train.210.011<-Arima(ts.train, order=c(2,1,0),seasonal = list(order=c(0,1,1), period=36),method="ML");AIC(train.210.011)#AIC:219.9475
train.210.001<-Arima(ts.train, order=c(2,1,0),seasonal = list(order=c(0,0,1), period=36),method="ML");AIC(train.210.001)#AIC:402.2084
train.210.111<-Arima(ts.train, order=c(2,1,0),seasonal = list(order=c(1,1,1), period=36),method="ML");AIC(train.210.111)#AIC:221.9475
train.210.110<-Arima(ts.train, order=c(2,1,0),seasonal = list(order=c(1,1,0), period=36),method="ML");AIC(train.210.110)#AIC:219.9475
train.110.011<-Arima(ts.train, order=c(1,1,0),seasonal = list(order=c(0,1,1), period=36),method="ML");AIC(train.110.011)#AIC:230.0437


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

plot(f.train.210.011) #AIC와 rmse를 모두 비교해본 결과 train.210.001 모델이 최적합 모델이었다.

##################################################################
##########################ARCH, GARCH#############################
##################################################################
library(zoo)
install.packages("fGarch")
library(fGarch)
detach(package:zoo)
head(mean_temp_final)
test<-data.frame(mean_temp_final$Temp)
test$temp.YMD<-paste(mean_temp_final$Year,mean_temp_final$Month,mean_temp_final$Date,sep="-")
test[2]
mean_temp_final1000<-zoo(test[,1],order.by = as.Date(strptime(as.character(test[,2]),"%Y-%m-%d")))
mean_temp_final1000
plot(mean_temp_final1000,main="Temperature_10days")
lr210.011$residuals



res.lr210.011<-lr210.011$residuals
squared.res.lr210.011<-res.lr210.011^2
par(mfcol=c(3,1))
plot(squared.res.lr210.011,main="Squared Residuals")
#우리의 주기에서 계속 튀어올라가는 모습을 발견할 수 있다.

acf.squared.lr210.011<-acf(squared.res.lr210.011,main="ACF Squared Residuals",lag.max = 500)
pacf.squared.lr210.011<-pacf(squared.res.lr210.011,main="PACF Squared Residuals",lag.max=500)



#arch(1) = garch(1,0)
model10<-garchFit(~garch(1,0),data=lr210.011$residuals, trace=F)
stand.res<-model10@residuals/model10@sigma.t #standardized residuals
acf(stand.res,lag.max = 50)
summary(model10)#AIC: 3.923345


#arch(2) 
model20<-garchFit(~garch(2,0),data=lr210.011$residuals, trace=F)
stand.res<-model20@residuals/model20@sigma.t #standardized residuals
acf(stand.res,lag.max = 50)
summary(model20)#AIC: 3.892721


#arch(3) 
model30<-garchFit(~garch(3,0),data=lr210.011$residuals, trace=F)
stand.res<-model30@residuals/model30@sigma.t #standardized residuals
acf(stand.res,lag.max = 50)
summary(model30)#AIC: 3.825165

#arch(4)
model40<-garchFit(~garch(4,0),data=lr210.011$residuals, trace=F)
stand.res<-model40@residuals/model40@sigma.t #standardized residuals
acf(stand.res,lag.max = 50)
summary(model40)#AIC: 3.719718
stand.res

#arch(5)
model50<-garchFit(~garch(5,0),data=lr210.011$residuals, trace=F)
stand.res<-model50@residuals/model50@sigma.t #standardized residuals
acf(stand.res,lag.max = 50)
summary(model50)#AIC: 3.748857

#arch(6)
model60<-garchFit(~garch(6,0),data=lr210.011$residuals, trace=F)
stand.res<-model60@residuals/model60@sigma.t #standardized residuals
acf(stand.res,lag.max = 50)
summary(model60)#AIC: 3.769172

#arch(7)
model70<-garchFit(~garch(7,0),data=lr210.011$residuals, trace=F)
stand.res<-model70@residuals/model70@sigma.t #standardized residuals
acf(stand.res,lag.max = 50)
summary(model70)#AIC: 3.801703

#garch(1,1)
model11<-garchFit(~garch(1,1),data=lr210.011$residuals, trace=F)
stand.res<-model80@residuals/model80@sigma.t #standardized residuals
acf(stand.res,lag.max = 50)
summary(model80)#AIC: 3.733357

#garch(2,1)
model21<-garchFit(~garch(2,1),data=lr210.011$residuals, trace=F)
stand.res<-model21@residuals/model21@sigma.t #standardized residuals
acf(stand.res,lag.max = 50)
summary(model21)#AIC: 3.764238

#garch(3,1)
model31<-garchFit(~garch(3,1),data=lr210.011$residuals, trace=F)
stand.res<-model31@residuals/model31@sigma.t #standardized residuals
acf(stand.res,lag.max = 50)
summary(model31)#AIC: 3.748139

#garch(4,1)
model41<-garchFit(~garch(4,1),data=lr210.011$residuals, trace=F)
stand.res<-model41@residuals/model41@sigma.t #standardized residuals
acf(stand.res,lag.max = 50)
summary(model41)#AIC: 3.734901

#garch(5,1)
model51<-garchFit(~garch(5,1),data=lr210.011$residuals, trace=F)
stand.res<-model51@residuals/model51@sigma.t #standardized residuals
acf(stand.res,lag.max = 50)
summary(model51)#AIC: 3.766556

##########################################################
###################ARCH(4) chosen! #######################
##########################################################
library(forecast)
install.packages("bayesGARCH")
library(bayesGARCH)
gbayes<-bayesGARCH()

#arch(4)
#FULL MODEL
#ARIMA(2,1,0)(0,1,1)+ARCH(4)
arch04$fitted.values
ht.arch04=arch04$fitted.values[,1]^2
plot(ht.arch04,main="Conditional Variances")


fit212011=fitted.values(lr210.011)
low=fit212011-1.96*sqrt(ht.arch04)
high=fit212011+1.96*sqrt(ht.arch04)
plot(mean_temp_final1000,type='l')
lines(low,col='red')
lines(high,col='blue')
