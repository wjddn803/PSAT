#################################Monthly Average!&Daily Average!#############################################
####TimeSeries DataExploring####
mean_temp_final100<-ts(mean_temp_final$Temp,frequency=36,start=c(2015,1,5),end=c(2018,4,25))
mean_temp_final200<-ts(mean_temp_final2$Temp,frequency=365,start=c(2015,1,1),end=c(2018,5,2))
mean_temp_final300<-ts(mean_temp_final3$Temp,frequency=12,start=c(2015,1),end=c(2018,4))

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


#####################################정상화 과정!############################################
########Standardize Variance########
#log(mean_temp_final100)하면 NaNs produced된다고 나온다. 다른 변환방법이 없을까?
#Box-Cox를 이용하자!
#Averaging 15days
plot(mean_temp_final100)
lamda1<-BoxCox.lambda(mean_temp_final100)
Box_temp_final100<-BoxCox(mean_temp_final100, lamda1)
plot(Box_temp_final100)
plot(mean_temp_final200)
lamda2<-BoxCox.lambda(mean_temp_final200)
Box_temp_final200<-BoxCox(mean_temp_final200, lamda2)
plot(Box_temp_final200)

plot(mean_temp_final300)
lamda3<-BoxCox.lambda(mean_temp_final300)
Box_temp_final300<-BoxCox(mean_temp_final300, lamda3)
plot(Box_temp_final300)

####주기설정####

lag.plot(Box_temp_final100,set=c(1:100),pch=".",main=Box_temp_final100,diag.col = "red",do.lines = T)#주기 36
lag.plot(Box_temp_final300,set=c(1:41),pch=".",main=Box_temp_final300,diag.col = "red",do.lines = T)#주기12

########추세차분을 해보자########
diff_Box_temp_final100<-diff(Box_temp_final100)
diff_Box_temp_final200<-diff(Box_temp_final200)
diff_Box_temp_final300<-diff(Box_temp_final300)

plot(diff_Box_temp_final100)
plot(diff_Box_temp_final200)
plot(diff_Box_temp_final300)

########계절성을 제거해보자########
diffseas_Box_temp_final100<-diff(diff_Box_temp_final100,lag=36)
diffseas_Box_temp_final200<-diff(diff_Box_temp_final200,lag=365)
diffseas_Box_temp_final300<-diff(diff_Box_temp_final300,lag=12)

plot(diffseas_Box_temp_final100,main="Both differencing_final100", ylab="Temperature",xlab="Time")
plot(diffseas_Box_temp_final200,main="Both differencing_final200", ylab="Temperature",xlab="Time")
plot(diffseas_Box_temp_final300,main="Both differencing_final300", ylab="Temperature",xlab="Time")

########Classical Decomposition으로 분해해보자########

####추세추출(MA filter)####
trend.temp100<-smooth.ma(mean_temp_final100, q=18)
trend.temp200<-smooth.ma(mean_temp_final200, q=182)
trend.temp300<-smooth.ma(mean_temp_final300, q=6)

plot(trend.temp100, type = "l", main = "Trend of temp100")
plot(trend.temp200, type = "l", main = "Trend of temp200")
plot(trend.temp300, type = "l", main = "Trend of temp300")

####계절성 추출(Seasonal smoothing)####
season.temp100<-season(mean_temp_final100,d=36)
season.temp200<-season(mean_temp_final200,d=365)
season.temp300<-season(mean_temp_final300,d=12)

plot(season.temp100, type = "l", main = "Seasonality of temp100")
plot(season.temp200, type = "l", main = "Seasonality of temp200")
plot(season.temp300, type = "l", main = "Seasonality of temp300")

####추세 제거####
fit100.1<- mean_temp_final100-trend.temp100
fit200.1<- mean_temp_final200-trend.temp200
fit300.1<- mean_temp_final300-trend.temp300

plot(fit100.1)
plot(fit200.1)
plot(fit300.1)

####계절성 제거####
fit100.2<-fit100.1-season.temp100
fit200.2<-fit200.1-season.temp200
fit300.2<-fit300.1-season.temp300

plot(fit100.2)
plot(fit200.2)
plot(fit300.2)

####한꺼번에 해주는 과정!####
decom_mean_temp_final100<-decompose(mean_temp_final100)
plot(decom_mean_temp_final100)
decom_mean_temp_final200<-decompose(mean_temp_final200)
plot(decom_mean_temp_final200)
decom_mean_temp_final300<-decompose(mean_temp_final300)
plot(decom_mean_temp_final300)

decom_Boxcox_temp_final100<-decompose(Box_temp_final100)
plot(decom_Boxcox_temp_final100)
decom_Boxcox_temp_final200<-decompose(Box_temp_final200)
plot(decom_Boxcox_temp_final200)
decom_Boxcox_temp_final300<-decompose(Box_temp_final300)
plot(decom_Boxcox_temp_final300)

#########잔차가 정상성 만족하는지 확인해보자~########
test(mean_temp_final100)
test(diffseas_Box_temp_final100)#얘가 제일 IID를 기각하지 못하므로 정상성을 만족하는 잔차!
test(fit100.2) 

test(mean_temp_final200)
test(diffseas_Box_temp_final200)
test(fit200.2) 
#그냥 전체적으로 다 정상성 만족못하고 잔차가 correlated되어있다. 

test(mean_temp_final300)
test(diffseas_Box_temp_final300)#얘가 제일 IID를 기각하지 못하므정 정상성을 만족하는 잔차!
test(fit300.2) 


####전체적으로 봤을 때 정상성이 월별, 10일 평균별 은 정상성이 잘 제거가 되는데 일별은 잘 제거되지 않는다. 
####일별 데이터의 잔차가 correlated되어 있으면 어떻게 해야할까?
####정상성을 만족하는 모델에 데이터를 적합하여 잔차를 정상화해보자!

auto.arima(Box_temp_final200)#응 AIC 7000이야.... 안되게ㅔㅆ다....
#일별은 포기하고 15일별, 월별을 예측하자
#우리가 아까 잔차가 정상성을 확인해서 ACF, PADF로 봐서 판단해도 되나 귀찮으니까 auto.arima를 쓰자


#Daily_15days
plot(Box_temp_final100)
test(Box_temp_final100) #ACF보니까 계절성, ACF값의 지수적으로 감소하지 않으므로 차분의 필요성 확인

auto.arima(Box_temp_final100)#ARIMA(1,0,1)(1,1,0)[36] with drift #AIC:697.4
lr<-Box_temp_final100
lr101.110<-Arima(lr, order=c(1,0,1),seasonal = list(order=c(1,1,0), period=36),method="ML");AIC(lr101.110)#AIC:693.3506
lr111.110<-Arima(lr, order=c(1,1,1),seasonal = list(order=c(1,1,0), period=36),method="ML") ;AIC(lr111.110)#AIC:703.1256
lr201.110<-Arima(lr, order=c(2,0,1),seasonal = list(order=c(1,1,0), period=36),method="ML") ;AIC(lr201.110)#AIC:695.2429
lr101.111<-Arima(lr, order=c(1,0,1),seasonal = list(order=c(1,1,1), period=36),method="ML") ;AIC(lr101.111)#AIC:695.0597
lr112.111<-Arima(lr, order=c(1,1,2),seasonal = list(order=c(1,1,1), period=36),method="ML") ;AIC(lr112.111)#AIC:694.2969
test(lr112.111$residuals)#IID 만족으로 나온다!!!!!!
flr112.111<-forecast::forecast(lr112.111, h=36)
plot(flr112.111)


#분산 안정화 안시킨거
plot(mean_temp_final100)
test(mean_temp_final100)
auto.arima(mean_temp_final100)#ARIMA(2,1,0)(0,1,1)[36] #AIC:334.42

lr<-mean_temp_final100
lr200.011<-Arima(lr, order=c(2,0,0),seasonal = list(order=c(0,1,1), period=36),method="ML");AIC(lr200.011)#AIC:335.8445
lr210.001<-Arima(lr, order=c(2,1,0),seasonal = list(order=c(0,0,1), period=36),method="ML");AIC(lr210.001)#AIC:519.6748
lr210.111<-Arima(lr, order=c(2,1,0),seasonal = list(order=c(1,1,1), period=36),method="ML");AIC(lr210.111)#AIC:335.3382
lr210.110<-Arima(lr, order=c(2,1,0),seasonal = list(order=c(1,1,0), period=36),method="ML");AIC(lr210.110)#AIC:335.0971
lr110.011<-Arima(lr, order=c(1,1,0),seasonal = list(order=c(0,1,1), period=36),method="ML");AIC(lr110.011)#AIC:347.6502
test(lr210.110$residuals)#IID 그래도 나름 만족으로 나온다!!!!!!


flr210.110<-forecast::forecast(lr210.110, h=72)
plot(flr210.110)
flr210.110
daily15<-data.frame(flr210.110)


#Monthly
plot(Box_temp_final300)
test(Box_temp_final300)
auto.arima(Box_temp_final300)#ARIMA(0,0,0)(1,1,0)[12] AIC:229.64
lr2<-Box_temp_final300
lr000.110<-Arima(lr2, order=c(0,0,0),seasonal = list(order=c(1,1,0), period=12),method="ML");AIC(lr000.110)#229.64
lr010.100<-Arima(lr2, order=c(0,1,0),seasonal = list(order=c(1,0,0), period=12),method="ML");AIC(lr010.100)#AIC:355.3457
lr010.110<-Arima(lr2, order=c(0,1,0),seasonal = list(order=c(1,1,0), period=12),method="ML");AIC(lr010.110)#AIC:242.3444
lr001.110<-Arima(lr2, order=c(0,0,1),seasonal = list(order=c(1,1,0), period=12),method="ML");AIC(lr001.110)#AIC:231.2466
lr100.110<-Arima(lr2, order=c(1,0,0),seasonal = list(order=c(1,1,0), period=12),method="ML");AIC(lr100.110)#AIC:231.1579
lr010.010<-Arima(lr2, order=c(0,1,0),seasonal = list(order=c(0,1,0), period=12),method="ML");AIC(lr010.010)#AIC:245.2393

flr000.110<-forecast::forecast(lr000.110,h=25)
plot(flr100.110)
flr100.110
monthly<-boxcox.inv(data.frame(flr000.110),lamda3)

#분산 안정화 안시킨거
plot(mean_temp_final300)
test(mean_temp_final300)
auto.arima(mean_temp_final300)#ARIMA(0,0,0)(0,1,0)[12], AIC:95.67
lr3<-mean_temp_final300
lr000.010<-Arima(lr3, order=c(0,0,0),seasonal = list(order=c(0,1,0), period=12),method="ML");AIC(lr000.010)#AIC:95.6677
lr010.010<-Arima(lr3, order=c(0,1,0),seasonal = list(order=c(0,1,0), period=12),method="ML");AIC(lr010.010)#AIC:101.0778
lr110.010<-Arima(lr3, order=c(1,1,0),seasonal = list(order=c(0,1,0), period=12),method="ML");AIC(lr110.010)#AIC:101.4566
lr010.110<-Arima(lr3, order=c(0,1,0),seasonal = list(order=c(1,1,0), period=12),method="ML");AIC(lr010.110)#AIC:101.9213
lr110.000<-Arima(lr3, order=c(0,0,0),seasonal = list(order=c(1,1,1), period=12),method="ML");AIC(lr110.000)#AIC:96.85007

flr000.010<-forecast::forecast(lr000.010,h=25)
plot(flr000.010)
monthly<-data.frame(flr000.010)

write.csv(monthly,file="C:/Users/Jungwoo Lim/Desktop/monthly_temp.csv")
write.csv(daily15,file="C:/Users/Jungwoo Lim/Desktop/daily15_temp.csv")
