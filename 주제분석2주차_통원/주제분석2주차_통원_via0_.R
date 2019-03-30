
nomore_data_total<-read.csv("nomore_total.csv",header=T)
nomore_data_via0<-read.csv("nomore_via0.csv",header=T)
nomore_data_via1<-read.csv("nomore_via1.csv",header=T)


#########################################################
#########회귀모형을 via0의 변수를 넣고 돌려보자############
#########################################################
nomore_data_via0$Year<-as.factor(nomore_data_via0$Year)
nomore_data_via0$Month<-as.factor(nomore_data_via0$Month)
nomore_data_via0$Date_num<-as.factor(as.character(nomore_data_via0$Date_num))
summary(nomore_data_via0)
str(nomore_data_via0)
names(nomore_data_via0)

#Linear Regression
fit_via0<-lm(BookingPrice1 ~ Year+Month+Date_num+BookingAgency1+destination+popular+
               Date_left+rank2017_ver2+MovingTime_TOTAL+
               FirstFlightDep_hr_f+FirstFlightArr_hr_f+Airports2+Airport2_iso_country+
               Airport2_Distance, data=nomore_data_via0)

par(mfrow=c(2,2)); plot(fit_via0)
summary(fit_via0)
#흠 NA가 너무 많이 뜨는 것 같은데

#Linear Regression2
fit2_via0<-step(fit_via0,direction = "both")
formula(fit2_via0)
nomore_data_via0
#BookingPrice1 ~ Year + Month + Date_num + BookingAgency1 + destination + 
#popular + MovingTime_TOTAL + FirstFlightArr_hr_f

summary(fit2_via0)


# Normality test of errors

library(tseries)
jarque.bera.test(fit2_via0$residuals) # p-value < 2.2e-16 쉬바
install.packages("nortest")
library(nortest)
ad.test(fit2_via0$residuals)# 쉬바
# Dissatisfy Nor

# Remedy
library(MASS)
bc<-boxcox(fit2_via0)
cbind(bc$x,bc$y)
which.max(bc$y)
lambda<-bc$x[which.max(bc$y)] 
lambda # 0.26

# lambda is almost 0 (0 is in 95% confidence interval on plot) 
# At this time, we can use log(y) instead of y^lambda because they have very small differences.
# Merit of using log(y) than y^lambda : Convenience of interpretation 

fit3_via0<-lm(log(BookingPrice1) ~ Year + Month + Date_num + BookingAgency1 + destination + 
                MovingTime_TOTAL + FirstFlightArr_hr_f, data=nomore_data_via0)

fit4_via0<-lm(BookingPrice1^lambda ~ Year + Month + Date_num + BookingAgency1 + destination + 
               MovingTime_TOTAL + FirstFlightArr_hr_f, data=nomore_data_via0)


summary(fit3_via0)
summary(fit4_via0)
jarque.bera.test(fit3_via0$residuals) # 쉬바
ad.test(fit3_via0$residuals) # 쉬바

jarque.bera.test(fit4_via0$residuals)
ad.test(fit4_via0$residuals)

# Homoscedasticity test of errors
install.packages("car")
library(car)
ncvTest(fit3_via0) # 쉬바
ncvTest(fit4_via0) # 쉬바

# Independency test of errors
library(lmtest)
dwtest(fit3_via0)  # p = 0.7605 -> Satisfy 
dwtest(fit4_via0) # p = 0.7605 -> Satisfy

summary(fit4_via0)
alias(fit3_via0)
# Multicolinearity test of variables
vif(lm(log(BookingPrice1) ~ Year  + Date_num + BookingAgency1 + destination + 
      MovingTime_TOTAL + FirstFlightArr_hr_f, data=nomore_data_via0))

vif(lm(log(BookingPrice1) ~ Year  + Date_num + BookingAgency1 + destination + 
         FirstFlightArr_hr_f, data=nomore_data_via0))
fit5_via0<-lm(log(BookingPrice1) ~ Year  + Date_num + BookingAgency1 + destination + 
                FirstFlightArr_hr_f, data=nomore_data_via0)
summary(fit5_via0)
par(mfrow=c(2,2));plot(fit5_via0)

vif(fit3_via0)
vif(fit4_via0)

outlierTest(fit5_via0)#2번
nomore_data_via0<-nomore_data_via0[-c(466,1158,468,216,467,356,316,215,736,280,240),]

nomore_data_via0
fit6_via0<-lm(log(BookingPrice1) ~ Year + Date_num + BookingAgency1 + destination + 
                FirstFlightArr_hr_f, data=nomore_data_via0)

summary(fit6_via0)
plot(fit6_via0)

outlierTest(fit6_via0)

corrplot::corrplot(nomore_data_via0)
# All of VIF of variables is smaller than 10 (VIFs < 10) 
# Satisfy

# Now insert the others! come back to 'fit_whole5'
# Final Regression Equation : fit_whole5
summary(fit_whole5)
par(mfrow=c(2,2));plot(fit_whole5)


#매우 차이가 있군! 변수들이 유의미한 설명 변수이군!

#현재 가격 분포
final_total_data4$BookingPrice1
ggplot(final_total_data4,aes(x=BookingPrice1))+geom_histogram(binwidth = 100000,color='black',fill='white')


