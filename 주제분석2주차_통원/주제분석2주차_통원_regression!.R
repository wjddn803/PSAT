write.csv(final_total_data2,file="C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data2/no_more_datahandling.csv")
getwd()

#########################################################
#########회귀모형을 전체 변수를 넣고 돌려보자############
#########################################################

summary(final_total_data2)
str(final_total_data2)
names(final_total_data2)

#Linear Regression
fit_whole<-lm(BookingPrice1 ~ Year+Month+Date_num+BookingAgency1+destination+popular+viaNum+
                Date_left+rank2017_ver2+StayTime_TOTAL+MovingTime_TOTAL, data=final_total_data2)

par(mfrow=c(2,2)); plot(fit_whole)


#Remove Outliers_try1
final_total_data3<-subset(final_total_data2,subset = final_total_data2$BookingPrice1<3000000)

#Linear Regression2
fit_whole2<-lm(BookingPrice1 ~ Year+Month+Date_num+BookingAgency1+destination+popular+viaNum+
                 Date_left+rank2017_ver2+StayTime_TOTAL+MovingTime_TOTAL, data=final_total_data3)

par(mfrow=c(2,2)); plot(fit_whole2)

#Remedy_try1
table(final_total_data3$Year)
table(final_total_data3$Month)
table(final_total_data3$Date_num)
t1<-data.frame(table(final_total_data3$BookingAgency1))#수가 적은애들이 꽤 있군
table(final_total_data3$destination)#STN은 현저하게 적다
table(final_total_data3$popular)
table(final_total_data3$viaNum)
table(final_total_data3$Date_left)
table(final_total_data3$rank2017_ver2)


final_total_data4<-subset(final_total_data3,subset=BookingAgency1!="Air Macau" & BookingAgency1!="Air China" &
                            BookingAgency1!="Austrian"& BookingAgency1!="EVA Air" & BookingAgency1!="Hawaiian Airlines" & 
                            BookingAgency1!="Lao Airlines" & BookingAgency1!="MIAT Mongolian Airlines"& BookingAgency1!="Shenzhen Airlines" &
                            BookingAgency1!="Turkish Airlines" & BookingAgency1!="Ural Airlines" & destination !="STN")








#Linear Regression3
fit_whole3<-lm(BookingPrice1 ~ Year+Month+Date_num+BookingAgency1+destination+popular+viaNum+
                 Date_left+rank2017_ver2+StayTime_TOTAL+MovingTime_TOTAL, data=final_total_data4)

par(mfrow=c(2,2)); plot(fit_whole3)
summary(fit_whole3)

#Linear Regression3.1
fit_whole3.1<-step(fit_whole3,direction = "both")
formula(fit_whole3.1)

#Linear Regression4
fit_whole4<-lm(BookingPrice1 ~ Year+Month+Date_num+BookingAgency1+destination+viaNum+
                 StayTime_TOTAL+MovingTime_TOTAL, data=final_total_data4)

par(mfrow=c(2,2)); plot(fit_whole4)
summary(fit_whole4)
####연도, 월, 날짜, 항공사, 경유횟수, 경유시간, 이동시간 모두 별이 뜬다. 


# Normality test of errors

library(tseries)
jarque.bera.test(fit_whole4$residuals) # p-value < 2.2e-16 쉬바
install.packages("nortest")
library(nortest)
ad.test(fit_whole4$residuals)# 쉬바
# Dissatisfy Nor

# Remedy
bc<-boxcox(fit_whole4)
cbind(bc$x,bc$y)
which.max(bc$y)
lambda<-bc$x[which.max(bc$y)] 
lambda # 0.18

# lambda is almost 0 (0 is in 95% confidence interval on plot) 
# At this time, we can use log(y) instead of y^lambda because they have very small differences.
# Merit of using log(y) than y^lambda : Convenience of interpretation 

fit_whole5<-lm(formula=log(BookingPrice1) ~ Year+Month+Date_num+BookingAgency1+destination+viaNum+
                 StayTime_TOTAL+MovingTime_TOTAL, data=final_total_data4)


jarque.bera.test(fit_whole5$residuals) # 쉬바
ad.test(fit_whole5$residuals) # 쉬바


# Homoscedasticity test of errors
install.packages("car")
library(car)
ncvTest(fit_whole5) # 쉬바

# Independency test of errors
library(lmtest)
dwtest(fit_whole5)  # p = 0.7603 -> Satisfy 

# Multicolinearity test of variables...
# Befor that! : 'Temporarily' delete two 'NA' variables ('engine.type' & 'fuel.system')
# You can find these in 'summary(fit_whole5)'
summary(fit_whole5)

fit_whole6<-lm(formula=log(BookingPrice1) ~ Year+Date_num+BookingAgency1+destination+viaNum+
                 StayTime_TOTAL+MovingTime_TOTAL, data=final_total_data4)

# Multicolinearity test of variables
vif(lm(formula=log(BookingPrice1) ~ Year+Date_num+BookingAgency1+destination+viaNum+
         StayTime_TOTAL+MovingTime_TOTAL, data=final_total_data4))
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
