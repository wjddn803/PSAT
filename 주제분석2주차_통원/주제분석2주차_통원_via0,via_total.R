#############################################
########## Regression 통합본 ################
#############################################
getwd()
setwd("c:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/Data2")
total_data<-read.csv("no_more_datahandling.csv",header=T)

total<-read.csv("nomore_total.csv",header=T)
via0_data<-read.csv("nomore_via0.csv",header=T)
via1_data<-read.csv("nomore_via1.csv",header=T)
via2_data<-read.csv("nomore_via2.csv",header=T)
via3_data<-read.csv("nomore_via3.csv",header=T)

################ TOTAL ######################
total$Year<-as.factor(as.character(total$Year))
total$Month<-as.factor(as.character(total$Month))
total$Date_num<-as.factor(as.character(total$Date_num))

summary(total)
str(total)
names(total)

#Linear Regression
fit_whole<-lm(BookingPrice1 ~ Year+Month+Date_num+BookingAgency1+destination+popular+viaNum+
                Date_left+rank2017_ver2+StayTime_TOTAL+MovingTime_TOTAL, data=total)

par(mfrow=c(2,2)); plot(fit_whole)
summary(fit_whole)
#Remedy_try1
table(total$Year)
table(total$Month)
table(total$Date_num)
table(total$BookingAgency1)#수가 적은애들이 꽤 있군
table(total$destination)#STN은 현저하게 적다
table(total$popular)
table(total$viaNum)
table(total$Date_left)
table(total$rank2017_ver2)

total2<-subset(total,subset=BookingAgency1!="Air Macau" & BookingAgency1!="Air China" &
                            BookingAgency1!="Austrian"& BookingAgency1!="EVA Air" & BookingAgency1!="Hawaiian Airlines" & 
                            BookingAgency1!="Lao Airlines" & BookingAgency1!="MIAT Mongolian Airlines"& BookingAgency1!="Shenzhen Airlines" &
                            BookingAgency1!="Turkish Airlines" & BookingAgency1!="Ural Airlines" & destination !="STN")

#Linear Regression2
fit_whole2<-lm(BookingPrice1 ~ Year+Month+Date_num+BookingAgency1+destination+popular+viaNum+
                Date_left+rank2017_ver2+StayTime_TOTAL+MovingTime_TOTAL, data=total2)
par(mfrow=c(2,2)); plot(fit_whole2)
summary(fit_whole2)


#Linear Regression3
fit_whole3<-step(fit_whole2,direction = "both")
formula(fit_whole3)

#Linear Regression4
fit_whole4<-lm(BookingPrice1 ~ Year+Month+Date_num+BookingAgency1+destination+viaNum+
                 StayTime_TOTAL+MovingTime_TOTAL, data=total2)

par(mfrow=c(2,2)); plot(fit_whole4)
summary(fit_whole4)
####연도, 월, 날짜, 항공사, 경유횟수, 경유시간, 이동시간 모두 별이 뜬다. 


# Normality test of errors
install.packages("tseries")
library(tseries)
jarque.bera.test(fit_whole4$residuals) # p-value < 2.2e-16 쉬바
install.packages("nortest")
library(nortest)
ad.test(fit_whole4$residuals)# 쉬바
# Dissatisfy Nor

# Remedy
install.packages("MASS")
library(MASS)
bc<-boxcox(fit_whole4)
cbind(bc$x,bc$y)
which.max(bc$y)
lambda<-bc$x[which.max(bc$y)] 
lambda # 0.02

# lambda is almost 0 (0 is in 95% confidence interval on plot) 
# At this time, we can use log(y) instead of y^lambda because they have very small differences.
# Merit of using log(y) than y^lambda : Convenience of interpretation 

fit_whole5<-lm(formula=log(BookingPrice1) ~ Month+Date_num+BookingAgency1+destination+viaNum+
                 StayTime_TOTAL+MovingTime_TOTAL, data=total2)

summary(fit_whole5)
jarque.bera.test(fit_whole5$residuals) # 쉬바
ad.test(fit_whole5$residuals) # 쉬바


# Homoscedasticity test of errors
install.packages("car")
library(car)
ncvTest(fit_whole5) # 쉬바

# Independency test of errors
install.packages("lmtest")
library(lmtest)
dwtest(fit_whole5) # 쉬바

#뭐가 문제일까? Year을 넣어보자!

fit_whole6<-lm(formula=log(BookingPrice1) ~ Year+Month+Date_num+BookingAgency1+destination+viaNum+
                 StayTime_TOTAL+MovingTime_TOTAL, data=total2)


jarque.bera.test(fit_whole6$residuals) #쉬바
ad.test(fit_whole6$residuals) # 쉬바
ncvTest(fit_whole6) #이런..
dwtest(fit_whole6) #0.7603 올

exp(fit_whole6$coefficients)


#흠 그러면 변수 선택이 잘못된 걸까? LASSO로 변수선택을 다시 해보자!(Shrinkage Method)
### nomore lasso ###
install.packages('glmnet')
library(glmnet)
names(total)
fit_whole6
formula(fit_whole6)
total_lasso<-subset(total,subset=is.na(total$BookingPrice1)!=T,select=c(Year,Month,Date_num,BookingAgency1,destination,
                                                                        viaNum,StayTime_TOTAL,MovingTime_TOTAL,BookingPrice1))
set.seed(2018)
n=nrow(total_lasso)

grid=c(10^seq(3,0,length=1000),0)

index<-sample(1:n,0.71*n,replace=F)
train<-total_lasso[index,]
test<-total_lasso[-index,]
tr.x<-model.matrix(BookingPrice1~.,data=train)[,-1]
tr.y=train$BookingPrice1
te.x<-model.matrix(BookingPrice1~.,data=test)[,-1]
te.y=test$BookingPrice1
te.mse=function(yhat,y) mean((y-yhat)^2)
lasso=glmnet(tr.x,tr.y,alpha=1,lambda = grid)
coef.lasso<-coef(lasso)
coef.lasso
te.yhat=predict(lasso,s=grid,newx = te.x)

mse=apply(te.yhat,2,te.mse,y=te.y)
plot(1:1001,mse,type='l',col='red',lwd=2,xlab="Index of Lambda",ylab="Test Mse")
par(mfrow=c(1,1))

l=which(mse==min(mse))
lasso$lambda[l]
l
coef.lasso[,l]
round(coef.lasso[,l],3)

#그닥 다를게 없어서 그냥 ....


# Multicolinearity test of variables
vif(lm(formula=log(BookingPrice1) ~ Year+Date_num+BookingAgency1+destination+viaNum+
         StayTime_TOTAL+MovingTime_TOTAL, data=total2))


# All of VIF of variables is smaller than 10 (VIFs < 10) 
# Satisfy

# Final Regression Equation : fit_whole6
summary(fit_whole6)
par(mfrow=c(2,2));plot(fit_whole6)



#현재 가격 분포
total2$BookingPrice1
install.packages("ggplot2")
library(ggplot2)
ggplot(total2,aes(x=BookingPrice1))+geom_histogram(binwidth = 100000,color='black',fill='white')

###############음 일단 경유지 별로 해석해볼 수 있을까? ################


################ via0 ######################
via0_data
via0_data$Year<-as.factor(as.character(via0_data$Year))
via0_data$Month<-as.factor(as.character(via0_data$Month))
via0_data$Date_num<-as.factor(as.character(via0_data$Date_num))
via0_data$Airports2<-as.factor(as.character(via0_data$Airports2))
via0_data$Airport2_iso_country<-as.factor(as.character(via0_data$Airport2_iso_country))


summary(via0_data)
str(via0_data)
names(via0_data)

#Linear Regression
fit_via0<-lm(BookingPrice1 ~ Year+Month+Date_num+BookingAgency1+destination+popular+Airports2+
                Airport2_iso_country+Airport2_Distance+Date_left+rank2017_ver2+MovingTime_TOTAL, data=via0_data)

par(mfrow=c(2,2)); plot(fit_via0)


#Linear Regression2
fit2_via0<-step(fit_via0,direction = "both")
formula(fit2_via0)

#Linear Regression3
fit3_via0<-lm(BookingPrice1 ~ Month + Date_num + BookingAgency1 + destination + 
                MovingTime_TOTAL, data=via0_data)

par(mfrow=c(2,2)); plot(fit3_via0)
summary(fit3_via0)
 


# Normality test of errors
install.packages("tseries")
library(tseries)
jarque.bera.test(fit3_via0$residuals) # p-value < 2.2e-16 쉬바
install.packages("nortest")
library(nortest)
ad.test(fit3_via0$residuals)# 쉬바
# Dissatisfy Nor

# Remedy
install.packages("MASS")
library(MASS)
bc<-boxcox(fit3_via0)
cbind(bc$x,bc$y)
which.max(bc$y)
lambda<-bc$x[which.max(bc$y)] 
lambda # 0.26

# lambda is almost 0 (0 is in 95% confidence interval on plot) 
# At this time, we can use log(y) instead of y^lambda because they have very small differences.
# Merit of using log(y) than y^lambda : Convenience of interpretation 

fit4_via0<-lm(formula=BookingPrice1 ~ Month + Date_num + BookingAgency1 + destination + 
                MovingTime_TOTAL, data=via0_data)


jarque.bera.test(fit4_via0$residuals) # 쉬바
ad.test(fit4_via0$residuals) # 쉬바


# Homoscedasticity test of errors
install.packages("car")
library(car)
ncvTest(fit4_via0) # 쉬바

# Independency test of errors
install.packages("lmtest")
library(lmtest)
dwtest(fit4_via0) # 쉬바

#뭐가 문제일까? Year을 넣어보자!

fit5_via0<-lm(formula=BookingPrice1 ~ Year+Month + Date_num + BookingAgency1 + destination + 
                 MovingTime_TOTAL, data=via0_data)


jarque.bera.test(fit5_via0$residuals) #쉬바
ad.test(fit5_via0$residuals) # 쉬바
ncvTest(fit5_via0) #이런..
dwtest(fit5_via0) #0.7605 올

summary(fit5_via0)



# Multicolinearity test of variables
vif(lm(formula=log(BookingPrice1) ~ Year+ Date_num + BookingAgency1 + destination + 
         MovingTime_TOTAL, data=via0_data))



# Final Regression Equation : fit6_via0
fit6_via0<-lm(formula=BookingPrice1 ~ Year + Month + Date_num + BookingAgency1 + destination  
                , data=via0_data)

summary(fit6_via0)
par(mfrow=c(2,2));plot(fit6_via0)


################ via1,2,3 ######################

