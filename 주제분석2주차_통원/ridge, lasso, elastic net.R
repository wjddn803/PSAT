library(MASS)
library(glmnet)
setwd('C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/Data2')
check<-read.csv("no_more_datahandling.csv",header=T)

total<-read.csv("nomore_total.csv",header=T)
via0_data<-read.csv("nomore_via0.csv",header=T)
via1_data<-read.csv("nomore_via1.csv",header=T)
via2_data<-read.csv("nomore_via2.csv",header=T)
via3_data<-read.csv("nomore_via3.csv",header=T)


getwd()
total$Year<-as.factor(total$Year)
total$Month<-as.factor(total$Month)
total$Date_num<-as.factor(total$Date_num)
total$popular<-as.factor(total$popular)
total$FirstFlightDep_hr_f<-as.factor(as.character(total$FirstFlightDep_hr_f))

names(total)

total<-subset(total,subset=is.na(BookingPrice1)!=T,select=c(Year,Month,Date_num,Date_left,destination,viaNum,
                             FirstFlightDep_hr_f,rank2017_ver2,popular,BookingAgency1,
                             StayTime_TOTAL,MovingTime_TOTAL,BookingPrice1))

set.seed(2018)
summary(na.omit(total))
n<-nrow(total)
str(total)
index<-sample(1:n,0.71*n,replace=F)
train<-total[index,]
test<-total[-index,]
tr.x<-model.matrix(BookingPrice1~.,data=train)[,-1]
tr.y=train$BookingPrice1
te.x<-model.matrix(BookingPrice1~.,data=test)[,-1]
te.y=test$BookingPrice1

str(total)


fit.lasso<-glmnet(tr.x,tr.y,family="gaussian",alpha=1)
fit.ridge<-glmnet(tr.x,tr.y,family="gaussian",alpha=0)
fit.elnet<-glmnet(tr.x,tr.y,family="gaussian",alpha=0.5)

fit.lasso.cv<-cv.glmnet(tr.x,tr.y,type.measure = "mse",alpha=1,family="gaussian")
fit.ridge.cv<-cv.glmnet(tr.x,tr.y,type.measure = "mse",alpha=0,family="gaussian")
fit.elnet.cv<-cv.glmnet(tr.x,tr.y,type.measure = "mse",alpha=.5,family="gaussian")



for(i in 0:100) {
  assign(paste("fit", i, sep=""),cv.glmnet(tr.x,tr.y,type.measure = "mse",
                                           alpha=i/100,family="gaussian"))
}

par(mfrow=c(3,2))
plot(fit.lasso,xvar="lambda")
plot(fit10, main="LASSO")

plot(fit.ridge,xvar="lambda")
plot(fit0, main="Ridge")

plot(fit.elnet,xvar="lambda")
plot(fit5,main = "Elastic Net")



#MSE on test set
for(i in 0:100) {
  assign(paste("fit", i, sep=""),cv.glmnet(tr.x,tr.y,type.measure = "mse",
                                           alpha=i/100,family="gaussian"))
}


yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=te.x)
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=te.x)
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=te.x)
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=te.x)
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=te.x)
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=te.x)
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=te.x)
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=te.x)
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=te.x)
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=te.x)
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=te.x)

rmse0 <- sqrt(mean((te.y - yhat0)^2))
rmse1 <- sqrt(mean((te.y - yhat1)^2))
rmse2 <- sqrt(mean((te.y - yhat2)^2))
rmse3 <- sqrt(mean((te.y - yhat3)^2))
rmse4 <- sqrt(mean((te.y - yhat4)^2))
rmse5 <- sqrt(mean((te.y - yhat5)^2))
rmse6 <- sqrt(mean((te.y - yhat6)^2))
rmse7 <- sqrt(mean((te.y - yhat7)^2))
rmse8 <- sqrt(mean((te.y - yhat8)^2))
rmse9 <- sqrt(mean((te.y - yhat9)^2))
rmse10 <- sqrt(mean((te.y - yhat10)^2))

test.y<-data.frame(te.y)
head(test.y)
library(dplyr)
names(test.y)
str(test.y)
test.y$te.y = as.numeric(test.y$te.y)
test.y$rmse = test.y$te.y - as.vector(test.y$yhat3)

test.y$yhat3<-yhat3
test.y$mse<-mean((test.y$te.y-test.y$yhat3)^2)


set.seed(2018)

##### Using built-in function 'lm' #####

# Fit a linear regression model.
fit = lm(formula=BookingPrice1 ~ Year+Date_num+destination+viaNum+
           StayTime_TOTAL+MovingTime_TOTAL, data=train)

summary(fit)

# beta hat (LSE)
fit$coef

Yhat = fit$fitted
Yhat = predict(fit)
train.RMSE = sqrt(mean((tr.y-Yhat)^2))
train.RMSE

Yhat0 = predict(fit,test)
test.RMSE = sqrt(mean((te.y-Yhat0)^2))
test.RMSE


install.packages("randomForest")
library(randomForest)
library(glmnet)
install.packages("tidyverse")
library(tidyverse)
install.packages("caret")
library(caret)


############ Method2 #############

######## Ridge #########
# Find the best lambda using cross-validation
# Cross-Validation을 통해 best lambda를 찾자!
cv <- cv.glmnet(tr.x, tr.y, alpha = 0)
# Display the best lambda value
cv$lambda.min

# Train Data에 적합~
model <- glmnet(tr.x, tr.y, alpha = 0, lambda = cv$lambda.min)
# coefficients
coef(model)


# Test Data로 예측~
library(dplyr)
library(Matrix)
predictions <- model %>% predict(te.x) %>% as.vector()
# RMSE, Rsquare
data.frame(
  RMSE = RMSE(predictions, test$BookingPrice1),
  Rsquare = R2(predictions, test$BookingPrice1)
)

######## LASSO #########

# Cross-Validation을 통해 best lambda를 찾자!
set.seed(2018) 
cv2 <- cv.glmnet(tr.x, tr.y, alpha = 1)
cv2$lambda.min

# Train Data에 적합~
model2 <- glmnet(tr.x, tr.y, alpha = 1, lambda = cv2$lambda.min)
# coefficients
coef(model2)

# Test Data로 예측~
predictions2 <- model2 %>% predict(te.x) %>% as.vector()
# RMSE, Rsquare
data.frame(
  RMSE = RMSE(predictions2, test$BookingPrice1),
  Rsquare = R2(predictions2, test$BookingPrice1)
)

######## Elsastic Net #########
# Train Data에 적합~
set.seed(2018)
model3 <- train(
  BookingPrice1 ~., data = train, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 300
)
# Best Tune을 통해 best lambda를 찾자!
model3$bestTune
coef(model3$finalModel, model3$bestTune$lambda)

# Test Data로 예측~
predictions <- model %>% predict(te.x)%>% as.vector()
# RMSE, Rsquare
data.frame(
  RMSE = RMSE(predictions, test$BookingPrice1),
  Rsquare = R2(predictions, test$BookingPrice1)
)

#################################################
############## Random Forest ####################
#################################################

library('ggplot2')
install.packages("ggthemes")
library('ggthemes') 
install.packages("scales")
library('scales')
library('randomForest') 
install.packages("gridExtra")
library('gridExtra')
install.packages("corrplot")
library('corrplot') 
install.packages("GGally")
library('GGally')
install.packages("e1071")
library('e1071')



str(train)
dim(train)
dim(test)

price_model<-randomForest(BookingPrice1~.,data=train,mtry=3)

Importance<-importance(price_model)
varImpPlot(price_model)
par(mfrow=c(1,1))

prediction<-predict(price_model,test)

data.frame(
  RMSE = RMSE(predictions, test$BookingPrice1),
  Rsquare = R2(predictions, test$BookingPrice1)
)

final_simul<-cbind(prediction,test)

hoyun<-read.csv("TicketFinal_Ver2.csv",header=T)


 #Moving Time1
change_data3$MovingTime1.1<-ifelse(change_data3$MovingTime1=="","0시간 0분",
                                   ifelse(grepl("시간",change_data3$MovingTime1)==F,plaste("00시간",change_data3MovingTime1),as.character(change_data3$MovingTime1)))
split_MovingTime1<-str_split(change_data3$MovingTime1.1,"시간",n=2)
df<-data.frame(matrix(unlist(split_MovingTime1),nrow=20576,byrow=T))
names(df)<-c("MovingTime1_hr","MovingTime1_min")
df$MovingTime1_min<-gsub("분","",df$MovingTime1_min)
df$MovingTime1_hr<-as.integer(as.character(df$MovingTime1_hr))
df$MovingTime1_min<-as.integer(as.character(df$MovingTime1_min))
df$MovingTime1_total<-(df$MovingTime1_hr*60+df$MovingTime1_min)
change_data3<-cbind(df,change_data3)

#MovingTime2
change_data3$MovingTime2.1<-ifelse(change_data3$MovingTime2=="","0시간 0분",
                                   ifelse(grepl("시간",change_data3$MovingTime2)==F,paste("00시간",change_data3$MovingTime2),as.character(change_data3$MovingTime2)))
split_MovingTime2<-str_split(change_data3$MovingTime2.1,"시간",n=2)
df<-data.frame(matrix(unlist(split_MovingTime2),nrow=20576,byrow=T))
names(df)<-c("MovingTime2_hr","MovingTime2_min")
df$MovingTime2_min<-gsub("분","",df$MovingTime2_min)
df$MovingTime2_hr<-as.integer(as.character(df$MovingTime2_hr))
df$MovingTime2_min<-as.integer(as.character(df$MovingTime2_min))
df$MovingTime2_total<-(df$MovingTime2_hr*60+df$MovingTime2_min)
change_data3<-cbind(df,change_data3)

#MovingTime3
change_data3$MovingTime3.1<-ifelse(change_data3$MovingTime3=="","0시간 0분",
                                   ifelse(grepl("시간",change_data3$MovingTime3)==F,paste("00시간",change_data3$MovingTime3),as.character(change_data3$MovingTime3)))
split_MovingTime3<-str_split(change_data3$MovingTime3.1,"시간",n=2)
df<-data.frame(matrix(unlist(split_MovingTime3),nrow=20576,byrow=T))
names(df)<-c("MovingTime3_hr","MovingTime3_min")
df$MovingTime3_min<-gsub("분","",df$MovingTime3_min)
df$MovingTime3_hr<-as.integer(as.character(df$MovingTime3_hr))
df$MovingTime3_min<-as.integer(as.character(df$MovingTime3_min))
df$MovingTime3_total<-(df$MovingTime3_hr*60+df$MovingTime3_min)
change_data3<-cbind(df,change_data3)

#MovingTime4
change_data3$MovingTime4.1<-ifelse(change_data3$MovingTime4=="","0시간 0분",
                                   ifelse(grepl("시간",change_data3$MovingTime4)==F,paste("00시간",change_data3$MovingTime4),as.character(change_data3$MovingTime4)))
split_MovingTime4<-str_split(change_data3$MovingTime4.1,"시간",n=2)
df<-data.frame(matrix(unlist(split_MovingTime4),nrow=20576,byrow=T))
names(df)<-c("MovingTime4_hr","MovingTime4_min")
df$MovingTime4_min<-gsub("분","",df$MovingTime4_min)
df$MovingTime4_hr<-as.integer(as.character(df$MovingTime4_hr))
df$MovingTime4_min<-as.integer(as.character(df$MovingTime4_min))
df$MovingTime4_total<-(df$MovingTime4_hr*60+df$MovingTime4_min)
change_data3<-cbind(df,change_data3)


#Total_Movingtime
hoyun$MovingTime_TOTAL<-ifelse(hoyun$viaNum==0,hoyun$MovingTime1_total,
                               ifelse(hoyun$viaNum==1,hoyun$MovingTime1_total+hoyun$MovingTime2_total,
                                      ifelse(hoyun$viaNum==2,hoyun$MovingTime1_total+hoyun$MovingTime2_total+hoyun$MovingTime3_total,
                                             hoyun$MovingTime1_total+hoyun$MovingTime2_total+hoyun$MovingTime3_total+hoyun$MovingTime4_total)))



