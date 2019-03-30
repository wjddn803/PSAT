
getwd()
setwd("C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data2")


#월별로 시각화를 해보았다. 
test<-subset(nomore_data_total,subset=nomore_data_total$Month==5,select=c("Date_num","BookingPrice1"))
names(nomore_data_total)
str(test)


ggplot(aes(x=test$Date_num,y=mean(test$BookingPrice1),color=test$Date_num))+geom_line()
test<-data.frame(table(test$Date_num))

test2$Date_num<-as.factor(test2$Date_num)
test2<-test %>% group_by(Date_num) %>% summarize(mean_price=mean(BookingPrice1))
ggplot(test2,aes(x=test2$Date_num,y=test2$mean_price))+geom_col(width=0.2)
str(test2)


library("dplyr")
install.packages("ggplot2")
library(ggplot2)


