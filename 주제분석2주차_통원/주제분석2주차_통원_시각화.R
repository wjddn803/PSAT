getwd()
setwd("C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data")

visualize_data<-read.csv("final_data9.csv",header=T)

#######################################################
#############연착가능성 변수 ##########################
#######################################################

########연착과 정상출발의 비율은 얼마인가?########
library(ggplot2)
visualize_data
str(visualize_data)
visualize_data$delay<-as.factor(visualize_data$delay)
ggplot(visualize_data,aes(x=delay,fill=delay))+geom_bar(width = 0.4)
table(visualize_data$delay)
##########where people go?##############
where<-data.frame(table(visualize_data$Departure))
where_want<-subset(where,subset=Freq>15000)
library(dplyr)
library(ggplot2)
attach(visualize_data)
##########when people go?################
when<-data.frame(table(Month))
when2<-data.frame(table(Date))
when3<-data.frame(table(Month,Date,delay))
count(delay)
when3$Freq<-as.integer(when3$Freq)
when4<-when3 %>% group_by(when3$Month,when3$delay) %>% summarize(sum_Freq=sum(Freq))
ggplot(when4,aes(x=when4$`when3$Month`,y=when4$sum_Freq,group=when4$`when3$delay`,color=when4$`when3$delay`))+geom_line(size=2)+ggtitle("월별 지연 횟수")
when5<-when3 %>% group_by(when3$Date,when3$delay) %>% summarize(sum_Freq=sum(Freq))
ggplot(when5,aes(x=when5$`when3$Date`,y=when5$sum_Freq,group=when5$`when3$delay`,color=when5$`when3$delay`))+geom_line(size=2)+ggtitle("일별 지연 횟수")


##########what did people use?##############
driven<-arrange(data.frame(table(Airline)),Freq)


##########is time matter?#################
time<-data.frame(table(plan_hr,delay))
time %>% ggplot(aes(x=plan_hr,y=Freq,group=delay,color=delay))+geom_line(size=2)+ggtitle("시간별 지연 횟수")

#########

