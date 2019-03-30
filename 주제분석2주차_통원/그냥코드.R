getwd()
setwd("C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data2")

t1<-read.csv("TicketJW.csv")
t2<-read.csv("TicketJW2.csv")
t3<-read.csv("ticket_sooominy.csv")
t4<-read.csv("Ticketsein1.csv")
t5<-read.csv("TicketHoyun.csv")
t6<-read.csv("은진.csv")
t7<-read.csv("Ticket동희.csv")
t8<-read.csv("룸메형.csv")
total<-rbind(t1,t2,t3,t4,t5,t6,t7,t8)


data3<-subset(total,subset=Date!="Date"& BookingPrice!=""&BookingPrice !="//")
data4<-na.omit(data3)

write.csv(data4, file="C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data2/TicketFinal_Ver1.csv")

table(change_data4$BookingAgency1)
table(change_data4$viaNum)


install.packages("Rserve")
library(Rserve)
Rserve(debug=F,port=6311,args=NULL)

library(rJava)

change_data4
split_facility<-str_split(change_data4$Facility,"좌석 공간이")

df<-data.frame(matrix(unlist(split_facility),ncol=3,byrow=T))
names(df)<-c("FirstSpec","SecondSpec","ThirdSpec")
cbind(change_data4,df)

#항공기 facility 스펙을 더미화하자
change_data4$firstFlight_chair_aver_down<-ifelse(grepl("평균보다 좁음",change_data4$Facility)==T,1,0)
change_data4$firstFlight_chair_aver<-ifelse(grepl("평균임",change_data4$Facility)==T,1,0)
change_data4$firstFlight_chair_aver_up<-ifelse(grepl("평균보다 넓음",change_data4$Facility)==T,1,0)
change_data4$firstFlight_chair_bed<-ifelse(grepl("침대형",change_data4$Facility)==T,1,0)
change_data4$firstFlight_chair_extra<-ifelse(grepl("엑스트라 리클라이너",change_data4$Facility)==T,1,0)
change_data4$firstFlight_chair_consent<-ifelse(grepl("좌석 전원 콘센트",change_data4$Facility)==T,1,0)
change_data4$firstFlight_chair_movie<-ifelse(grepl("주문형 동영상",change_data4$Facility)==T,1,0)
change_data4$firstFlight_chair_wifi<-ifelse(grepl("Wi-fi",change_data4$Facility)==T,1,0)
change_data4$firstFlight_chair_movie<-ifelse(grepl("주문형 동영상",change_data4$Facility)==T,1,0)
change_data4$firstFlight_chair_movie<-ifelse(grepl("주문형 동영상",change_data4$Facility)==T,1,0)
change_data4$firstFlight_chair_movie<-ifelse(grepl("주문형 동영상",change_data4$Facility)==T,1,0)
change_data4$firstFlight_chair_movie<-ifelse(grepl("주문형 동영상",change_data4$Facility)==T,1,0)

levels(change_data4$Facility)
