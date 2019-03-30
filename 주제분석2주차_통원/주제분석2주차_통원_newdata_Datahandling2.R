getwd()
setwd("C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data2")


final_total_data<-read.csv("final_total_data.csv",header=T)
final_via0_data<-read.csv("final_via0_data.csv",header=T)
final_via1_data<-read.csv("final_via1_data.csv",header=T)
final_via2_data<-read.csv("final_via2_data.csv",header=T)
final_via3_data<-read.csv("final_via3_data.csv",header=T)


#iata_code에 맞는 continent로 잡아주자!
airports_final2<-read.csv("airports_final2.csv",header=T)

airports_final2$continent<-as.character(airports_final2$continent)
airports_final2$new_continent<-ifelse(airports_final2$iso_country!="US" & airports_final2$iso_country!="CA",
                                      airports_final2$continent,"NA")

airports_final2<-subset(airports_final2,subset=airports_final2$iata_code!="" & airports_final2$iata_code!="-" & airports_final2$iata_code!="0",
                        select=c("latitude_deg","longitude_deg","new_continent","iso_country","iata_code"))
airports_final3<-airports_final2

#iata_code를 key로 하여 우리 데이터랑 합치자!
final_total_data$Airports2<-ifelse(final_total_data$Airports2=="",NA,as.character(final_total_data$Airports2))
final_total_data$Airports4<-ifelse(final_total_data$Airports4=="",NA,as.character(final_total_data$Airports4))
final_total_data$Airports6<-ifelse(final_total_data$Airports6=="",NA,as.character(final_total_data$Airports6))
final_total_data$Airports8<-ifelse(final_total_data$Airports8=="",NA,as.character(final_total_data$Airports8))

airports_final3$iata_code<-as.character(airports_final3$iata_code)

final_total_data2<-left_join(final_total_data,airports_final3,by=c("Airports2"="iata_code"))
names(final_total_data2)
names(final_total_data2)[38:41]<-c("Airport2_latitude_deg","Airport2_longitude_deg","Airport2_new_continent","Airport2_iso_country")

final_total_data3<-left_join(final_total_data2,airports_final3,by=c("Airports4"="iata_code"))
names(final_total_data3)
names(final_total_data3)[42:45]<-c("Airport4_latitude_deg","Airport4_longitude_deg","Airport4_new_continent","Airport4_iso_country")

final_total_data4<-left_join(final_total_data3,airports_final3,by=c("Airports6"="iata_code"))
names(final_total_data4)
names(final_total_data4)[46:49]<-c("Airport6_latitude_deg","Airport6_longitude_deg","Airport6_new_continent","Airport6_iso_country")

final_total_data5<-left_join(final_total_data4,airports_final3,by=c("Airports8"="iata_code"))
names(final_total_data5)
names(final_total_data5)[50:53]<-c("Airport8_latitude_deg","Airport8_longitude_deg","Airport8_new_continent","Airport8_iso_country")

final_total_data6<-final_total_data5

final_total_data6$Airport2_latitude_deg<-as.numeric(as.character(final_total_data6$Airport2_latitude_deg))
final_total_data6$Airport2_longitude_deg<-as.numeric(as.character(final_total_data6$Airport2_longitude_deg))
final_total_data6$Airport4_latitude_deg<-as.numeric(as.character(final_total_data6$Airport4_latitude_deg))
final_total_data6$Airport4_longitude_deg<-as.numeric(as.character(final_total_data6$Airport4_longitude_deg))
final_total_data6$Airport6_latitude_deg<-as.numeric(as.character(final_total_data6$Airport6_latitude_deg))
final_total_data6$Airport6_longitude_deg<-as.numeric(as.character(final_total_data6$Airport6_longitude_deg))
final_total_data6$Airport8_latitude_deg<-as.numeric(as.character(final_total_data6$Airport8_latitude_deg))
final_total_data6$Airport8_longitude_deg<-as.numeric(as.character(final_total_data6$Airport8_longitude_deg))

str(final_total_data6)
#New variable (distance)
install.packages('geosphere')
library(geosphere)

F.dat<-final_total_data6
F.dat
#####Airports2의 거리를 구해보자!
names(F.dat)
x=NULL 
y=NULL
for(i in 1:nrow(F.dat)){
  x= distm(c(126.6083,37.4722), c(F.dat[i,39], F.dat[i,38]), fun = distHaversine)
  y=c(y,x)
}

F.dat <- cbind(F.dat, y)

names(F.dat)[54] <- 'Airport2_Distance'
str(F.dat)


#####Airports4의 거리를 구해보자!
names(F.dat)
x=NULL 
y=NULL
for(i in 1:nrow(F.dat)){
  x= distm(c(126.6083,37.4722), c(F.dat[i,43], F.dat[i,42]), fun = distHaversine)
  y=c(y,x)
}

F.dat <- cbind(F.dat, y)

names(F.dat)[55] <- 'Airport4_Distance'
str(F.dat)


#####Airports6의 거리를 구해보자!

names(F.dat)
x=NULL 
y=NULL
for(i in 1:nrow(F.dat)){
  x= distm(c(126.6083,37.4722), c(F.dat[i,47], F.dat[i,46]), fun = distHaversine)
  y=c(y,x)
}

F.dat <- cbind(F.dat, y)

names(F.dat)[56] <- 'Airport6_Distance'
str(F.dat)

#####Airports8의 거리를 구해보자!
names(F.dat)
x=NULL 
y=NULL
for(i in 1:nrow(F.dat)){
  x= distm(c(126.6083,37.4722), c(F.dat[i,51], F.dat[i,50]), fun = distHaversine)
  y=c(y,x)
}

F.dat <- cbind(F.dat, y)

names(F.dat)[57] <- 'Airport8_Distance'
str(F.dat)

final_total_data7<-F.dat


names(final_total_data7)
final2_total_data<-subset(final_total_data7,select=c(Year, Month, Date_num,BookingAgency1,BookingPrice1,BookingPrice2,
                                                     BookingPrice3,BookingPrice3,BookingPrice4,BookingPrice5,Airports2,
                                                     Airports4,Airports6,Airports8,Staylocation,Staylocation2,Staylocation3,
                                                     StayTime1_total,StayTime2_total,StayTime3_total,
                                                     MovingTime1_total,MovingTime2_total,MovingTime3_total,MovingTime4_total,
                                                     FirstFlightDep_hr_f,FirstFlightArr_hr_f,secondFlightDep_hr_f,secondFlightArr_hr_f,
                                                     thirdFlightDep_hr_f,thirdFlightArr_hr_f,fourthFlightDep_hr_f,fourthFlightArr_hr_f,
                                                     rank2016,rank2017,Airport2_latitude_deg,Airport2_longitude_deg,Airport2_new_continent,
                                                     Airport2_iso_country,Airport2_Distance,Airport4_latitude_deg,Airport4_longitude_deg,
                                                     Airport4_new_continent,Airport4_iso_country,Airport4_Distance,Airport6_latitude_deg,
                                                     Airport6_longitude_deg,Airport6_new_continent,Airport6_iso_country,Airport6_Distance,
                                                     Airport8_latitude_deg,Airport8_longitude_deg,Airport8_new_continent,Airport8_iso_country,
                                                     Airport8_Distance))

final2_via0_data<-subset(final_total_data7,subset=viaNum==0,select=c(Year, Month, Date_num,BookingAgency1,BookingPrice1,BookingPrice2,
                                                                    BookingPrice3,BookingPrice3,BookingPrice4,BookingPrice5,Airports2,
                                                                    MovingTime1_total,FirstFlightDep_hr_f,FirstFlightArr_hr_f,rank2016,
                                                                    rank2017,Airport2_latitude_deg,Airport2_longitude_deg,Airport2_new_continent,
                                                                    Airport2_iso_country,Airport2_Distance))

final2_via1_data<-subset(final_total_data7,subset=viaNum==1,select=c(Year, Month, Date_num,BookingAgency1,BookingPrice1,BookingPrice2,
                                                                     BookingPrice3,BookingPrice3,BookingPrice4,BookingPrice5,Airports2,
                                                                     Airports4,Staylocation,StayTime1_total,MovingTime1_total,MovingTime2_total,
                                                                     FirstFlightDep_hr_f,FirstFlightArr_hr_f,secondFlightDep_hr_f,secondFlightArr_hr_f,
                                                                     rank2016,rank2017,Airport2_latitude_deg,Airport2_longitude_deg,Airport2_new_continent,
                                                                     Airport2_iso_country,Airport2_Distance,Airport4_latitude_deg,Airport4_longitude_deg,
                                                                     Airport4_new_continent,Airport4_iso_country,Airport4_Distance))    

final2_via2_data<-subset(final_total_data7,subset=viaNum==2,select=c(Year, Month, Date_num,BookingAgency1,BookingPrice1,BookingPrice2,
                                                                     BookingPrice3,BookingPrice3,BookingPrice4,BookingPrice5,Airports2,
                                                                     Airports4,Airports6,Staylocation,Staylocation2,StayTime1_total,StayTime2_total,
                                                                     MovingTime1_total,MovingTime2_total,MovingTime3_total,
                                                                     FirstFlightDep_hr_f,FirstFlightArr_hr_f,secondFlightDep_hr_f,secondFlightArr_hr_f,
                                                                     thirdFlightDep_hr_f,thirdFlightArr_hr_f,rank2016,rank2017,Airport2_latitude_deg,Airport2_longitude_deg,Airport2_new_continent,
                                                                     Airport2_iso_country,Airport2_Distance,Airport4_latitude_deg,Airport4_longitude_deg,
                                                                     Airport4_new_continent,Airport4_iso_country,Airport4_Distance,Airport6_latitude_deg,
                                                                     Airport6_longitude_deg,Airport6_new_continent,Airport6_iso_country,Airport6_Distance))                                                                       

final2_via3_data<-subset(final_total_data7,subset=viaNum==3,select=c(Year, Month, Date_num,BookingAgency1,BookingPrice1,BookingPrice2,
                                                                     BookingPrice3,BookingPrice3,BookingPrice4,BookingPrice5,Airports2,
                                                                     Airports4,Airports6,Airports8,Staylocation,Staylocation2,Staylocation3,
                                                                     StayTime1_total,StayTime2_total,StayTime3_total,
                                                                     MovingTime1_total,MovingTime2_total,MovingTime3_total,MovingTime4_total,
                                                                     FirstFlightDep_hr_f,FirstFlightArr_hr_f,secondFlightDep_hr_f,secondFlightArr_hr_f,
                                                                     thirdFlightDep_hr_f,thirdFlightArr_hr_f,fourthFlightDep_hr_f,fourthFlightArr_hr_f,
                                                                     rank2016,rank2017,Airport2_latitude_deg,Airport2_longitude_deg,Airport2_new_continent,
                                                                     Airport2_iso_country,Airport2_Distance,Airport4_latitude_deg,Airport4_longitude_deg,
                                                                     Airport4_new_continent,Airport4_iso_country,Airport4_Distance,Airport6_latitude_deg,
                                                                     Airport6_longitude_deg,Airport6_new_continent,Airport6_iso_country,Airport6_Distance,
                                                                     Airport8_latitude_deg,Airport8_longitude_deg,Airport8_new_continent,Airport8_iso_country,
                                                                     Airport8_Distance))                                                                                                                                           

write.csv(final2_total_data,file="C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data2/final_total_data.csv")
write.csv(final2_via0_data,file="C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data2/final_via0_data.csv")
write.csv(final2_via1_data,file="C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data2/final_via1_data.csv")
write.csv(final2_via2_data,file="C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data2/final_via2_data.csv")
write.csv(final2_via3_data,file="C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data2/final_via3_data.csv")


