
nomore_datahandling<-read.csv("no_more_datahandling.csv",header=T)
names(nomore_datahandling)
nomore_total<-subset(nomore_datahandling,select=c(Year, Month, Date_num,Date,Date_left,popular,
                                                  BookingAgency1,BookingPrice1,
                                                  Airports2,Airports4,Airports6,Airports8,destination,
                                                  viaNum,
                                                  Staylocation,Staylocation2,Staylocation3,
                                                  StayTime1_total,StayTime2_total,StayTime3_total,StayTime_TOTAL,
                                                  MovingTime1_total,MovingTime2_total,MovingTime3_total,MovingTime4_total,MovingTime_TOTAL,
                                                  FirstFlightDep_hr_f,FirstFlightArr_hr_f,secondFlightDep_hr_f,secondFlightArr_hr_f,
                                                  thirdFlightDep_hr_f,thirdFlightArr_hr_f,fourthFlightDep_hr_f,fourthFlightArr_hr_f,
                                                  rank2016,rank2017,rank2017_ver2,
                                                  Airport2_new_continent,Airport2_iso_country,Airport2_Distance,
                                                  Airport4_new_continent,Airport4_iso_country,Airport4_Distance,
                                                  Airport6_new_continent,Airport6_iso_country,Airport6_Distance,
                                                  Airport8_new_continent,Airport8_iso_country,Airport8_Distance))

nomore_via0<-subset(nomore_datahandling,subset=viaNum==0,select=c(Year, Month, Date_num,Date,Date_left,popular,
                                                                  BookingAgency1,BookingPrice1,
                                                                  Airports2,destination,
                                                                  viaNum,
                                                                  MovingTime1_total,MovingTime_TOTAL,
                                                                  FirstFlightDep_hr_f,FirstFlightArr_hr_f,
                                                                  rank2016,rank2017,rank2017_ver2,
                                                                  Airport2_new_continent,Airport2_iso_country,Airport2_Distance))

nomore_via1<-subset(nomore_datahandling,subset=viaNum==1,select=c(Year, Month, Date_num,Date,Date_left,popular,
                                                                  BookingAgency1,BookingPrice1,
                                                                  Airports2,Airports4,destination,
                                                                  viaNum,
                                                                  Staylocation,
                                                                  StayTime1_total,StayTime2_total,StayTime_TOTAL,
                                                                  MovingTime1_total,MovingTime2_total,MovingTime_TOTAL,
                                                                  FirstFlightDep_hr_f,FirstFlightArr_hr_f,secondFlightDep_hr_f,secondFlightArr_hr_f,
                                                                  rank2016,rank2017,rank2017_ver2,
                                                                  Airport2_new_continent,Airport2_iso_country,Airport2_Distance,
                                                                  Airport4_new_continent,Airport4_iso_country,Airport4_Distance))
                                                                  
nomore_via2<-subset(nomore_datahandling,subset=viaNum==2,select=c(Year, Month, Date_num,Date,Date_left,popular,
                                                                  BookingAgency1,BookingPrice1,
                                                                  Airports2,Airports4,Airports6,destination,
                                                                  viaNum,
                                                                  Staylocation,Staylocation2,
                                                                  StayTime1_total,StayTime2_total,StayTime_TOTAL,
                                                                  MovingTime1_total,MovingTime2_total,MovingTime3_total,MovingTime_TOTAL,
                                                                  FirstFlightDep_hr_f,FirstFlightArr_hr_f,secondFlightDep_hr_f,secondFlightArr_hr_f,
                                                                  thirdFlightDep_hr_f,thirdFlightArr_hr_f,
                                                                  rank2016,rank2017,rank2017_ver2,
                                                                  Airport2_new_continent,Airport2_iso_country,Airport2_Distance,
                                                                  Airport4_new_continent,Airport4_iso_country,Airport4_Distance,
                                                                  Airport6_new_continent,Airport6_iso_country,Airport6_Distance))
                                                                  
nomore_via3<-subset(nomore_datahandling,subset=viaNum==3,select=c(Year, Month, Date_num,Date,Date_left,popular,
                                                                  BookingAgency1,BookingPrice1,
                                                                  Airports2,Airports4,Airports6,Airports8,destination,
                                                                  viaNum,
                                                                  Staylocation,Staylocation2,Staylocation3,
                                                                  StayTime1_total,StayTime2_total,StayTime3_total,StayTime_TOTAL,
                                                                  MovingTime1_total,MovingTime2_total,MovingTime3_total,MovingTime4_total,MovingTime_TOTAL,
                                                                  FirstFlightDep_hr_f,FirstFlightArr_hr_f,secondFlightDep_hr_f,secondFlightArr_hr_f,
                                                                  thirdFlightDep_hr_f,thirdFlightArr_hr_f,fourthFlightDep_hr_f,fourthFlightArr_hr_f,
                                                                  rank2016,rank2017,rank2017_ver2,
                                                                  Airport2_new_continent,Airport2_iso_country,Airport2_Distance,
                                                                  Airport4_new_continent,Airport4_iso_country,Airport4_Distance,
                                                                  Airport6_new_continent,Airport6_iso_country,Airport6_Distance,
                                                                  Airport8_new_continent,Airport8_iso_country,Airport8_Distance))                                                                                                                                          

write.csv(nomore_total,file="C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data2/nomore_total.csv")
write.csv(nomore_via0,file="C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data2/nomore_via0.csv")
write.csv(nomore_via1,file="C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data2/nomore_via1.csv")
write.csv(nomore_via2,file="C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data2/nomore_via2.csv")
write.csv(nomore_via3,file="C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data2/nomore_via3.csv")

