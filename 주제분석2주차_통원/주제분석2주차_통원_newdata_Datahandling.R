getwd()

setwd("C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data")
library(stringr)
library(dplyr)

##########################################################
##################Data Handling ver2######################
##########################################################

change_data<-read.csv(file.choose(), header=T)


##########변수들을 수정, 추가해보자!##########
#splitDate
split_date<-str_split(change_data$Date,"-",n=3)
df<-data.frame(matrix(unlist(split_date),nrow=20576,byrow=T))
names(df)<-c("Year","Month","Date_num")
change_data2<-cbind(df,change_data)

#Booking Agency개수를 나타내는 열을 만들어보자.
change_data2$BookingAgency1<-ifelse(is.na(change_data2$BookingAgency1)==T,"",as.character(change_data2$BookingAgency1))
change_data2$BookingAgency2<-ifelse(is.na(change_data2$BookingAgency2)==T,"",as.character(change_data2$BookingAgency2))
change_data2$BookingAgency3<-ifelse(is.na(change_data2$BookingAgency3)==T,"",as.character(change_data2$BookingAgency3))
change_data2$BookingAgency4<-ifelse(is.na(change_data2$BookingAgency4)==T,"",as.character(change_data2$BookingAgency4))
change_data2$BookingAgency5<-ifelse(is.na(change_data2$BookingAgency5)==T,"",as.character(change_data2$BookingAgency5))

attach(change_data2)
change_data2$agencynum<-ifelse(BookingAgency5!="",5,
                               ifelse(BookingAgency4!="",4,
                                      ifelse(BookingAgency3!="",3,
                                             ifelse(BookingAgency2!="",2,1))))
change_data2$FirstFlightDep<-as.character(FirstFlightDep)

#경유횟수를 나타내는 열을 만들어보자.
change_data2$viaNum<-ifelse(change_data2$Airports3=="",0,
                            ifelse(change_data2$Airports5=="",1,
                                   ifelse(change_data2$Airports7=="",2,3)))

str(change_data2$viaNum)


detach(change_data2)

####시간을 분리시켜 주자!

#First는 모든 항공기가 다 있지만 second부터는 없다. 그리고 공백을 substr처리하면
#행수가 맞지 않아 unlist가 안되는 문제점을 발견. 
#그래서 가짜 데이터를 집어넣었다!
#FirstFlightDep
split_firstFlightDep<-str_split(substr(change_data2$FirstFlightDep,4,8),":",n=2)
df<-data.frame(matrix(unlist(split_firstFlightDep),nrow=20576,byrow=T))
names(df)<-c("firstFlightDep_hr","firstFlightDep_min")
change_data2<-cbind(df,change_data2)
change_data2$firstFlightDep_hr<-as.integer(change_data2$firstFlightDep_hr)
change_data2$FirstFlightDep_hr_f<-ifelse(substr(change_data2$FirstFlightDep,1,2)=="오후",(change_data2$firstFlightDep_hr)+12,change_data2$firstFlightDep_hr)

#FirstFlightArr
split_firstFlightArr<-str_split(substr(change_data2$FirstFlightArr,4,8),":",n=2)
df<-data.frame(matrix(unlist(split_firstFlightArr),nrow=20576,byrow=T))
names(df)<-c("firstFlightArr_hr","firstFlightArr_min")
change_data2<-cbind(df,change_data2)
change_data2$firstFlightArr_hr<-as.integer(change_data2$firstFlightArr_hr)
change_data2$FirstFlightArr_hr_f<-ifelse(substr(change_data2$FirstFlightArr,1,2)=="오후",(change_data2$firstFlightArr_hr)+12,change_data2$firstFlightArr_hr)

#SecondFlightDep
change_data2$SecondFlightDep2<-ifelse(change_data2$SecondFlightDep=="","없음 00:00",as.character(change_data2$SecondFlightDep))
split_SecondFlightDep<-str_split(substr(change_data2$SecondFlightDep2,4,8),":",n=2)
df<-data.frame(matrix(unlist(split_SecondFlightDep),nrow=20576,byrow=T))
names(df)<-c("secondFlightDep_hr","secondFlightDep_min")
change_data2<-cbind(df,change_data2)
change_data2$secondFlightDep_hr<-as.integer(as.character(change_data2$secondFlightDep_hr))
change_data2$secondFlightDep_hr_f<-ifelse(substr(change_data2$SecondFlightDep2,1,2)=="오후",(change_data2$secondFlightDep_hr)+12,change_data2$secondFlightDep_hr)


#SecondFlightArr
change_data2$SecondFlightArr2<-ifelse(change_data2$SecondFlightArr=="","없음 00:00",as.character(change_data2$SecondFlightArr))
split_SecondFlightArr<-str_split(substr(change_data2$SecondFlightArr2,4,8),":",n=2)
df<-data.frame(matrix(unlist(split_SecondFlightArr),nrow=20576,byrow=T))
names(df)<-c("secondFlightArr_hr","secondFlightArr_min")
change_data2<-cbind(df,change_data2)
change_data2$secondFlightArr_hr<-as.integer(as.character(change_data2$secondFlightArr_hr))
change_data2$secondFlightArr_hr_f<-ifelse(substr(change_data2$SecondFlightArr2,1,2)=="오후",(change_data2$secondFlightArr_hr)+12,change_data2$secondFlightArr_hr)


#ThirdFlightDep
change_data2$ThirdFlightDep2<-ifelse(change_data2$ThirdFlightDep=="","없음 00:00",as.character(change_data2$ThirdFlightDep))
split_ThirdFlightDep<-str_split(substr(change_data2$ThirdFlightDep2,4,8),":",n=2)
df<-data.frame(matrix(unlist(split_ThirdFlightDep),nrow=20576,byrow=T))
names(df)<-c("thirdFlightDep_hr","thirdFlightDep_min")
change_data2<-cbind(df,change_data2)
change_data2$thirdFlightDep_hr<-as.integer(as.character(change_data2$thirdFlightDep_hr))
change_data2$thirdFlightDep_hr_f<-ifelse(substr(change_data2$ThirdFlightDep2,1,2)=="오후",(change_data2$thirdFlightDep_hr)+12,change_data2$thirdFlightDep_hr)


#ThirdFlightArr
change_data2$ThirdFlightArr2<-ifelse(change_data2$ThirdFlightArr=="","없음 00:00",as.character(change_data2$ThirdFlightArr))
split_ThirdFlightArr<-str_split(substr(change_data2$ThirdFlightArr2,4,8),":",n=2)
df<-data.frame(matrix(unlist(split_ThirdFlightArr),nrow=20576,byrow=T))
names(df)<-c("thirdFlightArr_hr","thirdFlightArr_min")
change_data2<-cbind(df,change_data2)
change_data2$thirdFlightArr_hr<-as.integer(as.character(change_data2$thirdFlightArr_hr))
change_data2$thirdFlightArr_hr_f<-ifelse(substr(change_data2$ThirdFlightArr2,1,2)=="오후",(change_data2$thirdFlightArr_hr)+12,change_data2$thirdFlightArr_hr)

#FourthFlightDep
change_data2$FourthFlightDep2<-ifelse(change_data2$FourthFlightDep=="","없음 00:00",as.character(change_data2$FourthFlightDep))
split_FourthFlightDep<-str_split(substr(change_data2$FourthFlightDep2,4,8),":",n=2)
df<-data.frame(matrix(unlist(split_FourthFlightDep),nrow=20576,byrow=T))
names(df)<-c("fourthFlightDep_hr","fourthFlightDep_min")
change_data2<-cbind(df,change_data2)
change_data2$fourthFlightDep_hr<-as.integer(as.character(change_data2$fourthFlightDep_hr))
change_data2$fourthFlightDep_hr_f<-ifelse(substr(change_data2$FourthFlightDep2,1,2)=="오후",(change_data2$fourthFlightDep_hr)+12,change_data2$fourthFlightDep_hr)

#FourthFlightArr
change_data2$FourthFlightArr2<-ifelse(change_data2$FourthFlightArr=="","없음 00:00",as.character(change_data2$FourthFlightArr))
split_FourthFlightArr<-str_split(substr(change_data2$FourthFlightArr2,4,8),":",n=2)
df<-data.frame(matrix(unlist(split_FourthFlightArr),nrow=20576,byrow=T))
names(df)<-c("fourthFlightArr_hr","fourthFlightArr_min")
change_data2<-cbind(df,change_data2)
change_data2$fourthFlightArr_hr<-as.integer(as.character(change_data2$fourthFlightArr_hr))
change_data2$fourthFlightArr_hr_f<-ifelse(substr(change_data2$FourthFlightArr2,1,2)=="오후",(change_data2$fourthFlightArr_hr)+12,change_data2$fourthFlightArr_hr)


change_data3<-change_data2
library(stringr)


####Moving Time, Stay Time다 분으로 바꾸어 주자
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


#StayTime1
change_data3$StayTime1.1<-ifelse(change_data3$Staytime=="","0시간 0분",
                                 ifelse(grepl("시간",change_data3$Staytime)==F,paste("00시간",change_data3$Staytime),as.character(change_data3$Staytime)))
split_StayTime1<-str_split(change_data3$StayTime1.1,"시간",n=2)
df<-data.frame(matrix(unlist(split_StayTime1),nrow=20576,byrow=T))
names(df)<-c("StayTime1_hr","StayTime1_min")
df$StayTime1_min<-gsub("분","",df$StayTime1_min)
df$StayTime1_hr<-as.integer(as.character(df$StayTime1_hr))
df$StayTime1_min<-as.integer(as.character(df$StayTime1_min))
df$StayTime1_total<-(df$StayTime1_hr*60+df$StayTime1_min)
change_data3<-cbind(df,change_data3)

#StayTime2
change_data3$StayTime2.1<-ifelse(change_data3$Staytime2=="","0시간 0분",
                                 ifelse(grepl("시간",change_data3$Staytime2)==F,paste("00시간",change_data3$Staytime2),as.character(change_data3$Staytime2)))
split_StayTime2<-str_split(change_data3$StayTime2.1,"시간",n=2)
df<-data.frame(matrix(unlist(split_StayTime2),nrow=20576,byrow=T))
names(df)<-c("StayTime2_hr","StayTime2_min")
df$StayTime2_min<-gsub("분","",df$StayTime2_min)
df$StayTime2_hr<-as.integer(as.character(df$StayTime2_hr))
df$StayTime2_min<-as.integer(as.character(df$StayTime2_min))
df$StayTime2_total<-(df$StayTime2_hr*60+df$StayTime2_min)
change_data3<-cbind(df,change_data3)

#StayTime3
change_data3$StayTime3.1<-ifelse(change_data3$Staytime3=="","0시간 0분",
                                 ifelse(grepl("시간",change_data3$Staytime3)==F,paste("00시간",change_data3$Staytime3),as.character(change_data3$Staytime3)))
split_StayTime3<-str_split(change_data3$StayTime3.1,"시간",n=2)
df<-data.frame(matrix(unlist(split_StayTime3),nrow=20576,byrow=T))
names(df)<-c("StayTime3_hr","StayTime3_min")
df$StayTime3_min<-gsub("분","",df$StayTime3_min)
df$StayTime3_hr<-as.integer(as.character(df$StayTime3_hr))
df$StayTime3_min<-as.integer(as.character(df$StayTime3_min))
df$StayTime3_total<-(df$StayTime3_hr*60+df$StayTime3_min)
change_data3<-cbind(df,change_data3)



Airlines<-read.csv("Airlines.csv")
change_data4<-change_data3

#Expedia, CheapOair, Cheapticket으로 나온애들은 BookingAgency2로 바꾸어주자.
change_data4$BookingAgency1<-ifelse(change_data4$BookingAgency1=="Expedia",as.character(change_data4$BookingAgency2) ,as.character(change_data4$BookingAgency1))
change_data4$BookingAgency1<-ifelse(change_data4$BookingAgency1=="CheapOair",as.character(change_data4$BookingAgency2) ,as.character(change_data4$BookingAgency1))
change_data4$BookingAgency1<-ifelse(change_data4$BookingAgency1=="Cheapticket",as.character(change_data4$BookingAgency2) ,as.character(change_data4$BookingAgency1))
change_data4$BookingAgency1<-ifelse(change_data4$BookingAgency1=="Expedia",as.character(change_data4$BookingAgency4) ,as.character(change_data4$BookingAgency1))

#BookingAgency를 그냥 항공사로 만들어주고 별점데이터랑 묶기 위해서 영어로 바꾸어 주자!
str(change_data3$BookingAgency1)
change_data4$BookingAgency1<-as.factor((change_data4$BookingAgency1))
levels(change_data4$BookingAgency1)  
change_data5<-subset(change_data4,subset=change_data4$BookingAgency1!="Skyticket" & change_data4$BookingAgency1!="")

change_data
levels(Airlines$Airlines)
#Skyticket 2, Expedia 20, CheapOair 735,  "" 158
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="LOT 폴란드항공","LOT Polish",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="S7항공","S7 Airlines",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="러시아항공","Aerofloat",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="가루다항공","Garuda Indonesia",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="대한항공","Korean Air",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="델타항공","Delta Air Lines",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="라오항공","Lao Airlines",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="MIAT 몽골항공","MIAT Mongolian Airlines",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="뉴질랜드항공","Air New Zealand",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="루프트한자","Lufthansa",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="말레이항공","Malaysia Airlines",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="베트남항공","Vietnam Airlines",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="비엣젯항공","VietJet Air",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="샤먼항공","Xiamen Airlines",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="스쿳항공","Scoot",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="심천항공","Shenzhen Airlines",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="싱가포르항공","Singapore Airlines",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="쓰촨항공","Sichuan Airlines",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="아메리칸항공","American Airlines",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="아시아나","Asiana Airlines",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="아스타나항공","Air Astana",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="에미레이트항공","Emirates",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="에바항공","EVA Air",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="에어 세르비아","Air Serbia",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="에어마카오","Air Macau",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="에어아시아 X","AirAsiaX",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="에어인디아","Air India",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="에어캐나다","Air Canada",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="에티오피아항공","Ethiopian Airlines",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="에티하드항공","Etihad Airways",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="영국항공","British Airways",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="오스트리아항공","Austrian",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="우랄항공","Ural Airlines",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="유나이티드항공","United Airlines",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="이탈리아항공","Alitalia",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="일본항공","Japan Airlines",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="전일본공수","ANA All Nippon Airways",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="제주항공","Jeju Air",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="제트 에어웨이스","Jet Airways",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="중국남방항공","China Southern",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="중화항공","Air China",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="카타르항공","Qatar Airways",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="캐세이패시픽항공","Cathay Pacific",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="콴타스항공","Quantas Airways",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="타이 에어아시아 엑스","AirAsiaX",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="터키항공","Turkish Airlines",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="톈진항공","Tienjin Airlines",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="하와이안항공","Hawaiian Airlines",as.character(change_data5$BookingAgency1))
change_data5$BookingAgency1<-ifelse(change_data5$BookingAgency1=="하이난항공","Hainan Airlines",as.character(change_data5$BookingAgency1))



names(Airlines)<-c("rank2017","Airlines","rank2016")
change_data6$BookingAgency1<-as.character(change_data6$BookingAgency1)
Airlines$Airlines<-as.character(Airlines$Airlines)
change_data6<-left_join(change_data5,Airlines,by=c("BookingAgency1"="Airlines"))


#Airports2를 한글에서 코드로 바꾸어주자!
change_data6$Airports2<-ifelse(change_data6$Airports2=="간사이 국제공항","KIX",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="광저우 바이윈 국제공항","CAN",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="김해국제공항","PUS",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="깜라인 국제공항","CXR",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="나리타 국제공항","NAA",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="노이바이 국제공항","HAN",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="뉴도하 국제공항","DOH",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="다낭 국제공항","DAD",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="다롄 저우수이쯔 국제공항","DLC",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="대니얼 K. 이노우에 국제공항","HNL",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="도쿄 국제공항","HND",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="돈므앙 국제공항","DMK",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="두바이 국제공항","DXB",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="디트로이트 메트로폴리탄 웨인 카운티 공항","DTW",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="떤선ㅤㄴㅕㅅ 국제공항","SGN",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="로마 피우미치노 공항","FCO",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="로스앤젤레스 국제공항","LAX",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="마카오 국제공항","MFM",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="매캐런 국제공항","LAS",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="뮌헨 국제공항","MUC",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="바르샤바 쇼팽 국제공항","WAW",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="밴쿠버 국제공항","YVR",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="베네치아 테세라 공항","VCE",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="베이징 수도 국제공항","PEK",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="볼레 국제공항","ADD",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="블라디보스토크 국제공항","VVO",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="상하이 푸둥 국제공항","PVG",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="상하이 훙차오 국제공항","SHA",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="샌프란시스코 국제공항","SFO",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="샤먼 가오치 국제공항","XMN",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="선양 타오셴 국제공항","SHE",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="선전 바오안 국제공항","SZX",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="셰레메티예보 국제공항","SVO",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="수카르노 하타 국제공항","CGK",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="시드니 공항","SYD",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="시안 셴양 국제공항","XIY",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="시애틀 터코마 국제공항","SEA",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="싱가포르 창이 공항","SIN",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="아부다비 국제공항","AUH",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="아스타나 국제공항","TSE",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="아타튀르크 국제공항","IST",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="알마티 국제공항","ALA",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="왓따이 국제공항","VTE",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="우한 톈허 국제공항","WUH",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="응우라라이 국제공항","DPS",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="이르쿠츠크 국제공항","IKT",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="존 F. 케네디 국제공항","JFK",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="지난 야오창 공항","TNA",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="청두 솽류 국제공항","CTU",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="충칭 장베이 국제공항","CKG",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="칭기즈 칸 국제공항","ULN",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="쿠알라룸푸르 국제공항","KUL",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="타이베이 쑹산 공항","TSA",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="타이완 타오위안 국제공항","TPE",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="톈진 빈하이 국제공항","TSN",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="토론토 피어슨 국제공항","YYZ",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="톨마초보 국제공항","OVB",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="파리 샤를 드 골 공항","CDG",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="프랑크푸르트 공항","FRA",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="하바롭스크 공항","KHV",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="하얼빈 타이핑 국제공항","HRB",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="홍콩 국제공항","HKG",as.character(change_data6$Airports2))
change_data6$Airports2<-ifelse(change_data6$Airports2=="히스로 공항","LHR",as.character(change_data6$Airports2))


#Airports4를 한글에서 코드로 바꾸어주자!
change_data6$Airports4<-ifelse(change_data6$Airports4=="Dongying Shengli Airport","DOY",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="간사이 국제공항","KIX",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="개트윅 공항","LGW",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="광저우 바이윈 국제공항","CAN",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="노이바이 국제공항","HAN",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="뉴어크 리버티 국제공항","EWR",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="댈러스 포트워스 국제공항","DFW",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="더블린 공항","DUB",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="덴버 국제공항","DEN",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="도모데도보 공항","DME",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="뒤셀도르프 국제공항","DUS",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="떤선ㅤㄴㅕㅅ 국제공항","SGN",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="라과디아 공항","LGA",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="런던 스탠스테드 공항","STN",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="로건 국제공항","BOS",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="로널드 레이건 워싱턴 내셔널 공항","DCA",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="로마 피우미치노 공항","FCO",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="로메-토코인 공항","LFW",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="로스앤젤레스 국제공항","LAX",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="리나테 공항","LIN",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="매캐런 국제공항","LAS",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="멜버른 공항","MEL",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="몬트리올 피에르 엘리오트 트뤼도 국제공항","YUL",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="뮌헨 국제공항","MUC",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="밀라노 말펜사 공항","MXP",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="베를린 테겔 국제공항","TXL",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="베오그라드 니콜라 테슬라 국제공항","BEG",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="볼레 국제공항","ADD",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="브뤼셀 공항","BRU",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="브리즈번 공항","BNE",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="비엔나 국제공항","VIE",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="샌프란시스코 국제공항","SFO",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="선전 바오안 국제공항","SZX",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="셰레메티예보 국제공항","SVO",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="솔트레이크시티 국제공항","SLC",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="시드니 공항","SYD",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="시애틀 터코마 국제공항","SEA",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="시카고 오헤어 국제공항","ORD",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="싱가포르 창이 공항","SIN",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="아부다비 국제공항","AUH",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="아스타나 국제공항","TSE",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="암스테르담 스키폴 국제공항","AMS",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="애들레이드 공항","ADL",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="에딘버그 공항","EDI",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="오사카 국제공항","ITM",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="오클랜드 국제공항","AKL",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="오타와 맥도널드 카르티에 국제공항","YOW",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="우한 톈허 국제공항","WUH",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="워싱턴 덜레스 국제공항","IAD",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="응우라라이 국제공항","DPS",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="인디라 간디 국제공항","DEL",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="정저우 신정 국제공항","CGO",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="존 F. 케네디 국제공항","JFK",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="지난 야오창 공항","TNA",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="차트라파티 시바지 국제공항","BOM",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="청두 솽류 국제공항","CTU",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="충칭 장베이 국제공항","CKG",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="취리히 공항","ZRH",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="칭다오 류팅 국제공항","TAO",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="카훌루이 공항","OGG",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="캘거리 국제공항","YYC",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="케언스 공항","CNS",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="콜초보 공항","SVX",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="토론토 피어슨 국제공항","YYZ",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="톨마초보 국제공항","OVB",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="파리 샤를 드 골 공항","CDG",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="파리 오를리 공항","ORY",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="포르부에 공항","ABJ",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="포틀랜드 국제공항","PDX",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="푸저우 창러 국제공항","FOC",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="프라하 바츨라프 하벨 국제공항","PRG",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="프랑크푸르트 공항","FRA",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="항저우 샤오산 국제공항","HGH",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="홍콩 국제공항","HKG",as.character(change_data6$Airports4))
change_data6$Airports4<-ifelse(change_data6$Airports4=="히스로 공항","LHR",as.character(change_data6$Airports4))



#Airports6를 한글에서 코드로 바꾸어주자!
change_data6$Airports6<-ifelse(change_data6$Airports6=="개트윅 공항","LGW",as.character(change_data6$Airports6))
change_data6$Airports6<-ifelse(change_data6$Airports6=="노이바이 국제공항","HAN",as.character(change_data6$Airports6))
change_data6$Airports6<-ifelse(change_data6$Airports6=="뉴어크 리버티 국제공항","EWR",as.character(change_data6$Airports6))
change_data6$Airports6<-ifelse(change_data6$Airports6=="더블린 공항","DUB",as.character(change_data6$Airports6))
change_data6$Airports6<-ifelse(change_data6$Airports6=="뒤셀도르프 국제공항","DUS",as.character(change_data6$Airports6))
change_data6$Airports6<-ifelse(change_data6$Airports6=="라과디아 공항","LGA",as.character(change_data6$Airports6))
change_data6$Airports6<-ifelse(change_data6$Airports6=="런던 사우스엔드 공항","SEN",as.character(change_data6$Airports6))
change_data6$Airports6<-ifelse(change_data6$Airports6=="런던 시티 공항","LCY",as.character(change_data6$Airports6))
change_data6$Airports6<-ifelse(change_data6$Airports6=="로마 피우미치노 공항","FCO",as.character(change_data6$Airports6))
change_data6$Airports6<-ifelse(change_data6$Airports6=="로스앤젤레스 국제공항","LAX",as.character(change_data6$Airports6))
change_data6$Airports6<-ifelse(change_data6$Airports6=="밀라노 말펜사 공항","MXP",as.character(change_data6$Airports6))
change_data6$Airports6<-ifelse(change_data6$Airports6=="시드니 공항","SYD",as.character(change_data6$Airports6))
change_data6$Airports6<-ifelse(change_data6$Airports6=="싱가포르 창이 공항","SIN",as.character(change_data6$Airports6))
change_data6$Airports6<-ifelse(change_data6$Airports6=="에딘버그 공항","EDI",as.character(change_data6$Airports6))
change_data6$Airports6<-ifelse(change_data6$Airports6=="존 F. 케네디 국제공항","JFK",as.character(change_data6$Airports6))
change_data6$Airports6<-ifelse(change_data6$Airports6=="충칭 장베이 국제공항","CKG",as.character(change_data6$Airports6))
change_data6$Airports6<-ifelse(change_data6$Airports6=="클리블랜드 홉킨스 국제공항","CLE",as.character(change_data6$Airports6))
change_data6$Airports6<-ifelse(change_data6$Airports6=="킹 압둘아지즈 국제공항","JED",as.character(change_data6$Airports6))
change_data6$Airports6<-ifelse(change_data6$Airports6=="파리 샤를 드 골 공항","CDG",as.character(change_data6$Airports6))
change_data6$Airports6<-ifelse(change_data6$Airports6=="파리 오를리 공항","ORY",as.character(change_data6$Airports6))
change_data6$Airports6<-ifelse(change_data6$Airports6=="프랑크푸르트 공항","FRA",as.character(change_data6$Airports6))
change_data6$Airports6<-ifelse(change_data6$Airports6=="히스로 공항","LHR",as.character(change_data6$Airports6))



#Airports8를 한글에서 코드로 바꾸어주자!
change_data6$Airports8<-ifelse(change_data6$Airports8=="개트윅 공항","LGW",as.character(change_data6$Airports8))
change_data6$Airports8<-ifelse(change_data6$Airports8=="라과디아 공항","LGA",as.character(change_data6$Airports8))
change_data6$Airports8<-ifelse(change_data6$Airports8=="런던 사우스엔드 공항","SEN",as.character(change_data6$Airports8))
change_data6$Airports8<-ifelse(change_data6$Airports8=="런던 시티 공항","LCY",as.character(change_data6$Airports8))
change_data6$Airports8<-ifelse(change_data6$Airports8=="파리 오를리 공항","ORY",as.character(change_data6$Airports8))
change_data6$Airports8<-ifelse(change_data6$Airports8=="프랑크푸르트 공항","FRA",as.character(change_data6$Airports8))



names(change_data6)

final_total_data<-subset(change_data6,select=c(Year, Month,Date_num,BookingAgency1, agencynum, BookingPrice1,BookingPrice2,
                                               BookingPrice3,BookingPrice4,BookingPrice5,Airports2,Airports4,Airports6,Airports8, 
                                               Staylocation, Staylocation2, Staylocation3, viaNum, StayTime1_total, StayTime2_total,StayTime3_total,  
                                               MovingTime1_total, MovingTime2_total,MovingTime3_total,MovingTime4_total,FirstFlightDep_hr_f,
                                               FirstFlightArr_hr_f,secondFlightDep_hr_f,secondFlightArr_hr_f,thirdFlightDep_hr_f,
                                               thirdFlightArr_hr_f,fourthFlightDep_hr_f,fourthFlightArr_hr_f, Facility,rank2017,rank2016))


final_via0_data<-subset(final_total_data,subset=viaNum==0,select=c(Year,Month,Date_num,BookingAgency1,agencynum,BookingPrice1,BookingPrice2,BookingPrice3,
                                                                   BookingPrice4,BookingPrice5,Airports2,MovingTime1_total,FirstFlightDep_hr_f,
                                                                   FirstFlightArr_hr_f,Facility))
final_via1_data<-subset(final_total_data,subset=viaNum==1,select=c(Year,Month,Date_num,BookingAgency1,agencynum,BookingPrice1,BookingPrice2,BookingPrice3,
                                                                   BookingPrice4,BookingPrice5,Airports2,Airports4,StayTime1_total,Staylocation,MovingTime1_total,
                                                                   MovingTime2_total,FirstFlightDep_hr_f,FirstFlightArr_hr_f,secondFlightDep_hr_f,
                                                                   secondFlightArr_hr_f,Facility,rank2017,rank2016))
                                                                   
                                                                   
final_via2_data<-subset(final_total_data,subset=viaNum==2,select=c(Year,Month,Date_num,BookingAgency1,agencynum,BookingPrice1,BookingPrice2,BookingPrice3,
                                                                   BookingPrice4,BookingPrice5,Airports2,Airports4,Airports6,StayTime1_total,StayTime2_total,
                                                                   Staylocation,Staylocation2,MovingTime1_total,MovingTime2_total,MovingTime3_total,FirstFlightDep_hr_f,FirstFlightArr_hr_f,
                                                                   secondFlightDep_hr_f,secondFlightArr_hr_f,thirdFlightDep_hr_f,thirdFlightArr_hr_f,
                                                                   Facility,rank2017,rank2016))
                                                                   
                                                                   
                                                                  

final_via3_data<-subset(final_total_data,subset=viaNum==3,select=c(Year, Month,Date_num,BookingAgency1, agencynum, BookingPrice1,BookingPrice2,
                                                                   BookingPrice3,BookingPrice4,BookingPrice5,Airports2,Airports4,Airports6,Airports8, 
                                                                   Staylocation, Staylocation2, Staylocation3, StayTime1_total, StayTime2_total,StayTime3_total,  
                                                                   MovingTime1_total, MovingTime2_total,MovingTime3_total,MovingTime4_total,FirstFlightDep_hr_f,
                                                                   FirstFlightArr_hr_f,secondFlightDep_hr_f,secondFlightArr_hr_f,thirdFlightDep_hr_f,
                                                                   thirdFlightArr_hr_f,fourthFlightDep_hr_f,fourthFlightArr_hr_f, Facility,rank2017,rank2016))

write.csv(final_total_data,file="C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data2/final_total_data.csv")
write.csv(final_via0_data,file="C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data2/final_via0_data.csv")
write.csv(final_via1_data,file="C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data2/final_via1_data.csv")
write.csv(final_via2_data,file="C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data2/final_via2_data.csv")
write.csv(final_via3_data,file="C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data2/final_via3_data.csv")

getwd()
