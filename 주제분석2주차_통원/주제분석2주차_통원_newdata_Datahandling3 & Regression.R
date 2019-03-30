##############################################################
##################Multiple Linear Regression##################
##############################################################

final_total_data<-read.csv("final_total_data.csv",header=T)
final_via0_data<-read.csv("final_via0_data.csv",header=T)
final_via1_data<-read.csv("final_via1_data.csv",header = T)
final_via2_data<-read.csv("final_via2_data.csv",header = T)
final_via3_data<-read.csv("final_via3_data.csv",header = T)



#####데이터를 time 변수로!
final_total_data$Date<-paste(final_total_data$Year,final_total_data$Month,final_total_data$Date_num,sep="-")
final_total_data$Date<-as.Date(final_total_data$Date)
str(final_total_data)

#####Data Visualization#####
#시간 변수
year<-data.frame(table(final_total_data$Year))
month<-data.frame(table(final_total_data$Month))
date<-data.frame(table(final_total_data$Date_num))
library(ggplot2)
ggplot(year,aes(x=Var1,y=Freq,fill=Var1))+geom_col(width=0.2,show.legend = TRUE)
ggplot(month,aes(x=Var1,y=Freq,fill=Var1))+geom_col(width=0.2,show.legend = TRUE)
ggplot(date,aes(x=Var1,y=Freq,fill=Var1))+geom_col(width=0.2,show.legend = TRUE)

#공항별
names(final_total_data)
Airports2<-data.frame(table(final_total_data$Airports2))
Airports4<-data.frame(table(final_total_data$Airports4))
Airports6<-data.frame(table(final_total_data$Airports6))
Airports8<-data.frame(table(final_total_data$Airports8))

ggplot(tail(arrange(Airports2,Freq),20),aes(x=Var1,y=Freq,fill=Var1))+geom_col(width=0.2,show.legend = TRUE)#SVO러시아쪽, 아시아쪽이 많은 것으로 보아 경유지랑 아시아쪽이 많다.
ggplot(tail(arrange(Airports4,Freq),20),aes(x=Var1,y=Freq,fill=Var1))+geom_col(width=0.2,show.legend = TRUE)#CDG, SIN, SYD,FCO,FRA 등등 여기는 아시아도 좀 나타난다.
ggplot(Airports6,aes(x=Var1,y=Freq,fill=Var1))+geom_col(width=0.2,show.legend = TRUE)#CDG,FCO, 등의 유럽과 미국,호주쪽이 많았음
ggplot(Airports8,aes(x=Var1,y=Freq,fill=Var1))+geom_col(width=0.2,show.legend = TRUE)#LCY, SEN모두 영국

#경유별 가격분포
final_via0_data$BookingPrice1<-as.numeric(gsub(",","",final_via0_data$BookingPrice1))
viaNum0<-data.frame(table(final_via0_data$BookingPrice1))
viaNum0$Var1<-as.numeric(as.character(viaNum0$Var1))
ggplot(viaNum0,aes(x=Var1))+geom_histogram(binwidth = 100000,color='black',fill='white')
#직항인데 200만원 넘는 아이들은 뭐하는 애들일까?
final_via1_data$BookingPrice1<-as.numeric(gsub(",","",final_via1_data$BookingPrice1))
viaNum1<-data.frame(table(final_via1_data$BookingPrice1))
viaNum1$Var1<-as.numeric(as.character(viaNum1$Var1))
ggplot(viaNum1,aes(x=Var1))+geom_histogram(binwidth = 100000,color='black',fill='white')
#경유 1회인데 250만원 넘는 애들은 뭐하는 애들일까?
final_via2_data$BookingPrice1<-as.numeric(gsub(",","",final_via2_data$BookingPrice1))
viaNum2<-data.frame(table(final_via2_data$BookingPrice1))
viaNum2$Var1<-as.numeric(as.character(viaNum2$Var1))
ggplot(viaNum2,aes(x=Var1))+geom_histogram(binwidth = 100000,color='black',fill='white')
#경유 2회인데 200만원 넘는 애들은 뭐하는 애들일까?
final_via3_data$BookingPrice1<-as.numeric(gsub(",","",final_via3_data$BookingPrice1))
viaNum3<-data.frame(table(final_via3_data$BookingPrice1))
viaNum3$Var1<-as.numeric(as.character(viaNum3$Var1))
ggplot(viaNum3,aes(x=Var1))+geom_histogram(binwidth = 100000,color='black',fill='white')
#경유 3회인데 125만원 넘는애들은 뭐하는 애들일까?



####전체 데이터로 회귀를한번 돌려보기 전에!

###############################################
#################Data Handling 3###############
###############################################

#필요한 변수 : 성수기, 비수기 / 현재까지 몇개월 남았는지 / rank변수 / 목적지 변수 / 총 Staytime / 총 Moving Time


#엇 그런데 가격이 없는 애들이 있어서 한번 빼주었다. 
final_total_data<-subset(final_total_data,subset=final_total_data$BookingAgency1!="")

levels(final_total_data$Month)
#성수기, 비수기 변수 만들어주자!
final_total_data$popular<-ifelse(final_total_data$Month!="1" & final_total_data$Month!="8",0,1)


final_total_data$viaNum<-ifelse(is.na(final_total_data$Airports4)==T,0,
                                ifelse(is.na(final_total_data$Airports6)==T,1,
                                        ifelse(is.na(final_total_data$Airports8)==T,2,3)))
               

#현재까지 얼마나 남았나?
final_total_data$Date_today<-as.Date("2018-05-18")
str(final_total_data$Date_today)
final_total_data$Date_left<-as.Date(final_total_data$Date)-as.Date(final_total_data$Date_today)



#Rank는 1-50위, 51-100위, 그 이하 로 나누자

final_total_data$rank2017_ver2<-ifelse(is.na(final_total_data$rank2017)==T,3,
                                       ifelse(final_total_data$rank2017<51,1,
                                          ifelse(final_total_data$rank2017<=100,2,3)))
                                            

#목적지 변수를 만들자
final_total_data$destination<-ifelse(final_total_data$viaNum==0,as.character(final_total_data$Airports2),
                                     ifelse(final_total_data$viaNum==1,as.character(final_total_data$Airports4),
                                            ifelse(final_total_data$viaNum==2,as.character(final_total_data$Airports6),as.character(final_total_data$Airports8))))
                                                  
#Total_staytime
final_total_data$StayTime_TOTAL<-ifelse(final_total_data$viaNum==0,0,
                                        ifelse(final_total_data$viaNum==1,final_total_data$StayTime1_total,
                                               ifelse(final_total_data$viaNum==2,final_total_data$StayTime1_total+final_total_data$StayTime2_total,final_total_data$StayTime1_total+final_total_data$StayTime2_total+final_total_data$StayTime3_total)))

#Total_Movingtime
final_total_data$MovingTime_TOTAL<-ifelse(final_total_data$viaNum==0,final_total_data$MovingTime1_total,
                                        ifelse(final_total_data$viaNum==1,final_total_data$MovingTime1_total+final_total_data$MovingTime2_total,
                                               ifelse(final_total_data$viaNum==2,final_total_data$MovingTime1_total+final_total_data$MovingTime2_total+final_total_data$MovingTime3_total,
                                                      final_total_data$MovingTime1_total+final_total_data$MovingTime2_total+final_total_data$MovingTime3_total+final_total_data$MovingTime4_total)))




#먼저 변수 타입을 다 봐주자
str(final_total_data)
final_total_data$Year<-as.factor(as.character(final_total_data$Year))
final_total_data$Month<-as.factor(as.character(final_total_data$Month))
final_total_data$Date_num<-as.factor(as.character(final_total_data$Date_num))
final_total_data$FirstFlightDep_hr_f<-as.factor(as.character(final_total_data$FirstFlightDep_hr_f))
final_total_data$FirstFlightArr_hr_f<-as.factor(as.character(final_total_data$FirstFlightArr_hr_f))
final_total_data$secondFlightDep_hr_f<-as.factor(as.character(final_total_data$secondFlightDep_hr_f))
final_total_data$secondFlightArr_hr_f<-as.factor(as.character(final_total_data$secondFlightArr_hr_f))
final_total_data$thirdFlightDep_hr_f<-as.factor(as.character(final_total_data$thirdFlightDep_hr_f))
final_total_data$thirdFlightArr_hr_f<-as.factor(as.character(final_total_data$thirdFlightArr_hr_f))
final_total_data$fourthFlightDep_hr_f<-as.factor(as.character(final_total_data$fourthFlightDep_hr_f))
final_total_data$fourthFlightArr_hr_f<-as.factor(as.character(final_total_data$fourthFlightArr_hr_f))
final_total_data$rank2017_ver2<-as.factor(as.character(final_total_data$rank2017_ver2))
final_total_data$BookingPrice1<-gsub(",","",final_total_data$BookingPrice1)
final_total_data$BookingPrice1<-as.integer(as.character(final_total_data$BookingPrice1))
final_total_data$destination<-as.factor(final_total_data$destination)
final_total_data$Date_left<-as.integer(as.character(final_total_data$Date_left))
final_total_data$popular<-as.factor(as.character(final_total_data$popular))

final_total_data2<-final_total_data
