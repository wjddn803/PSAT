####OpenDataSet####
getwd()
setwd("C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data")
#FlightAllArrival<-read.csv("FlightDelay.csv",header = T)
FlightAllDeparture<-read.csv("FlightDelayDeparture.csv",header = T)
#FlightDelayArrival<-read.csv("FlightDelayAbnormalDeparture.csv",header=T)
#FlightDelayDeparture<-read.csv("FlightDelayAbnormal.csv",header=T)

weather_2015<-read.csv("1516_weather.csv",header=T)
weather_2016<-read.csv("1617_weather.csv",header=T)
weather_2017<-read.csv("1718_weather.csv",header=T)
weather_2018<-read.csv("18_weather.csv", header=T)

Departure_Data<-na.omit(FlightAllDeparture)

install.packages("stringr")
library("stringr")
install.packages("dplyr")
library("dplyr")
install.packages("plyr")
library("plyr")
install.packages("tidyr")
library("tidyr")
#앞으로 시간을 다 분리시켜놓을 건데 그 이유는 시간의 형태가 "00:24" 인 factor로 데이터가 이루어져있기 때문
#이러면 우리가 대소 비교나 시간대를 보는데 굉장히 힘듦
#그렇기 때문에 따로 분리시켜서 integer처리를 해놓는 것
####SplitDate2015####

split_2015_f<-str_split(weather_2015$date, "-", n = 3)
df <- data.frame(matrix(unlist(split_2015_f), nrow=8760, byrow=T))
names(df)<-c("year","month","date_no")
split_2015_s<-str_split(df[,3]," ",n=2)
df2<-data.frame(matrix(unlist(split_2015_s),nrow=8760,byrow=T))
names(df2)<-c("date","time")
weather_2015f<-cbind(cbind(df[-3],df2),weather_2015[-2])
weather_2015f$year<-as.integer(as.character(weather_2015f$year))
weather_2015f$month<-as.integer(as.character(weather_2015f$month))
weather_2015f$date<-as.integer(as.character(weather_2015f$date))


####SplitDate2016####

split_2016_f<-str_split(weather_2016$date, "-", n = 3)
df3 <- data.frame(matrix(unlist(split_2016_f), nrow=8784, byrow=T))
names(df3)<-c("year","month","date_no")
split_2016_s<-str_split(df3[,3]," ",n=2)
df4<-data.frame(matrix(unlist(split_2016_s),nrow=8784,byrow=T))
names(df4)<-c("date","time")
weather_2016f<-cbind(cbind(df3[-3],df4),weather_2016[-2])
weather_2016f$year<-as.integer(as.character(weather_2016f$year))
weather_2016f$month<-as.integer(as.character(weather_2016f$month))
weather_2016f$date<-as.integer(as.character(weather_2016f$date))


####SplitDate2017####

split_2017_f<-str_split(weather_2017$date, "-", n = 3)
df5 <- data.frame(matrix(unlist(split_2017_f), nrow=8760, byrow=T))
names(df5)<-c("year","month","date_no")
split_2017_s<-str_split(df5[,3]," ",n=2)
df6<-data.frame(matrix(unlist(split_2017_s),nrow=8760,byrow=T))
names(df6)<-c("date","time")
weather_2017f<-cbind(cbind(df5[-3],df6),weather_2017[-2])
weather_2017f$year<-as.integer(as.character(weather_2017f$year))
weather_2017f$month<-as.integer(as.character(weather_2017f$month))
weather_2017f$date<-as.integer(as.character(weather_2017f$date))

####SplitDate2018####

split_2018_f<-str_split(weather_2018$date, "-", n = 3)
df <- data.frame(matrix(unlist(split_2018_f), nrow=2904, byrow=T))
names(df)<-c("year","month","date_no")
split_2018_s<-str_split(df[,3]," ",n=2)
df2<-data.frame(matrix(unlist(split_2018_s),nrow=2904,byrow=T))
names(df2)<-c("date","time")
weather_2018f<-cbind(cbind(df[-3],df2),weather_2018[-2])
weather_2018f$year<-as.integer(as.character(weather_2018f$year))
weather_2018f$month<-as.integer(as.character(weather_2018f$month))
weather_2018f$date<-as.integer(as.character(weather_2018f$date))


#weather데이터가 2015,2016,2017,2018로 나누어져 있어서 일단 따로따로 전처리를 해주었다. 

####SplitTime in Departure Data####
#Departure Date에서도 시간을 분리시켜 주었다. 세 번 한 이유는 plan, expectation, arrival이 있어서!
split_time_depart1<-str_split(Departure_Data$Plan,":",n=2)
df<-data.frame(matrix(unlist(split_time_depart1),nrow=nrow(Departure_Data),byrow=T))
names(df)<-c("plan_hr","plan_min")
split_time_depart2<-str_split(Departure_Data$Expectation,":",n=2)
df2<-data.frame(matrix(unlist(split_time_depart2),nrow=nrow(Departure_Data),byrow=T))
names(df2)<-c("exp_hr","exp_min")
split_time_depart3<-str_split(Departure_Data$Arrival,":",n=2)
df3<-data.frame(matrix(unlist(split_time_depart3),nrow=nrow(Departure_Data),byrow=T))
names(df3)<-c("dep_hr","dep_min")


#변수 integer로 형변환 시켜줬는데 그 전에 character달아준 이유는 이걸 그냥 integer로 치면 
#csv에 있던 시간 데이터 성질이 나와서 다른 데이터 값으로 바뀌기 때문
Departure_Dataf<-cbind(df,df2,df3,Departure_Data)
str(Departure_Dataf)

Departure_Dataf$plan_hr<-as.integer(as.character(Departure_Dataf$plan_hr))
Departure_Dataf$plan_min<-as.integer(as.character(Departure_Dataf$plan_min))
Departure_Dataf$exp_hr<-as.integer(as.character(Departure_Dataf$exp_hr))
Departure_Dataf$exp_min<-as.integer(as.character(Departure_Dataf$exp_min))
Departure_Dataf$dep_hr<-as.integer(as.character(Departure_Dataf$dep_hr))
Departure_Dataf$dep_min<-as.integer(as.character(Departure_Dataf$dep_min))

Departure_Dataf2<-Departure_Dataf

####Full join 위한 key 만들기####
#두 테이블을 합치려면 공유하는 key가 있어야 한다. 연월일시간만 가지고 공유하는 키값을 만들 예정이다. 
#그래야 시간대별로 날씨가 들어가니까!
###Depart Data###
attach(Departure_Dataf2)
DepartTime_Check<-data.frame(Year,Month,Date,plan_hr,plan_min)
DepartTime_Check$key_plan_hr<-ifelse(DepartTime_Check$plan_min>=30,plan_hr+1,plan_hr)
DepartTime_Check2<-data.frame(paste(Year,Month,Date,DepartTime_Check$key_plan_hr,sep="-"))
colnames(DepartTime_Check2)<-"Check"
Departure_Dataf2<-cbind(Departure_Dataf2,DepartTime_Check2)
detach(Departure_Dataf2)
str(Departure_Dataf2)

###Weather Data###
total_weather<-rbind(weather_2015f,weather_2016f,weather_2017f,weather_2018f)
split_time_weather<-str_split(total_weather$time,":",n=2)
df100<-data.frame(matrix(unlist(split_time_weather),nrow=nrow(total_weather),byrow=T))
names(df100)<-c("weather_hr","weather_min")

total_weatherf<-cbind(df100,total_weather)
total_weatherf$weather_min<-NULL
write.csv(total_weatherf,file="C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data/final_weather.csv",row.names = TRUE)

WeatherTime_Check<-data.frame(paste(total_weatherf$year,total_weatherf$month,total_weatherf$date,total_weatherf$weather_hr,sep="-"))
colnames(WeatherTime_Check)<-"Check"
total_weatherf<-cbind(total_weatherf,WeatherTime_Check)

####Full join, inner join####
#full join을 하니까 얘가 제대로 합쳐진것인지 모르겠어서 일단 innerjoin으로 보았다. 
Departure_Dataf2$Check<-as.character(Departure_Dataf2$Check)
total_weatherf$Check<-as.character(total_weatherf$Check)
test<-full_join(Departure_Dataf2,total_weatherf,by=c("Check"="Check"))
test2<-inner_join(Departure_Dataf2,total_weatherf,by=c("Check"="Check"))
test2$year<-NULL
test2$month<-NULL
test2$date<-NULL
test2$arr_hr<-NULL
test2$arr_min<-NULL

#연월일변수는 중복되니까 하나 빼준다. 


####화물지우기/Arrival Departuretime으로 바꾸기####
test3<-subset(test2,subset=Kind=="여객")
test3$Departure_time<-test3$Arrival
test3$Arrival<-NULL#개빡치게 변수명 바꾸려면 오류뜬다 일단 이렇게라도 할라고


####지연이면 1, 지연아니면 0####
attach(test3)
str(test3)

test3$delay<-ifelse((dep_hr*60+dep_min)-(plan_hr*60+plan_min)>=30,1,
                    ifelse(test3$Situation=="회항",NA,
                           ifelse(test3$Situation=="취소",NA,0)))


detach(test3)




####airport data####
#changeDataType
airports<-read.csv("airports_final.csv",header=T)
airports$latitude_deg<-as.numeric(as.character(airports$latitude_deg))
airports$longitude_deg<-as.numeric(as.character(airports$longitude_deg))
airports$iata_code<-as.character(airports$iata_code)
str(airports)

#Departure열에 괄호 앞에 나와있는거 빼고 join할때 쓸 key 열 만든다. 
#Join까지 한다. 이때 iata_code에는 있는데 test5에는 없고, test5에는 있는데 iata_code에는 없는 obs들모 ㅁ두 삭제

attach(airports)
airports2<-subset(airports,subset=iata_code!="",select=c(latitude_deg,longitude_deg,iata_code))
airports3<-unique(airports2)
test3$airport_arrival<-substr(test3$Departure,1,3)

test4<-subset(test3,subset=is.na(test3$Departure)==FALSE)
test5<-full_join(test4,airports2,by=c("airport_arrival"="iata_code"))
test6.1<-test5
test6<-subset(test5,subset=is.na(test5$Departure)==FALSE)

detach(airports)

final<-test6
semi_final<-test6.1

#################################################################################
####################################변수추가#####################################
#################################################################################
install.packages('geosphere')
library(geosphere)

F.dat<-final
x=NULL 
y=NULL
for(i in 1:nrow(F.dat)){
  x= distm(c(126.6083,37.4722 ), c(F.dat[i,50], F.dat[i,49]), fun = distHaversine)
  y=c(y,x)
}

F.dat <- cbind(F.dat, y)

names(F.dat)[51] <- 'Distance'
str(F.dat)
write.csv(F.dat,file = "C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data/Final_all_data.csv")


library(dplyr)

data<-read.csv("Final_all_data.csv",header = T)
str(data)

#################################################################
#####################필요한 변수만 빼기 #########################
#################################################################

names(data)
data_final<-subset(data,select=c(plan_hr,plan_min,exp_hr,exp_min,dep_hr,dep_min,Airline,Name,Departure,
                                 Year,Month,Date,temp,windSpeed,windDirection,humidity,spotAtmospherePressure,
                                 sealevelAtmospherePressure,visibility,delay,airport_arrival,latitude_deg,longitude_deg,
                                 Distance))


#################################################################
#######################운행별로 범주화 하기######################
#################################################################
airline_count<-data.frame(table(data_final$Airline))
airline_count$Freq<-as.integer(airline_count$Freq)
airline_count$Var1<-as.character(airline_count$Var1)
data_final$Airline<-as.character(data_final$Airline)
data_final_count<-left_join(data_final,airline_count,by=c("Airline"="Var1"))

data_final_count$Freq2<-ifelse(data_final_count$Freq<1000,1,ifelse(data_final_count$Freq<10000,2,
                                                                   ifelse(data_final_count$Freq<100000,3,4)))
airports<-read.csv("airports.csv",header=T)
airports_need<-data.frame(airports$continent,airports$iso_country,airports$iata_code)
airports_need2<-na.omit(airports_need)


data_final_count$airport_arrival<-as.character(data_final_count$airport_arrival)
airports_need2$airports.iata_code<-as.character(airports_need2$airports.iata_code)
airports_need3<-unique(airports_need2)
data_final_count$airport_arrival<-as.character(data_final_count$airport_arrival)
airports_need3$airports.iata_code<-as.character(airports_need3$airports.iata_code)
data_final_count_dep<-left_join(data_final_count,airports_need3,by=c("airport_arrival"="airports.iata_code"))

data_final_final<-data_final_count_dep

data_final_final2<-subset(data_final_final,select=c(plan_hr,plan_min,Airline,Departure,Year,Month,Date,
                                                         temp,windSpeed,humidity,delay,Distance,Freq,Freq2))
airports_final2<-read.csv("airports_final2.csv",header=T)
airports_final3<-airports_final2[1:5]
airports_final4<-unique(airports_final3)
airports_final5<-subset(airports_final4,is.na(airports_final4$continent==TRUE))

airports_final4$continent<-as.character(airports_final4$continent)
airports_final4$new_continent<-ifelse(airports_final4$iso_country!="US" & airports_final4$iso_country!="CA",airports_final4$continent,"NA")
data_final_final2$airport_arrival<-as.character(data_final_final$airport_arrival)

airports_final4$iata_code<-as.character(airports_final4$iata_code)
data_final3<-left_join(data_final_final2,airports_final4,by=c("airport_arrival"="iata_code"))
data_final4<-data_final3[,-18]


mean_final<-data_final4

attach(mean_final)
mean_final$newTime<-ifelse(Date<10,5,ifelse(Date<20,15,25))
mean_final$time_check<-(paste(Year,Month,mean_final$newTime,sep="-"))
detach(mean_final)


mean_final2<-left_join(mean_final,meanTemperature,by=c("time_check"="data.YMD"))
mean_final2<-left_join(mean_final2,meanwindSpeed,by=c("time_check"="data.YMD"))
mean_final2<-left_join(mean_final2,meanhumidity,by=c("time_check"="data.YMD"))

names(mean_final2)

real_final<-subset(mean_final2,select=c(plan_hr,plan_min,Airline,Departure,Year,Month,Date,
                                        mean.temp,mean.windSpeed,mean.humidity,Distance,Freq,
                                        Freq2,airport_arrival,latitude_deg,longitude_deg,
                                        iso_country,new_continent,delay))

real_final2<-subset(real_final,subset=is.na(real_final$delay)==F)
write.csv(real_final2,file="C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data/final_data9.csv")

