####OpenDataSet####

getwd()
setwd("C:/Users/Jungwoo Lim/Documents/2018�� ����/PSAT/�����/Data")
#FlightAllArrival<-read.csv("FlightDelay.csv",header = T)
FlightAllDeparture<-read.csv("FlightDelayDeparture.csv",header = T)
#FlightDelayArrival<-read.csv("FlightDelayAbnormalDeparture.csv",header=T)
#FlightDelayDeparture<-read.csv("FlightDelayAbnormal.csv",header=T)

weather_2015<-read.csv("1516_weather.csv",header=T)
weather_2016<-read.csv("1617_weather.csv",header=T)
weather_2017<-read.csv("1718_weather.csv",header=T)

airport<-read.csv("airports.csv",header=T)
airport

Departure_Data<-na.omit(FlightAllDeparture)
install.packages("stringr")
library("stringr")
install.packages("dplyr")
library("dplyr")
install.packages("plyr")
library("plyr")
install.packages("tidyr")
library("tidyr")
#������ �ð��� �� �и����ѳ��� �ǵ� �� ������ �ð��� ���°� "00:24" �� factor�� �����Ͱ� �̷�����ֱ� ����
#�̷��� �츮�� ��� �񱳳� �ð��븦 ���µ� ������ ����
#�׷��� ������ ���� �и����Ѽ� integeró���� �س��� �ͤ�
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



#weather�����Ͱ� 2015,2016,2017�� �������� �־ �ϴ� ���ε��� ��ó���� ���־���. 

####SplitTime in Departure Data####
#Departure Date������ �ð��� �и����� �־���. �� �� �� ������ plan, expectation, arrival�� �־!
split_time_depart1<-str_split(Departure_Data$Plan,":",n=2)
df<-data.frame(matrix(unlist(split_time_depart1),nrow=nrow(Departure_Data),byrow=T))
names(df)<-c("plan_hr","plan_min")
split_time_depart2<-str_split(Departure_Data$Expectation,":",n=2)
df2<-data.frame(matrix(unlist(split_time_depart2),nrow=nrow(Departure_Data),byrow=T))
names(df2)<-c("exp_hr","exp_min")
split_time_depart3<-str_split(Departure_Data$Arrival,":",n=2)
df3<-data.frame(matrix(unlist(split_time_depart3),nrow=nrow(Departure_Data),byrow=T))
names(df3)<-c("dep_hr","dep_min")


#���� integer�� ����ȯ ������µ� �� ���� character�޾��� ������ �̰� �׳� integer�� ġ�� 
#csv�� �ִ� �ð� ������ ������ ���ͼ� �ٸ� ������ ������ �ٲ�� ����
Departure_Dataf<-cbind(df,df2,df3,Departure_Data)
str(Departure_Dataf)

Departure_Dataf$plan_hr<-as.integer(as.character(Departure_Dataf$plan_hr))
Departure_Dataf$plan_min<-as.integer(as.character(Departure_Dataf$plan_min))
Departure_Dataf$exp_hr<-as.integer(as.character(Departure_Dataf$exp_hr))
Departure_Dataf$exp_min<-as.integer(as.character(Departure_Dataf$exp_min))
Departure_Dataf$dep_hr<-as.integer(as.character(Departure_Dataf$dep_hr))
Departure_Dataf$dep_min<-as.integer(as.character(Departure_Dataf$dep_min))

Departure_Dataf2<-Departure_Dataf

####Full join ���� key �����####
#�� ���̺��� ��ġ���� �����ϴ� key�� �־�� �Ѵ�. �����Ͻð��� ������ �����ϴ� Ű���� ���� �����̴�. 
#�׷��� �ð��뺰�� ������ ���ϱ�!
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
total_weather<-rbind(weather_2015f,weather_2016f,weather_2017f)
split_time_weather<-str_split(total_weather$time,":",n=2)
df100<-data.frame(matrix(unlist(split_time_weather),nrow=nrow(total_weather),byrow=T))
names(df100)<-c("weather_hr","weather_min")

total_weatherf<-cbind(df100,total_weather)
total_weatherf$weather_min<-NULL
WeatherTime_Check<-data.frame(paste(total_weatherf$year,total_weatherf$month,total_weatherf$date,total_weatherf$weather_hr,sep="-"))
colnames(WeatherTime_Check)<-"Check"
total_weatherf<-cbind(total_weatherf,WeatherTime_Check)

####Full join, inner join####
#full join�� �ϴϱ� �갡 ����� ������������ �𸣰ھ �ϴ� innerjoin���� ���Ҵ�. 
Departure_Dataf2$Check<-as.character(Departure_Dataf2$Check)
total_weatherf$Check<-as.character(total_weatherf$Check)
test<-full_join(Departure_Dataf2,total_weatherf,by=c("Check"="Check"))
test2<-inner_join(Departure_Dataf2,total_weatherf,by=c("Check"="Check"))
test2$year<-NULL
test2$month<-NULL
test2$date<-NULL
test2$arr_hr<-NULL
test2$arr_min<-NULL

#�����Ϻ����� �ߺ��Ǵϱ� �ϳ� ���ش�. 


####ȭ�������/Arrival Departuretime���� �ٲٱ�####
test3<-subset(test2,subset=Kind=="����")
test3$Departure_time<-test3$Arrival
test3$Arrival<-NULL#����ġ�� ������ �ٲٷ��� ������� �ϴ� �̷��Զ� �Ҷ��


####�����̸� 1, �����ƴϸ� 0####
attach(test3)
str(test3)
test3$delay<-ifelse((dep_hr*60+dep_min)-(plan_hr*60+plan_min)>=30,1,0)
a<-subset(test3,subset=delay==1)
detach(test3)

###������ ������ ����!
final<-test3
#�� �ؿ� �ڵ�� ���� ���ص� ��. �ʿ���� ��������� ���� �����ϴ°�!
remove(DepartTime_Check,Departure_Dataf,df,df100,df2,df3,df4,df5,df6,split_2015_f,split_2015_s,split_2016_f,split_2016_s,
       split_2017_f,split_2017_s,split_time_depart1,split_time_depart2,split_time_depart3,split_time_weather,weather_2015,
       weather_2016,weather_2017,total_weather,test2,test)

write.csv(final,file="C:/Users/Jungwoo Lim/Documents/2018�� ����/PSAT/�����/Data/final_data3.csv",row.names = TRUE)

str(final)