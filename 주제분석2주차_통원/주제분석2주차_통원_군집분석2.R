getwd()
setwd("c:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/Data2")
total_data<-read.csv("no_more_datahandling.csv",header=T)

total<-read.csv("nomore_total.csv",header=T)
via0_data<-read.csv("nomore_via0.csv",header=T)
via1_data<-read.csv("nomore_via1.csv",header=T)
via2_data<-read.csv("nomore_via2.csv",header=T)
via3_data<-read.csv("nomore_via3.csv",header=T)
  
#################################################
############ Clustering 을 해보자 ###############
#################################################


########## TOTAL ############

library(cluster)
str(total)
names(total)
total<-subset(total,select=c(Year,Month,Date_num,Date_left,popular,BookingAgency1,BookingPrice1,destination,
                             viaNum,StayTime_TOTAL,MovingTime_TOTAL,FirstFlightDep_hr_f,rank2017_ver2))


gower_dist<-daisy(total, metric="gower",stand=T)#아 오류가 뜬다...

###################### via0부터 해봐야지... ####################### 
str(via0_data)
via0_data$Year<-as.factor(as.character(via0_data$Year))
via0_data$Month<-as.factor(as.character(via0_data$Year))
via0_data$Date_num<-as.factor(as.character(via0_data$Year))
via0_data$FirstFlightDep_hr_f<-as.factor(as.character(via0_data$FirstFlightDep_hr_f))
via0_data$rank2017_ver2<-as.factor(as.character(via0_data$rank2017_ver2))


gower_dist<-daisy(via0_data,metric = "gower",stand=T)

sil_width<-c(NA)

for(i in 2:20){
  pam_fit <- pam(gower_dist,diss = TRUE,k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}
par(mfrow=c(1,1))
plot(1:20, sil_width,xlab = "Number of clusters",ylab = "Silhouette Width")
lines(1:20, sil_width)#6개 혹은 16개....
levels(via0_data$destination) #이건 또 10갠데...

pam_fit <- pam(gower_dist, diss = TRUE, k = 10)
via0_data[pam_fit$medoids,]

group1<-via0_data[which(pam_fit$clustering==1),]
group2<-via0_data[which(pam_fit$clustering==2),]
group3<-via0_data[which(pam_fit$clustering==3),]
group4<-via0_data[which(pam_fit$clustering==4),]
group5<-via0_data[which(pam_fit$clustering==5),]
group6<-via0_data[which(pam_fit$clustering==6),]

#가격에 따라서는 꽤 차이가 있다. 
summary(group1$BookingPrice1)
summary(group2$BookingPrice1)
summary(group3$BookingPrice1)
summary(group4$BookingPrice1)
summary(group5$BookingPrice1)
summary(group6$BookingPrice1)

#시간대별로도 꽤 차이가 있어 보인다. 
summary(group1$FirstFlightDep_hr_f)
summary(group2$FirstFlightDep_hr_f)
summary(group3$FirstFlightDep_hr_f)
summary(group4$FirstFlightDep_hr_f)
summary(group5$FirstFlightDep_hr_f)
summary(group6$FirstFlightDep_hr_f)

names(via0_data)
summary(group1$Airports2)
summary(group2$Airports2)
summary(group3$Airports2)
summary(group4$Airports2)
summary(group5$Airports2)
summary(group6$Airports2)


#Visualization
group1$check<-"1"
group2$check<-"2"
group3$check<-"3"
group4$check<-"4"
group5$check<-"5"
group6$check<-"6"


via0_group<-rbind(group1,group2,group3,group4,group5,group6)
library(ggplot2)
#도착지 별로 보자
ggplot(via0_group,aes(x=destination,y=BookingPrice1,color=check, size=2))+geom_point()자

#처음 출발시간대 별로 보자
ggplot(via0_group,aes(x=FirstFlightDep_hr_f,y=BookingPrice1,color=check))+geom_point()

#남은 시간대 별로 보자
ggplot(via0_group,aes(x=Date_left,y=BookingPrice1,color=check))+geom_point()

#moving time으로 보자
ggplot(via0_group,aes(x=MovingTime1_total,y=BookingPrice1,color=check))+geom_point()


######################### via1 ######################
str(via1_data)
via1_data$Year<-as.factor(as.character(via1_data$Year))
via1_data$Month<-as.factor(as.character(via1_data$Year))
via1_data$Date_num<-as.factor(as.character(via1_data$Year))
via1_data$FirstFlightDep_hr_f<-as.factor(as.character(via1_data$FirstFlightDep_hr_f))
via1_data$FirstFlightArr_hr_f<-as.factor(as.character(via1_data$FirstFlightArr_hr_f))
via1_data$secondFlightDep_hr_f<-as.factor(as.character(via1_data$secondFlightDep_hr_f))
via1_data$secondFlightArr_hr_f<-as.factor(as.character(via1_data$secondFlightArr_hr_f))
via1_data$rank2017_ver2<-as.factor(as.character(via1_data$rank2017_ver2))

via_data<-subset(via1_data,select=c(Year,Month,Date_num,Date_left,popular,BookingAgency1,
                                     BookingPrice1,Airports2,Airports4,destination,Staylocation,
                                     StayTime1_total,StayTime2_total,StayTime_TOTAL,MovingTime1_total,
                                     MovingTime2_total,MovingTime_TOTAL,FirstFlightDep_hr_f,FirstFlightArr_hr_f,
                                     rank2017_ver2,Airport2_iso_country,Airport2_Distance,Airport4_iso_country,
                                     Airport4_Distance))

gower_dist<-daisy(via1_data,metric = "gower",stand=T)

sil_width<-c(NA)

for(i in 2:10){
  pam_fit <- pam(gower_dist,diss = TRUE,k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}
par(mfrow=c(1,1))
plot(1:20, sil_width,xlab = "Number of clusters",ylab = "Silhouette Width")
lines(1:20, sil_width)#6개 혹은 16개....
levels(via1_data$destination) #이건 또 10갠데...

pam_fit <- pam(gower_dist, diss = TRUE, k = 10)
via1_data[pam_fit$medoids,]

group1<-via1_data[which(pam_fit$clustering==1),]
group2<-via1_data[which(pam_fit$clustering==2),]
group3<-via1_data[which(pam_fit$clustering==3),]
group4<-via1_data[which(pam_fit$clustering==4),]
group5<-via1_data[which(pam_fit$clustering==5),]
group6<-via1_data[which(pam_fit$clustering==6),]

#가격에 따라서는 꽤 차이가 있다. 
summary(group1$BookingPrice1)
summary(group2$BookingPrice1)
summary(group3$BookingPrice1)
summary(group4$BookingPrice1)
summary(group5$BookingPrice1)
summary(group6$BookingPrice1)

#시간대별로도 꽤 차이가 있어 보인다. 
summary(group1$FirstFlightDep_hr_f)
summary(group2$FirstFlightDep_hr_f)
summary(group3$FirstFlightDep_hr_f)
summary(group4$FirstFlightDep_hr_f)
summary(group5$FirstFlightDep_hr_f)
summary(group6$FirstFlightDep_hr_f)

names(via1_data)
summary(group1$Airports2)
summary(group2$Airports2)
summary(group3$Airports2)
summary(group4$Airports2)
summary(group5$Airports2)
summary(group6$Airports2)


#Visualization
group1$check<-"1"
group2$check<-"2"
group3$check<-"3"
group4$check<-"4"
group5$check<-"5"
group6$check<-"6"


via1_group<-rbind(group1,group2,group3,group4,group5,group6)
library(ggplot2)
#도착지 별로 보자
ggplot(via1_group,aes(x=destination,y=BookingPrice1,color=check, size=2))+geom_point()자

#처음 출발시간대 별로 보자
ggplot(via1_group,aes(x=FirstFlightDep_hr_f,y=BookingPrice1,color=check))+geom_point()

#남은 시간대 별로 보자
ggplot(via1_group,aes(x=Date_left,y=BookingPrice1,color=check))+geom_point()

#moving time으로 보자
ggplot(via1_group,aes(x=MovingTime1_total,y=BookingPrice1,color=check))+geom_point()
