hoyun$MovingTime1.1<-ifelse(hoyun$MovingTime1=="","0시간 0분",
                                   ifelse(grepl("시간",hoyun$MovingTime1)==F,plaste("00시간",hoyunMovingTime1),as.character(hoyun$MovingTime1)))
split_MovingTime1<-str_split(hoyun$MovingTime1.1,"시간",n=2)
df<-data.frame(matrix(unlist(split_MovingTime1),nrow=20576,byrow=T))
names(df)<-c("MovingTime1_hr","MovingTime1_min")
df$MovingTime1_min<-gsub("분","",df$MovingTime1_min)
df$MovingTime1_hr<-as.integer(as.character(df$MovingTime1_hr))
df$MovingTime1_min<-as.integer(as.character(df$MovingTime1_min))
df$MovingTime1_total<-(df$MovingTime1_hr*60+df$MovingTime1_min)
hoyun<-cbind(df,hoyun)

#MovingTime2
hoyun$MovingTime2.1<-ifelse(hoyun$MovingTime2=="","0시간 0분",
                                   ifelse(grepl("시간",hoyun$MovingTime2)==F,paste("00시간",hoyun$MovingTime2),as.character(hoyun$MovingTime2)))
split_MovingTime2<-str_split(hoyun$MovingTime2.1,"시간",n=2)
df<-data.frame(matrix(unlist(split_MovingTime2),nrow=20576,byrow=T))
names(df)<-c("MovingTime2_hr","MovingTime2_min")
df$MovingTime2_min<-gsub("분","",df$MovingTime2_min)
df$MovingTime2_hr<-as.integer(as.character(df$MovingTime2_hr))
df$MovingTime2_min<-as.integer(as.character(df$MovingTime2_min))
df$MovingTime2_total<-(df$MovingTime2_hr*60+df$MovingTime2_min)
hoyun<-cbind(df,hoyun)

#MovingTime3
hoyun$MovingTime3.1<-ifelse(hoyun$MovingTime3=="","0시간 0분",
                                   ifelse(grepl("시간",hoyun$MovingTime3)==F,paste("00시간",hoyun$MovingTime3),as.character(hoyun$MovingTime3)))
split_MovingTime3<-str_split(hoyun$MovingTime3.1,"시간",n=2)
df<-data.frame(matrix(unlist(split_MovingTime3),nrow=20576,byrow=T))
names(df)<-c("MovingTime3_hr","MovingTime3_min")
df$MovingTime3_min<-gsub("분","",df$MovingTime3_min)
df$MovingTime3_hr<-as.integer(as.character(df$MovingTime3_hr))
df$MovingTime3_min<-as.integer(as.character(df$MovingTime3_min))
df$MovingTime3_total<-(df$MovingTime3_hr*60+df$MovingTime3_min)
hoyun<-cbind(df,hoyun)

#MovingTime4
hoyun$MovingTime4.1<-ifelse(hoyun$MovingTime4=="","0시간 0분",
                                   ifelse(grepl("시간",hoyun$MovingTime4)==F,paste("00시간",hoyun$MovingTime4),as.character(hoyun$MovingTime4)))
split_MovingTime4<-str_split(hoyun$MovingTime4.1,"시간",n=2)
df<-data.frame(matrix(unlist(split_MovingTime4),nrow=20576,byrow=T))
names(df)<-c("MovingTime4_hr","MovingTime4_min")
df$MovingTime4_min<-gsub("분","",df$MovingTime4_min)
df$MovingTime4_hr<-as.integer(as.character(df$MovingTime4_hr))
df$MovingTime4_min<-as.integer(as.character(df$MovingTime4_min))
df$MovingTime4_total<-(df$MovingTime4_hr*60+df$MovingTime4_min)
hoyun<-cbind(df,hoyun)

#viaNum
hoyun$viaNum<-ifelse(is.na(hoyun$Airports4)==T,0,
                     ifelse(is.na(hoyun$Airports6)==T,1,
                            ifelse(is.na(hoyun$Airports8)==T,2,3)))


#Total_Movingtime
hoyun$MovingTime_TOTAL<-ifelse(hoyun$viaNum==0,hoyun$MovingTime1_total,
                                     ifelse(hoyun$viaNum==1,hoyun$MovingTime1_total+hoyun$MovingTime2_total,
                                            ifelse(hoyun$viaNum==2,hoyun$MovingTime1_total+hoyun$MovingTime2_total+hoyun$MovingTime3_total,
                                                   hoyun$MovingTime1_total+hoyun$MovingTime2_total+hoyun$MovingTime3_total+hoyun$MovingTime4_total)))

hoyun$key<-paste(gsub(",","",hoyun$BookingPrice1),hoyun$MovingTime_TOTAL,sep="-")

final_simul$key2<-paste(final_simul$BookingPrice,final_simul$MovingTime_TOTAL,sep="-")

hoyun2<-right_join(final_simul,hoyun,by=c("key2"="key"))

hoyun3<-unique(hoyun2)
write.csv(hoyun2,file="c:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/Data2/hoyun2.csv")
getwd()
hoyun3<-unique(hoyun2)
