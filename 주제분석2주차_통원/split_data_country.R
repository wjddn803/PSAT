levels(total$destination)
France_data1<-subset(total,subset=total$destination=="CDG") 
France_data2<-subset(total,subset=total$destination=="ORY") 
France_data<-rbind(France_data1,France_data2)

write.csv(France_data,file="c:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/Data2/France_data.csv")

USA_data<-subset(total,subset=total$destination=="EWR" & total$destination=="JFK" &
                   total$destination=="LAX" & total$destination=="LGA" &
                   total$destination=="SEN")
USA_data1<-subset(total,subset=total$destination=="EWR")
USA_data2<-subset(total,subset=total$destination=="JFK")
USA_data3<-subset(total,subset=total$destination=="LAX")
USA_data4<-subset(total,subset=total$destination=="LGA")
USA_data5<-subset(total,subset=total$destination=="SEN")
USA_data<-rbind(USA_data1,USA_data2,USA_data3,USA_data4,USA_data5)

write.csv(USA_data,file="c:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/Data2/USA_data.csv")

England_data1<-subset(total,subset=total$destination=="LCY")
England_data2<-subset(total,subset=total$destination=="LGW")
England_data3<-subset(total,subset=total$destination=="LHR")
England_data4<-subset(total,subset=total$destination=="SEN")
England_data5<-subset(total,subset=total$destination=="STN")

England_data<-rbind(England_data1,England_data2,England_data3,England_data4,England_data5)

write.csv(England_data,file="c:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/Data2/England_data.csv")

Italy_data<-subset(total,subset=total$destination=="FCO")
write.csv(Italy_data,file="c:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/Data2/Italy_data.csv")

Singapore_data<-subset(total,subset=total$destination=="SIN")
write.csv(Singapore_data,file="c:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/Data2/Singapore_data.csv")

Austrailia_data<-subset(total,subset=total$destination=="SYD")
write.csv(Austrailia_data,file="c:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/Data2/Austrailia_data.csv")

