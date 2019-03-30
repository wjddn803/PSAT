getwd()
setwd("C:/Users/Jungwoo Lim/Documents/2018년 과제/데이터사이언스와 통계적사고")

file<-read.csv("2016년+시군구+합계출산율.csv",header=T)

library(dplyr)

###############################################
################ Data Handling ################
###############################################

data1<-file[-1,]

#변수명 수정
names(data1)<-c("region","total","15-19","20-24","25-29","30-34","35-39","40-44","44-49")


#데이터 타입 수정
str(data1)
data1$total<-as.numeric(as.character(data1$total))
data1$`15-19`<-as.numeric(as.character(data1$`15-19`))
data1$`20-24`<-as.numeric(as.character(data1$`20-24`))
data1$`25-29`<-as.numeric(as.character(data1$`25-29`))
data1$`30-34`<-as.numeric(as.character(data1$`30-34`))
data1$`35-39`<-as.numeric(as.character(data1$`35-39`))
data1$`40-44`<-as.numeric(as.character(data1$`40-44`))
data1$`44-49`<-as.numeric(as.character(data1$`44-49`))
data2<-data1

#시, 도 구별해주는 코드 생성
data2$region_code<-ifelse(grepl("시",data2$region)==T,"C",
                          ifelse(grepl("도",data2$region)==T,"DO","TOTAL"))
data3<-data2


#변수타입 재설정
str(data3)
data3$region_code<-as.factor(data3$region_code)
final<-data3

#연령대 구분 코드 생성
label20_cho<-data.frame(cbind(data3$`20-24`,"20"))
label20_hoo<-data.frame(cbind(data3$`25-29`,"20"))
label30_cho<-data.frame(cbind(data3$`30-34`,"30"))
label30_hoo<-data.frame(cbind(data3$`35-39`,"30"))
label40_cho<-data.frame(cbind(data3$`40-44`,"40"))
label40_hoo<-data.frame(cbind(data3$`44-49`,"40"))


final2<-rbind(label20_cho,label20_hoo,label30_cho,label30_hoo,label40_cho,label40_hoo)
names(final2)<-c("FTR","label")
final2$FTR<-as.numeric(as.character(final2$FTR))
final2$label<-as.factor(final2$label)

#### T-test ####
# package download
library(lsr)
library(onewaytests)

# independence sample t-test
independentSamplesTTest (formula =total~region_code, data=final[-1,])

#### F-test ####

# F-test
aov.test(FTR~label, data=final2)
str(final2)





