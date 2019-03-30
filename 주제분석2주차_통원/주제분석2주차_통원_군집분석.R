data<-read.csv("nomore_total.csv",header=T)
interaction.plot(
  data$destination,
  data$Month,
  data$BookingPrice1
)
par(mfrow=c(1,1))

names(data)



##############PAM Clustering################
library(cluster)

gower_dist<-daisy(dat, metric="gower",stand=T)
sil_width<-c(NA)

for(i in 2:10){
  pam_fit <- pam(gower_dist,diss = TRUE,k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

plot(1:10, sil_width,xlab = "Number of clusters",ylab = "Silhouette Width")
lines(1:10, sil_width)

pam_fit <- pam(gower_dist, diss = TRUE, k = 2)
dat[pam_fit$medoids,]
group1<-dat[which(pam_fit$clustering==1),];summary(group1)
group2<-dat[which(pam_fit$clustering==2),];summary(group2)

#summary
summary(group1$price_avg)
summary(group2$price_avg)

#visualization 
dat$cluster<-ifelse(pam_fit$clustering==1,1,2)
ggplot(aes(x = outerGPU_Compute, y = price_avg), data = dat) +
  geom_point(aes(color = factor(cluster)))

ggplot(aes(x = factor(cluster),fill=memory), data = dat) +geom_bar(width=0.5)

ggplot(aes(x = factor(cluster),fill=factor(monitor)), data = dat) +geom_bar(width=0.5)
