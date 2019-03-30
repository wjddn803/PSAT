dat<-read.csv("auto.csv")


#Factorizing dummy variables
dat$symboling <- factor(dat$symboling)
dat$make <- factor(dat$make)
dat$fuel.type <- factor(dat$fuel.type)
dat$aspiration <- factor(dat$aspiration)
dat$num.of.doors <- factor(dat$num.of.doors)
dat$body.style <- factor(dat$body.style)
dat$drive.wheels <- factor(dat$drive.wheels)
dat$engine.type <- factor(dat$engine.type)
dat$num.of.cylinders <- factor(dat$num.of.cylinders)
dat$fuel.system <- factor(dat$fuel.system)



#Linear Regression
fit1 <- lm(price ~ ., data=dat)
summary(fit1)
par(mfrow=c(2,2)); plot(fit1)
#Remove Outliers
dat<-dat[-c(2,26,106,124,177,149,193),]

# Subset Selection
library(MASS)
library(leaps)
fits<-regsubsets(price~.,data = dat,nvmax = 64,method = "backward")
bw<-summary(fits)
which.min(bw$cp)  
which.max(bw$adjr2)
which.min(bw$bic)

null=lm(price~1,data=dat)
full=lm(price~.,data=dat)
step(full, scope=list(lower=null, upper=full), direction = "backward")


fit2 <-lm(formula = price ~ engine.size + make + curb.weight + engine.type +
            num.of.cylinders + width + aspiration + peak.rpm + 
            length + wheel.base + height + highway.mpg + body.style + 
            fuel.system + compression.ratio, data = dat)


# Normality test of errors
shapiro.test(fit2$residuals) # p-value = 1.496e-06
library(tseries)
jarque.bera.test(fit2$residuals) # p-value < 2.2e-16
library(nortest)
ad.test(fit2$residuals)# p-value = 0.0003989 
# Dissatisfy Nor

# Remedy
bc<-boxcox(fit2)
cbind(bc$x,bc$y)
which.max(bc$y)
lambda<-bc$x[which.max(bc$y)] 
lambda # 0.101

# lambda is almost 0 (0 is in 95% confidence interval on plot) 
# At this time, we can use log(y) instead of y^lambda because they have very small differences.
# Merit of using log(y) than y^lambda : Convenience of interpretation 

fit3 <- lm(formula = log(price) ~ engine.size + make + curb.weight + engine.type +
             num.of.cylinders + width + aspiration + peak.rpm + 
             length + wheel.base + height + highway.mpg + body.style + 
             fuel.system + compression.ratio, data = dat)


shapiro.test(fit3$residuals) # p-value = 0.6396
jarque.bera.test(fit3$residuals) # p-value = 0.4704
ad.test(fit3$residuals) # p-value = 0.5858

par(mfrow=c(1,1)); plot(fit3,which = c(2)) # how cute it is!


# Homoscedasticity test of errors
library(car)
ncvTest(fit3) #  p = 0.2688713 -> Satisfy

# Independency test of errors
library(lmtest)
dwtest(fit3)  # p = 0.7948 -> Satisfy

# Multicolinearity test of variables...
# Befor that! : 'Temporarily' delete two 'NA' variables ('engine.type' & 'fuel.system')
# You can find these in 'summary(fit3)'
summary(fit3)

fitt<-lm(formula = log(price) ~ engine.size + make + curb.weight + horsepower + 
           num.of.cylinders + width + aspiration + peak.rpm +  length + wheel.base + height + 
           highway.mpg + body.style + compression.ratio, data = dat)

# Multicolinearity test of variables
vif(lm(formula = log(price) ~ engine.size + make + curb.weight + horsepower + 
         width + aspiration + peak.rpm +  length + wheel.base + height + 
         highway.mpg + body.style + compression.ratio, data = dat))
# All of VIF of variables is smaller than 10 (VIFs < 10) 
# Satisfy

# Now insert 'engine.type' & 'fuel.system' in the 'fitt' -> come back to 'fit3'
# Final Regression Equation : fit3
summary(fit3)
