setwd("H:\\USF fall 2018\\Managerial analytics\\Regression project")

data=read.csv('insurance.csv')

names(data)

head(data)
attach(data)

data$sex=NULL
data$children=NULL
data$region=NULL

#head(data2)
re=lm(data$charges~data$bmi,data=data)
summary(re)

data$smoker=ifelse(data$smoker=='yes',1,0)

head(data)

write.csv(data,file="clean_data.csv")

data=read.csv('final_data.csv')

attach(data)
head(data)

reg=lm(data$charges~.,data=data)
summary(reg)

RSS <- c(crossprod(reg$residuals))
MSE <- RSS / length(reg$residuals)
RMSE <- sqrt(MSE)
RMSE

Rmse=sqrt(mean(reg$residuals^2))

predictions =predict(reg)

predictions

pred_mat=as.matrix(predictions)

comparison = cbind(data$charges,pred_mat)

head(comparison)

comparison

plot(data$charges,pred_mat)

plot(reg$fitted.values,rstandard(reg))
abline(0,0)
write.csv(comparison,file='predictions.csv')

reg

qqnorm(reg)
qqline(reg,lwd=3)

confint(pred_mat)

predict(reg,data$charges,interval = "confidence")

reg

a<-data$charges
reg2=lm(a~.-data$charges,data=data)
reg2


test=data.frame(x=c(18,33.77,0))
predict(reg,test,interval = "predict")

data2=data
head(data2)
data2$charges=NULL

library(corrplot)

xx=cor(data2)
corrplot(xx,method="circle")

cor(data2)

xx


install.packages("car")
library(car)
vif(reg)

reg

#Testing for a own value.
predict(reg,data.frame(ï..age=30,bmi=25,smoker=1),interval="confidence")



reg


reg3=lm(data$charges~data$bmi+data$smoker,data=data)
reg3


anova(reg3,reg)
