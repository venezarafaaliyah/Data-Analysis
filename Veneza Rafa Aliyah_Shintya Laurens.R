library(MASS)
data=read.csv("C:/Users/Asus/Downloads/Tugas Praktikum 1.csv")
head(data)
y=data$Saving
x=data$Income

library(lmtest)
#Regression
model=lm(y~x, data=data) 
summary(model)
confint(model, level=0.99)
predict(model,data)

#Linearity Test
library(lmtest)
reset_test=resettest(model, power=2)
reset_test

#Independent
library(car)
durbinWatsonTest(model)
library(lmtest)
dwtest(model)
acf(model$residuals, lag.max = 15, plot = TRUE)

#Identical
library(skedastic)
bptest(model)
ncvTest(model)

#Normality
shapiro.test(model$residuals)
qqnorm(model$residuals, pch = 1, frame = FALSE)
qqline(model$residuals, col = "steelblue", lwd = 2)
hist(model$residuals)

#Residual Plots
par(mfrow = c(2, 2))
plot(model)

#WLS -> Megatasi Tidak Identik
lm.mod_1=lm(y~x, data=data)
summary(lm.mod_1)
bptest(lm.mod_1)  #library(lmtest)
ncvTest(lm.mod_1) #library(car)

resi_data=data.frame(log_e2=log(lm.mod_1$residuals^2), x)
resi_mdl=lm(log_e2~x, data=resi_data)
h_est=exp(resi_mdl$fitted.values)
mdl_wls=lm(y~x, data=data, weights=1/h_est)
summary(mdl_wls)
print(round(coef(summary(mdl_wls)),5))
ncvTest(mdl_wls) #menggunakan ncv karena mampu mengakomodasi adanya pembobot
bptest(mdl_wls)

