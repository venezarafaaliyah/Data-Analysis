data=read.csv("C:/Users/Asus/Downloads/insurance.csv", header=T, sep=",")
x1=data$age
x2=data$bmi
y=data$charges
N=nrow(data)
lm.mod_1=lm(y~x1+x2, data=data)
summary(lm.mod_1)
library(lmtest)
bptest(lm.mod_1)
library(car)
ncvTest(lm.mod_1)
resi_data=data.frame(log_e2=log(lm.mod_1$residuals^2),x1,x2)
resi_md1=lm(log_e2~x1+x2, data=resi_data)
h_est=exp(resi_md1$fitted.values)
mdl_wls=lm(y~x1+x2, data=data, weights=1/h_est)
summary(mdl_wls)
print(round(coef(summary(mdl_wls)),5))
ncvTest(mdl_wls)

data=read.csv("C:/Users/Asus/Downloads/Data tidak normal.csv",header=T, sep=",")
y=data$Y
x=data$X
lm.mod_2=lm(y~x)
shapiro.test(lm.mod_2$residuals)
library(MASS)
bc=boxcox(y~x, lambda=seq(-5,5,1/100))
lambda=bc$x[which.max(bc$y)]
lambda
g=exp(mean(log(y)))
mdl.trans=update(lm.mod_2,((((.)^lambda)-1)/((lambda*g)^(lambda-1)))~.)
summary(mdl.trans)
shapiro.test(mdl.trans$residuals)

data=read.csv("C:/Users/Asus/Downloads/Data Autokorelasi.csv", header=T, sep=",")
lm.mod_3=lm(Y~X1+X2,data=data)
dwtest(lm.mod_3)
res.ts=residuals(lm.mod_3)[2:20] #Create Lag residuals
lag1res=residuals(lm.mod_3)[1:19] #Create Lag residuals
lagdata1=data.frame(res.ts, lag1res) #Bind time series data
acp=coef(lm(res.ts~lag1res -1, data=lagdata1)) #Rho coef (respon=residual, prediktor=lag residual)
acp
lag1y=data$Y[1:19] #lag for x and y
y=data$Y[2:20] #lag for x and y
lag1x_1=data$X1[1:19] #lag for x and y
x1=data$X1[2:20] #lag for x and y
lag1x_2=data$X2[1:19] #lag for x and y
x2=data$X2[2:20] #lag for x and y
y.co=y-(acp*lag1y) #X-(rho*lagX)
x1.co=x1-(acp*lag1x_1) #X-(rho*lagX)
x2.co=x2-(acp*lag1x_2) #X-(rho*lagX)
mdl.co=lm(y.co~x1.co+x2.co) #Regression OLS using transformation data
summary(mdl.co)
dwtest(mdl.co)  

library(MASS)
model2=rlm(y~x1+x2, data=data, method=c("MM"))
model2
summary(model2)
help(rlm)
