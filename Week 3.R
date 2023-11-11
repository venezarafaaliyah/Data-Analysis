library(MASS)
data=data.frame(state.x77)
head(data)
y=data$Life.Exp
x1=data$Population
x2=data$Income
x3=data$Illiteracy
x4=data$Murder
x5=data$HS.Grad
x6=data$Frost
x7=data$Area

#Regression
model=lm(Life.Exp ~., data=data) 
summary(model)
confint(model, level=0.99)
predict(model,data)

#Linearity Test
library(lmtest)
reset_test=resettest(step.model, power=2)
reset_test

# Stepwise
step.model=stepAIC(model, direction="both", trace = FALSE) # direction dapat diubah "backward" atau "forward"
summary(step.model)

#Update Model
model2=update(model, ~. -Income -Illiteracy -Area) 
summary(model2)

#Multico
library(car)
vif(model)
corr_mat=cor(data[,-4],method="pearson")
corr_mat
library(corrplot)
corrplot(corr_mat)

#Independent
library(car)
durbinWatsonTest(model)
library(lmtest)
dwtest(model)
acf(model$residuals, lag.max = 15, plot = TRUE)

#Identical
library(skedastic)
glejser(model)
bptest(model)

#Normality
shapiro.test(model$residuals)
qqnorm(model$residuals, pch = 1, frame = FALSE)
qqline(model$residuals, col = "steelblue", lwd = 2)
hist(model$residuals)
round(mean(model$residuals),4)
sd(model$residuals)
library(fitdistrplus)
fitn=fitdist(model$residuals, "norm")
summary(fitn)

#Residual Plots
par(mfrow = c(2, 2))
plot(model)

##WLS -> Megatasi Tidak Identik
data=read.csv("C:/Users/Asus/Downloads/insurance.csv", header=T, sep=",")
x1=data$age
x2=data$bmi
y=data$charges
N=nrow(data)
lm.mod_1=lm(y~x1+x2, data=data)
summary(lm.mod_1)
bptest(lm.mod_1)  #library(lmtest)
ncvTest(lm.mod_1) #library(car)
resi_data=data.frame(log_e2=log(lm.mod_1$residuals^2), x1,x2)
resi_mdl=lm(log_e2~x1+x2, data=resi_data)
h_est=exp(resi_mdl$fitted.values)
mdl_wls=lm(y~x1+x2, data=data, weights=1/h_est)
summary(mdl_wls)
print(round(coef(summary(mdl_wls)),5))
ncvTest(mdl_wls) #menggunakan ncv karena mampu mengakomodasi adanya pembobot

##Transformasi -> Megatasi Tidak Normal
data=read.csv("C:/Users/Asus/Downloads/Data tidak normal.csv",header=T,sep=",") #Data tidak normal on gdrive
y=data$Y
x=data$X
lm.mod_2=lm(y~x)
shapiro.test(lm.mod_2$residuals)
bc=boxcox(y ~ x,lambda = seq(-5,5,1/100))
lambda=bc$x[which.max(bc$y)]
lambda
g=exp(mean(log(y))) #geometric mean from original y
mdl.trans=update(lm.mod_2, ((((.)^lambda)-1)/((lambda*g)^(lambda-1)))~ .) #LM using new y (eq for new y check on MTB help)
summary(mdl.trans)
shapiro.test(mdl.trans$residuals)

##Cochran-Orcutt -> Mengatasi Tidak Independen
data=read.csv("C:/Users/Asus/Downloads/Data Autokorelasi.csv",header=T,sep=",") #Data Autokorelasi on Gdrive
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

##Ridge Regression -> Multidimensional data
y=longley[,"Employed"]
x=data.matrix(longley[,c(2:6,1)])
lm.mod4=lm(Employed~.,data=longley)
summary(lm.mod4)
vif(lm.mod4)
library(glmnet)
model=glmnet(x,y,alpha=0)#fit ridge regression model
summary(model) 
cv_model=cv.glmnet(x,y,alpha=0) #find optimal lambda
best_lambda=cv_model$lambda.min
best_lambda
plot(cv_model) 
best_model=glmnet(x,y,alpha=0,lambda=best_lambda)
coef(best_model)
plot(model, xvar = "lambda")
y_predicted=predict(model,s=best_lambda,newx=x)
sst=sum((y-mean(y))^2)
sse=sum((y_predicted-y)^2)
rsq=1-sse/sst
rsq
#################package lain
data=longley
lmod=lm(Employed~GNP+Unemployed+Armed.Forces+Population+Year+GNP.deflator,data=longley)
summary(lmod)
vif(lmod)
longley.y=longley[,"Employed"]
longley.X=data.matrix(longley[,c(2:6,1)])
library(lmridge)
mod=lmridge(longley.y~longley.X, data=data,K=seq(0,1,0.1)) #find optimal lambda (k=lambda)
summary(mod) #optimal lambda by minimum MSE; smaller increment longer running time
kest(mod) #optimal lambda by different refferences (LW (lm.ridge))
best.mod=lmridge(longley.y~longley.X,data=data,K= 0.02907) #use optimal lambda(k) basef on LW (lm.ridge)
summary(best.mod)
vif(best.mod)


