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
model=lm(Life.Exp ~ ., data=data) 
summary(model)
confint(model, level=0.99)
predict(model,data)

# Stepwise
step.model=stepAIC(model, direction = "both",    trace = FALSE) # direction dapat diubah "backward" atau "forward"
summary(step.model)

#Update Model
model2=update(model, ~. -Income -Illiteracy -Area) 
summary(model2)

library(lmtest)
reset_test=resettest(model, power=2)
reset_test

reset_test=resettest(step.model, power=2)
reset_test

step.model=stepAIC(model, direction="both",trace=FALSE)
summary(step.model)

model2=update(model, ~. -Income - Illiteracy -Area)
summary(model2)

library(car)
vif(model)
corr_mat=cor(data[,-4],method="pearson")
corr_mat
library(corrplot)
corrplot(corr_mat)

library(car)
durbinwatsonTest(model)
library(lmtest)
dwtest(model)
acf(model$residuals, lag.max = 15, plot = TRUE)

library(skedastic)
glejser(model)
bptest(model)

shapiro.test(model$residuals)
qqnorm(model$residuals, pch = 1, frame=FALSE)
qqline(model$residuals, col = "steelblue", lwd = 2)
hist(model$residuals)
round(mean(model$residuals),4)
sd(model$residuals)
library(fitdistrplus)
fitn = fitdist(model$residuals, "norm")
summary(fitn)

par(mfrow = c(2,2))
plot(model)






