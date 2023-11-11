data("iris")
set.seed(123)
train=sample(seq(nrow(iris)), size=floor(0.75*nrow(iris)), replace=F)
train_data=iris[train,]
test_data=iris[-train,]

library(MASS) #Disc.
library(biotools) #Box M
library(pROC)
library(caret)
library(mvnormtest)
library(corrplot)
library(PerformanceAnalytics)
library(psych)
library(klaR)
library(ggplot2)

mshapiro.test(t(iris[,-5]))
boxM(data=iris[,-5], grouping = iris[,5])
r=cor(iris[,-5])
r
corrplot(r)
chart.Correlation(iris[,-5])
cortest.bartlett(r,n=nrow(iris[,-5]))
m=manova(as.matrix(iris[,-5])~iris[,5])
summary(object=m, test='Wilks')
  
linear=lda(Species~., data=train_data)           
linear
sw=stepclass(Species~., data=train_data, method="lda", criterion="AS")
lda(sw$formula, data=train_data)

predict=predict(linear, newdata=test_data, probability=TRUE)
win.graph()
partimat(Species~Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
         data=train_data,  method="lda")

cm1=confusionMatrix(test_data$Species, predict$class)
cm1
cm=cm1$table
cm
n=sum(cm)
nc=nrow(cm)
diag=diag(cm)
rowsum=apply(cm,1,sum)
colsum=apply(cm,2,sum)
p=rowsum/n
q=colsum/n
accuracy = sum(diag)/n
precission=diag/colsum
recall=diag/rowsum
f1=2*precission*recall/(precission+recall)
data.frame(accuracy, precission, recall, f1)
roc_lda=multiclass.roc(test_data$Species, predict$posterior[,1])
auc(roc_lda)
rs=roc_lda[['rocs']]
plot.roc(rs[[3]])

## Quadratic discriminant analysis - QDA
library(MASS)
quadratic<-qda(formula=Species~., data=train_data)
quadratic
predict<-predict(object = quadratic, newdata = test_data,probability=TRUE)
confusionMatrix(test_data$Species, predict$class)

## Mixture discriminant analysis - MDA
library(mda)
mixture<-mda(formula=Species~., data=train_data)
mixture
predict<-predict(object = mixture, newdata = test_data,probability=TRUE)
confusionMatrix(test_data$Species, predict)

## Flexible discriminant analysis - FDA
library(mda)
fleksibel<-fda(formula=Species~., data=train_data)
fleksibel
predict<-predict(object = fleksibel, newdata = test_data,probability=TRUE)
confusionMatrix(test_data$Species, predict)

## Regularized discriminant analysis - RDA
library(klaR)
regular<-rda(formula=Species~., data=train_data)
regular
predict<-predict(object = regular, newdata = test_data,probability=TRUE)
confusionMatrix(test_data$Species, predict$class)
