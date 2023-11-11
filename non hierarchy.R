library(cluster) #Clustering
library(factoextra) #Clustering & Viz
library(tidyverse) #Data Manipulation
library(dplyr)
library(fpc) #Clustering
library(ggiraphExtra) #Radar plot
library(clValid) #Choose c
library(mclust)
library(factoextra)


#Data Preparation
data=read.csv("C:/SEM 7/DATA ANALYTIC/dataclus.csv", header=TRUE, sep=",")
head(data)
str(data)
rownames(data)=data$X
dataclus=data[,2:6]
head(dataclus)
boxplot(dataclus)
datafix=scale(dataclus) #Standardize

#Choose Optimum k for K-Medoids
win.graph()
fviz_nbclust(dataclus, pam, method="wss") #Elbow Method
fviz_nbclust(dataclus, pam, method="silhouette") #Silhouette Method
gap_stat=clusGap(dataclus,pam, K.max=10, B=50)
fviz_gap_stat(gap_stat) #Gap Stat Method
npam=pamk(dataclus)
npam$nc
intern=clValid(dataclus, nClust = 2:24,clMethods = c("hierarchical","kmeans","pam"), validation = "internal")
summary(intern)

#2
#Run K-Medoids
set.seed(123)
res=pam(dataclus,2)
print(res)
summary(res)
df.clus=data.frame(dataclus,res$cluster) #Adding Cluster to DF
View(df.clus)
win.graph()
fviz_cluster(res,data=dataclus) #Cluster Plot
res$medoids #Represented object from each clusters

#Model Criterion
ssil_score=mean(silhouette(df.clus$res.cluster, dmatrix=as.matrix(res$diss))[,3]) #Silhouette Score
ssil_score

#4
#Run K-Medoid
set.seed(123)
res=pam(dataclus,4)
print(res)
summary(res)
df.clus=data.frame(dataclus,res$cluster) #Adding Cluster to DF
View(df.clus)
win.graph()
fviz_cluster(res,data=dataclus) #Cluster Plot
res$medoids #Represented object from each clusters

#Model Criterion
ssil_score=mean(silhouette(df.clus$res.cluster, dmatrix=as.matrix(res$diss))[,3]) #Silhouette Score
ssil_score
mod_cri(df.clus,4)

#Exploring Each Clusters
table(res$clustering)
