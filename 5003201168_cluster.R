library(cluster) #Clustering
library(factoextra) #Clustering & Viz
library(tidyverse) #Data Manipulation
library(dplyr)
library(fpc) #Clustering
library(ggiraphExtra) #Radar plot
library(clValid) #Choose c
library(mclust)

#Data Preparation
data=read.csv("C:/SEM 7/DATA ANALYTIC/CLUSTERING TASK/Country-data.csv", header=TRUE)
head(data)
str(data)
rownames(data)=data$X
dataclus=data[,2:10]
head(dataclus)
boxplot(dataclus)
datafix=scale(dataclus) #Standardize

# Delete Outlier
Q1 <- quantile(dataclus, 0.25)
Q3 <- quantile(dataclus, 0.75)

IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

outliers <- datac[dataclus < lower_bound | dataclus > upper_bound]

print(outliers)


#Run Hierarchial Method
d=dist(datafix, method="euclidean") #Distance
hc=hclust(d, method="single") #Clustering
win.graph()
plot(hc, cex=0.6, hang=-1) #Dendogram
sub_grp=cutree(hc,k=4) #cut tree into 4 groups
plot(hc, cex=0.6, hang=-1)
rect.hclust(hc, k=4, border = 2:5)
fviz_cluster(list(data=datafix, cluster=sub_grp))
df.clus=data.frame(dataclus,sub_grp) #Adding Cluster to DF

#Exploring Each Clusters
table(sub_grp) #Number of members in each clusters
df.clus %>%
  mutate(cluster=sub_grp) %>%
  group_by(cluster) %>%
  summarise_all("mean") #Desc of each clusters

#Model Criterions
score_SL=mean(silhouette(df.clus$sub_grp, dmatrix=as.matrix(d))[,3]) #Silhouette score
score_SL 

mod_cri = function(Data, c)
{
  n = dim(Data)[1]
  p = dim(Data)[2]
  X = Data[,1:(p-1)]
  Group = Data[,p]
  p = dim(X)[2]
  Mean.X = matrix(ncol = p, nrow = (c+1))
  for (i in 1:c)
  {
    for (j in 1:p)
    {
      Mean.X[i,j] = mean(X[which(Group==i),j])
      Mean.X[(c+1),j] = mean(X[,j])
    }
  }
  SST = matrix(ncol=p, nrow=n)
  for (i in 1:n)
  {
    for (j in 1:p)
    {
      SST[i,j] = (X[i,j] - Mean.X[(c+1),j])^2
    }
  }
  SST = sum(sum(SST))
  SSE = matrix(ncol=p, nrow=n)
  for (i in 1:n)
  {
    for (j in 1:p)
    {
      for (k in 1:c)
      {
        if (Group[i]==k)
        {
          SSE[i,j] = (X[i,j] - Mean.X[k,j])^2
        }
      }
    }
  }
  SSE = sum(sum(SSE))
  Rsq = (SST-SSE)/SST
  icdrate = 1-Rsq
  Pseudof = (Rsq/(c-1))/((icdrate)/(n-c))
  ssb=SST-SSE
  list(SSW=SSE, SST=SST, SSB=ssb, Rsq=Rsq, icdrate=icdrate, pseudof=Pseudof)
}
mod_cri(df.clus,4)

#Choose Optimum k for K-Means
fviz_nbclust(dataclus, kmeans, method="wss") #Elbow Method
fviz_nbclust(dataclus, kmeans, method="silhouette") #Silhouette Method
intern=clValid(dataclus, nClust = 1:nrow(dataclus),clMethods = c("kmeans"), validation = "internal")
summary(intern)

#2
#Run K-Medoids
set.seed(123)
res=pam(datafix,2)
print(res)
summary(res)
df.clus=data.frame(datafix,res$cluster) #Adding Cluster to DF
View(df.clus)
win.graph()
fviz_cluster(res,data=datafix) #Cluster Plot
res$medoids #Represented object from each clusters

#Model Criterion
ssil_score=mean(silhouette(df.clus$res.cluster, dmatrix=as.matrix(res$diss))[,3]) #Silhouette Score
mod_cri(df.clus,2)


