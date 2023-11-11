library(cluster) #Clustering
library(factoextra) #Clustering & Viz
library(tidyverse) #Data Manipulation
library(dplyr)
library(fpc) #Clustering
library(ggiraphExtra) #Radar plot
library(clValid) #Choose c
library(mclust)
library(factoextra)

install.packages("fpc")
install.packages("ggiraphExtra")
install.packages("clValid")
install.packages("mclust")

#Data Preparation
data=read.csv("C:/SEM 7/DATA ANALYTIC/dataclus.csv", header=TRUE, sep=",")
head(data)
str(data)
rownames(data)=data$X
dataclus=data[,2:6]
head(dataclus)
boxplot(dataclus)
datafix=scale(dataclus) #Standardize

#Run Hierarchial Method
d=dist(dataclus,method="euclidean")#Distance
hc=hclust(d, method="single")#clustering
win.graph()
plot(hc, cex=0.6, hang=-1)
sub_grp=cutree(hc, k=4)
plot(hc,cex=0.6,hang=-1)
rect.hclust(hc, k=4, border=2:5)
fviz_cluster(list(data=dataclus, cluster=sub_grp))
df.clus=data.frame(dataclus,sub_grp)

#Exploring Each Cluster
table(sub_grp)
df.clus%>%
  mutate(cluster=sub_grp)%>%
  group_by(cluster)%>%
  summarise_all("mean")

#Model Criterion
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


#Complete
#Run Hierarchial Method
d=dist(dataclus,method="euclidean")#Distance
hc=hclust(d, method="complete")#clustering
win.graph()
plot(hc, cex=0.6, hang=-1)
sub_grp=cutree(hc, k=4)
plot(hc,cex=0.6,hang=-1)
rect.hclust(hc, k=4, border=2:5)
fviz_cluster(list(data=dataclus, cluster=sub_grp))
df.clus=data.frame(dataclus,sub_grp)

#Exploring Each Cluster
table(sub_grp)
df.clus%>%
  mutate(cluster=sub_grp)%>%
  group_by(cluster)%>%
  summarise_all("mean")

#Model Criterion
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

#Average
#Run Hierarchial Method
d=dist(dataclus,method="euclidean")#Distance
hc=hclust(d, method="complete")#clustering
win.graph()
plot(hc, cex=0.6, hang=-1)
sub_grp=cutree(hc, k=4)
plot(hc,cex=0.6,hang=-1)
rect.hclust(hc, k=4, border=2:5)
fviz_cluster(list(data=dataclus, cluster=sub_grp))
df.clus=data.frame(dataclus,sub_grp)

#Exploring Each Cluster
table(sub_grp)
df.clus%>%
  mutate(cluster=sub_grp)%>%
  group_by(cluster)%>%
  summarise_all("mean")

#Model Criterion
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

#Ward
#Run Hierarchial Method
d=dist(dataclus,method="euclidean")#Distance
hc=hclust(d, method="ward")#clustering
win.graph()
plot(hc, cex=0.6, hang=-1)
sub_grp=cutree(hc, k=4)
plot(hc,cex=0.6,hang=-1)
rect.hclust(hc, k=4, border=2:5)
fviz_cluster(list(data=dataclus, cluster=sub_grp))
df.clus=data.frame(dataclus,sub_grp)

#Exploring Each Cluster
table(sub_grp)
df.clus%>%
  mutate(cluster=sub_grp)%>%
  group_by(cluster)%>%
  summarise_all("mean")

#Model Criterion
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
