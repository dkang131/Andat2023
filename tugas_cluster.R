setwd('C:/Users/darre/Desktop/SEM 7/AnDat/Post Midterm')

library(cluster) #Clustering
library(factoextra) #Clustering & Viz
library(tidyverse) #Data Manipulation
library(dplyr)
library(fpc) #Clustering
library(ggiraphExtra) #Radar plot
library(clValid) #Choose c
library(mclust)

data <- read.csv('wine-clustering.csv',header = T, sep = ',')
head(data)
View(data)
boxplot(data)

id <- seq(1,nrow(data))
data <- data.frame(id,data)
rownames(data) = data$id
dataclus = data[,2:ncol(data)]
head(dataclus)
View(cor(dataclus))
#dataclus <- subset(dataclus, select = c('Alcohol','Malic_Acid','Magnesium','Ash',
#                                        'OD280'))
#dataclus <- dataclus %>% select(-c('Proline','Magnesium'))
#head(dataclus)

#standarisasi data
datafix = scale(dataclus)
head(datafix)

#### K means ####
# menentukan K
fviz_nbclust(datafix, kmeans, method = 'wss') # elbow method
fviz_nbclust(datafix, kmeans, method = 'silhouette')# silhoutte method
intern=clValid(datafix, nClust = 2:24,clMethods = c("kmeans"), validation = "internal")
summary(intern)

# run k means
km_fit = kmeans(datafix, centers = 3) #center = nilai k
km_fit
df.clus = data.frame(datafix,km_fit$cluster)
head(df.clus)
#win.graph()
fviz_cluster(km_fit, data = datafix)
km_fit$centers

# exploring each cluster
table(km_fit$cluster)
summary <- df.clus %>%
  mutate(cluster = km_fit$cluster) %>%
  group_by(cluster) %>%
  summarise_all('mean')

#Model Criterion
sil_score=mean(silhouette(df.clus$km_fit.cluster, dmatrix=as.matrix(km_fit$centers))[,3]) #Silhouette Score
sil_score

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

mod_cri(df.clus,3)
