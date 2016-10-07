rm(list=ls())

#set current working directory
setwd("G:/Analytics/Edwisor/Edwisor/Advanced Predictive Analytics/R code")

#load libraries
library(NbClust)

#Load data
data(wine, package="rattle")
head(wine)

#standadize the data
df = data.frame(scale(wine[-1]))

#extract number of clusters to bulid
set.seed(1234)
nc = NbClust(df, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])

# wssplot <- function(data, nc=15, seed=1234){
#   wss <- (nrow(data)-1)*sum(apply(data,2,var))
#   for (i in 2:nc){
#     set.seed(seed)
#     wss[i] <- sum(kmeans(data, centers=i)$withinss)}
#   plot(1:nc, wss, type="b", xlab="Number of Clusters",
#        ylab="Within groups sum of squares")}
#wssplot(df)   

barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

#K-mean clustering
set.seed(1234)
fi_km <- kmeans(df, 3, nstart=25)
fi_km$size

#How well your kmeans is??
Cluster_accuracy = table(wine$Type, fi_km$cluster)

#Hierarchial clustering
data = iris
data = data[,1:4]

#build cluster
clusters = hclust(dist(data[, 3:4]))
plot(clusters)
clusterCut = cutree(clusters, 3)
table(clusterCut, iris$Species)

#using single linkage method
clusters = hclust(dist(iris[, 3:4]), method = 'single')
plot(clusters)
clusterCut = cutree(clusters, 3)
table(clusterCut, iris$Species)







