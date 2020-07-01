library(ggplot2)
#data visualization
library(readr)
#csv file I/O
system("ls../input")
#read iris data set
summary(iris)
names((iris))
class(iris)
dim(iris)
head(iris)
#plotting iris
pairs(iris[,1:4])
#plotting with colours
pairs(iris[,1:4],col=iris[,5],oma=c(4,4,6,12))
par(xpd=TRUE)
legend(0.85,0.6,as.vector(unique(iris$Species)),fill=c(1,2,3))
#cluster all data and use the k-means method
library(dplyr)
library(corrplot)
data("iris")
#we set seed to create our visualization
set.seed(8593)
#assigning iris dataset to variable iris2
iris2<-iris
iris2
#removing the species by assigning them to null
iris2$Species <- NULL
#now 3 clusters
(kmeans.result <-kmeans(iris2,3))
#versicolor and virginca is overlapping each other in above data
table(iris$Species,kmeans.result$cluster)
#plotting the cluster itself
plot(iris2[c("Sepal.Length","Sepal.Width")],col = kmeans.result$cluster)
points(kmeans.result$centers[c("Sepal.Length","Sepal.Width")],col=1:3,pch=8,cex=2)
library(cluster)
library(fpc)
data("iris")
data_for_clustering<-iris[,-5]
cluster_iris <- kmeans(data_for_clustering,centers = 3)
plotcluster(data_for_clustering,cluster_iris$cluster)
#clusplot data
clusplot(data_for_clustering,cluster_iris$cluster,color = TRUE,shade = TRUE)
