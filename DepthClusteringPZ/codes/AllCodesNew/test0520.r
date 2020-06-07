### test for k means
rm(list = ls())
setwd("D:/master preparation/MasterThesis/codes/AllCodesNew")
source("euclid.r")
source("K_means.r")
source("kern_Mdepth.r")
source("TRM.r")
library(mlbench)
library(ggplot2)
library(dplyr)
library(factoextra)
library(ggpubr)
#######################################################
## Kmeans
data(iris)

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

### kmeans from R
irisCluster <- kmeans(iris[, 1:4], 3, nstart = 20)
plot(iris[,3:4],col = iris[,5],pch = irisCluster$cluster)

#### self defined kmenas with dist mean
irisCluster1 = K_means(iris[,1:4],distFun=euclid, nIitter = 10, k=3)
plot(iris[,3:4],col = iris[,5],pch = irisCluster1$clusters)

irisCluster2 = K_means(iris[,1:4],distFun = euclid,nIitter = 10, k = 3, arg = "median")
plot(iris[,3:4],col = iris[,5],pch = irisCluster2$clusters)

irisCluster3 = K_means(iris[,1:4],distFun = kern_Mdepth,k = 3,nItter)
plot(iris[,3:4],col = iris[,5],pch = irisCluster3$clusters)


#######################################################
## 
### Hierarchical clustering

df <- USArrests
df <- df%>%na.omit() %>% scale() 

# Agglomerative Hierarchical Clustering from stats

# Dissimilarity matrix
# d <- dist(df, method = "euclidean")
d <- as.dist(euclid(df,df))

#d = dist(df,"euclidean")
# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d)

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)
rect.hclust(hc1, k = 4, border = 2:5)
sub_grp <- cutree(hc1, k = 4)
fviz_cluster(list(data = df, cluster = sub_grp))


# Agglomerative Hierarchical Clustering from self defined depth

d2 <- as.dist(kern_Mdepth(df))
hc2 <- hclust(d2)

plot(hc2, cex = 0.6, hang = -1)
rect.hclust(hc2, k = 4, border = 2:5)

sub_grp2 <- cutree(hc2, k = 4)
fviz_cluster(list(data = df, cluster = sub_grp2))


#### iris
data(iris)
df = scale(iris[,1:4])
d <- as.dist(euclid(df,df))
hc3 = hclust(d)
plot(hc3)
rect.hclust(hc3, k = 3, border = 2:5)

sub_grp2 <- cutree(hc3, k = 3)
plot(iris[,3:4],col = iris[,5],pch = sub_grp2)
fviz_cluster(list(data = df, cluster = sub_grp))


d2 <- as.dist(kern_Mdepth(df))
hc3 = hclust(1-d2)
plot(hc3)
rect.hclust(hc3, k = 3, border = 2:5)

sub_grp2 <- cutree(hc3, k = 3)
plot(iris[,3:4],col = iris[,5],pch = sub_grp2)

######################################################
## Spectral clustering

