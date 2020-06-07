##################################### 
## All data sets clustered by Kmeans
rm(list = ls())
setwd("D:/master preparation/MasterThesis/codes/AllCodesNew")
source("euclid.r")
source("K_means.r")
source("kern_Mdepth.r")
source("kern_SMdepth.r")
source("TRM.r")
library(mlbench)
library(mclust)
library(mclust)
#library(FlowSOM)
library("caret")
library(funtimes)

#####################################################
### Kmeans
par(mfrow=c(1,3))
data(iris)


df = scale(iris[,1:4])
## kmeans from R
d1 = kmeans(df,3)
plot(iris[,3:4],col = iris[,5],pch = d1$cluster,main = "Kmeans")

# evaluation
adjustedRandIndex(iris[,5],d1$cluster)
purity(iris[,5],d1$cluster)$pur

## self defined kmeans 
d1 = K_means(df,distFun = euclid,nItter = 10,k = 3)
plot(iris[,3:4],col = iris[,5],pch = d1$cluster,main = "Self Kmeans")

# evaluation
adjustedRandIndex(iris[,5],d1$cluster)
purity(iris[,5],d1$cluster)$pur

## self defined kmeans with depth function
d1 = K_means(df,distFun = kern_Mdepth,nItter = 10,k = 3)
plot(iris[,3:4],col = iris[,5],pch = d1$cluster,main = "self Kmeans with depth")

# evaluation
adjustedRandIndex(iris[,5],d1$cluster)
purity(iris[,5],d1$cluster)$pur



#####################################################
### wine
par(mfrow = c(1,3))
data("wine_1vs2")
#plot(wine_1vs2$X3,wine_1vs2$X4,col = wine_1vs2$C,main = "Wine data")

df = scale(wine_1vs2[,1:13])
## kmeans from R
d1 = kmeans(df,2)
plot(wine_1vs2[,3:4],col = wine_1vs2[,14],pch = d1$cluster,main = "Kmeans")

# evaluation
adjustedRandIndex(wine_1vs2[,14],d1$cluster)
purity(wine_1vs2[,14],d1$cluster)$pur

## self defined k means
d1 = K_means(df,distFun = euclid,k = 2)
plot(wine_1vs2[,3:4],col = wine_1vs2[,14],pch = d1$cluster,main = "self Kmeans")

# evaluation
adjustedRandIndex(wine_1vs2[,14],d1$cluster)
purity(wine_1vs2[,14],d1$cluster)$pur

## self defined k means with depth function

d1 = K_means(df,distFun = kern_Mdepth,nItter = 10,k = 2)
plot(wine_1vs2[,3:4],col = wine_1vs2[,14],pch = d1$cluster,,main = "self Kmeans with depth")

# evaluation
adjustedRandIndex(wine_1vs2[,14],d1$cluster)
purity(wine_1vs2[,14],d1$cluster)$pur

#####################################################
###
par(mfrow = c(1,3))
library(mlbench)
p <- mlbench.cassini(5000)
#plot(p)
##### kmeans from R
d1 = kmeans(p$x,3)
plot(p$x,col = d1$cluster,main = "Kmeans")

# evaluation
adjustedRandIndex(p$classes,d1$cluster)
purity(p$classes,d1$cluster)$pur

#### self defined kmeans
d1 = K_means(p$x,distFun = euclid,k = 3)
plot(p$x,col = d1$cluster,main = "self Kmeans")

# evaluation
adjustedRandIndex(p$classes,d1$cluster)
purity(p$classes,d1$cluster)$pur


#### self defined kmeans with depth function
d1 = K_means(p$x,distFun = kern_Mdepth,k = 3)
plot(p$x,col = d1$cluster,,main = "self Kmeans with depth")

# evaluation
adjustedRandIndex(p$classes,d1$cluster)
purity(p$classes,d1$cluster)$pur
#####################################################
par(mfrow = c(1,3))
p<-mlbench.spirals(300)
#plot(p)
##### kmeans from R
d1 = kmeans(p$x,2)
plot(p$x,col = d1$cluster,main = "Kmeans")

# evaluation
adjustedRandIndex(p$classes,d1$cluster)
purity(p$classes,d1$cluster)$pur

##### self defined K_means 
d1 = K_means(p$x,distFun = euclid,k = 2)
plot(p$x,col = d1$cluster,main = "self Kmeans")

# evaluation
adjustedRandIndex(p$classes,d1$cluster)
purity(p$classes,d1$cluster)$pur

##### self defined K_means with depth function
d1 = K_means(p$x,distFun = kern_Mdepth,k = 2)
plot(p$x,col = d1$cluster,main = "self Kmeans with depth")

# evaluation
adjustedRandIndex(p$classes,d1$cluster)
purity(p$classes,d1$cluster)$pur


#####################################################
par(mfrow = c(1,3))
p<-mlbench.spirals(300,1.5,0.05)
#plot(p)

##### kmeans from R
d1 = kmeans(p$x,2)
plot(p$x,col = d1$cluster)

# evaluation
adjustedRandIndex(p$classes,d1$cluster)
purity(p$classes,d1$cluster)$pur


##### self defined K_means 
d1 = K_means(p$x,distFun = euclid,k = 2)
plot(p$x,col = d1$cluster)

# evaluation
adjustedRandIndex(p$classes,d1$cluster)
purity(p$classes,d1$cluster)$pur

##### self defined K_means with depth function
d1 = K_means(p$x,distFun = kern_Mdepth,k = 2)
plot(p$x,col = d1$cluster)

# evaluation
adjustedRandIndex(p$classes,d1$cluster)
purity(p$classes,d1$cluster)$pur

#####################################################
p<-mlbench.shapes()
#plot(p)
par(mfrow = c(1,3))
##### kmeans from R
d1 = kmeans(p$x,4)
plot(p$x,col = d1$cluster,main = "Kmeans")

# evaluation
adjustedRandIndex(p$classes,d1$cluster)
purity(p$classes,d1$cluster)$pur


##### self defined K_means 
d1 = K_means(p$x,distFun = euclid,k = 4)
plot(p$x,col = d1$cluster,main = "self Kmeans")

# evaluation
adjustedRandIndex(p$classes,d1$cluster)
purity(p$classes,d1$cluster)$pur

##### self defined K_means with depth function
d1 = K_means(p$x,distFun = kern_Mdepth,k = 4)
plot(p$x,col = d1$cluster,main = "self Kmeans with depth")

# evaluation
adjustedRandIndex(p$classes,d1$cluster)
purity(p$classes,d1$cluster)$pur
#####################################################
p<-mlbench.smiley()
#plot(p)
par(mfrow = c(1,3))
##### kmeans from R
d1 = kmeans(p$x,4)
plot(p$x,col = d1$cluster,main = "Kmeans")

# evaluation
adjustedRandIndex(p$classes,d1$cluster)
purity(p$classes,d1$cluster)$pur


##### self defined K_means 
d1 = K_means(p$x,distFun = euclid,k = 4)
plot(p$x,col = d1$cluster,main = "self Kmeans")

# evaluation
adjustedRandIndex(p$classes,d1$cluster)
purity(p$classes,d1$cluster)$pur

##### self defined K_means with depth function
d1 = K_means(p$x,distFun = kern_Mdepth,k = 4)
plot(p$x,col = d1$cluster,main = "self Kmeans with depth")

# evaluation
adjustedRandIndex(p$classes,d1$cluster)
purity(p$classes,d1$cluster)$pur

#####################################################
p<-mlbench.twonorm(1000, d=2)
#plot(p)
par(mfrow = c(1,3))
##### kmeans from R
d1 = kmeans(p$x,2)
plot(p$x,col = d1$cluster,main = "Kmeans")

# evaluation
adjustedRandIndex(p$classes,d1$cluster)
purity(p$classes,d1$cluster)$pur


##### self defined K_means 
d1 = K_means(p$x,distFun = euclid,k = 2)
plot(p$x,col = d1$cluster,main = "self Kmeans")

# evaluation
adjustedRandIndex(p$classes,d1$cluster)
purity(p$classes,d1$cluster)$pur

##### self defined K_means with depth function
d1 = K_means(p$x,distFun = kern_Mdepth,k = 2)
plot(p$x,col = d1$cluster,main = "self Kmeans with depth")

# evaluation
adjustedRandIndex(p$classes,d1$cluster)
purity(p$classes,d1$cluster)$pur

