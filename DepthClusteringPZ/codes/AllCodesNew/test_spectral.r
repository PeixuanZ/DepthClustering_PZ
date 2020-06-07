rm(list = ls())
setwd("D:/master preparation/MasterThesis/codes/AllCodesNew")
source("graph_laplacian.r")
source("kern_KNN.r")
source("kern_RBF.r")
source("kern_MahalanobisDepth.r")
source("make.similarity.r")
source("similarity.r")
source("spectral_clustering.r")
source("TRM.R")
library(mclust)
library(pracma)
library(robustbase)
library(mlbench)
library(funtimes)
library(ddalpha)

########################### 
## All data sets

### Kmeans
data(iris)
par(mfrow = c(2,2))
df = scale(iris[,1:4])
x1 = spectral_clustering(df,3,arg ="RBF",kernel = kern_KNN)
plot(df[,3:4],col =  x1$cluster,main = "Spectral")
x2 = spectral_clustering(df,3,arg ="MahalanobisDepth",kernel = kern_KNN)
plot(df[,3:4],col = x2$cluster,main = "Spectral with depth")
x3 = spectral_clustering(df,3,arg ="MahalanobisDepth",kernel = kern_KNN,cut = "Ratio")
plot(df[,3:4],col = x3$cluster,main = "Spectral with depth and ratio cut")
x4 = spectral_clustering(df,3,arg ="MahalanobisDepth",kernel = kern_KNN,cut = "Ncut",normalized = "Normalized")
plot(df[,3:4],col = x4$cluster,main = "Spectral with depth and Ncut")

# evaluation
adjustedRandIndex(iris[,5],x1$cluster)
purity(iris[,5],x1$cluster)$pur

adjustedRandIndex(iris[,5],x2$cluster)
purity(iris[,5],x2$cluster)$pur

adjustedRandIndex(iris[,5],x3$cluster)
purity(iris[,5],x3$cluster)$pur

adjustedRandIndex(iris[,5],x4$cluster)
purity(iris[,5],x4$cluster)$pur

########################### 
### wine
par(mfrow = c(2,2))
data("wine_1vs2")
df = scale(wine_1vs2[,1:13])

x1 = spectral_clustering(df,2,arg ="RBF",kernel = kern_KNN)
plot(df[,3:4],col = x1$cluster,main = "Spectral")
x2 = spectral_clustering(df,2,arg ="MahalanobisDepth",kernel = kern_KNN)
plot(df[,3:4],col  = x2$cluster,main = "Spectral with depth")
x3 = spectral_clustering(df,2,arg = 0,kernel = kern_MahalanobisDepth,cut = "Ratio")
plot(df[,3:4],col = x3$cluster,main = "Spectral with depth and ratio cut")
x4 = spectral_clustering(df,2,arg =0,kernel = kern_MahalanobisDepth,cut = "Ncut",normalized = "Normalized")
plot(df[,3:4],col= x4$cluster,main = "Spectral with depth and Ncut")

# evaluation
adjustedRandIndex(wine_1vs2$C,x1$cluster)
purity(wine_1vs2$C,x1$cluster)$pur

adjustedRandIndex(wine_1vs2$C,x2$cluster)
purity(wine_1vs2$C,x2$cluster)$pur

adjustedRandIndex(wine_1vs2$C,x3$cluster)
purity(wine_1vs2$C,x3$cluster)$pur

adjustedRandIndex(wine_1vs2$C,x4$cluster)
purity(wine_1vs2$C,x4$cluster)$pur


############################################################

p <- mlbench.cassini(5000)
#plot(p)

par(mfrow = c(2,2))
df=p$x

x1 = spectral_clustering(df,3,arg ="RBF",kernel = kern_KNN)
plot(df,col = x1$cluster,main = "Spectral")
x2 = spectral_clustering(df,3,arg ="MahalanobisDepth",kernel = kern_KNN)
plot(df,col  = x2$cluster,main = "Spectral with depth")
x3 = spectral_clustering(df,3,arg ="MahalanobisDepth",kernel = kern_KNN,cut = "Ratio")
plot(df,col = x3$cluster,main = "Spectral with depth and ratio cut")
x4 = spectral_clustering(df,3,arg ="MahalanobisDepth",kernel = kern_KNN,cut = "Ncut",normalized = "Normalized")
plot(df,col= x4$cluster,main = "Spectral with depth and Ncut")

# evaluation
adjustedRandIndex(p$classes,x1$cluster)
purity(p$classes,x1$cluster)$pur

adjustedRandIndex(p$classes,x2$cluster)
purity(p$classes,x2$cluster)$pur

adjustedRandIndex(p$classes,x3$cluster)
purity(p$classes,x3$cluster)$pur

adjustedRandIndex(p$classes,x4$cluster)
purity(p$classes,x4$cluster)$pur

#########################################
p<-mlbench.spirals(300)
#plot(p)
par(mfrow = c(2,2))
df=p$x

x1 = spectral_clustering(df,2,arg ="RBF",kernel = kern_KNN)
plot(df,col = x1$cluster,main = "Spectral")
x2 = spectral_clustering(df,2,arg ="MahalanobisDepth",kernel = kern_KNN)
plot(df,col  = x2$cluster,main = "Spectral with depth")
x3 = spectral_clustering(df,2,arg ="MahalanobisDepth",kernel = kern_KNN,cut = "Ratio")
plot(df,col = x3$cluster,main = "Spectral with depth and ratio cut")
x4 = spectral_clustering(df,2,arg ="MahalanobisDepth",kernel = kern_KNN,cut = "Ncut",normalized = "Normalized")
plot(df,col= x4$cluster,main = "Spectral with depth and Ncut")

# evaluation
adjustedRandIndex(p$classes,x1$cluster)
purity(p$classes,x1$cluster)$pur

adjustedRandIndex(p$classes,x2$cluster)
purity(p$classes,x2$cluster)$pur

adjustedRandIndex(p$classes,x3$cluster)
purity(p$classes,x3$cluster)$pur

adjustedRandIndex(p$classes,x4$cluster)
purity(p$classes,x4$cluster)$pur
###################################################
p<-mlbench.spirals(300,1.5,0.05)
#plot(p)
par(mfrow = c(2,2))
df=p$x

x1 = spectral_clustering(df,2,arg ="RBF",kernel = kern_KNN)
plot(df,col = x1$cluster,main = "Spectral")
x2 = spectral_clustering(df,2,arg ="MahalanobisDepth",kernel = kern_KNN)
plot(df,col  = x2$cluster,main = "Spectral with depth")
x3 = spectral_clustering(df,2,arg ="MahalanobisDepth",kernel = kern_KNN,cut = "Ratio")
plot(df,col = x3$cluster,main = "Spectral with depth and ratio cut")
x4 = spectral_clustering(df,2,arg ="MahalanobisDepth",kernel = kern_KNN,cut = "Ncut",normalized = "Normalized")
plot(df,col= x4$cluster,main = "Spectral with depth and Ncut")
# evaluation
adjustedRandIndex(p$classes,x1$cluster)
purity(p$classes,x1$cluster)$pur

adjustedRandIndex(p$classes,x2$cluster)
purity(p$classes,x2$cluster)$pur

adjustedRandIndex(p$classes,x3$cluster)
purity(p$classes,x3$cluster)$pur

adjustedRandIndex(p$classes,x4$cluster)
purity(p$classes,x4$cluster)$pur
###################################################
p<-mlbench.shapes()
#plot(p)

par(mfrow = c(2,2))
df=p$x

x1 = spectral_clustering(df,4,arg ="RBF",kernel = kern_KNN)
plot(df,col = x1$cluster,main = "Spectral")
x2 = spectral_clustering(df,4,arg ="MahalanobisDepth",kernel = kern_KNN)
plot(df,col  = x2$cluster,main = "Spectral with depth")
x3 = spectral_clustering(df,4,arg ="MahalanobisDepth",kernel = kern_KNN,cut = "Ratio")
plot(df,col = x3$cluster,main = "Spectral with depth and ratio cut")
x4 = spectral_clustering(df,4,arg ="MahalanobisDepth",kernel = kern_KNN,cut = "Ncut",normalized = "Normalized")
plot(df,col= x4$cluster,main = "Spectral with depth and Ncut")
# evaluation
adjustedRandIndex(p$classes,x1$cluster)
purity(p$classes,x1$cluster)$pur

adjustedRandIndex(p$classes,x2$cluster)
purity(p$classes,x2$cluster)$pur

adjustedRandIndex(p$classes,x3$cluster)
purity(p$classes,x3$cluster)$pur

adjustedRandIndex(p$classes,x4$cluster)
purity(p$classes,x4$cluster)$pur
###################################################
p<-mlbench.smiley()
#plot(p)
par(mfrow = c(2,2))
df=p$x

x1 = spectral_clustering(df,4,arg ="RBF",kernel = kern_KNN)
plot(df,col = x1$cluster,main = "Spectral")
x2 = spectral_clustering(df,4,arg ="MahalanobisDepth",kernel = kern_KNN)
plot(df,col  = x2$cluster,main = "Spectral with depth")
x3 = spectral_clustering(df,4,arg ="MahalanobisDepth",kernel = kern_KNN,cut = "Ratio")
plot(df,col = x3$cluster,main = "Spectral with depth and ratio cut")
x4 = spectral_clustering(df,4,arg ="MahalanobisDepth",kernel = kern_KNN,cut = "Ncut",normalized = "Normalized")
plot(df,col= x4$cluster,main = "Spectral with depth and Ncut")
# evaluation
adjustedRandIndex(p$classes,x1$cluster)
purity(p$classes,x1$cluster)$pur

adjustedRandIndex(p$classes,x2$cluster)
purity(p$classes,x2$cluster)$pur

adjustedRandIndex(p$classes,x3$cluster)
purity(p$classes,x3$cluster)$pur

adjustedRandIndex(p$classes,x4$cluster)
purity(p$classes,x4$cluster)$pur
###################################################


p<-mlbench.twonorm(1000, d=2)
par(mfrow = c(2,2))
df=p$x

x1 = spectral_clustering(df,2,arg ="RBF",kernel = kern_KNN)
plot(df,col = x1$cluster,main = "Spectral")
x2 = spectral_clustering(df,2,arg ="MahalanobisDepth",kernel = kern_KNN)
plot(df,col  = x2$cluster,main = "Spectral with depth")
x3 = spectral_clustering(df,2,arg ="MahalanobisDepth",kernel = kern_KNN,cut = "Ratio")
plot(df,col = x3$cluster,main = "Spectral with depth and ratio cut")
x4 = spectral_clustering(df,2,arg ="MahalanobisDepth",kernel = kern_KNN,cut = "Ncut",normalized = "Normalized")
plot(df,col= x4$cluster,main = "Spectral with depth and Ncut")
# evaluation
adjustedRandIndex(p$classes,x1$cluster)
purity(p$classes,x1$cluster)$pur

adjustedRandIndex(p$classes,x2$cluster)
purity(p$classes,x2$cluster)$pur

adjustedRandIndex(p$classes,x3$cluster)
purity(p$classes,x3$cluster)$pur

adjustedRandIndex(p$classes,x4$cluster)
purity(p$classes,x4$cluster)$pur