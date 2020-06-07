## Affine transformation
rm(list =ls())
setwd("D:/master preparation/MasterThesis/codes/AllCodesNew")
source("graph_laplacian.r")
source("kern_KNN.r")
source("kern_RBF.r")
source("kern_MahalanobisDepth.r")
source("make.similarity.r")
source("similarity.r")
source("spectral_clustering.r")
source("TRM.R")
source("euclid.r")
source("K_means.r")
source("kern_Mdepth.r")
source("kern_SMdepth.r")
source("TRM.r")
library(mclust)
library(pracma)
library(robustbase)
library(mlbench)
library(funtimes)
library(ddalpha)
library(plotrix)
c1 = as.data.frame(draw.circle(2.5,8,0.6,border="red",lty=3,lwd=3))
c2 = as.data.frame(draw.circle(2.5,8,0.2,border="red",lty=3,lwd=3))

#cdat = read.csv("F:\\fromGOOGLE\\circle.csv")
cda = rbind(c1,c2)

plot(cda,main = "circle plot")

# Affine transformation
set.seed(123)
A = matrix(rnorm(4),ncol = 2)
b = rnorm(2)
X = as.matrix(cda)
Xnew = X%*%A + b

plot(Xnew,main = "circle after Affine transformation")


# kmeans

k1 = kmeans(cda,centers= 2)
plot(cda,col = k1$cluster)
k11 = kmeans(Xnew, centers = 2)
plot(Xnew,col = k11$cluster)

k3 = K_means(cda,distFun = kern_Mdepth, nItter = 10, k = 2)
plot(cda,col = k3$clusters)
k31 = K_means(Xnew, distFun = kern_Mdepth, nItter = 10, k = 2)
plot(Xnew,col = k31$cluster)

# spectral clustering

library(kernlab)

p<-mlbench.spirals(300)
plot(p)

sc <- spectral_clustering(p$x, 2, arg ="RBF",kernel = kern_KNN)
plot(p$x, col=sc$cluster)

A = matrix(c(cos(30),sin(30),-sin(30),cos(30)),ncol = 2)
b = rnorm(2)
X = as.matrix(p$x)
Xnew = X%*%A + b

#plot(Xnew)
sc1 <- spectral_clustering(Xnew, 2, arg ="RBF",kernel = kern_KNN)
plot(Xnew,col = sc1$cluster)

####
sc <- spectral_clustering(p$x, 2, arg ="SimpleDeth",kernel = kern_KNN)
plot(p$x, col=sc$cluster)

sc1 <- spectral_clustering(Xnew, 2, arg ="MahalanobisDepth",kernel = kern_KNN)
plot(Xnew,col = sc1$cluster)


m1 = kern_Mdepth(p$x)
m2 = kern_SMdepth(Xnew)
t(A)%*%A


## iris data
data(iris)
df = scale(iris[,3:4])
k1 = kmeans(df,centers = 3)
plot(iris[,3:4],col =iris[,5],pch = k1$cluster)

k3 = K_means(df,distFun = kern_SMdepth, k = 3)
plot(iris[,3:4],col =iris[,5],pch = k3$cluster)

A = matrix(c(cos(30),sin(30),-sin(30),cos(30)),ncol = 2)
b = rnorm(2)
Xnew = df%*%A + b

k1 = kmeans(Xnew,centers = 3)
plot(Xnew,col =iris[,5],pch = k1$cluster)
