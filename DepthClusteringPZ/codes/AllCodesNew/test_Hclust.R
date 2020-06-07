##################################### 
## All data sets clustered by Hierarchical clustering
rm(list = ls())
setwd("D:/master preparation/MasterThesis/codes/AllCodesNew")
source("euclid.r")
source("K_means.r")
source("kern_Mdepth.r")
source("kern_SMdepth.r")
source("TRM.r")
library(mlbench)
library(dendextend)

library(mclust)
#library(FlowSOM)
library("caret")
library(funtimes)
#####################################################
### iris
par(mfrow=c(1,3))
data(iris)
#plot(iris$Petal.Length,iris$Petal.Width,col = iris$Species,main = "iris data",pch = 2)
df = scale(iris[,1:4])

d <- as.dist(euclid(df,df))
hc1 <- hclust(d)
# plot(hc1, cex = 0.6)
# rect.hclust(hc1, k = 3, border = 2:4)
sub_grp <- cutree(hc1, k = 3)
plot(iris[,3:4],col=sub_grp,main = "hclust")

# evaluation
adjustedRandIndex(iris[,5],sub_grp)
purity(iris[,5],sub_grp)$pur

## self defined hclust with simple depth function
d <- as.dist(kern_SMdepth(df,df))
hc1 <- hclust(1-d)
# plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 3, border = 2:4)
sub_grp <- cutree(hc1, k = 3)
plot(iris[,3:4],col = sub_grp,main = "hclust with simple depth")

# evaluation
adjustedRandIndex(iris[,5],sub_grp)
purity(iris[,5],sub_grp)$pur


## self defined hclust with depth function
d <- as.dist(kern_Mdepth(df,df))
hc1 <- hclust(1-d)
# plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 3, border = 2:4)
sub_grp <- cutree(hc1, k = 3)
plot(iris[,3:4],col =sub_grp,main = "hclust with depth")

# evaluation
adjustedRandIndex(iris[,5],sub_grp)
purity(iris[,5],sub_grp)$pur


#####################################################
### wine
par(mfrow=c(1,3))
data("wine_1vs2")
#plot(wine_1vs2$X3,wine_1vs2$X4,col = wine_1vs2$C,main = "Wine data")

df = scale(wine_1vs2[,1:13])

d <- as.dist(euclid(df,df))
hc1 <- hclust(d)
# plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 2, border = 2:3)
sub_grp <- cutree(hc1, k = 2)
plot(wine_1vs2[,3:4],col = sub_grp,main = "hclust")

# evaluation
adjustedRandIndex(wine_1vs2[,14],sub_grp)
purity(wine_1vs2[,14],sub_grp)$pur

d <- as.dist(kern_SMdepth(df,df))
hc1 <- hclust(1-d)
# plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 2, border = 2:3)
sub_grp <- cutree(hc1, k = 2)
plot(wine_1vs2[,3:4],col= sub_grp,main = "hclust with simple depth")

# evaluation
adjustedRandIndex(wine_1vs2[,14],sub_grp)
purity(wine_1vs2[,14],sub_grp)$pur


## self defined hclust with depth function
d <- as.dist(kern_Mdepth(df,df))
hc1 <- hclust(1-d)
# plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 2, border = 2:3)
sub_grp <- cutree(hc1, k = 2)
plot(wine_1vs2[,3:4],col = sub_grp,main = "hclust with depth")

# evaluation
adjustedRandIndex(wine_1vs2[,14],sub_grp)
purity(wine_1vs2[,14],sub_grp)$pur

#####################################################
###
library(mlbench)
par(mfrow=c(1,3))
p <- mlbench.cassini(5000)
#plot(p)
#png("D:/master preparation/MasterThesis/figs/datasets/hclust/hclust3.png")
df = p$x

d <- as.dist(euclid(df,df))
hc1 <- hclust(d)
#plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 3, border = 2:3)
# sub_grp <- cutree(hc1, k = 3)
plot(df,col = sub_grp,main = "hclust")

# evaluation
adjustedRandIndex(p$classes,sub_grp)
purity(p$classes,sub_grp)$pur

d <- as.dist(kern_SMdepth(df,df))
hc1 <- hclust(1-d)
# plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 3, border = 2:3)
sub_grp <- cutree(hc1, k = 3)
plot(df,col =sub_grp,main = "hclust with simple depth")

# evaluation
adjustedRandIndex(p$classes,sub_grp)
purity(p$classes,sub_grp)$pur


## self defined hclust with depth function
d <- as.dist(kern_Mdepth(df,df))
hc1 <- hclust(1-d)
#plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 3, border = 2:3)
# sub_grp <- cutree(hc1, k = 3)
plot(df,col =  sub_grp,main = "hclust with depth")

# evaluation
adjustedRandIndex(p$classes,sub_grp)
purity(p$classes,sub_grp)$pur


#####################################################
#png("D:/master preparation/MasterThesis/figs/datasets/hclust/hclust4.png")
par(mfrow=c(1,3))
p<-mlbench.spirals(300)
df = p$x

d <- as.dist(euclid(df,df))
hc1 <- hclust(d)
# plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 2, border = 2:3)
sub_grp <- cutree(hc1, k = 2)
plot(df,col = sub_grp,main = "hclust")

# evaluation
adjustedRandIndex(p$classes,sub_grp)
purity(p$classes,sub_grp)$pur

d <- as.dist(kern_SMdepth(df,df))
hc1 <- hclust(1-d)
# plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 2, border = 2:3)
sub_grp <- cutree(hc1, k = 2)
plot(df,col = sub_grp,main = "hclust with simple depth")

# evaluation
adjustedRandIndex(p$classes,sub_grp)
purity(p$classes,sub_grp)$pur


## self defined hclust with depth function
d <- as.dist(kern_Mdepth(df,df))
hc1 <- hclust(1-d)
# plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 2, border = 2:3)
sub_grp <- cutree(hc1, k = 2)
plot(df,col = sub_grp,main = "hclust with depth")

# evaluation
adjustedRandIndex(p$classes,sub_grp)
purity(p$classes,sub_grp)$pur


#####################################################
par(mfrow=c(1,3))
p<-mlbench.spirals(300,1.5,0.05)
df = p$x

d <- as.dist(euclid(df,df))
hc1 <- hclust(d)
# plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 2, border = 2:3)
sub_grp <- cutree(hc1, k = 2)
plot(df,col = sub_grp,main = "hclust")

# evaluation
adjustedRandIndex(p$classes,sub_grp)
purity(p$classes,sub_grp)$pur

d <- as.dist(kern_SMdepth(df,df))
hc1 <- hclust(1-d)
# plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 2, border = 2:3)
sub_grp <- cutree(hc1, k = 2)
plot(df,col = sub_grp,main = "hclust with simple depth")

# evaluation
adjustedRandIndex(p$classes,sub_grp)
purity(p$classes,sub_grp)$pur


## self defined hclust with depth function
d <- as.dist(kern_Mdepth(df,df))
hc1 <- hclust(1-d)
# plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 2, border = 2:3)
sub_grp <- cutree(hc1, k = 2)
plot(df,col = sub_grp,main = "hclust with depth")

# evaluation
adjustedRandIndex(p$classes,sub_grp)
purity(p$classes,sub_grp)$pur


#####################################################
par(mfrow=c(1,3))
p<-mlbench.shapes()
#plot(p)
df = p$x
d <- as.dist(euclid(df,df))
hc1 <- hclust(d)
# plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 4, border = 2:3)
sub_grp <- cutree(hc1, k = 4)
plot(df,col = sub_grp,main = "hclust")

# evaluation
adjustedRandIndex(p$classes,sub_grp)
purity(p$classes,sub_grp)$pur

d <- as.dist(kern_SMdepth(df,df))
hc1 <- hclust(1-d)
# plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 4, border = 2:3)
sub_grp <- cutree(hc1, k = 4)
plot(df,col = sub_grp,main = "hclust with simple depth")

# evaluation
adjustedRandIndex(p$classes,sub_grp)
purity(p$classes,sub_grp)$pur


## self defined hclust with depth function
d <- as.dist(kern_Mdepth(df,df))
hc1 <- hclust(1-d)
# plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 4, border = 2:3)
sub_grp <- cutree(hc1, k = 4)
plot(df,col = sub_grp,main = "hclust with depth")

# evaluation
adjustedRandIndex(p$classes,sub_grp)
purity(p$classes,sub_grp)$pur
#####################################################
par(mfrow=c(1,3))
p<-mlbench.smiley()
#plot(p)
df = p$x
d <- as.dist(euclid(df,df))
hc1 <- hclust(d)
# plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 4, border = 2:3)
sub_grp <- cutree(hc1, k = 4)
plot(df,col = sub_grp,main = "hclust")

# evaluation
adjustedRandIndex(p$classes,sub_grp)
purity(p$classes,sub_grp)$pur

d <- as.dist(kern_SMdepth(df,df))
hc1 <- hclust(1-d)
# plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 4, border = 2:3)
sub_grp <- cutree(hc1, k = 4)
plot(df,col = sub_grp,main = "hclust with simple depth")

# evaluation
adjustedRandIndex(p$classes,sub_grp)
purity(p$classes,sub_grp)$pur


## self defined hclust with depth function
d <- as.dist(kern_Mdepth(df,df))
hc1 <- hclust(1-d)
# plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 4, border = 2:3)
sub_grp <- cutree(hc1, k = 4)
plot(df,col =  sub_grp,main = "hclust with depth")

# evaluation
adjustedRandIndex(p$classes,sub_grp)
purity(p$classes,sub_grp)$pur

#####################################################
par(mfrow=c(1,3))
p<-mlbench.twonorm(1000, d=2)
#plot(p)
df = p$x

d <- as.dist(euclid(df,df))
hc1 <- hclust(d)
# plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 2, border = 2:3)
sub_grp <- cutree(hc1, k = 2)
plot(df,col = sub_grp,main = "hclust")

# evaluation
adjustedRandIndex(p$classes,sub_grp)
purity(p$classes,sub_grp)$pur

d <- as.dist(kern_SMdepth(df,df))
hc1 <- hclust(1-d)
# plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 2, border = 2:3)
sub_grp <- cutree(hc1, k = 2)
plot(df,col = sub_grp,main = "hclust with simple depth")

# evaluation
adjustedRandIndex(p$classes,sub_grp)
purity(p$classes,sub_grp)$pur


## self defined hclust with depth function
d <- as.dist(kern_Mdepth(df,df))
hc1 <- hclust(1-d)
# plot(hc1, cex = 0.6, hang = -1)
# rect.hclust(hc1, k = 2, border = 2:3)
sub_grp <- cutree(hc1, k = 2)
plot(df,col = sub_grp,main = "hclust with depth")

# evaluation
adjustedRandIndex(p$classes,sub_grp)
purity(p$classes,sub_grp)$pur