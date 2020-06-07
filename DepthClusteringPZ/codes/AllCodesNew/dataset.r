########################### 
## All data sets

### Kmeans
data(iris)

### wine
data("wine_1vs2")

### 
set.seed(12)
X = c(rnorm(4),rnorm(mean = 4,sd= 2,n = 7))
Y = c(rnorm(4),rnorm(mean = 4,sd= 2,n = 7))
plot(X,Y)
dat1 = data.frame(X,Y.group = c(rep(1,4),rep(2,7)))

###
library(mlbench)
p <- mlbench.cassini(5000)
plot(p)

p<-mlbench.spirals(300)
plot(p)
p<-mlbench.spirals(300,1.5,0.05)
plot(p)

p<-mlbench.shapes()
plot(p)

p<-mlbench.smiley()
plot(p)

p<-mlbench.twonorm(1000, d=2)
 plot(p)