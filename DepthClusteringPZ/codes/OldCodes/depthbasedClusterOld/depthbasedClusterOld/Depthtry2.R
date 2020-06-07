data(iris)
library(ddalpha)
set.seed(124)
s = sample(nrow(iris))
iris2 = iris[s,1:4]
class = as.numeric(iris[s,5])
source("/Volumes/PXWIN/datadepth/DepthbasedClustering/RMD.R")
source("/Volumes/PXWIN/datadepth/DepthbasedClustering/TRM.R")

depth_mat = RMD(iris2)
#ind = c(1:2,51:52,101:102)
trm = TRM(depth_mat)
x = trm$I
y = trm$J
z = trm$TRM

library(Rcmdr)
library("plot3Drgl")
#hist3D(id1,id2,trm)
heatmap(depth_mat,col = cm.colors(256),Rowv=NA, Colv=NA)
# add or not(additional)
# change the colors and white and black(grey scales)
contour(depth_mat)
#library(rpca)
# rescale
Depth2 = (depth_mat-min(depth_mat))/(max(depth_mat)-min(depth_mat))
heatmap(Depth2, col = cm.colors(256))

depth_new = rpca(depth_mat)



# new contour function based on depth_mat
z = matrix(depth_mat,ncol = 1)
xgrid <-  1:150
ygrid <-  1:150
# Generate a dataframe with every possible combination of wt and hp
data.fit <-  expand.grid(x = xgrid, y = ygrid)
meltdata = data.frame(data.fit,z)
plot_ly(meltdata, x = ~x, y = ~y, z = ~z, type = "contour", 
        width = 600, height = 500)
