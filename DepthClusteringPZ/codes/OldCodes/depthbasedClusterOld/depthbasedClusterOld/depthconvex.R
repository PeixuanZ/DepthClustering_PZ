data(iris)
library(ddalpha)
iris2 = iris[,1:4]
class = as.numeric(iris[,5])
depth_mat = RMD(iris2)
#ind = c(1:2,51:52,101:102)
trm = TRM(depth_mat)
x = trm$I
y = trm$J
z = trm$TRM
library(scatterplot3d) 
scatterplot3d(x,y,z, pch=16, highlight.3d=TRUE,
              type="h", main="3D Scatterplot")

library(rgl)

plot3d(id1, id2, trm, col="red", size=3)

library(Rcmdr)
library("plot3Drgl")
#hist3D(id1,id2,trm)
heatmap(depth_mat)
contour(depth_mat)

# contour plot
library("plotly")
p <- plot_ly(data = df, x=~x,y=~y, z=~z, type = "contour", colorscale='Jet')
p

# new contour function based on depth_mat
z = matrix(depth_mat,ncol = 1)
xgrid <-  1:150
ygrid <-  1:150
# Generate a dataframe with every possible combination of wt and hp
data.fit <-  expand.grid(x = xgrid, y = ygrid)
meltdata = data.frame(data.fit,z)
plot_ly(meltdata, x = ~x, y = ~y, z = ~z, type = "contour", 
        width = 600, height = 500)
