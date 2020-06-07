###################################
## K Means
K_means <- function(x, distFun, finish="False",nItter = 100, k,arg = "mean") {
  centers = x[sample(nrow(x),k),]
  clusterHistory <- vector(nItter, mode="list")
  centerHistory <- vector(nItter, mode="list")
  centerHistory[[1]] <- centers

    for(i in 2:nItter){

      distsToCenters <- distFun(x, centers)
      if(identical(distFun,kern_Mdepth)|identical(distFun,kern_SMdepth)){
        clusters <- apply(distsToCenters, 1, which.max)
      }
      else{
        clusters <- apply(distsToCenters, 1, which.min)
      }
      
      if (arg=="mean"){
        centers <- apply(x, 2, tapply, clusters, mean)
      }
      else{
        centers <- apply(x, 2, tapply, clusters, median)
      }
      
      # Saving history
      clusterHistory[[i]] <- clusters
      centerHistory[[i]] <- centers
      if(identical(centerHistory[[i]],centerHistory[[i-1]])) break
    
  }
  
  list(clusters=clusterHistory[[i]], centers=centerHistory[[i]])
}