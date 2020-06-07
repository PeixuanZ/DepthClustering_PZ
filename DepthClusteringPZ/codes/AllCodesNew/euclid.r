euclid<- function(points1, points2,method = "euclidean") {
  #Use dist in the base package
  
  distanceMatrix <- matrix(NA, nrow=dim(points1)[1], ncol=dim(points2)[1])
  for(i in 1:nrow(points2)) {
    for(j in 1:nrow(points1)){
      distanceMatrix[j,i] <- dist(rbind(points1[j,],points2[i,]))
    }
  }
  return(distanceMatrix)
}