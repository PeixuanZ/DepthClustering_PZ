#####################################
## simple depth based kernel
library(robustbase)
library(ddalpha)
kern_SMdepth = function(points1,points2=NA){
  points1 = as.matrix(points1)
  if(length(points2)==1){
    ## compute the relative distance 
    X = points1
    MaD_mat <- matrix(NA, nrow = nrow(X),ncol = nrow(X))
    #Sigma = covMcd(X)$cov
    for (i in 1:nrow(X)){
      for(j in 1:nrow(X)){
        MaD_mat[i,j] = 1/(1+(X[i,]-X[j,])%*%(X[i,]-X[j,]))
      }
    }
  }
  else{
    points2 = as.matrix(points2)
    MaD_mat <- matrix(NA, nrow=dim(points1)[1], ncol=dim(points2)[1])
    #Sigma = covMcd(points1)$cov
    for (i in 1:dim(points1)[1]){
      for(j in 1:dim(points2)[1]){
        MaD_mat[i,j] = 1/(1+(points1[i,]-points2[j,])%*%(points1[i,]-points2[j,]))
      }
    }
    
  }
  return(MaD_mat)
}