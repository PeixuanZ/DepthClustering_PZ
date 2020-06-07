#####################################
## depth based kernel
library(matlib)
library(robustbase)
library(ddalpha)
kern_Mdepth = function(points1,points2=NA){
  points1 = as.matrix(points1)
  if(length(points2)==1){
    ## compute the relative distance 
    X = points1
    MaD_mat <- matrix(NA, nrow = nrow(X),ncol = nrow(X))
    Sigma = cov(points1)
    for (i in 1:nrow(X)){
      for(j in 1:nrow(X)){
        #Sigma = cov(rbind(X[i,]-X[j,]))
        MaD_mat[i,j] = 1/(1+(X[i,]-X[j,])%*%solve(Sigma)%*%(X[i,]-X[j,]))
      }
    }
  }
  else{
    points2 = as.matrix(points2)
    MaD_mat <- matrix(NA, nrow=dim(points1)[1], ncol=dim(points2)[1])
    Sigma = cov(points1)
    
    for (i in 1:dim(points1)[1]){
      for(j in 1:dim(points2)[1]){
        #Sigma = cov(rbind(points1[i,], points2[j,]))
        MaD_mat[i,j] = 1/(1+(points1[i,]-points2[j,])%*%solve(Sigma)%*%(points1[i,]-points2[j,]))
      }
    }
    
  }
  return(MaD_mat)
}