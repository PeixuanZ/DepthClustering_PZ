##
kern_MahalanobisDepth <- function(X,arg=0){
  MaD_mat <- matrix(NA,nrow = nrow(X),ncol = nrow(X))
  Mid= matrix(NA,1,1) # temporary store D(xij)
  # Compute the Minimum Covariance Determinant (MCD) estimator
  Sigma = covMcd(X)$cov
  
  for(i in 1:nrow(X)){
    for(j in 1:nrow(X)){
      MaD_mat[i,j] = 1/(1+(X[i,]-X[j,])%*%solve(Sigma)%*%(X[i,]-X[j,]))
    }
  }
  return(MaD_mat)
}