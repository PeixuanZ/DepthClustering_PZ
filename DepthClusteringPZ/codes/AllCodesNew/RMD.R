# robust Mahalanobis Dist D(xi xj)

RMD = function(data){
  row = dim(data)[1]
  col = dim(data)[2]
  Mat = matrix(NA,row,row)
  Mid= matrix(NA,1,1) # temporary store D(xij)
  # Compute the Minimum Covariance Determinant (MCD) estimator
  Sigma = covMcd(data)$cov
  X = as.matrix(data)
  
  for (i in 1:row)
  {
    for (j in 1:row)
    {
      Mid = 1+(X[i,]-X[j,])%*%solve(Sigma)%*%(X[i,]-X[j,])
      Mat[i,j] = solve(Mid)
    }
  }
  
  return(Mat)
}
