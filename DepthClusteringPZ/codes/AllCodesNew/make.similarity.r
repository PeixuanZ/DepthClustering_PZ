make.similarity <- function(X, arg) {
  N <- nrow(X)
  S <- matrix(rep(NA,N^2), ncol=N)
  if(arg=="RBF"){
    for(i in 1:N) {
      for(j in 1:N) {
        S[i,j] <- similarity(X[i,], X[j,],arg)
      }
    }
  }
  else if(arg=="MahalanobisDepth"){
    Sigma = cov(X)
    for(i in 1:N) {
      for(j in 1:N) {
        S[i,j] <- similarity(X[i,], X[j,],arg,Sigma)
      }
    }
  }
  else if(arg == "SimpleDepth"){
    for(i in 1:N) {
      for(j in 1:N) {
        S[i,j] <- similarity(X[i,], X[j,],arg)
      }
    }
  }
  
  S
}
