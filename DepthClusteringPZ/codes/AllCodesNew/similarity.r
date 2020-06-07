similarity <- function(x1, x2, arg,Sigma = 0) {
  if (arg == "RBF"){
    alpha = 1
    s = exp(- alpha * norm(as.matrix(x1-x2), type="F"))
  }
  else if(arg == "MahalanobisDepth"){
    s = 1/(1+(x1-x2)%*%solve(Sigma)%*%(x1-x2))
  }
  else if(arg == "SimpleDepth"){
    s = 1/(1+(x1-x2)%*%(x1-x2))
  }
  return(s)
}