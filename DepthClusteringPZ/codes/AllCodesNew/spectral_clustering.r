spectral_clustering <- function(X, n_eig,arg,kernel,normalized = "False",cut="False")
{
  X = as.matrix(X)
  W = kernel(X,arg)
  #W = forceAndCall(1,kernel,X,arg) # 1. weights matrix 
  L = graph_laplacian(W,normalized) # 2. compute graph laplacian
  
  ei = eigen(L, symmetric = TRUE)
  # 3. Compute the eigenvectors and values of L
  n = ncol(ei$vectors)
  res = ei$vectors[,(n - n_eig+1):n]
  
  if (cut == "Ratio"|cut == "Ncut"){
    ## Ratio: L = unnomarlized
    ## Ncut: L = Normalized
    Q = res
    res = Q/repmat(sqrt((diag(t(Q)%*%Q))),n,1)
  }else{
    res = res
  }
  
  
  return(kmeans(res,centers = n_eig,nstart = 5)) # return the eigenvectors of the n_eig smallest eigenvalues
}
