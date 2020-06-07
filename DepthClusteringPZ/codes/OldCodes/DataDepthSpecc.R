rm(list = ls())
library(mlbench)

set.seed(11)
obj <- mlbench.spirals(100,1,0.025)
my.data <-  4 * obj$x
plot(my.data)

library(kernlab)
sc <- specc(my.data, centers=2)
plot(my.data, col=sc, pch=4)            # estimated classes (x)

sc1 <- specc(my.data,kernel = "laplacedot",kpar="automatic",
             centers = 2)
plot(my.data,col = sc1,pch = 5)

##########################################
## self defined spectral clustering

kern_KNN <- function(X,
                     nn = 10){
  # mutual knn kernal
  # nn: the k nearest neighbors to consider
  D <- as.matrix( dist(X) ) # matrix of euclidean distances between data points in X
  
  # intialize the knn matrix
  knn_mat <- matrix(NA,
                    nrow = nrow(X),
                    ncol = nrow(X))
  
  # find the 10 nearest neighbors for each point
  for (i in 1: nrow(X)) {
    neighbor_index <- order(D[i,])[2:(nn + 1)]
    knn_mat[i,][neighbor_index] <- 1 
  }
  
  # Now we note that i,j are neighbors iff K[i,j] = 1 or K[j,i] = 1 
  knn_mat <- knn_mat + t(knn_mat) # find mutual knn
  
  knn_mat[ knn_mat == 2 ] = 1
  
  return(knn_mat)
}

kern_RBF <- function(X,alpha=1){
  #D <- as.matrix( dist(X) ) # matrix of euclidean distances between data points in X
  
  # intialize the RBF matrix
  RBF_mat <- matrix(NA,
                    nrow = nrow(X),
                    ncol = nrow(X))
  
  for (i in 1: nrow(X)) {
    for (j in 1:nrow(X)){
      RBF_mat[i,j] = exp(-alpha*norm(as.matrix(X[i,]-X[j,]),type = "F"))
    }
  }
  
  return(RBF_mat)
}


####
## data depth based kernel
kern_MahalanobisDepth <- function(X){
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

W1 = kern_MahalanobisDepth(my.data)
W2 = kern_RBF(my.data)
##
# define data-depth based kernel
############################################################                       
graph_laplacian <- function(W, normalized = TRUE)
{
  stopifnot(nrow(W) == ncol(W)) 
  
  g = colSums(W) # degrees of vertices
  n = nrow(W)
  
  if(normalized)
  {
    D_half = diag(1 / sqrt(g) )
    return( diag(n) - D_half %*% W %*% D_half )
  }
  else
  {
    return( diag(g) - W )
  }
}  

############################################################ 
spectral_clustering <- function(X, # matrix of data points
                                n_eig,kernel)
{
  
  W = forceAndCall(1,kernel,X) # 1. matrix of similarities 
  L = graph_laplacian(W) # 2. compute graph laplacian
  ei = eigen(L, symmetric = TRUE) # 3. Compute the eigenvectors and values of L
  n = nrow(L)
  res = ei$vectors[,(n - n_eig):(n - 1)]
  
  return(kmeans(res,n_eig)) # return the eigenvectors of the n_eig smallest eigenvalues
}
############################################################ 
# example
# do spectral clustering procedure

X_sc <- spectral_clustering(my.data,n_eig =2,kernel = kern_RBF)

plot(my.data,col = X_sc$cluster,pch = 5)
