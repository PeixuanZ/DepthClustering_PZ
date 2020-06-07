rm(list = ls())
library(mlbench)
library(pracma)
library(robustbase)
set.seed(111)
obj <- mlbench.spirals(300)
my.data <-  obj$x
plot(my.data)


c1 = read.csv("~/Downloads/circle.csv")
c1 = data.frame(c1$X0,c1$X1)
plot(c1)
##########################################
## self defined spectral clustering



############################################
## Similarity
similarity <- function(x1, x2, arg,Sigma = 0) {
  if (arg == "RBF"){
    alpha = 1
    s = exp(- alpha * norm(as.matrix(x1-x2), type="F"))
  }
  else if(arg == "MahalanobisDepth"){
    s = 1/(1+(x1-x2)%*%solve(Sigma)%*%(x1-x2))
  }
  return(s)
}

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
    Sigma = covMcd(X)$cov
    for(i in 1:N) {
      for(j in 1:N) {
        S[i,j] <- similarity(X[i,], X[j,],arg,Sigma)
      }
    }
  }
  
  S
}


#####################################################
kern_KNN <- function(X,arg,n.neighboors = 3){
  S <- make.similarity(X,arg)
  
  
  N <- length(S[,1])
  
  if (n.neighboors >= N) {  # fully connected
    A <- S
  } else {
    A <- matrix(rep(0,N^2), ncol=N)
    for(i in 1:N) { # for each line
      # only connect to those points with larger similarity 
      best.similarities <- sort(S[i,], decreasing=TRUE)[1:n.neighboors]
      for (s in best.similarities) {
        j <- which(S[i,] == s)
        A[i,j] <- S[i,j]
        A[j,i] <- S[i,j] # to make an undirected graph, ie, the matrix becomes symmetric
      }
    }
  }
  return(A)
}

kern_RBF <- function(X,arg =0,alpha=0.002){
  # alpha based on CV results
  N = nrow(X)
  RBF_mat = matrix(NA,nrow = N,ncol = N)
  for (i in 1:N){
    for (j in 1:N){
      RBF_mat[i,j] <- exp(-norm(as.matrix(X[i,]-X[j,]), type="F")/(2*alpha))
      }
  }

  return(RBF_mat)
}


####
## data depth based kernel
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

##
# define data-depth based kernel
############################################################                       
graph_laplacian <- function(W, normalized)
{
  
  ## Simple Laplacian: I-D^(-1)W
  ## Normalized Laplacian D^(-1/2)UD^(-1/2)
  ## Generalized Laplacian D^(-1)U
  stopifnot(nrow(W) == ncol(W)) 
  
  "%^%" <- function(M, power)
    with(eigen(M), vectors %*% (values^power * solve(vectors)))
  
  
  D <- diag(apply(W,1,sum))
  # unnormalized graph Laplacian U = D-W
  U <- D-W
  if (normalized == "False"){
    L = U
  }
  else if (normalized=="Simple"){
    # simple normalized
    L = diag(nrow(W)) - (D %^% (-1))%*% W 
  }
  else if (normalized=="Normalized"){
    
    
    L <- (D %^% (-1/2)) %*% U %*% (D %^% (-1/2))  # normalized Laplacian
  }
  else if (normalized=="Generalized"){
    L <-  (D %^% (-1))%*% U 
  }
  return(L)
}  

############################################################ 
spectral_clustering <- function(X, n_eig,arg,kernel,normalized = "False",cut="False")
{
  X = as.matrix(X)
  W = kernel(X,arg)
  #W = forceAndCall(1,kernel,X,arg) # 1. weights matrix 
  L = graph_laplacian(W,normalized) # 2. compute graph laplacian
  
  ei = eigen(L, symmetric = TRUE) # 3. Compute the eigenvectors and values of L
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


########
## test using circle data set, kernel = "RBF", cut = "Ratio", 

X = spectral_clustering(my.data,2,arg =0,kernel = kern_MahalanobisDepth,cut = "Ncut",normalized = "Normalized")

plot(my.data,col = X$cluster)



##############################################################
## simulations

sc <- spectral_clustering(my.data,2,arg = "MahalanobisDepth",kernel = kern_KNN,normalized = "False")
plot(my.data,col = sc$cluster)

sc <- spectral_clustering(my.data,2,arg = "MahalanobisDepth",kernel = kern_KNN,normalized = "Generalized")
plot(my.data,col = sc$cluster)

sc <- spectral_clustering(my.data,2,arg = "RBF",kernel = kern_KNN,normalized = "Normalized")

plot(p1$x,col = sc$cluster)


library(kernlab)
sc1 <- specc(p1$x,kernel = "rbfdot",kpar="automatic",
             centers = 3)
plot(p1$x,col = sc1,pch = 5)

#################################################################
## More mlbench datasets
## cassini
p1 <- mlbench.cassini(5000)
plot(p1)

sc <- spectral_clustering(p1$x,3,arg = "RBF",kernel = kern_KNN,normalized = "False")
plot(p1$x,col = sc$cluster)

sc <- spectral_clustering(p1$x,3,arg = "RBF",kernel = kern_KNN,cut = "Ratio")
plot(p1$x,col = sc$cluster)

## circle
p2<-mlbench.circle(300,2)
plot(p2)

sc <- spectral_clustering(p2$x,2,arg = "RBF",kernel = kern_KNN,normalized = "False")
plot(p2$x,col = sc$cluster)

## Shape 
p3<-mlbench.shapes()
plot(p3)

sc <- spectral_clustering(p3$x,2,arg = "RBF",kernel = kern_KNN,normalized = "False")
plot(p3$x,col = sc$cluster)

## Smile
p4 <- mlbench.smile()
plot(p4)
