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