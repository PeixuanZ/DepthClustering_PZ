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