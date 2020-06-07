# topright triangle function as a vector
library("robustbase")
TRM = function(matrix){
  n = nrow(matrix) # 1:n-1
  p = ncol(matrix) # i+1:p
  I = c()
  J = c()
  TRM = c()
  for (i in 1:(n-1)){
    for(j in (i+1):p){
      I = c(I,i)
      J = c(J,j)
      TRM = c(TRM, matrix[i,j])
    }
  }
  return(list(I = I,J = J,TRM=TRM))  
}