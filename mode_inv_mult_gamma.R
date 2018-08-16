mode_inv_mult_gamma<-function(alpha,beta=2,Sigma){
  library(Matrix)
  
  ## Check the dimension of Sigma
  nr<-dim(Sigma)[1]
  nc<-dim(Sigma)[2]
  if(nr==nc){p<-nr}else{return("Dimension mismatch for Sigma")}
  
  ## Check if Sigma is of less than full rank matrix
  r<-rankMatrix(Sigma)
  if(r<p)return("Rank of Sigma is less than full")
  
  ## Check if beta is positive
  
  if(beta<=0) return("Provide positive value for beta")
  
  ## Check if alpha is greater than (p-1)/2
  if(alpha<((p-1)/2))return("Value of alpha is less than (p-1)/2; hence making it degenrate.")
  
  mode=Sigma/(beta*(alpha+0.5*(p+1)))
  return(mode)
  
}

### Some basic testing ###

# n<-50
# P<-EuStockMarkets[1:n,]

## compute log-return 
# r<-diff(log(P))*100

# S<-t(r)%*%r

# mode_inv_mult_gamma(alpha=n/2,beta=2,Sigma = S)
# cov(r)

# p<-length(diag(S1))

# mode_inv_mult_gamma(alpha=n1/2,beta=2,Sigma = S1)*(n1+p+1)
# S1
### Test Ends ####
