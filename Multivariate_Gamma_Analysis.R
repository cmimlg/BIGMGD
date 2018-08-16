library(Matrix)
source("mode_inv_mult_gamma.R")

n1<-50
n2<-50

P1<-EuStockMarkets[1:n1,]
P2<-EuStockMarkets[(n1+1):(n1+n2),]

## compute log-return 
r1<-diff(log(P1))*100
r2<-diff(log(P2))*100

apply(r1,2,sd)
apply(r2,2,sd)

S1<-t(r1)%*%r1
S2<-t(r2)%*%r2


## MAP Estimate for Sigma 
## Consider S2 as current data
## Suppose r2 ~ N(0,Sigma)
## S2 = r2'r2
## S2 ~ MG(alpha=n2/2,beta=2,Sigma) - Sigma unknown
##
## Note: This particular choice of alpha and beta yields Wishart distribution.
##       However, we should play around with alpha and beta and check if other
##       choices are more suitable in some sense!
##
## Consider S1 as historical data and will use to build prior 
##
## Sigma ~ InvMG(a = 1, beta =2 , Psi = diag(S1))
##
## Posterior:
## Sigma | S2 ~ InbMG(alpha+a = n1/2+1, beta=2, S2 +Psi )


## One particular choice of Psi using S1
a<-1
Psi<-mode_inv_mult_gamma(alpha=n1/2,beta=2,Sigma = S1)

## MAP of Sigma
mode_inv_mult_gamma(alpha=(n2/2+1),beta=2,Sigma = S2+Psi)
## Sample covariance estimate of Sigma
cov(r2)


## Let's try non-informative improper prior (NIP)
## with a=0
## Psi=0

## MAP of Sigma with NIP
mode_inv_mult_gamma(alpha=(n2/2),beta=2,Sigma = S2)
## Sample covariance estimate of Sigma
cov(r2)
