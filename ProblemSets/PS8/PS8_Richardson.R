#4
set.seed(100)
N<-100000
K<-10
sigma<-0.5
X<-matrix(rnorm(N*K,mean = 0, sd=1),N,K)
X[,1] <- 1 
eps <- rnorm(N,mean=0,sd=0.5)
#5
betaTrue <- c(1.5, ???1, ???0.25, 0.75, 3.5, ???2, 0.5, 1, 1.25, 2)
y<-X%*%betaTrue + eps
beta.hat.matrix <-solve(t(X)%*%X)%*%(t(X)%*%y)
#step size
alpha <- 0.0000003

#Gradient function
gradient <- function(beta,y,X) {
  return ( as.vector(-2*t(X)%*%(y-X%*%beta)) )
}
#6
#gradient descent method 
iter  <- 1
beta0 <- 0*beta.hat.matrix
beta <- runif(dim(X)[2])
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,y,X)
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}
beta.hat.gd <-beta

#7
library(nloptr)
## Our objective function
objfun <- function(beta,y,X) {
  return (sum((y-X%*%beta)^2))
  # equivalently, if we want to use matrix algebra:
  # return ( crossprod(y-X%*%beta) )
}


## initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LFBGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

## Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)
beta.hat.LBFGS <-result$solution

## Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)

## Optimize!
result <- nloptr( x0=theta0,eval_f=objfun,opts=options,y=y,X=X)
beta.hat.NM  <- result$solution[1:(length(result$solution)-1)]

cbind(beta.hat.matrix,beta.hat.LBFGS, beta.hat.gd)

#8
## Our objective function
objfun  <- function(theta,y,X) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) ) 
  return (loglike)
}

gradient <- function (theta ,y,X) {
  grad <- as.vector (rep (0,length (theta )))
  beta <- theta [1:(length (theta) -1)]
  sig <- theta [length (theta)]
  grad [1:(length(theta) -1)] <- -t(X)%*%(y - X%*%beta )/(sig ^2)
  grad[ length (theta)] <- dim (X)[1] /sig - crossprod (y-X%*%beta )/(sig^3)
  return (grad)
}

beta0 <- runif(dim(X)[2]+1)

## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LFBGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

## Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)
beta.hat.mle.LBFGS <-result$solution

cbind(beta.hat.matrix,beta.hat.LBFGS, beta.hat.gd,beta.hat.mle.LBFGS)

#9
easyOLS<-lm(y~X-1)
easyOLS

library(stargazer)
stargazer(easyOLS)
