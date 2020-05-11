##########################################################
data = read.csv("dat_count3.csv", sep=";")

# Fit a general linera mixed model based on Poisson distribution
library(glmmTMB)
fit_poi = glmmTMB(clo ~ sex + (1|subjId), family=poisson, data=data)
summary(fit_poi)

##########################################################
## Implementation of a mixed effect model
dat <- data.frame(subjId=data$subjId, clo=data$clo, logobs=log(data$nobs))
X <- matrix(0,ncol=2,nrow=dim(dat)[1]);X
X[ ,1]<-1;X
X[ ,2]<-dat$subjId;X

library(numDeriv)

# Negative log-likelihood function
nll_ga <- function(u, beta, X, k){
  eta <- X%*%beta + u
  # nll if fixed and random effects
  lln_fix <- -sum(dpois(dat$clo,lambda = exp(eta),log=TRUE))
  lln_ran <- sum(dgamma(u, shape = k, scale = 1/k ,log=TRUE))
  #
  return(lln_fix + lln_ran)
}

# Laplace approximation
nll_LA_ga <- function(theta, X){
  beta <- theta[1:dim(X)[2]]
  k <- exp(theta[dim(X)[2]+1])
  est <- nlminb(rep(0,length(dat$clo)), objective = nll_ga, beta=beta, k=k, X=X, lower = 0.00000001)
  u <- est$par
  l.u <- est$objective
  H <- hessian(func = nll_ga, x = u, beta = beta, k = k, X=X)
  # return log-likelihood
  return(l.u + 0.5 * log(det(H/(2*pi))))
}
rn = rnorm(3)

fit_LA_ga <- nlminb(rn, nll_LA_ga, X=model.matrix(fit_poi))
coef_Q4 <- round(c(fit_LA_ga$par[-3], exp(fit_LA_ga$par[3])), 4)
c(coef_Q4, fit_poi$fit$par)