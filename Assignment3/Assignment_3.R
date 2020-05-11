##################################################################
## Assignment 3
##################################################################

## Import data
setwd("C:/Users/Bjorn/OneDrive/Dokument/University/DTU/02424 Advanced Dataanalysis and Statistical Modelling/labs/02424-Advanced-Dataanalysis-and-Statistical-Modelling/Assignment3")
data <- read.table("concrets.csv", sep = "",  header = TRUE)
dim(data)
head(data)
data

## Problem 1A - Done

## Problem 1B
library(reshape)
library(lme4)
data1 <- melt(data[, c("date", "y7", "y28", "batch", "air_temp")], measure.vars = c("y7", "y28"))
head(data1)
str(data1)

data2 <- within(data1, {
  y7 <- as.integer(variable == "y7")
  y28 <- as.integer(variable == "y28")
})
str(data2)

model <- lmer(value ~ 0 + y7 + y7:air_temp + y28 + y28:air_temp + (1 | batch), data = data2)
summary(model)
plot(model)
confint(model)

diagnos <- data.frame(Resid = resid(model, type = "pearson"), Fitted = fitted(model), variable = data2$air_temp)
plot(diagnos)

## Problem 2A - Done

## Problem 2B
library(glmmTMB)
#setwd("~/Desktop/DTU/Advanced\ Dataanalysis\ and\ Statistical\ Modelling/Assignments/Assignment\ 3")
setwd("C:/Users/Bjorn/OneDrive/Dokument/University/DTU/02424 Advanced Dataanalysis and Statistical Modelling/labs/02424-Advanced-Dataanalysis-and-Statistical-Modelling/Assignment3")
data <- read.table("dat_count3.csv", sep = ";",  header = TRUE)
dim(data)
head(data)
plot(clo~log(nobs),data=data,col=subjId,pch=19)

fit_TMB <- glmmTMB(formula = clo ~ log(nobs)  + (1|subjId), family=poisson(link = "log"), data=data) # Better option to use the previous "fit_poi" model.
fit_TMB$fit$par
summary(fit_TMB)
fit_poi = glmmTMB(clo ~ sex + (1|subjId), family=poisson, data=data)

1 - pchisq(270.4, 133)

## Implementation of a mixed effect model
dat <- data.frame(subjId=data$subjId, clo=data$clo, logobs=log(data$nobs), sex=data$sex)
X <- matrix(0,ncol=2,nrow=dim(dat)[1]);X
X[ ,1]<-1;X
X[ ,2]<-dat$sex;X 

library(numDeriv)

# Negative log-likelihood function
nll_ga <- function(u, beta, X, k){
  eta <- X%*%beta 
  # nll if fixed and random effects
  lln_fix <- -sum(dpois(dat$clo,lambda = exp(eta)*u[dat$subjId], log=TRUE))
  lln_ran <- -sum(dgamma(u, shape = k, scale = 1/k , log=TRUE))
  #
  return(lln_fix + lln_ran)
}
# Laplace approximation
nll_LA_ga <- function(theta, X){
  beta <- theta[1:dim(X)[2]]
  k <- exp(theta[dim(X)[2]+1])
  est <- nlminb(rep(1, length(unique(dat$subjId))), objective = nll_ga, beta=beta, k=k, X=X, lower = 0.00000001)
  u <- est$par
  l.u <- est$objective
  H <- hessian(func = nll_ga, x = u, beta = beta, k = k, X=X)
  # return log-likelihood
  return(l.u + 0.5 * log(det(H/(2*pi))))
}

fit_LA_ga <- nlminb(start_val, nll_LA_ga, X=model.matrix(fit_TMB))
coef_Q4 <- round(c(fit_LA_ga$par[-3], exp(fit_LA_ga$par[3])), 4)
c(coef_Q4, fit_TMB$fit$par)

# improve by using independence of u's in nlminb
# Laplace approximation
nll_LA_ga <- function(theta, X){
  beta <- theta[1:dim(X)[2]]
  #beta <- theta[3]
  k <- exp(theta[dim(X)[2]+1])
  u <- numeric(length(unique(dat$subjId)))
  
  for(i in 1:length(u)){
    u[i] <- nlminb(1, objective=nll_ga, beta=beta, k=k, X=X, lower=0.00000001)$par
  }
  l.u <- nll_ga(u, beta, X, k)
  H <- numeric(length(u))
  
  for(i in 1:length(u)){
    H[i] <- hessian(func = nll_ga, x = u[i], u=u, beta = beta, X=X)
  }
return(l.u + 0.5 * log(prod(H/(2*pi))))
}

start_val = c(-3.8072716, 1.8390356, -0.1675707 ) 
#nll_LA_ga(start_val, X=model.matrix(fit_TMB)) # for the optimization to work the returned hessian must be positive definite, which means if this does not give back a value there is something wrong with nll_LA_ga.
fit_LA_ga <- nlminb(start_val, nll_LA_ga, X=model.matrix(fit_TMB))
coef_Q4 <- round(c(fit_LA_ga$par[-3], exp(fit_LA_ga$par[3])), 4)
c(coef_Q4, fit_TMB$fit$par)

fit_LA_ga$iterations
-fit_LA_ga$objective
logLik(fit_TMB)
1-pchisq(2*(-fit_LA_ga$objective-logLik(fit_TMB)),df=1)
## Hence a significant improvement

# Importance sampling
nll_Laplace_simulate_ga <- function(theta, X, n, seed){
  set.seed(seed)
  beta <- theta[1:dim(X)[2]]
  #beta <- theta[-3]
  k <- exp(theta[dim(X)[2]+1])
  #k <- exp(theta[3]) # distribution of the U
  est <- nlminb(rep(1,length(unique(dat$subjId))), objective = nll_ga, beta=beta, k=k, X=X, lower = 0.00000001)
  u <- est$par
  l.u <- est$objective
  H <- diag(hessian(nll_ga,x=u, beta = beta, X=X, k=k))
  s <- sqrt(1/H)
  # do simulations
  L <- sapply(1:n, function(i) {
    u.sim <- rnorm(length(u), mean=u, sd=s)  
    # return log-likelihood
    return(exp(-nll_ga(u=u.sim,beta=beta,k=k,X=X))/prod(dnorm(u.sim,mean=u,sd=s)))
  })
  # return average negative log-likelihood
  return(list("nnl" = -log(mean(L)),"est"=est))
}
L_ga <- nll_Laplace_simulate_ga(fit_LA_ga$par, X=model.matrix(fit_TMB), n=k, seed=22)
L_ga$est$objective
L_ga$nnl


# Importance sampling using independent u's
nll_Laplace_simulate_ga <- function(theta, X, n, seed){
  set.seed(seed)
  beta <- theta[1:dim(X)[2]]
  #beta <- theta[-3]
  k <- exp(theta[dim(X)[2]+1])
  #beta <- theta[3]
  u <- numeric(length(unique(dat$subjId)))
  #k <- exp(theta[-3])
  #u <- est$par
  #l.u <- est$objective
  #est <- nlminb(rep(1,length(unique(dat$subjId))), objective = nll_ga, beta=beta, k=k, X=X, lower = 0.00000001)
  ## Use grouping structure
  for(i in 1:length(u)){
    u[i] <- nlminb(0, objective=nll_ga, beta=beta, k=k, X=X, lower=0.00000001)$par
  }
  #H <- hessian(func = nll_ga, x = u, beta = beta, k = k, X=X)
  # return log-likelihood
  # return(l.u + 0.5 * log(det(H/(2*pi))))
  #return(list("nnl" = -log(L),"est"=est))
  l.u <- nll_ga(u, beta, X, k)
  H <- numeric(length(u))
  for(i in 1:length(u)){
    H[i] <- hessian(func = nll_ga, x = u[i], u=u, beta = beta, X=X)
  }
  return(l.u + 0.5 * log(prod(H/(2*pi))))
  #k <- exp(theta[3]) # distribution of the U
  #est <- nlminb(rep(1,length(unique(dat$subjId))), objective = nll_ga, beta=beta, k=k, X=X, lower = 0.00000001)
  #u <- est$par
  u <- numeric(length(unique(dat$subjId)))
  ## Use grouping structure
  for(i in 1:length(u)){
    u[i] <- nlminb(1, objective=nll_ga, u=u,beta=beta,  beta=beta, k=k, X=X, lower = 0.00000001)$par
  }
  #H <- diag(hessian(nll_ga,x=u, beta = beta, X=X, k=k))
  s <- sqrt(1/H)
  # do simulations
  L <- sapply(1:n, function(i) {
    u.sim <- rnorm(length(u$par), mean=u, sd=s)  
    # return log-likelihood
    return(exp(-nll_ga(u=u.sim,beta=beta,k=k,X=X))/prod(dnorm(u.sim,mean=u,sd=s)))
  })
  # return average negative log-likelihood
  return(list("nnl" = -log(mean(L)),"est"=u))
}
L_ga <- nll_Laplace_simulate_ga(fit_LA_ga$par, X=model.matrix(fit_TMB), n=k, seed=22)
L_ga$est$objective
L_ga$nnl
