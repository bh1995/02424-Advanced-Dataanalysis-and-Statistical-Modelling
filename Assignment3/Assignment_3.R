##################################################################
## Assignment 3
##################################################################

## Import data
setwd("~/Desktop/DTU/Advanced\ Dataanalysis\ and\ Statistical\ Modelling/02424-Advanced-Dataanalysis-and-Statistical-Modelling/Assignment3")
data <- read.table("concrets.csv", sep = "",  header = TRUE)
dim(data)
head(data)
data

## Problem 1A - Done

## Problem 1B
library(reshape)
library(lme4)
data1 <- melt(data[, c("date", "y7", "y28", "batch", "air.temp")], measure.vars = c("y7", "y28"))
head(data1)
str(data1)

data2 <- within(data1, {
  y7 <- as.integer(variable == "y7")
  y28 <- as.integer(variable == "y28")
})
str(data2)

model <- lmer(value ~ 0 + y7 + y7:air.temp + y28 + y28:air.temp + (1 | batch), data = data2)
summary(model)
plot(model)
confint(model)

diagnos <- data.frame(Resid = resid(model, type = "pearson"), Fitted = fitted(model), variable = data2$air.temp)
plot(diagnos)

## Problem 2A - Done

## Problem 2B - Done
library(glmmTMB)
setwd("~/Desktop/DTU/Advanced\ Dataanalysis\ and\ Statistical\ Modelling/Assignments/Assignment\ 3")
data <- read.table("dat_count3.csv", sep = ";",  header = TRUE)
dim(data)
head(data)
plot(clo~subjId,data=data,col=sex,pch=19)

##################################################
## TMB Poisson
fit_TMB <- glmmTMB(formula = clo ~ offset(log(time)) + sex  + (1|subjId), family=poisson(link = "log"), data=data)
fit_TMB$fit$par

##################################################
## Implementation of a mixed effect model
dat <- data.frame(subjId=data$subjId, clo=data$clo, sex=data$sex, time=data$time)
dat$subjId <- as.factor(dat$subjId)
X <- matrix(0,ncol=2,nrow=dim(dat)[1])
X[ ,1]<-1
X[dat$sex == 'female',2] = 0
X[dat$sex == 'male',2] = 1
X

library(numDeriv)
##################################################
# Negative log-likelihood function
nll <- function(u, beta, X, k){
  ## An offset "log(dat$time)" is added to the Poisson distribution
  eta <- X%*%beta + log(dat$time)
  U <- u[dat$subjId]
  lambda <- exp(eta)*U
  return(-sum(dpois(dat$clo, lambda = lambda, log = TRUE))-sum(dgamma(u, shape = k, scale = 1/k, log = TRUE)))
}

##################################################
## Improve by using independence of u's in nlminb
nll.LA3 <- function(theta,X){
  beta <- theta[1:dim(X)[2]]
  sigma.u <- exp(theta[dim(X)[2]+1])
  ## Do for 1 of the random effects
  fun.tmp <- function(ui,u,beta,sigma.u,X,i){
    u <- rep(1,length(u))
    u[i]<-ui
    #print(i)
    nll(u,beta,X,sigma.u)
  }
  u <- numeric(length(levels(dat$subjId))) + 1
  ## Use grouping structure (10 1D optimization)
  for(i in 1:length(u)){
      u[i] <- nlminb(1,objective = fun.tmp, u=u, beta=beta, sigma.u=sigma.u,X=X,i=i,lower = 0.00000001)$par
  }
  l.u <- nll(u,beta,X,sigma.u)
  H <- numeric(length(u))
  ## Calculate the Hessian by considering only the diagonal elements
  for(i in 1:length(u)){
      H[i] <- hessian(func = fun.tmp, x = u[i],u=u, beta = beta, sigma.u = sigma.u, X=X,i=i)
  }
  l.u + 0.5 * log(prod(H/(2*pi)))
}

system.time(fit2 <- nlminb(c(-2,-1,1), nll.LA3, X=X))
## TMB against Laplance approximation
-fit2$objective
logLik(fit_TMB)
## TMB against Laplance approximation
fit2$par
fit_TMB$fit$par

##################################################
## Importance sampling
nll_Laplace_simulate <- function(theta,X,k,seed){
  beta <- theta[1:dim(X)[2]]
  sigma.u <- exp(theta[dim(X)[2]+1])
  ## Do for 1 of the random effects
  fun.tmp <- function(ui,u,beta,sigma.u,X,i){
    u <- rep(1,length(u))
    u[i]<-ui
    print(i)
    nll(u,beta,X,sigma.u)
  }
  u <- numeric(length(levels(dat$subjId))) + 1
  ## Use grouping structure (10 1D optimization)
  for(i in 1:length(u)){
    u[i] <- nlminb(1,objective = fun.tmp, u=u, beta=beta, sigma.u=sigma.u,X=X,i=i,lower = 0.00000001)$par
  }
  est <- u
  l.u <- nll(u,beta,X,sigma.u)
  H <- numeric(length(u))
  s <- sqrt(1/H)
  ## Calculate the Hessian by considering only the diagonal elements
  for(i in 1:length(u)){
    H[i] <- hessian(func = fun.tmp, x = u[i],u=u, beta = beta, sigma.u = sigma.u, X=X,i=i)
  }
  ## Simpulations
  L <- 0
  for(i in 1:k) {
    u.sim <- rgamma(length(u), shape = sigma.u, scale = 1/sigma.u)  
    L[i] <- exp(-nll(u=u.sim,beta=beta,X=X,k=sigma.u)) / prod(dgamma(u.sim, shape = sigma.u, scale = 1/sigma.u))
  }
  # return average negative log-likelihood
  return(list("nnl" = log(mean(L)),"est"=est,"logL"=log(L)))
}

k <- 10000
L <- nll_Laplace_simulate(fit2$par,X=X,k=k,seed=22)
# Overall negative log-likelihood across methods
c(L$nnl,-fit2$objective,logLik(fit_TMB))

##################################################
## Plot L over the number of iterations
plot(c(1:k),L$logL, main = "Negative log-likelihood over simulations", 
     xlab = "Simulations", ylab = "Negative log-likelihood")
abline(h = mean(L$logL), col = "red")
abline(h = L$nnl, col = "blue")
# Add a legend
legend(-400, -173.2, legend=c("Mean of log(L)", "Log of mean(L)"),
       col=c("red", "blue"), lty=1:1, cex=0.8)



##################################################
## Problem 2C 
##################################################
# Negative log-likelihood function
nll_problemC <- function(theta, X, Y){
  beta <- theta[1:dim(X)[2]]
  sigma.u <- exp(theta[dim(X)[2]+1])
  u <- numeric(length(levels(dat$subjId))) + 1
  lambda_ij <- exp(X%*%beta)*u[dat$subjId]
  # log-likelihood for each subjId (47 in total)
  ll_single <- sapply(1:length(u), function(i) {
    part1 <- prod(lambda_ij[i]^Y[i] / prod(Y[i]))
    part2 <- (1 / (sigma.u^(1/sigma.u))) * lgamma(sum(Y[i])+1/sigma.u) / lgamma(1/sigma.u)
    part3 <- (1/(sum(lambda_ij[i])+1))^(sum(Y[i])+1/sigma.u)
    print(log(part1*part2*part3))
    return(log(part1*part2*part3))
  })
  return(-sum(ll_single))
}

fit3 <- nlminb(fit2$par, nll_problemC, X=X, Y=dat$clo)
fit3$par
fit3$iterations
fit3$objective

fit2$par
fit_TMB$fit$par






