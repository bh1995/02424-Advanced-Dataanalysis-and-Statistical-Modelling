##################################################################
## Assignment 3
##################################################################

## Import data
setwd("~/Desktop/DTU/Advanced\ Dataanalysis\ and\ Statistical\ Modelling/Assignments/Assignment\ 3")
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
setwd("~/Desktop/DTU/Advanced\ Dataanalysis\ and\ Statistical\ Modelling/Assignments/Assignment\ 3")
data <- read.table("dat_count3.csv", sep = ";",  header = TRUE)
dim(data)
head(data)
plot(clo~log(nobs),data=data,col=subjId,pch=19)

fit_TMB <- glmmTMB(formula = clo ~ log(nobs)  + (1|subjId), family=poisson(link = "log"), data=data)
summary(fit_TMB)

1 - pchisq(270.4, 133)

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
  lln_fix <- -sum(dpois(data$subjId,lambda = exp(eta),log=TRUE))
  lln_ran <- -sum(dgamma(u, shape = k, scale = 1/k ,log=TRUE))
  #
  return(lln_fix + lln_ran)
}

# Laplace approximation
nll_LA_ga <- function(theta, X){
  beta <- theta[1:dim(X)[2]]
  k <- exp(theta[dim(X)[2]+1])
  est <- nlminb(rep(0,length(data$subjId)), objective = nll_ga, beta=beta, k=k, X=X)
  u <- est$par
  l.u <- est$objective
  H <- hessian(func = nll_ga, x = u, beta = beta, k = k, X=X)
  # return log-likelihood
  return(l.u + 0.5 * log(det(H/(2*pi))))
}

fit_LA_ga <- nlminb(rnorm(5), nll_LA_ga, X=model.matrix(fit_TMB))
coef_Q4 <- round(c(fit_LA_ga$par[-5], exp(fit_LA_ga$par[5])), 4)

-fit_LA_ga$objective
logLik(fit_TMB)
1-pchisq(2*(-fit_LA_ga$objective-logLik(fit_TMB)),df=1)
## Hence a significant improvement


