

#setwd(getwd())

data = read.csv("concrets.csv", sep=" ")

head(data)
summary(data)
data2 = data[,-1]
plot(data)
boxplot(data$y7)
boxplot(data$y28)
plot(data2$y28, col="blue", ylim = c(6, 31))
points(data$y7, col="red")

library(lme4)
library(nlme)

## Part 1 problem B

# Unsure if the mixed effects should be based on batch or y28.
fit1 = lme(y7+y28~ air.temp*batch, data=data2, random = ~ 1|batch, method="ML")
anova(fit1)
summary(fit1)
fit2 = lme(y7*y28~ air.temp, data=data2, random = ~ 1|batch, method="ML")
anova(fit1,fit2)



##  Part 2: Clothing insulation count data Problem A

data = read.csv("dat_count3.csv", sep=";")

str(data)
summary(data)

boxplot(data$clo)

# Print out ammount of people that changed cloths
for(i in 0:4){
  nr = sum((data$clo == i)==TRUE)
  procent = round(nr/length(data$clo)*100, 1)
  print(paste("clo",i,":",nr,",",procent,"%"))
}
# Majority of people did not change cloths. Same ammount of people (17.6%) changed their cloths 1 and 2 times. 

boxplot(data$tInOp) # There seems to be three outlier tInOp values
plot(data$subjId, data$time) # There are some outliers in the total measurement time for certain subjects
plot(data$subjId, data$nobs)

# See differences between people that changed cloths
# People that changed 0 times
clo0 = c()
for(i in length(data$subjId)){
  for(j in which(data$clo == 0)){
    clo0 = rbind(clo0, data[j,])
  }
}
clo1 = c()
for(i in length(data$subjId)){
  for(j in which(data$clo == 1)){
    clo1 = rbind(clo1, data[j,])
  }
}
clo2 = c()
for(i in length(data$subjId)){
  for(j in which(data$clo == 2)){
    clo2 = rbind(clo2, data[j,])
  }
}
clo3 = c()
for(i in length(data$subjId)){
  for(j in which(data$clo == 3)){
    clo3 = rbind(clo3, data[j,])
  }
}
clo4 = c()
for(i in length(data$subjId)){
  for(j in which(data$clo == 4)){
    clo4 = rbind(clo4, data[j,])
  }
}

summary(clo0)
summary(clo1)
summary(clo2)
summary(clo3)
summary(clo4)
# Through comparing the levels of changed cloths, the mean tempreture varied slighlty little between clothing levels. The largest
# difference is the outddor temp. (colder) for those whome changed their cloths 3 times. 
boxplot(clo0$tOut, clo1$tOut, clo2$tOut, main="Temp. outside", col=c("#99FF66", "#66FF99", "#99CCFF"), 
        xlab="Clothing change", ylab="Temp.")
xtick=seq(0, 2, by=1)
axis(1, at=xtick, labels=F)
text(x=c(1,2,3),  par("usr")[3], 
     labels = xtick, pos = 1, xpd = T, srt=10)
#legend("top", c("0 change", "1 change", "1 change"), fill=c("#99FF66", "#66FF99", "#99CCFF"))
boxplot(clo0$tInOp, clo1$tInOp, clo2$tInOp, main="Temp. inside", col=c("#99FF66", "#66FF99", "#99CCFF"),
        xlab="Clothing change", ylab="Temp.")
xtick=seq(0, 2, by=1)
axis(1, at=xtick, labels=F)
text(x=c(1,2,3),  par("usr")[3], labels = xtick, pos = 1, xpd = T, srt=10)

## 2)
library(glmmTMB)
library(DHARMa)


# ex.
bovine <- glmmTMB(resp ~ period + (1|herd),
                   family=binomial, data=cbpp)

# Fit a general linera mixed model based on binomial distribution
data$clo = as.numeric(data$clo)
data$nobs = as.numeric(data$nobs) 
data$resp = cbind(data$clo, (data$nobs - data$clo))

fit_binom = glmmTMB(resp ~ sex + (1|subjId), family=binomial, data=data) # This produces NA on some interaction variables
summary(fit_binom)
res_binom = simulateResiduals(fit_binom)
plot(res_binom, rank = TRUE)



# Fit a general linera mixed model based on Poisson distribution
fit_poi = glmmTMB(clo ~ sex + (1|subjId), family=poisson, data=data)
summary(fit_poi)
fit_poi$fit$par # see paramters of fixed and random effects of poisson model.
res_poi = simulateResiduals(fit_poi)
plot(res_poi, asFactor = TRUE)

# Different fit of poisson model
library(glmmTMB)
#data <- read.table("dat_count3.csv", sep = ";",  header = TRUE)
#dim(data)
#head(data)
plot(clo~log(nobs),data=data,col=subjId,pch=19)

fit_TMB <- glmmTMB(formula = clo ~ log(nobs)  + (1|subjId), family=poisson(link = "log"), data=data)
summary(fit_TMB)

# It was found that the binomial and poisson based models have very similar residual deviance and AIC. The binomial model 
# seems to have slightly lower AIC and residuals deviance however. Judgeing the QQ plots of rediduals, both models have near
# perfect fits which could indicate over fitting. 

## Part 2 Problem B
library(numDeriv)

# To fit a hierarchical model we need to do a laplace approximation. This is by optimzing with the help of the negative
# log liklihood. Ex. 6.6, p.242
# negative log liklihood function.
loglh = function(U, beta, X, k){
  theta = X%*%beta + log(U[data$subjId]) # 6.38, p.242
  loglh_fixed = -sum(dpois(data$clo, lambda=exp(theta), log=T))
  loglh_random = -sum(dgamma(U, shape=k, scale=1/k, log=T))
  return(loglh_fixed+loglh_random)
}

# Laplace approximation
laplace_gamma = function(theta, X){
  beta = theta[-3]
  k = exp(theta[3]) 
  opt = nlminb(rep(0.5, 47), objective=loglh, beta=beta, k=k, X=X, lower=1e-8)
  U = opt$par
  obj = opt$objective
  H = hessian(func = loglh, x = U, beta = beta, k = k, X=X)
  # return log-likelihood
  return(obj+0.5*log(det(H/(2*pi))))
}
# Optimize over neg. logliklihood by optimizing over the laplace approximation.
fit_gamma_lap = nlminb(rnorm(3), laplace_gamma, X=model.matrix(fit_poi))
#round(c(fit_gamma_lap$par[-5], exp(fit_gamma_lap$par[5])), 6) # Print coefficients.
fit_gamma_lap$iterations
fit_poi$fit$par
####################################

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
  lln_fix <- -sum(dpois(dat$clo, lambda=exp(eta)*u, log=TRUE))
  lln_ran <- -sum(dgamma(u, shape=k, scale = 1/k , log=TRUE))
  
  return(lln_fix + lln_ran)
}

# Laplace approximation
nll_LA_ga <- function(theta, X){
  beta <- theta[1:dim(X)[2]]
  #beta <- theta[-3]
  k <- exp(theta[dim(X)[2]+1])
  #k <- exp(theta[3])
  est <- nlminb(rep(0,length(dat$subjId)), objective=nll_ga, beta=beta, k=k, X=X, lower=0.00000001)
  u <- est$par
  l.u <- est$objective
  H <- hessian(func=nll_ga, x=u, beta=beta, k=k, X=X)
  # return log-likelihood
  return(l.u + 0.5 * log(det(H/(2*pi))))
}

fit_LA_ga <- nlminb(c(fit_TMB$obj$par), nll_LA_ga, X=model.matrix(fit_TMB))
coef_Q4 <- round(c(fit_LA_ga$par[-3], exp(fit_LA_ga$par[3])), 4)
c(coef_Q4, fit_TMB$fit$par)
fit_LA_ga$objective

logLik(fit_TMB)
logLik(fit_LA_ga)
###########################

# Importance sampling
nll_Laplace_simulate_ga <- function(theta, X, n, seed){
  set.seed(seed)
  beta <- theta[1:dim(X)[2]]
  #beta <- theta[-3]
  k <- exp(theta[dim(X)[2]+1])
  #k <- exp(theta[3]) # distribution of the U
  est <- nlminb(rep(0,length(dat$subjId)), objective = nll_ga, beta=beta, k=k, X=X, lower = 0.00000001)
  u <- est$par
  l.u <- est$objective
  H <- diag(hessian(nll_ga, x=u, beta=beta, X=X, k=k))
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
L_ga <- nll_Laplace_simulate_ga(fit_LA_ga$par, X=model.matrix(fit_TMB), n=k, seed=1)
L_ga$est$objective
fit_LA_ga$objective
fit_TMB$fit$objective

L_ga$nnl








