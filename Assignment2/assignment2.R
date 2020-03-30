
setwd("C:/Users/Bjorn/OneDrive/Dokument/University/DTU/02424 Advanced Dataanalysis and Statistical Modelling/labs/02424-Advanced-Dataanalysis-and-Statistical-Modelling/Assignment2")

#data = read.csv("dioxin.csv")
library(gclus)
data(ozone)
head(ozone)
## 1.


## 2.
fit = glm(Ozone~., data=ozone)
anova(fit, test ="Chisq")
summary(fit)

plot(fit)

Rd = residuals(fit,type='deviance')

plot(ozone$Temp,Rd, xlab='Temp', ylab='Deviance residuals')
plot(ozone$InvHt,Rd, xlab='InvHt', ylab='Deviance residuals') # Residuals seem to have an inverse fan/ cone shape
plot(ozone$Pres,Rd, xlab='Pres', ylab='Deviance residuals')
plot(ozone$Vis,Rd, xlab='Vis', ylab='Deviance residuals') # Looks like there is a trend in the residuals
plot(ozone$Hgt,Rd, xlab='Hgt', ylab='Deviance residuals') # Residuals seem to fan out in a cone shape
plot(ozone$Hum,Rd, xlab='Hum', ylab='Deviance residuals')
plot(ozone$InvTmp,Rd, xlab='InvTmp', ylab='Deviance residuals')
plot(ozone$Wind,Rd, xlab='wind', ylab='Deviance residuals')

## 3.
fit2 = glm(1/Ozone~., data=ozone)
summary(fit2)
anova(fit2, test="Chisq")

Rd2 = residuals(fit,type='deviance')

plot(ozone$Temp,Rd2, xlab='Temp', ylab='Deviance residuals')
plot(ozone$InvHt,Rd2, xlab='InvHt', ylab='Deviance residuals') # Residuals are not more random in nature, although there is a clear concentration at 500-1500 and 5000 line at 5000
plot(ozone$Pres,Rd2, xlab='Pres', ylab='Deviance residuals')
plot(ozone$Vis,Rd2, xlab='Vis', ylab='Deviance residuals') # Residuals do not show any clear trend
plot(ozone$Hgt,Rd2, xlab='Hgt', ylab='Deviance residuals') # Residuals have now less of a cone shape
plot(ozone$Hum,Rd2, xlab='Hum', ylab='Deviance residuals')
plot(ozone$InvTmp,Rd2, xlab='InvTmp', ylab='Deviance residuals')
plot(ozone$Wind,Rd2, xlab='wind', ylab='Deviance residuals')

## 4.
fit3 = glm(Ozone~., data=ozone, family = Gamma(link = "inverse"))
fit4 = glm(Ozone~., data=ozone, family = gaussian(link = "log"))
anova(fit3, fit, test="Chisq")
summary(fit3)
anova(fit4, fit, test="Chisq")
summary(fit4)
# Although both models seem to have similar ammounts of significance it would seem that fit3 using the
# Gamma(link = "inverse") has less Resid. Dev than fit 3 and it would the better choice.

## 5.
anova(fit2, fit, test="Chisq") # anova can not be used as fit2 has its dependent Ozone variable transformed by inverse.
summary(fit2)
summary(fit3)
# Comparing the model from question 3 and question 4, I would definitly choose question 4 model as it provides higher
# significance on more variables. 

## 6.

# According to p. 75 in the book, Definition 3.14 - Leverage:
# var(beta) = sigma^2 * (x'x)^-1
one_mat = matrix(1, ncol=dim(x)[1], nrow=1)
x = as.matrix(ozone)
x = x[,-1]
x2 = cbind(c(one_mat), x) # This is to get the correct design matrix.

mu = x2%*%fit3$coefficients
w_beta = diag(fit3$weights%*%t(mu))
###################################################
X = model.matrix(fit3)
mu = predict(fit3, type="response")
g_prime_mu = 1/mu
V_mu = mu**2
w = 0.1652551 #fit3$weights
W = w/((g_prime_mu**2)*V_mu)
W_ = diag(w*V_mu)

disp_mat = solve(t(X) %*% W %*% X)
summary(fit3)$cov.scaled
all.equal(disp_mat, summary(fit3)$cov.scaled)


# fit5 = glm(Ozone~ Temp+I(Temp*Temp)+Hum+InvHt, data=ozone)
# summary(fit5)
# drop1(fit5)

## Part 2: 1., 2.
model1 = glm(Ozone~Temp, data=ozone, family=Gamma(link = "inverse"))
summary(model1)
model2 = glm(Ozone~Temp+Hum, data=ozone, family=Gamma(link = "inverse"))
anova(model1, model2, test="Chisq")
model3 = glm(Ozone~Temp+Hum+InvHt, data=ozone, family=Gamma(link = "inverse"))
anova(model2, model3, test="Chisq")
model4 = glm(Ozone~Temp+Hum+InvHt+Vis, data=ozone, family=Gamma(link = "inverse"))
anova(model3, model4, test="Chisq") # Shows that model4 is not significantly better than model3
model5 = glm(Ozone~Temp+Hum+InvHt+Wind, data=ozone, family=Gamma(link = "inverse"))
anova(model3, model5, test="Chisq") # Shows that model5 is not significantly better than model3
model6 = glm(Ozone~ Temp+I(Temp*Temp)+Hum+InvHt, data=ozone, family=Gamma(link = "inverse"))
anova(model3, model6, test="Chisq")
model7 = glm(Ozone~ Temp+I(Temp*Temp)+Hum+InvHt*Temp, data=ozone, family=Gamma(link = "inverse"))
anova(model6, model7, test="Chisq")
model8 = glm(Ozone~ Temp+I(Temp*Temp)+InvHt*Temp, data=ozone, family=Gamma(link = "inverse"))
anova(model7, model8, test="Chisq") 
summary(model8) # This seems to be a good fit
anova(model1, model8, test="Chisq")

# Residuals plots look fine, independent, randomly distributed and not skewed. 
plot(model8) 
hist(model8$residuals, breaks=50, probability=TRUE)
lines(density(model8$residuals), # density plot
      lwd=2, # thickness of line
      col="chocolate3")

