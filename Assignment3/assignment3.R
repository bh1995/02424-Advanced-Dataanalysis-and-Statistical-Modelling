
setwd("C:/Users/Bjorn/OneDrive/Dokument/University/DTU/02424 Advanced Dataanalysis and Statistical Modelling/labs/02424-Advanced-Dataanalysis-and-Statistical-Modelling/Assignment3")
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
boxplot(clo0$tOut, clo1$tOut, clo2$tOut, clo3$tOut, main="Temp. outside")
boxplot(clo0$tInOp, clo1$tInOp, clo2$tInOp, clo3$tInOp, main="Temp. inside")


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
fit_poi = glmmTMB(clo ~ sex  + (1|subjId), family=poisson, data=data)
summary(fit_poi)
res_poi = simulateResiduals(fit_poi)
plot(res_poi, asFactor = TRUE)

# It was found that the binomial and poisson based models have very similar residual deviance and AIC. The binomial model 
# seems to have slightly lower AIC and residuals deviance however. Judgeing the QQ plots of rediduals, both models have near
# perfect fits which could indicate over fitting. 

