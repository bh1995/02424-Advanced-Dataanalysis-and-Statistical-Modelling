
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



