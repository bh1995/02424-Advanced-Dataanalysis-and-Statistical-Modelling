data = read.csv("ozone.csv")
library(gclus)
library(ggplot2)
library(ggpubr)
library(stats)
library(lmtest)
library(vars)
library(rcompanion)
data(ozone)
head(ozone)
## exercise1

hist(ozone$Temp) #we can show the distribution of data 
hist(ozone$Hum)

cross.table <- table(ozone$Ozone,ozone$Wind,dnn = c("Ozone","Wind"))
cross.table; #so we cna see that we dont have measurment in each of combination 

cor(ozone) #the strongest cor with ozon has: +tem, -InvTem, +Hgt
#Wind seems to have no cor

##exercise2 
M1 <- glm(Ozone~., data=ozone, family = "gaussian")
summary(M1)
M1.residuals = residuals(M1,type='pear')
plot(ozone$Vis,M1.residuals) 
plot(ozone$Hgt,M1.residuals) #
plot(ozone$Hum,M1.residuals) #
plot(ozone$InvTmp,M1.residuals)#
plot(ozone$Wind,M1.residuals)#
plot(ozone$Temp,M1.residuals)
plot(ozone$InvHt,M1.residuals)
plot(ozone$Pres,M1.residuals)

#there are few plots where we can see cone shape, this would suggest use of transforamtion of data

## exercise3 

M.log <- glm(Ozone~., family =gaussian(link="log"),data=ozone)
summary(M.log)

M.inv <- glm(Ozone~., family =gaussian(link="inverse"),data=ozone)
summary(M.inv)

lrtest(M.log,M.inv) # not sure in lm test can be use for different links ...
compareGLM(M.log,M.inv) 

Residuals.M.log = residuals(M.log,type='pear')
Residuals.M.inv = residuals(M.inv,type='pear')

#I will check just residuals which showed some trends before 
plot(ozone$Hgt,Residuals.M.log) 
plot(ozone$Hum,Residuals.M.log)
plot(ozone$InvTmp,Residuals.M.log)
plot(ozone$Wind,Residuals.M.log)

plot(ozone$Hgt,Residuals.M.inv) 
plot(ozone$Hum,Residuals.M.inv)
plot(ozone$InvTmp,Residuals.M.inv)
plot(ozone$Wind,Residuals.M.inv)

## here I am not sure if it is anyhow better.. 

## exercise4
M2 <- glm(Ozone ~ Temp+InvTmp+Hgt+InvHt , data=ozone, family = "gaussian")
summary(M1)
M3 <- glm(Ozone ~ Temp*InvTmp*Hgt*InvHt , data=ozone, family = "gaussian")
summary(M2)
lrtest(M2,M3)

##exercise5
#compring M2 and M.log, M2 seems better since has more significant var 



##exercise6 
#still working on.. 
summary(M1)$cov.scaled

##exercise 2.1 I will take model M2 and add interactions which were signifcant in M3
summary(M3)

#Here I have problem... non of the interactions are significant .. so what else I can do ?


