################################
## Read data
setwd("~/Desktop/DTU/Advanced\ Dataanalysis\ and\ Statistical\ Modelling/Assignments")
## Read the .txt file holding the data
HE <- read.table("clothingSum.csv", sep=",", header=TRUE, as.is=TRUE)
fem <- subset(HE, (sex == 'female'))
male <- subset(HE, (sex == 'male'))

HE <- fem
summary(HE)
head(HE)
HE
## Overview of the data
dim(HE)
names(HE)
head(HE)
tail(HE)
summary(HE)
str(HE)

## Histogram for the temperature
hist(HE$tOut, xlab="Temperature", ylab="Density", prob=TRUE, col="lightblue", main="Histogram for outdoor temperature", breaks=14)
hist(HE$tInOp, xlab="Temperature", ylab="Density", prob=TRUE, col="lightblue", main="Histogram for indoor temperature", breaks=14)

## Scater plot
plot(HE$tOut, HE$clo, xlab="Oudoor temperature", ylab="Cloth insulation",main="Oudoor temperature for various cloth insulation")
abline(line(HE$tOut, HE$clo), type="b", col=2)

plot(HE$tInOp, HE$clo, xlab="Indoor temperature", ylab="Cloth insulation",main="Oudoor temperature for various cloth insulation")
abline(line(HE$tInOp, HE$clo), type="b", col=2)

plot(HE$tOut, HE$tInOp, xlab="Outdoor temperature insulation", ylab="Indoor temperature",main="Oudoor temperature vs indoor temperature")
abline(line(HE$tOut, HE$tInOp), type="b", col=2)

## Boxplot
boxplot(HE$tOut, HE$tInOp,
        names=c("Outdoor temperature", "Indoor temperature"), 
        xlab="Attribute", ylab="Temperature", main="Boxplot for the outdoor/indoor temperatures", col=c("lightgreen","Lightblue"))
legend("topright", paste0("Temperature ", 0:1), lty=1, col=c("lightgreen","lightblue"))

## Single regression line with CI and PI
library("ggplot2")

## clo vs outdoor temperature
# 1. Add predictions 
fit_tOut <- lm(clo ~ tOut, data = HE)
pred.int <- predict(fit_tOut, interval = "prediction")
mydata <- cbind(HE, pred.int)

# 2. Regression line + confidence intervals
p <- ggplot(mydata, aes(tOut, clo)) + geom_point() + stat_smooth(method = lm)

# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed") + geom_line(aes(y = upr), color = "red", linetype = "dashed")

## clo vs indoor temperature
fit_tInOp <- lm(clo ~ tInOp, data = HE)
pred.int <- predict(fit_tInOp, interval = "prediction")
mydata <- cbind(HE, pred.int)
p <- ggplot(mydata, aes(tInOp, clo)) + geom_point() + stat_smooth(method = lm)
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed") + geom_line(aes(y = upr), color = "red", linetype = "dashed")

## Fitting of tOut and tInOp as a funtion of clo
fit <- lm(clo ~ tOut + tInOp, data = HE)
summary(fit)

## QQ-plot
qqnorm(HE$tOut,main="Normality test for day 0", xlab = "Teoretical quantiles",ylab = "Outdoor temperature")
qqline(HE$tOut, col="blue")

qqnorm(HE$tInOp,main="Normality test for day 1", xlab = "Teoretical quantiles",ylab = "Indoor temperature")
qqline(HE$tInOp, col="red")

qqnorm(HE$clo,main="Normality test for day 2", xlab = "Teoretical quantiles",ylab = "Clothing level")
qqline(HE$clo, col="green")

## Confidence intervals
t.test(HE$tOut, conf.level=0.95)$conf.int
t.test(HE$tInOp, conf.level=0.95)$conf.int
t.test(HE$clo, conf.level=0.95)$conf.int

## Model check
plot(fit$fitted.values, HE$tOut, xlab = "Fitted values", ylab = "Outdoor temperature")
plot(HE$tOut, fit$residuals, xlab = "Outdoor temperature", ylab = "Residuals")
plot(fit$fitted.values, fit$residuals, xlab = "Fitted values", ylab = "Residuals")

## Normal QQ-plot for residuals
qqnorm(fit$residuals, ylab = "Residuals", xlab = "Z-scores")
qqline(fit$residuals)

## Prediction
D_test <- subset(HE, (day == 1))
pred <- predict(fit, newdata = D_test, interval = "prediction", level = 0.95)                   
cbind(day = D_test$day, Q = D_test$clo, pred)
pred                   

## Plot
df = data.frame(cbind(id = D_test$day, t=D_test$clo, Q = D_test$tOut, pred))
plot(D_test$tOut, D_test$clo, col='blue')
abline(line(D_test$tOut, D_test$clo), col='red')
abline(line(df$Q, df$fit), col='green', lty="dashed")
abline(line(df$Q, df$lwr), col='black')
abline(line(df$Q, df$upr), col='black')

##################################################################################################################################
## Generalized linear model
glm_clo <- glm(clo ~ tOut + tInOp, data = HE)
anova(glm_clo)
summary(glm_clo)

glm_clo_2 <- glm(clo ~ tOut, data = HE)
anova(glm_clo_2)
summary(glm_clo_2)

# Gender comparison
fem <- subset(HE, (sex == 'female'))
male <- subset(HE, (sex == 'male'))

boxplot(fem$clo, male$clo,
        names=c("clo female", "clo male"), 
        xlab="Attribute", ylab="Clothing level", main="Boxplot for gender clothing level", col=c("lightgreen","Lightblue"))

glm_clo_fem <- glm(clo ~ tOut + tInOp, data = fem)
summary(glm_clo_fem)

glm_clo_male <- glm(clo ~ tOut + tInOp, data = male)
summary(glm_clo_male)


