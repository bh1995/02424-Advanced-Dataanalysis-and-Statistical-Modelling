#Assgment3 problem_A question_1

setwd("C:/Users/Olena/Desktop/sem2/adv analysis/Assigment3")
data <- read.csv("concrets.csv", sep=" ", header=TRUE, as.is=TRUE)
library(MASS)
library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots
library(lme4)
data$date <- as.Date(data$date) #to dispaly just month and year 

qplot(x=date, y=y7,
      data=data, na.rm=TRUE,
      main="Strength of concrete after 7 days",
      xlab="Date", ylab="Strength")

qplot(x=date, y=y28,
      data=data, na.rm=TRUE,
      main="Strength of concrete after 7 days",
      xlab="Date", ylab="Strength")

#Assgment3 problem_A question_2

data2 <- aggregate(data[, 2:3], list(data$batch), mean)

#Assgment3 problem_A qestion_3

M1 <- lmer( y28 ~ air.temp + (1| batch), data =data,REML = FALSE)
summary(M1)
#since total variance is (3.8+2.9) 6.7 so the baches explain ( 3.8/6.7) 56% (so 56% of variance is explained by fixed effect) 
M2 <- lmer( y28 ~ (1| batch), data =data,REML = FALSE)
summary(M2)

plot(M1) # cant see any patern 
plot(M2)

qqnorm(resid(M1))
qqline(resid(M1))

boxplot(y7 ~ batch, data = data)
boxplot(y28 ~ batch, data = data)



#air temperature doesnt seem be significant 
#to add: 1 plots 2 compare two models 3 interpretation for the model 

#Assgment3 problem_A question_4
anova(M1,M2)

(split_plot <- ggplot(aes(y7, air.temp), data = data) + 
    geom_point() + 
    facet_wrap(~ batch) + 
    xlab("strength") + 
    ylab("air temp"))

#plot showing lack of dependence 
  