#Preamble
library(rjags)
library(R2jags)
library(coda)
library(graphics)
library(dplyr)
#Load Data
setwd("~/Dropbox/School Stuff/Current Classes/Measurement Class/Assignments/Probset1")
data <- read.csv("~/Dropbox/School Stuff/Current Classes/Measurement Class/Assignments/Probset1/_data_problem_Set_01.csv")
data <- select(data, y, x1, x2, x3)
attach(data)

#Correlation
cor(x1,y)
cor(x2,y)
cor(x3,y)
cor(x1,x2)
cor(x1,x3)
cor(x2,x3)

#Linear Models
mod.1 <- lm(y ~ x1)
summary(mod.1)
mod.2 <- lm(y ~ x2)
summary(mod.2)
mod.3 <- lm(y ~ x3)
summary(mod.3)
mod.4 <- lm(y ~ x1 + x2)
summary(mod.4)
mod.5 <- lm(y ~ x1 + x2)
summary(mod.5)
mod.6 <- lm(y ~ x1 + x3)
summary(mod.6)
mod.7 <- lm(y ~ x2 + x3)
summary(mod.7)
mod.8 <- lm(y ~ x1 + x2 +x3)
summary(mod.8)

lmlist <- list(summary(mod.1)$r.squared,summary(mod.2)$r.squared,summary(mod.3)$r.squared,summary(mod.4)$r.squared,summary(mod.5)$r.squared,summary(mod.6)$r.squared,summary(mod.7)$r.squared,summary(mod.8)$r.squared)













print(Sys.time() - time1)