#Measurement Homework 1
#packages
library(rms)
library(car)
#setting working directory and loading data
setwd("C:/Users/thchen/Dropbox/13_Spring 2015/PSLC 597D")
data<-read.csv("_data_problem_Set_01.csv",header=T,sep=",")
attach(data)

#summarizing data
summary(data)

#correlations
cor(data)
cor(data[-9,])
#removing what seems like the outlier (obs 9) does not change much except for the correlation between y and x1

#i assume that y is the outcome variable
#scatterplots and fitted lines
par(mfrow=c(3,2))
plot(y~x1)
abline(lm(y~x1))
plot(y~x1,data=data[-9,])
abline(lm(y~x1,data=data[-9,]))

plot(y~x2)
abline(lm(y~x2))
plot(y~x2,data=data[-9,])
abline(lm(y~x2,data=data[-9,]))

plot(y~x3)
abline(lm(y~x3))
plot(y~x3,data=data[-9,])
abline(lm(y~x3,data=data[-9,]))
#the effect of removing obs on the relationship between y and the x's noted above is evident here. only the fitted line between y and x1 shows a large change in slope

#plotting density
par(mfrow=c(2,2))
plot(density(y))
plot(density(x1))
plot(density(x2))
plot(density(x3))
#the x's probably do not need to be transformed for linear regression

#linear regression with all three varibles
m1<-lm(y~x1+x2+x3)
summary(m1)

#outliers? 9 seems to be one.
influencePlot(m1)
dfbetasPlots(m1)
plot(covratio(m1))

#by all measures obs 9 should be removed
#going to remove observation 9 and see what happens
m2<-lm(y~x1+x2+x3,data=data[-9,])
summary(m2)

#difference between the models?
par(mfrow=c(1,1))
cbind(predict(m1)[-9],predict(m2))
plot(predict(m1)[-9],predict(m2))
#rank of predictions between two models seem to be similar

#checking residuals
plot(density(residuals(m1)))

#checking for heteroscedasticity
plot(predict(m2),residuals(m2)^2)
#looks very un-uniform
#no information on clustering, so just run white's robust standard error
m3<-robcov(ols(y~x1+x2+x3,data=data[-9,],x=T,y=T))
m3
#standard errors become a little larger, nothing substantive changes
#it seems like all three x's are significantly related to the outcome variable y. this relationship only appeared after i removed obs 9, which i deemed an outlier.

#what happens if i include every single interaction?
m4<-lm(y~x1*x2*x3,data=data[-9,])
summary(m4)
#the interaction terms are quite small compared to the constituent terms; everything demonstrates a weak relationship with y

#i found this excercise very difficult, in the sense that i had trouble playing with the data without having any theoretical expectations in mind



