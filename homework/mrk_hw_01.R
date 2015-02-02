rm(list=ls())
setwd("/Users/kenwick2/Dropbox/measurement/homework/")
library(randomForest)
library(polywog)
library(ggplot2)

data=read.csv("hw_01.csv")

#I don't like this variable 
data$unit = NULL

fit1 = polywog(y~ x1 + x2 + x3,
		data = data,
		nfolds = 3,
		degree = 3,
		boot  = 10,
		method="alasso"
		)
plot(fit1)
#What the hell is up with those confidence intervals?...damnit polywog, you were supposed to be the chosen one!

fit1$lambda.cv

fit2 = polywog(y~x1 + x2 + x3,
		data = data,
		degree = 2,
		boot  = 10,
		nfolds = 3,
		method = "scad"
		)
plot(fit2)
fit2$lambda.cv
#You were supposed to bring balance to the model, not destroy it! 

#To hell with it, let's just do rf
library(randomForestSRC)



rf.mod = rfsrc(y~x1+x2+x3, data = data, ntree=1000, importance="permute")
rf.mod
#lovely, explaining -3.77 percent of the oob variance

rf.mod$importance
#I never did like x1

plot.variable(rf.mod, partial=TRUE)
#well...at least the CIs contain the point estimate this time

#perhaps there are interactions afoot
find.interaction(rf.mod)
#Indeed there are! Inefficiency may be a problem...let's make with the interactions

data$x1x2 = data$x1*data$x2
data$x1x3 = data$x1*data$x3
data$x2x3 = data$x2*data$x3

#perhaps some polynomials would be to your liking?
data$x1_2 = data$x1^2
data$x2_2 = data$x2^2
data$x3_2 = data$x3^2
data$x1_3 = data$x1^3
data$x2_3 = data$x2^3
data$x3_3 = data$x3^3

rf.mod1 = rfsrc(y~., data = data, ntree=1000, importance="permute")
rf.mod1
#well, I've negatively explained less...that's good

#to hell with over-fitting...let's just try a tree
library(rpart)

tree = rpart(y~., data = data)
summary(tree)
plot(tree)
text(tree, use.n = FALSE)

#more splits, I wants to overfit
treee = rpart(y~., data = data, xval=0, minsplit=1, minbucket=1)
summary(treee)
plot(treee)
text(treee, use.n = FALSE)

#MOAR SPLITS!!!! 

MOAR = rpart(y~., data = data, xval=0, minsplit=1, minbucket=1, cp=-1)
summary(MOAR)
plot(MOAR)

#TIME!















