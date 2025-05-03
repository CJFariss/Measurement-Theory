rm(list=ls())

setwd('/Users/laurietumaneng/Dropbox/597B - Measurement/Homework/Problem Set 1')
ps1 <- Problem.Set.1
names(ps1)

##Scatterplot
library(car)
scatterplotMatrix(ps1)

##Correlation 
cor(ps1, use="complete.obs")
#the variables look like they are weakly correlated 

##Regression
ols <- lm(y ~ x1 + x2 + x3, data=ps1)
summary(ols)
#x2 is statistically significant with y at the 0.05 level 
#with an r-squared of 0.2, the model doesn't appear to be a great fit for the data

##Residual Plot
unstandardizedPredicted <- predict(ols)
unstandardizedResiduals <- resid(ols)
standardizedPredicted <- (unstandardizedPredicted - 
                            mean(unstandardizedPredicted)) / sd(unstandardizedPredicted)
standardizedResiduals <- (unstandardizedResiduals - 
                            mean(unstandardizedResiduals)) / sd(unstandardizedResiduals)
plot(standardizedPredicted, standardizedResiduals, 
     main = "Standardized Residuals Plot", xlab = "Standardized Predicted Values", 
     ylab = "Standardized Residuals")
abline(0,0)
#there is one major outlier according to the residuals plot 
#with a 4 on standardized residuals

##Histogram
hist(standardizedResiduals, freq = FALSE)
curve(dnorm, add = TRUE)
#the residuals are not normally distributed

##PP plot
probDist <- pnorm(standardizedResiduals)
plot(ppoints(length(standardizedResiduals)), sort(probDist), 
     main = "PP Plot", xlab = "Observed Probability", ylab = "Expected Probability")
abline(0,1)

##Prediction and CI
reg1 <- lm(y~x1, data=ps1)
reg2 <- lm(y~x2, data=ps1)
reg3 <- lm(y~x3, data=ps1)

SE1 <- predict(reg1, interval="confidence")
SE2 <- predict(reg2, interval="confidence")
SE3 <- predict(reg3, interval="confidence")

par(mfrow=c(2,2))
Sort<-order(ps1$x1)
plot(ps1$x1,ps1$y, main="X1")
abline(reg1,lwd=3)
lines(sort(ps1$x1),SE1[Sort,2],col="black",lwd=2,lty=2)
lines(sort(ps1$x1),SE1[Sort,3],col="black",lwd=2,lty=2)

Sort<-order(ps1$x2)
plot(ps1$x2,ps1$y, main="X2")
abline(reg2,lwd=3)
lines(sort(ps1$x2),SE2[Sort,2],col="black",lwd=2,lty=2)
lines(sort(ps1$x2),SE2[Sort,3],col="black",lwd=2,lty=2)

Sort<-order(ps1$x3)
plot(ps1$x3,ps1$y, main="X3")
abline(reg3,lwd=3)
lines(sort(ps1$x3),SE3[Sort,2],col="black",lwd=2,lty=2)
lines(sort(ps1$x3),SE3[Sort,3],col="black",lwd=2,lty=2)

##Random Forest 
library(randomForest)
ps1.rf  <- randomForest(ps1[,-5], ps1[,5], prox=TRUE)
summary(ps1.rf)
varImpPlot(ps1.rf)
#in out of sample prediction, this model says x2 works best
#partial dependence plots
partialPlot(ps1.rf, ps1, x1)
partialPlot(ps1.rf, ps1, x2)
partialPlot(ps1.rf, ps1, x3)
#the partial dependence plots show that there aren't any linear relationships 
#between the independent and dependent variables 