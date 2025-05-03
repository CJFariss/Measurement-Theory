###Measurement Homework 1###
#Wonjun Song#
#02/03/2015#
#rm(list=ls())

#packages
library(foreign)
library(MASS)
library(corrgram)
library(Hmisc)
library(ellipse)
library(car)
library(rstan)


#working directory
setwd("/Users/Wonsdadasdjun Song/Dropbox/Measurement/PS1")
ps1data <- read.csv(file="data_PS1.csv")
ps1data
summary(ps1data)

#don't need Unit variable
ps1data$unit = NULL

#simple correlation
corr <- cor(ps1data, use="pairwise.complete.obs")
corr
rcorr(as.matrix(ps1data))
corrgram(ps1data, order=NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt, main="Homework 1 Data (unsorted)")
# comment: among the correlations, x2 and x3 show stronger positive correlation while x2 and y show stronger negative correlation, compared to other values.

#distribution 
#histogram
x1 <- ps1data$x1
h1 <- hist(x1, breaks=10, col="red", xlab="x1", main="Histogram with Normal Curve (x1)") 
x1fit<-seq(min(x1),max(x1),length=40) 
y1fit<-dnorm(x1fit,mean=mean(x1),sd=sd(x1)) 
y1fit <- y1fit*diff(h1$mids[1:2])*length(x1) 
lines(x1fit, y1fit, col="blue", lwd=2)

x2 <- ps1data$x2
h2 <- hist(x2, breaks=10, col="red", xlab="x2", main="Histogram with Normal Curve (x2)") 
x2fit<-seq(min(x2),max(x2),length=40) 
y2fit<-dnorm(x2fit,mean=mean(x2),sd=sd(x2)) 
y2fit <- y2fit*diff(h2$mids[1:2])*length(x2) 
lines(x2fit, y2fit, col="blue", lwd=2)

x3 <- ps1data$x3
h3 <- hist(x3, breaks=10, col="red", xlab="x3", main="Histogram with Normal Curve (x3)") 
x3fit<-seq(min(x3),max(x3),length=40) 
y3fit<-dnorm(x3fit,mean=mean(x3),sd=sd(x3)) 
y3fit <- y1fit*diff(h3$mids[1:2])*length(x3) 
lines(x3fit, y3fit, col="blue", lwd=2)

x4 <- ps1data$y
h4 <- hist(x4, breaks=10, col="red", xlab="y", main="Histogram with Normal Curve (y)") 
x4fit<-seq(min(x4),max(x4),length=40) 
y4fit<-dnorm(x4fit,mean=mean(x4),sd=sd(x4)) 
y4fit <- y4fit*diff(h4$mids[1:2])*length(x4) 
lines(x4fit, y4fit, col="blue", lwd=2)

#kernel density
kx1 <- density(ps1data$x1)
plot(kx1, main="Kernel Density of x1")
polygon(kx1, col="red", border="blue")

kx2 <- density(ps1data$x2)
plot(kx2, main="Kernel Density of x2")
polygon(kx2, col="red", border="blue")

kx3 <- density(ps1data$x3)
plot(kx3, main="Kernel Density of x3")
polygon(kx3, col="red", border="blue")

kx4 <- density(ps1data$y)
plot(kx4, main="Kernel Density of y")
polygon(kx4, col="red", border="blue")
# comment: density plots show that the x variables are uniformly distributed while the y variable is skewed to the right. so i'll try linear regression.

#simple linear regression
mod <- lm(y ~ x1 + x2 + x3, data=ps1data)
summary(mod)
# comment: only x2 is a negatively significant variable. 

#outlier test
outlierTest(mod)
qqPlot(mod, main="QQ Plot")
leveragePlots(mod)
# comment: observation #9 seems to be an outlier. 

#correlation and linear regression without outlier
#create subset data
ps1data.out <- subset(ps1data, ps1data$y!=17728)
ps1data.out

corr.out <- cor(ps1data.out, use="pairwise.complete.obs")
corr.out
corrgram(ps1data.out, order=NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt, main="Homework 1 Data (unsorted)")

mod.out <- lm(y ~ x1 + x2 + x3, data=ps1data.out)
summary(mod.out)
# comment: the model shows better fit without the outlier observation. now all of variables are significant (x1 and x3 postively, x2 negatively). correlation values and signs have changed too. 

#rstan stuff (not really my code)
#set speed
set_cppo("fast")

# set time start variable
time1 <- Sys.time()
print(Sys.time() - time1)

#attach data without outlier
attach(ps1data.out)

# -------------------------------------------------- #
# define STAN model
# -------------------------------------------------- #
model <- "
data {
int<lower=0> n;  
vector[n] y; 
vector[n] x1;
vector[n] x2;
vector[n] x3;
}
parameters {
real alpha; 
real beta1;
real beta2;
real beta3;
real<lower=0> sigma;
}
model {
alpha ~ normal(300,100); # priors (these are variances not precision)
beta1 ~ normal(20,10);
beta2 ~ normal(-15,10);
beta3 ~ normal(9,10);
y ~ normal(alpha + beta1 * x1 + beta2 * x2 + beta3 * x3, sigma);
}
"
# -------------------------------------------------- #

# create data list
n = nrow(ps1data.out)
data <- list(y = y, x1=x1, x2=x2, x3=x3, n=n)

# fit stan model
fit <- stan(model_code = model, data = data, iter = 1000, chains = 4)
fit
lol<-summary(fit)

# comment: a similar results pops up as linear regression model. means of beta1, 2, and 3 are very similar as coefficient estimates for x1, 2, and 3. quantitle distributions are different from 0 also. 






