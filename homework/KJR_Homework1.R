rm(list=ls())
library(MASS)
setwd("C:/Users/Kevin/Dropbox/Grad School Year 2 Term 2/Measurement/Homework 1")
data <- read.csv("data_problem_Set_01.csv")

attach(data)
####
summary(data)

par(mfrow=c(2,2))
plot(density(y))
plot(density(x1))
plot(density(x2))
plot(density(x3))

# the xs are all close to normal, y has a large outlier 17228, when you drop it y becomes closer to normal. 

par(mfrow=c(1,1))
plot(density(y[y!=17728]))

## look at univariate relationships with outlier
pairs(y~x1+x2+x3,data)

cor(data)

# there does not seem to be any stong relationship between any of the xs. the best is between x2 and x3 
# y is negatively related to x2, but not strongly related to x1 or x3


## look at univariate relationships without outlier
data.out <- subset(data, data$y!=17728)
pairs(y~x1+x2+x3,data.out)

cor(data.out)
## when dropping the oulier, y and x1 show a stronger relationship

mod.1 <- lm(y~x1+x2+x3,data=data)
summary(mod.1)

mod.2 <- lm(y~x1+x2+x3, data=data.out)
summary(mod.2)


# see if bayesian magic fixes this 
library(rstan) # load rstan library
set_cppo("fast")  # for best running speed


# set time start variable
time1 <- Sys.time()
print(Sys.time() - time1)


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
n = nrow(data)
data <- list(y = y, x1=x1, x2=x2, x3=x3, n=n)

# fit stan model
fit <- stan(model_code = model, data = data, iter = 1000, chains = 4)
fit



## FINAL NOTES ##
### Without the outlier there is a positive relationship between X1, X3 and Y. And a negative relationship
### between X2 and Y. If you include the outlier these relationships disppear and X2 is only negatively
### related to Y. If I use the coefs from the no outlier model as priors for Bayes OLS on the full data
### I get something similar ish back. I don't know what means. Bayes is magic right? 
