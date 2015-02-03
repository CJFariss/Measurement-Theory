set<-read.csv("/Users/Chuyu/Desktop/problemset.csv")
summary(set)

##Data Description
par(mfrow=c(2,2))
plot(density(y))
plot(density(x1))
plot(density(x2))
plot(density(x3))

## all Xs are close to the normal distribution, there is an outlier for y

##delete the outlier##
subset <- subset(set, set$y!=17728)
attach(subset)

## the naive OLS Regression
model1<-lm(y~x1+x2+x3,data=subset)
summary(model1)

# x2 is negatively related to y, while both x1 and x3 are positively correlated with y#

##OLS with MCMC##
library(rstan) 
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
data <- list(y=y,x1=x1,x2=x2,x3=x3, n=24) #after delete the outlier, dim is 25-1=24

# fit stan model
fit <- stan(model_code = model, data = data, iter = 1000, chains = 4)

# extract draws from stan model object
output <- extract(fit, permuted = TRUE)

# print names
names(output)

# summarize parameters
lapply(output, mean) 

lapply(output, sd) 

print(Sys.time() - time1)

##in terms of coefficients, the output of the MCMC model is very similar to the OLS 