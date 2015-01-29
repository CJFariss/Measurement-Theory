########################
# Problem Set 1 R Code #
########################

### In the future, I will use Sweave. Mea culpa.

# Load libraries
library(ellipse)
library(MASS)
library(rstan)

# Change directory
setwd("~/Dropbox/psu/classes/plsc_597b_measurement")

# Load data
dat <- read.csv(file="hw_01.csv",head=TRUE,sep=",")

# Inspect data
summary(dat)

# visually inspect data
truehist(dat$y)
truehist(dat$x1)
truehist(dat$x2)
truehist(dat$x3)

### Discussion: The X variables appear to be uniformly distributed over a 0-100 interval, while the Y variable appears to be generated from an exponential distribution

# look for correlations
ctab <- cor(dat)
ctab
plotcorr(ctab)

### Discussion: X1 and X3 are weakly and positively correlated with Y, while X2 is moderately and negatively correlated with Y

# Linear model
mod <- lm(dat$y ~ dat$x1 + dat$x2 + dat$x3)
summary(mod)

### Discussion: X2 is the only `statistically significant' variable in the model. X2 appears negatively correlated with Y. The usual disclaimers about small sample sizes apply here.
### Also, I should probably drop the outlier from Y. Not knowing the data, however, I am hesitant to do so.

# Do some STAN stuff
set_cppo("fast")  # for best running speed

# -------------------------------------------------- #
# define STAN model - everything that shows up in the data block has to be declared in R and then passed to STAN
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
real beta;
real gamma;
real delta;
real<lower=0> sigma;
}
model {
alpha ~ normal(3000,1000); # priors (these are variances not precision)
beta ~ normal(11,100);
gamma ~ normal(-75,100);
delta ~ normal(38,3000);
y ~ normal(alpha + beta * x1 + gamma * x2 + delta * x3, sigma);
}
"
# -------------------------------------------------- #

# create data list
n <- length(dat$x1)
data <- list(y=dat$y, x1=dat$x1, x2=dat$x2, x3=dat$x3, n=n)

# fit stan model
fit <- stan(model_code = model, data = data, iter = 1000, chains = 4)

# extract draws from stan model object
output <- extract(fit, permuted = TRUE)

# print names
names(output)

# summarize parameters
lapply(output, mean)
lapply(output, sd)

# organizing
cbind(unlist(lapply(output, mean)),unlist(lapply(output, sd)))

### Discussion: STAN generates similar output as lm() -- not that I know what that means right now. So, X2 and Y appear to be negatively correlated, to the degree that we can make a claim on so little data.