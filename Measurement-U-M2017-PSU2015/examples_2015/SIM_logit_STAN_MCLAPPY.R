library(rstan) # load rstan library
library(multicore)

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
    int<lower=0, upper=1> y[n]; 
    vector[n] x;  
  }
  parameters {
    real alpha; 
    real beta;
  }
  model {
    alpha ~ normal(0,10); # priors (these are variances not precision)
    beta ~ normal(0,10); 

    y ~ bernoulli_logit(alpha + beta * x);
  }
"
# -------------------------------------------------- #


# simulate x1 and set the "true" population values alpha and beta 
n <- 1000
x <- rnorm(n,0,1) 
alpha <- 1.250000 
beta <- 2.500000

# systematic component of the model 
xb <- alpha + beta * x

# transform the linear term xb using the logit function 
# so that theta is bound from 0 to 1 
theta <- 1 / (1 + exp(-xb))

# generate the dependent variable y with theta and measurment error 
error <- runif(n,0,1) 
y <- ifelse(error < theta,1,0)

# create data list
data <- list(y = y, x=x, n=n)

# fit linear model
summary(glm(y~x, family=binomial(link="logit")))

# fit stan model
ITERATIONS <- 1000000

time1 <- Sys.time()
#fit <- stan(model_code = model, data = data, iter = ITERATIONS, chains = 4)
print(Sys.time() - time1)

time1 <- Sys.time()

sflist <- mclapply(1:4, mc.cores = 4, 
            function(i) fit <- stan(model_code = model, data = data, iter = ITERATIONS, chains = 1, chain_id = i, refresh = -1))
print(Sys.time() - time1)

# collect the results back into a single Stan fit object
fit <- sflist2stanfit(sflist)

# extract draws from stan model object
output <- extract(fit, permuted = TRUE)

# print names
names(output)

# summarize parameters
lapply(output, mean)
lapply(output, sd)

print(Sys.time() - time1)

