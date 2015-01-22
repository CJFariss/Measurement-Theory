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
    vector[n] x;  
  }
  parameters {
    real alpha; 
    real beta;
    real<lower=0> sigma;
  }
  model {
    alpha ~ normal(0,10); # priors (these are variances not precision)
    beta ~ normal(0,10);

    y ~ normal(alpha + beta * x, sigma);
  }
"
# -------------------------------------------------- #


# simulate data
n <- 100
x <- rnorm(n,0,1) 
alpha <- 1.250000 
beta <- 2.500000
y <- alpha + beta * x + rnorm(n)

# create data list
data <- list(y = y, x=x, n=n)

# fit linear model
summary(lm(y~x))

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



# set time start variable
time1 <- Sys.time()
print(Sys.time() - time1)


