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
  }
  parameters {
    real mu;
    real mu10;
    real mu100;
    real mu1;
  }
  transformed parameters {
  }
  model {
    mu ~ normal(0,1); # priors (these are variances not precision)
    mu10 ~ normal(0,10);
    mu100 ~ normal(0,100);
    mu1 ~ normal(0,0.1);
  }
"
# -------------------------------------------------- #



# fit stan model
fit <- stan(model_code = model, iter = 1000, chains = 4)

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


par(mfrow=c(2,2))
truehist(output$mu)
truehist(output$mu10)
truehist(output$mu100)
truehist(output$mu1)

