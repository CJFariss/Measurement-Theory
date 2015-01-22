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
    real y;
  }
  model {
    y ~ logistic(0,1);
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



