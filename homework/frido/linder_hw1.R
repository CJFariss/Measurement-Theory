## ----echo=FALSE----------------------------------------------------------

## ----packages, echo=TRUE, message=FALSE----------------------------------
library(knitr, quietly = TRUE)
opts_chunk$set(echo=FALSE)
opts_chunk$set(tidy=FALSE)
library(edarf, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(randomForest, quietly = TRUE)

## ----cache=TRUE, fig.height=4, fig.cap='Bivariate Scatterplots with Y', fig.pos='[ht!]'----
setwd("~/Dropbox/Spring2015/PLSC597B_Measurement/hw1")
dat <- read.table('_data_problem_Set_01.csv', sep = ",", header = T)

# Scatterplot with each x
pdat <- data_frame(x = c(dat$x1, dat$x2, dat$x3), y = rep(dat$y, 3), 
                   var = rep(c("x1", "x2", "x3"), each = nrow(dat)))
p <- ggplot(data = pdat, aes(x, y))
p <- p + geom_point(aes(x, y, color = var))
p <- p + facet_wrap( ~ var)
p

## ----cache=TRUE, fig.height=5, fig.cap='Partial Dependence Plot from Random Forest', fig.pos='[ht!]'----
# Look at correlations
fit <- randomForest(y ~ x1 + x2 + x3, data = dat, importance = T)
imp <- as.data.frame(fit$importance)
imp$var <- rownames(imp)
pd1 <- partial_dependence(fit, dat, c("x1"), cutoff = 22)
pd2 <- partial_dependence(fit, dat, c("x2"), cutoff = 21)
pd3 <- partial_dependence(fit, dat, c("x3"), cutoff = 24)
colnames(pd1) <- colnames(pd2) <- colnames(pd3) <- c("x", "y_hat")
pd <- rbind(pd1, pd2, pd3)
pd$var <- c(rep("x1", 22), rep("x2", 21), rep("x3", 24))

p <- ggplot(pd, aes(x, y_hat))
p <- p + facet_wrap(~ var, ncol = 1)
p <- p + geom_line()
p <- p + geom_point()
p

## ----all-code, ref.label=all_labels(), echo=TRUE, eval=FALSE, fig.keep='none'----
## NA

