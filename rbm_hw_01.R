library(foreign)
data = read.csv("_data_problem_Set_01.csv")

names(data)
summary(data) 

data$unit

plot(density(data$x1))
plot(density(data$x2))
plot(density(data$x3))

# all look pretty normally distributed

plot(density(data$y))
#Clear separation between high and low; looks like gamma

log.y = log(data$y)
plot(density(log.y))

# normal with fat tail; student-t esque 


cor(data$x1, data$y)
cor(data$x2, data$y)
cor(data$x3, data$y)

# Not much going on, it's hard to interpret

#Look at correlation with transformed variable
cor(data$x1, log.y)
cor(data$x2, log.y)
cor(data$x3, log.y)

#All of the correlations look much nicer and are on a more managable magnitude


frequentist = lm(data$y ~ data$x1 + data$x2 + data$x3)
summary(frequentist)

#NOTHING IS SIGNIFICANT AT THE P<.05 LEVEL

l.frequentist = lm(log.y ~ data$x1 + data$x2 + data$x3)
summary(l.frequentist)

# ALL THE STARS! THE LOG-LEVEL MODEL HAS EVERY IV BEING SIGNIFICANT.

x1.x2 = data$x1*data$x2
x1.x3 = data$x1*data$x3
x2.x3 = data$x2*data$x3

l2.freq = lm(log.y ~ data$x1 + data$x2 + data$x3 + x1.x2 + x1.x3 + x2.x3)
summary(l2.freq)

# doesn't look like there are diadic interactions and all other significants disappear


#### SOLUTION ####

# These data are derived from the gravitational force equation: 
# F(g) = gravitationalConstant* ((mass1 * mass2) / radius^2)

# This data set was used as a data exploration project by the Professor; it was given to students and they were instructed
# to play around with it for ~1 or 2 hours and look for statistical relationships.

logX1 = ln(data$x1)
logX2 = ln(data$x2)
logX3 = ln(data$x3)

sol = lm(formula = data$y ~ logX1 + logX2 + logX3)
summary(sol)

# And now we have an R^2 statistic of 1. Why can't social science data do this?
