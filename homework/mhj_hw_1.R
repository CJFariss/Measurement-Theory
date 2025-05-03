setwd("C:/Users/minnie/Dropbox/Penn State/2nd year/measurement/measurement/homework")

library(foreign)
library(car)
data <- read.csv("hw_01.csv")
attach(data)

View(data)

## my comments on the relationship between variables are written throughout the script


# I first explore the data by looking at the distribution of the variables 
plot(density(x1))  
plot(density(x2))
plot(density(x3))
plot(density(y))  # I sense that there is an outlier in y
plot(y) #there is an outlier 

# now I plot x variables against the y varaibel 
plot(x1, y)
lines(lowess(x1, y)) #positive relationship between x1 and y 
plot(x2, y)
lines(lowess(x2, y)) # slightly negative relationship x2 and y 
plot(x3, y)
lines(lowess(x3, y)) # almost no relationship x3 and y 

# I run some linear models 
m1 <- lm(y~x1)
summary(m1) # positive coefficient, but not statistically significant, very low model fit according to R-squared

m2 <- lm(y~x2)
summary(m2) # negative coefficient, statistically significant at 95% confidence level, slightly better model fit than x1

m3 <- lm(y~x3)
summary(m3) # negative coefficient, not statistically significant, very low model fit 

m4 <- lm(y~x1+x2)
summary(m4) # only x2 statistically significant (negative); model fit isn't too good 

m5 <- lm(y~x2+x3)
summary(m5) #only x2 statistically significant and the effect is bigger and negative; model fit has improved a little bit

m6 <- lm(y~x1+x3)
summary(m6) # neither x variable significant. Not a good model fith 

m7 <- lm(y~x1+x2+x3)
summary(m7) # only x2 is statistically significant at the 95% confidence level, not a good model fit 

# correlation between y and x variables
cor(y, x1) # very low correlation

cor(y, x2) #moderate negative correlation 

cor(y, x3) # very low correlation 

# correlation between x variables 
cor(x1, x2) # very low negative correlation 

cor(x3, x2) #moderate positive correlation 

cor(x1, x3)  #low negative correlation 


# look at the fit of m7 (the one with all variables)
plot(m7)  #unit 9 is an outlier 

influencePlot(m7)  ## unit 9 seems to be an outlier with very high studentized residual 

data1 <- data[-9,]  ### seems like there is an outlier (unit 9) so I omit the outlier and rerun the models 

# I explore the distribution of the variables and the relationship between the variables in the unit9-omitted dataset
plot(density(data1$y)) #looks more like normal now 
plot(data1$y) #little bit concentrated at the bottom but looks dispersed 

plot(data1$x1,data1$y)
lines(lowess(data1$x1,data1$y)) # positive relationship
plot(data1$x2,data1$y)
lines(lowess(data1$x2,data1$y)) # very slightly positive and then negative relationship
plot(data1$x3,data1$y)
lines(lowess(data1$x3,data1$y)) # slightly positive and then negative relationship 

cor(data1$x1, data1$y) # high positive correlation 
cor(data1$x2, data1$y) # moderate negative correlation
cor(data1$x3, data1$y) # very low negative correlation

#I run ols models with the new data (unit 9 omitted)
m11 <- lm(y~x1, data=data1)
summary(m11) #x1 positive and statistically significant (p-value virtually 0); good model fit (R-squared is 0.51)

m12 <- lm(y~x2, data=data1)
summary(m12) #x2 negative and statistically significant at 95% confidence level. model fit did not change much from the non-omitted model

m13 <- lm(y~x3, data=data1)
summary(m13) # x3 statistically still insignificant; model fit not good

m14 <- lm(y~x1+x2, data=data1)
summary(m14) # both significant; model fit increased from only having x1 

m15 <- lm(y~x2+x3, data=data1)
summary(m15) #only x2 significant 

m16 <- lm(y~x1+x3, data=data1)
summary(m16) # only x1 statistically significant and the model fit is quite good 

m17 <- lm(y~x1+x2+x3, data=data1)
summary(m17) 
# all x variables significant to some degree (x1 and x2'sp-value are virtually 0, and x3 significant at 95% confidence level)
# The model fit is also much better with the outlier exclued -- R-squared is 0.7066 now. 
# It seems that x1 is most highly related the y variable. 

cor(data1$x1, data1$x2) # negative 
cor(data1$x2, data1$x3) #positive 
cor(data1$x1, data1$x3) #negative 

# now I make x variables as the dependent variable 

mod1 <- lm(x1~ y+x2 + x3, data=data1)
summary(mod1)

mod2 <- lm(x2~ y+x1 + x3, data=data1)
summary(mod2)

mod1 <- lm(x3 ~ y+x2 + x1, data=data1)
summary(mod1)

# With the x variables as the dependent variable, we see that the x variables are related with each other as well. 
# expecially, variable x2 is statistically significant at the 99% confidence level when x1 and x3 are dependent variables.
# also it is interesting how altough the correlation between x1 and x2 is negative, the coefficient of x1 when x2 is the dependent variable is positive.
# since all variables are significant when y, x1, x2, and x3 are each modeled as the dependent variable,
# it may be that they are all very related to one another.  
