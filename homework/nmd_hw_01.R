# Nicholas Dietrich
# Measurement Theory
# Problem Set 1

# First I set the directory where the problem set lives
setwd("/Users/nicholasdietrich/Desktop/Measurement Theory")
# You'll have to change this if you're following along at home.

# First let's read in the data
?read.csv
data <- read.csv(file="_data_problem_Set_01.csv")
names(data)
attach(data)

# Now let's get a sense for how the variables are distributed
summary(x1)
summary(x2)
summary(x3)
summary(y) # y has a weird distribution.
plot(density(x1))
plot(density(x2))
plot(density(x3))
plot(density(y)) # bimodal?

# The x vars appear to be distributed somewhat differently over the same range.

# Can I plot all three x density curves in one figure?
plot.new() #open a blank plot window
plot.window(xlim=c(0, 100), ylim=c(0,.015)) #designate dimensions for plot
lines(x=density(x1)) #add one line for each x variable
lines(x=density(x2))
lines(x=density(x3))

# Now we add axes and labels...
axis(1, #Indicates which side of the plot you want to add axis label (1=bottom, 2=left, 3=top, 4=right
     at=seq(from=0,to=100,by=10), #Indicates where to add labels
     label=seq(from=0,to=100,by=10),
     lty=1, #Change axis line type
     col="black", #Change axis and tick mark color
     las=0,) #Change whether labels are parallel (=0) or perpendicular (=1) to axis
labels <- seq(from=0,to=.015,by=.003)
axis(2, at=labels, label=labels, lty=1, col="black", las=0)
box() #Put a box around it
title(main="Density of x variables", #Main title
      xlab="Value", # Add x-axis label
      ylab="Density", # Add y-axis label 
      font.main=1, #Set font type for title
      font.lab=1, # Set font type for x-y labels
      cex.main=1, # Change font size for main title
      cex.sub=1, # Change font size for subtitle
      cex.lab = 1, # Change font size for x-y labels
      col.main="black", #Change color of main title
      col.sub="black", # Change color of subtitle
      col.lab = "black" # Change color of x-y labels
)
# Each x var is centered at a different value

# Let's check to see how the variables correlate with each other.
cor(data)
# x2 has a moderate negative correlation with y.
# The other two x vars are not correlated with y very strongly.
# Some of the x vars are correlated with each other moderately to weakly.

# How well do the x variables predict y?
?lm
lm_x1 <- lm(y ~ x1)
lm_x2 <- lm(y ~ x2)
lm_x3 <- lm(y ~ x3)
lm_x <- lm(y ~ x1+x2+x3)

summary(lm_x1)
summary(lm_x2)
summary(lm_x3)
# In a bivariate linear regression, only x2 is significant.
# x2 predicts y negatively.

summary(lm_x)
# In a multivariate linear regression, x2 remains the only significant independent var.
# Higher values of x2 are associated with lower values of y.
# x2 is the only variable with any predictive power for y, despite the correlation
# between the x variables. The small sample size and heavily skewed distribution of y
# make it difficult to know how reliable this relationship is.

# Hey, let's try a random forest.
library(devtools)
library(edarf)
library(randomForest)

fit_rf <- randomForest(y ~ x1+x2+x3) # Set up the random forest
imp <- fit_rf$importance[, 1] # Find importance of each variable
plot_imp(names(imp), imp) # I stole all of this code from Zach's website

# The exploratory random forest analysis confirms our suspicions that x2 is
# the most useful of the x variables for predicting y.
