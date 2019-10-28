#Simple Regressions Assignment 1

#rnorm generates distribution of numbers between 0 and 1, just like rand()
x <- rnorm(10, mean = 2, sd = 3) 
mean(x)
sd(x)
print(x)
set.seed(42) #by setting seed, can replicate the exact same values as other people doing this.
rnorm(10)

#Generate errors from a single unknown parameter, which is standard deviation
beta_0 <- 2 #this is the y-intercept
beta_1 <- 3 #slope?
sigma <- 1  #standard deviation
n_obs <- 1000

#Now build up the right side of the equation
# create predictor variable:
x_1 <- as.numeric(scale(rnorm(n_obs)))

#use the linear model to generate y values based on parameters we made
y <- beta_0 + beta_1 * x_1 + rnorm(n_obs, mean = 0, sd = sigma)

library(tidyverse)
#scatterplot
#THIS IS WHAT I NEED TO DO WITH MY SPIKE TIME DATA THAT'S NOT WITHIN A VARIABLE!! :
ggplot(data = NULL, aes(x_1, y)) + geom_point()
# The term coefficients means Beta parameters and residuals = observation-by-observation errors (squard deviations), and this is what they are called in R as well
fit1 <- lm(y ~ x_1)
fit1$coefficients #fitting coefficients to the x predictor to get parameter estimates?
#Coefficients are similar to our original betas because the observations were 1000! so lots of data


#Practice Exercises #1
#use a data frame and add 2 more continuous predictors
PredVars=data.frame("FirstPredictor" = integer(0),"SecondPredictor" = integer(0), "ThirdPredictor" = integer(0))

#Generating Parameters
beta_0 <- 10
beta_1 <- 4
sigma <- 8
n_obs <- 500

#Generating Predictor Data in a data frame
x_1 <- as.numeric(scale(rnorm(n_obs)))
x_2 <- as.numeric(scale(rnorm(n_obs)))
x_3 <- as.numeric(scale(rnorm(n_obs)))
PredVars = data.frame("FirstPredictor" = x_1, "SecondPredictor" = x_2, "ThirdPredictor" = x_3)

PredVars <- mutate(PredVars,YAnswer = beta_0 + x_1*beta_1 + x_2*beta_1 + x_3*beta_1 + sigma)
ggplot(data = PredVars, aes(y = PredVars$YAnswer,x = x_1,x_2,x_3)) + geom_point()

fit_x1 <- lm(PredVars$YAnswer ~ x_1)
fit_x2 <- lm(PredVars$YAnswer ~ x_2)
fit_x3 <- lm(PredVars$YAnswer ~ x_3)