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

LM_Fun <- function(beta0,beta1,sigma,x_1,n_obs){
  PredVars1 <- data.frame("YAnswer" = integer(n_obs))
  PredVars1 <- mutate(PredVars1,YAnswer = beta_0 + x_1*beta_1 + sigma)
  ggplot(data = PredVars, aes(y = YAnswer,x = x_1)) + geom_point()
  output <- PredVars1
}
  
#Was supposed to add lm(PredVars1$YAnswer ~ x_1) to previous function


sim_simple_lm <- function(n_obs, beta_0, beta_1, sigma) { x_1 <- as.numeric(scale(rnorm(n_obs))) 
errors <- rnorm(n_obs, 0, sigma) 
y <- beta_0 + beta_1 * x_1 + errors 
fit <- lm(y ~ x_1) 
  output <- list(n_obs = n_obs, 
                 beta_0 = beta_0, 
                 beta_0_hat = fit$coef["(Intercept)"], 
                 beta_1 = beta_1, 
                 beta_1_hat = fit$coef["x_1"], 
                 sigma = sigma, 
                 sigma_hat = sd(fit$resid)) 
return(output)
}


set.seed(854)
sim_simple_lm(10,1,2,10)


n_sims <- 1e4 #n_sims = 10000, but in scientific notation
sim_results <- data.frame(sim = 1:n_sims,
                          beta_0_hat = NA,
                          beta_1_hat = NA,
                          sigma_hat = NA)
for(this_sim in 1:n_sims) {
  if(this_sim %% 1000 == 0) {cat("starting simulation", this_sim, "\n")}
  #do something every 1000th iteration is what Var %% Num == 0 means. Num = 1000 in this case but it could be every 10 or every 50 iterations for example.
  #If you type in "this_sim %% 1000" you get 0.
  this_fit <- sim_simple_lm(10,1,2,10)
  sim_results[this_sim,2:4] <- c(this_fit$beta_0_hat,
                                 this_fit$beta_1_hat,
                                 this_fit$sigma_hat)
}
summary(sim_results[,2:4])



this_fit1 <- as.data.frame(this_fit)
GatheredFit <- gather(this_fit1)
ggplot(data = NULL, aes(y = GatheredFit, x = 1:10)) + geom_point()
plot(GatherFit)
    

# Dummy coding

mysleep <- sleep 
levels(mysleep$group) <- c("pre", "post") 
head(mysleep)
summary(mysleep)
summary(lm(extra ~ group, data = mysleep))
mysleep %>% group_by(group) %>% summarize(mean = mean(extra))
mysleep <- mysleep %>% mutate(group.num = as.numeric(group) - 1)
summary(lm(extra ~ group.num, mysleep))

# Recall from our previous discussion of variable scaling that centering a variable means transforming it such that the mean is zero (or extremely close to zero, such as when we apply R’s scale() function). Now, think about how this relates to the interpretation of our linear model coeﬃcients. How do we interpret the intercept? 

# the intercept is the predicted value of y when all predictors have a value of zero. This is just simple arithmetic because when a value of a predictor is zero, then it adds nothing to the predicted value of y because a slope parameter times zero is obviously always zero. So if all predictors are zero, the only terms left in our equation are the intercept and error terms. Now, think about the impact of centering a categorical predictor like group. In our previous model, the intercept corresponded to the mean value of the “pre” condition. What does the intercept correspond to when we center the group variable? 

mysleep_norm <- as.data.frame(mysleep)
mysleep_norm <- mysleep_norm %>% mutate(extra = rnorm(extra, mean = 0))
summary(mysleep_norm)
head(mysleep_norm)
mysleep_norm <- mysleep_norm %>% mutate(rnorm(group.c, mean = 0)) #creates 2 levels?
mysleep_norm$group.c <- as.numeric(mysleep$group) - 1.5 
xtabs(~ group + group.c, mysleep_norm) #cross tabulate function, contingency table
#creates 2 levels of 0.5 and -0.5???

#2.Fit a model with the new group.c predictor as the only predictor of extra. How is it diﬀerent and similar to the original model with group or group.num?


summary(lm(mysleep$extra ~ mysleep_norm$group.c, mysleep))

# 3. What does the new intercept correspond to? Calculate that value with another method to demonstrate this interpretation of the coeﬃcient.



#Factor Coding

contrasts(iris$Species)
                                        