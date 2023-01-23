# install.packages("datarium")
library(tidyverse)
library(datarium)
data("marketing", package = "datarium")
marketing <- as_tibble(marketing) # better printing

# see the help file for the data
?marketing

# frequentist linear regression
fit_lm <- lm(
  sales ~ youtube + facebook + newspaper, 
  data = marketing
)

summary(fit_lm)
confint(fit_lm)

marketing

# Bayesian Analysis # 
library(tidyverse)
library(R2jags)
library(coda)

set.seed(99)

x <- as.matrix(marketing[1:3])
y <- marketing$sales
x[1,1]
x

#JAGS model#
normal_model <- function(){
  for (i in 1:n){
    y[i] ~ dnorm(mu[i], tau.x)
    mu[i] <- alpha + beta_yt*x[i,1] + beta_fb*(x[i,2]) + beta_news*(x[i,3])
  }
  tau.x <- 1/sigma^2
  
  alpha ~ dnorm(0,0.0001) #intercept
  beta_yt ~ dnorm(0, 0.0001) #slope
  beta_fb ~ dnorm(0, 0.0001)
  beta_news ~ dnorm(0, 0.0001)
  sigma ~ dunif(0, 356)
}

jags_data <- list(y=y, x = x, n = nrow(marketing))
jags_data
jags_params <- c("alpha", "beta_yt", "beta_fb", "beta_news")

jagsfit <- jags(
  data = jags_data, 
  parameters.to.save = jags_params,
  n.iter = 10000, 
  model.file = normal_model
)
  
jagsfit.mcmc <- as.mcmc(jagsfit)
summary(jagsfit.mcmc)
plot(jagsfit.mcmc)
densplot(jagsfit.mcmc) #keep alpha, betas but focus on betas

#0 is only included in CI for newspaper, no significant dif

#####
summary.jagsfit.mcmc <- summary(jagsfit.mcmc)

summary(fit_lm)
round(confint(fit_lm),3)
plot(jagsfit.mcmc)
densplot(jagsfit.mcmc)

round(summary.jagsfit.mcmc$quantiles,3)

#Table for Frequentist
summary(fit_lm)$coefficients

#Table for Bayesian





  