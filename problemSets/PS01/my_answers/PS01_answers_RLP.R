#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("C:/Users/Usuario/Documents/GitHub/StatsII_2026/problemSets/PS01/my_answers")

#####################
# Problem 1
#####################

#The goal is to implement Kolmogorov–Smirnov test in R myself, assuming a normal
#distribution, and to apply it to 1,000 Cauchy random variables.

#I will start generating my simulated data. "set.seed" ensures replicability,
#rcaucy(1000) generates the observations for X.

set.seed(123)
x <- rcauchy(1000)

#I will now generate a function that implements manually the Kolmogorov-Smirnov test.
#The function will first sort the data, then construct the Empirical Cumulative
#Distribution Function (ECDF), and finally compare it to the theoretical normal
#distribution function.
#The D statistic is thus the maximal absolute difference between the ECDF and
#the theoretical normal distribution function.

ks_manual <- function(data) {
  data <- sort(data)
  ECDF <- ecdf(data)
  D <- max(abs(ECDF(data) - pnorm(data)))
  return(D)
}

ks_manual(x)

#Let's assign the function to the statistic D.

D <- ks_manual(x)
D

#Now I use the "ks.test()" function to see whether my own function that I have
#elaborated manually is right.

ks.test(x, "pnorm")

#####################
# Problem 2
#####################

#I will use the code given to generate the data for this problem.

set.seed (123)
data2 <- data.frame(x = runif(200, 1, 10))
data2$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

#Now I need to estimate an OLS regression using BFGS.

#First, I define a function for the sum of residual squares for a lineal model.
#Given values for the beta parameters, it will calculate the distance between
#the observed values and the predicted ones (by the model).

sosr <- function(beta, x, y) {
  y_hat <- beta[1] + beta[2] * x
  sum((y - y_hat)^2)
}

#I will start with values of 0, so as to have a starting point for the algorithm.

beta0 <- c(0, 0)

#Finally, I use the BFGS algorithm to find the values of beta that minimise the
#function for the sum of residual squares for lineal model (that I have previously
#defined).

algo_bfgs <- optim(beta0, sosr,
                  x = data2$x,
                  y = data2$y,
                  method = "BFGS")

#The element "$par" will contain the values that minimise the function, this is,
#those that correspond to the estimated OLS coefficients.

str(algo_bfgs)
algo_bfgs$par

#As a caution check, I run an actual OLS model as is usually done automatically
#in R.

lm_model <- lm(y ~ x, data=data2)
summary(lm_model)
library(stargazer)
stargazer(lm_model)
