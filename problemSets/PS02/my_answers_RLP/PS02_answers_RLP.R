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

# load data
load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))

#I will view the dataset, just to know what I am working with:

View(climateSupport)

#Now, I will reply to the questions:

#1. Remember, we are interested in predicting the likelihood of an individual supporting
#a policy based on the number of countries participating and the possible sanctions for
#non-compliance.
#Fit an additive model. Provide the summary output, the global null hypothesis,
#and p-value. Please describe the results and provide a conclusion.

#The global null hypothesis would state that neither the number of countries nor
#the sanctions have any effect on support for a policy. This would mean that all
#the coefficients are 0.

#This null model would be defined by this function:

model_null <- glm(choice ~ 1,
                  data = climateSupport,
                  family = binomial)
anova_test_q1 <- anova(model_null, model_q1, test = "Chisq")
head(climateSupport)
str(climateSupport)

unique(climateSupport$countries)
unique(climateSupport$sanctions)

model_q1 <- glm(choice ~ countries + sanctions,
                data = climateSupport,
                family = binomial(link = "logit"))
summary(model_q1)
stargazer(model_q1)
library(stargazer)


stargazer(anova_test_q1)

#The ANOVA test gives us a a p-value (for the whole model) of 0.001, which is great!

#Question 2-

#a) Although this is maybe not the exact exercise Jeff wants us to do, we can use
#the predict() function:

p1_q2a_160 <- predict(model_q1,
              newdata = data.frame(countries="160 of 192", sanctions="5%"),
              type="link")

p2_q2a_160 <- predict(model_q1,
              newdata = data.frame(countries="160 of 192", sanctions="15%"),
              type="link")

answer_q2_a <- exp(p2_q2a_160 - p1_q2a_160)
print(answer_q2_a)

#b)

p1_q2_20 <- predict(model_q1,
                     newdata = data.frame(countries="20 of 192", sanctions="5%"),
                     type="link")

p2_q2_20 <- predict(model_q1,
                     newdata = data.frame(countries="20 of 192", sanctions="15%"),
                     type="link")

answer_q2_b <- exp(p2_q2_20 - p1_q2_20)
print(answer_q2_b)

#c) In this case, sanctions are held constant at "none"

p1_q3_80 <- predict(model_q1,
             newdata = data.frame(countries="80 of 192", sanctions="None"),
             type="response")
print(p1_q3_80)

#Question 3- I will create an interaction model:

model_q3 <- glm(choice ~ countries * sanctions, data=climateSupport, family=binomial)
summary(model_q3)
stargazer(model_q3)

#Now we run an ANOVA test to see if this model is appropriate compared to the
#one we already have.

anova_test_q3 <- anova(model_q1, model_q3, test = "Chisq")
summary(anova_test_q3)
stargazer(anova_test_q3)
