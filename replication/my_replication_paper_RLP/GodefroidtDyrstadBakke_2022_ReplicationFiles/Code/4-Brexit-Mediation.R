#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# PROJECT: The Past, Brexit, and the Future in Northern Ireland: A Quasi-Experiment
# AUTHOR: ** anonymized for review **
# CONTACT: ** anonymized for review **
# LAST MODIFIED: February 8, 2022

# INFO: This R file contains the code necessary to replicate the 
# mediation analyses (ACMEs) reported in the main paper

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Settings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
getwd()
set.seed(1234)


# Installing and loading packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if(length(new.pkg)) install.packages(new.pkg, dependencies=TRUE)
sapply(pkg, require, character.only=TRUE)
}

pkgs <- c("haven", "Hmisc", "mediation", "dplyr", "WeightIt") 
ipak(pkgs)


# Load and subset the data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Brexit <- read_dta("Data/Brexit.dta")

#select the data
Brexit <- Brexit %>% dplyr::select(age, employment_1, exposure, #failed balance tests
                                   referendum, referendum2, time_zero, date, #referendum indicators
                                   cause_1,cause_2,cause_3,cause_4,cause_5,cause_6,cause_7,cause_8, #DVs 1 (med.)
                                   remain, independence, unification) #DVs 2
summary(Brexit)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### Causal Mediation Effects ########  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#models with entropy matching
W.out <- weightit(referendum ~ age + employment_1 + exposure,
                  data = Brexit, estimand = "ATT", method = "ebal")

#total effect (same for both models)
fit.totaleffect = lm(unification ~ referendum, Brexit, weights = W.out$w)
summary(fit.totaleffect)


#### Fit outcome and mediator models ####
### Cause_8: Partitioning of Ireland ###
cause.8.fit <- lm(cause_8 ~ referendum + age + employment_1 + exposure, 
                  data = Brexit) #cause_8 mediator
unify.8.fit <- glm(unification ~ cause_8 + referendum + age + employment_1 + exposure,
                   data = Brexit, family = binomial("probit")) #unification outcome
remain.8.fit <- glm(remain ~ cause_8 + referendum + age + employment_1 + exposure,
                    data = Brexit, family = binomial("probit")) #remain outcome

### Cause_7: Illegitimate rule of Westminster ###
cause.7.fit <- lm(cause_7 ~ referendum + age + employment_1 + exposure,
                  data = Brexit) #cause_7 mediator
unify.7.fit <- glm(unification ~ cause_7 + referendum + age + employment_1 + exposure, 
                   data = Brexit, family = binomial("probit")) #unification outcome
remain.7.fit <- glm(remain ~ cause_7 + referendum + age + employment_1 + exposure,
                    data = Brexit, family = binomial("probit")) #remain outcome

#### Fit ACME models ####
### Cause_8: Partitioning of Ireland ###
cause.8.unify <- mediate(cause.8.fit, unify.8.fit, 
                         treat = "referendum", mediator = "cause_8",
                         robustSE = TRUE, sims = 1000)
cause.8.remain <- mediate(cause.8.fit, remain.8.fit, 
                          treat = "referendum", mediator = "cause_8",
                          robustSE = TRUE, sims = 1000)

### Cause_7: Illegitimate rule of Westminster ###
cause.7.unify <- mediate(cause.7.fit, unify.7.fit, 
                         treat = "referendum", mediator = "cause_7",
                         robustSE = TRUE, sims = 1000)
cause.7.remain <- mediate(cause.7.fit, remain.7.fit, 
                          treat = "referendum", mediator = "cause_7",)
 
#### Get results ####
summary(cause.8.unify)
summary(cause.8.remain)
summary(cause.7.unify)
summary(cause.7.remain)

