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

lapply(c("nnet", "MASS"),  pkgTest)

# set wd for current folder
setwd("C:/Users/Usuario/Documents/GitHub/StatsII_2026/problemSets/PS03/my_answers_RLP")

#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/gdpChange.csv", stringsAsFactors = F)

View(gdp_data)

#I have seen that gpd_data$GDPWdiff is a numeric variable. I will transform it 
#into a character variable with three possible categories: "positive", "negative",
#or "no change".

#As the are too little cases with a variation of 0, I consider that a difference
#of 10 or -10 = no change.

library(dplyr)

gdp_data <- gdp_data %>%
  mutate(GDPWdiff_char = case_when(
    GDPWdiff > 10 ~ "positive",
    GDPWdiff < -10 ~ "negative",
    TRUE           ~ "no change"
  ))

table(gdp_data$GDPWdiff_char)

#Now I can construct and interpret an unordered multinomial logit with GDPWdiff_char
#as the output and ”no change” as the reference category, including the estimated
#cutoff points and coefficients.

#I define "no change" as the reference category
gdp_data$GDPWdiff_char <- relevel(as.factor(gdp_data$GDPWdiff_char), ref = "no change")

model_multinom <- multinom(GDPWdiff_char ~ REG + OIL, data = gdp_data)
summary(model_multinom)

library(stargazer)

stargazer(model_multinom)

#The next step is constructing and interpreting an ordered multinomial logit with
#GDPWdiff as the outcome variable, including the estimated cutoff points and
#coefficients.

#First, I put the categories of gdp_data$GDPWdiff_car in order:

gdp_data$GDP_ord <- factor(gdp_data$GDPWdiff_car, 
                           levels = c("negative", "no change", "positive"), 
                           ordered = TRUE)

#I run the model with the new gdp_data$GDPWdiff_ord
model_ord <- polr(GDP_ord ~ REG + OIL, data = gdp_data, Hess = TRUE)
summary(model_ord)

stargazer(model_ord)

#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/MexicoMuniData.csv")

View(mexico_elections)

#1st subquestion is the poisson model (and the other two subquestions are directly
#solved in the LaTex file:

model_poisson_mex <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, 
                     family = poisson(link = "log"), 
                     data = mexico_elections)

summary(model_poisson_mex)

stargazer(model_poisson_mex, 
          type = "text",           
          report = "vct*",         
          column.labels = c("Poisson Model"),
          single.row = TRUE)       

library(modelsummary)
modelsummary(model_poisson_mex, stars = TRUE)
