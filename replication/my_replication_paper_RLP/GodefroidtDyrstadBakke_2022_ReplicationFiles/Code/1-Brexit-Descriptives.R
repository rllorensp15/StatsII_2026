#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# PROJECT: The Past, Brexit, and the Future in Northern Ireland: A Quasi-Experiment
# AUTHOR: ** anonymized for review **
# CONTACT: ** anonymized for review **
# LAST MODIFIED: February 8, 2022

# INFO: This R file contains the code necessary to replicate Table 1 (Desc. Statistics)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())


# Installing and loading packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if(length(new.pkg)) install.packages(new.pkg, dependencies=TRUE)
sapply(pkg, require, character.only=TRUE)
}

pkgs <- c("haven", "dplyr", "tidyr", "vtable") 
ipak(pkgs)


# Load and subset data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Brexit <- read_dta("Data/Brexit.dta")

#select the data
Brexit <- Brexit %>% dplyr::select(male, age, education, employment_1, exposure,
                                   referendum, referendum2, time_zero,
                                   cause_1,cause_2,cause_3,cause_4,cause_5,cause_6,cause_7,cause_8,
                                   remain, independence, unification)
summary(Brexit)


# Inspect data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# N participants per condition
Brexit %>% drop_na(cause_7) %>% group_by(referendum) %>% summarise(n = n())
Brexit %>% drop_na(remain) %>% group_by(referendum) %>% summarise(n = n())

# Summary statistics for outcome variables (Table 1)
sumstat <- Brexit %>% 
  
  # Select and rename outcome variables 
  dplyr::select(
    `Economic inequalities and poverty` = cause_1,
    `Community or Religious Inequalities` = cause_2,
    `Government repression and discrimination` = cause_3,
    `Lack of real democracy in NI` = cause_4,
    `Extremist Republicans` = cause_5,
    `Extremist Loyalists` = cause_6,
    `Illegitimate rule from Westminster` = cause_7,
    `The partition of Ireland` = cause_8,
    `Remain part of the UK` = remain,
    `Become an independent state` = independence,
    `Unify with the rest of Ireland` = unification
    )

  # Write .csv table with summary stats
sumtable(sumstat,
         summ = list(
           c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'),
           c('notNA(x)','mean(x)')
         ),
         summ.names = list(
           c('N','Mean','SD','Min','Max'),
           c('Count','Percent')
         ), out = "csv", file = "sumstat.csv")
