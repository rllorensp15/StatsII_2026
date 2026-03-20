#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# PROJECT: The Past, Brexit, and the Future in Northern Ireland: A Quasi-Experiment
# AUTHOR: ** anonymized for review **
# CONTACT: ** anonymized for review **
# LAST MODIFIED: February 8, 2022

# INFO: This R file contains the code necessary to replicate the 
# RDDs reported in the main paper (plots)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Settings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
set.seed(1234)
Sys.setlocale("LC_TIME", "C")


# Installing and loading packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if(length(new.pkg)) install.packages(new.pkg, dependencies=TRUE)
sapply(pkg, require, character.only=TRUE)
}

pkgs <- c("haven", "dplyr") 
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
####### Perceptions of the Causes ########  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### 1. RDDs: Economic and political causes ####

# Cause 1: Economic inequalities and poverty
Brexit.1 <- Brexit %>% drop_na(cause_1, employment_1)
W.out.1 <- weightit(referendum ~ age + employment_1 + exposure,
                    data = Brexit.1, estimand = "ATT", method = "ebal") #create weights
econ.rdd = lm(cause_1 ~ time_zero+referendum+(time_zero*referendum), data = Brexit.1,
              weights = W.out.1$w)
Brexit.1$econ.rdd.pred <- predict(econ.rdd)
summary(econ.rdd)

# Cause 2: Community or Religious Inequalities
Brexit.2 <- Brexit %>% drop_na(cause_2, employment_1)
W.out.2 <- weightit(referendum ~ age + employment_1 + exposure,
                    data = Brexit.2, estimand = "ATT", method = "ebal") #create weights
comm.rdd = lm(cause_2 ~ time_zero+referendum+(time_zero*referendum), data = Brexit.2,
              weights = W.out.2$w)
Brexit.2$comm.rdd.pred <- predict(comm.rdd)
summary(comm.rdd)

# Cause 3: Government repression and discrimination
Brexit.3 <- Brexit %>% drop_na(cause_3, employment_1)
W.out.3 <- weightit(referendum ~ age + employment_1 + exposure,
                    data = Brexit.3, estimand = "ATT", method = "ebal") #create weights
disc.rdd = lm(cause_3 ~ time_zero+referendum+(time_zero*referendum), data = Brexit.3,
              weights = W.out.3$w)
Brexit.3$disc.rdd.pred <- predict(disc.rdd)
summary(disc.rdd)

# Cause 4: Lack of real democracy in NI
Brexit.4 <- Brexit %>% drop_na(cause_4, employment_1)
W.out.4 <- weightit(referendum ~ age + employment_1 + exposure,
                    data = Brexit.4, estimand = "ATT", method = "ebal") #create weights
dem.rdd = lm(cause_4 ~ time_zero+referendum+(time_zero*referendum), data = Brexit.4,
              weights = W.out.4$w)
Brexit.4$dem.rdd.pred <- predict(dem.rdd)
summary(dem.rdd)


#### 2. RDDs: Actor-based causes ####

# Cause 5: Extremist Republicans
Brexit.5 <- Brexit %>% drop_na(cause_5, employment_1)
W.out.5 <- weightit(referendum ~ age + employment_1 + exposure,
                    data = Brexit.5, estimand = "ATT", method = "ebal") #create weights
rep.rdd = lm(cause_5 ~ time_zero+referendum+(time_zero*referendum), data = Brexit.5,
             weights = W.out.5$w)
Brexit.5$rep.rdd.pred <- predict(rep.rdd)
summary(rep.rdd)

# Cause 6: Extremist Loyalists
Brexit.6 <- Brexit %>% drop_na(cause_6, employment_1)
W.out.6 <- weightit(referendum ~ age + employment_1 + exposure,
                    data = Brexit.6, estimand = "ATT", method = "ebal") #create weights
loy.rdd = lm(cause_6 ~ time_zero+referendum+(time_zero*referendum), data = Brexit.6,
             weights = W.out.6$w)
Brexit.6$loy.rdd.pred <- predict(loy.rdd)
summary(loy.rdd)

# Cause 7: Illegitimate rule from Westminster
Brexit.7 <- Brexit %>% drop_na(cause_7, employment_1)
W.out.7 <- weightit(referendum ~ age + employment_1 + exposure,
                    data = Brexit.7, estimand = "ATT", method = "ebal") #create weights
ill.rdd = lm(cause_7 ~ time_zero+referendum+(time_zero*referendum), data = Brexit.7,
             weights = W.out.7$w)
Brexit.7$ill.rdd.pred <- predict(ill.rdd)
summary(ill.rdd)

# Cause 8: The partition of Ireland
Brexit.8 <- Brexit %>% drop_na(cause_8, employment_1)
W.out.8 <- weightit(referendum ~ age + employment_1 + exposure,
                    data = Brexit.8, estimand = "ATT", method = "ebal") #create weights
part.rdd = lm(cause_8 ~ time_zero+referendum+(time_zero*referendum), data = Brexit.8,
             weights = W.out.8$w)
Brexit.8$part.rdd.pred <- predict(part.rdd)
summary(part.rdd)


#### Figure 4 #####

### Open plot environment
jpeg("Figures/Fig4.jpeg", width = 7, height = 10, units = 'in', res = 500)

### Set margins
par(mfrow=c(4,2), mar = c(3,3,3,2))

### PLOT 1: Cause 1 ###
plot(Brexit.1$date,Brexit.1$cause_1, 
     pch=1, frame.plot = FALSE,col="grey90",
     xaxs="i",yaxs="i",
     ylim=c(0.8,5.2), 
     main = "Economic Inequalities",
     xlab = "", ylab ="",
     font.main=2, cex.axis=1.3, cex.main=1.4) 
with(subset(Brexit.1, referendum==0),lines(date, econ.rdd.pred, col="black", lty=1, lwd=4))
with(subset(Brexit.1, referendum==1),lines(date, econ.rdd.pred, col="gray", lty=1, lwd=4))
abline(v=as.Date("2016-06-24"), col="black", lwd=2, lty=2)
text(as.Date("2016-05-31"), 3, "0.011(0.004)\n p=.002", cex = 1.3)
text(as.Date("2016-07-06"), 3, "-0.027(0.013)\n p=.039", cex = 1.3)

### PLOT 2: Cause 2 ###
plot(Brexit.2$date,Brexit.2$cause_2, 
     pch=1, frame.plot = FALSE,col="grey90",
     xaxs="i",yaxs="i",
     ylim=c(0.8,5.2), 
     main = "Community or Religious Inequalities",
     xlab = "", ylab ="",
     font.main=2, cex.axis=1.3, cex.main=1.4) 
with(subset(Brexit.2, referendum==0),lines(date, comm.rdd.pred, col="black", lty=1, lwd=4))
with(subset(Brexit.2, referendum==1),lines(date, comm.rdd.pred, col="gray", lty=1, lwd=4))
abline(v=as.Date("2016-06-24"), col="black", lwd=2, lty=2)
text(as.Date("2016-05-31"), 3, "0.006(0.003)\n p=.040", cex = 1.3)

# Plot 3: Cause 3
plot(Brexit.3$date,Brexit.3$cause_3, 
     pch=1, frame.plot = FALSE,col="grey90",
     xaxs="i",yaxs="i",
     ylim=c(0.8,5.2), 
     main = "Repression and discrimination",
     xlab = "", ylab ="",
     font.main=2, cex.axis=1.3, cex.main=1.4) 
with(subset(Brexit.3, referendum==0),lines(date, disc.rdd.pred, col="black", lty=1, lwd=4))
with(subset(Brexit.3, referendum==1),lines(date, disc.rdd.pred, col="gray", lty=1, lwd=4))
abline(v=as.Date("2016-06-24"), col="black", lwd=2, lty=2)
text(as.Date("2016-05-31"), 3, "0.009(0.003)\n p=.011", cex = 1.3)

# Plot 4: Cause 4
plot(Brexit.4$date,Brexit.4$cause_4, 
     pch=1, frame.plot = FALSE,col="grey90",
     xaxs="i",yaxs="i",
     ylim=c(0.8,5.2), 
     main = "Lack of real democracy in NI",
     xlab = "", ylab ="",
     font.main=2, cex.axis=1.3, cex.main=1.4) 
with(subset(Brexit.4, referendum==0),lines(date, dem.rdd.pred, col="black", lty=1, lwd=4))
with(subset(Brexit.4, referendum==1),lines(date, dem.rdd.pred, col="gray", lty=1, lwd=4))
abline(v=as.Date("2016-06-24"), col="black", lwd=2, lty=2)
text(as.Date("2016-05-31"), 3, "0.011(0.004)\n p=.002", cex = 1.3)

# Plot 5: Cause 5
plot(Brexit.5$date,Brexit.5$cause_5, 
     pch=1, frame.plot = FALSE,col="grey90",
     xaxs="i",yaxs="i",
     ylim=c(0.8,5.2), 
     main = "Extremist Republicans",
     xlab = "", ylab ="",
     font.main=2, cex.axis=1.3, cex.main=1.4) 
with(subset(Brexit.5, referendum==0),lines(date, rep.rdd.pred, col="black", lty=1, lwd=4))
with(subset(Brexit.5, referendum==1),lines(date, rep.rdd.pred, col="gray", lty=1, lwd=4))
abline(v=as.Date("2016-06-24"), col="black", lwd=2, lty=2)
text(as.Date("2016-05-31"), 3, "-0.014(0.003)\n p<.001", cex = 1.3)
text(as.Date("2016-07-06"), 3, "0.030(0.013)\n p=.023", cex = 1.3)

# Plot 6: Cause 6
plot(Brexit.6$date,Brexit.6$cause_6, 
     pch=1, frame.plot = FALSE,col="grey90",
     xaxs="i",yaxs="i",
     ylim=c(0.8,5.2), 
     main = "Extremist Loyalists",
     xlab = "", ylab ="",
     font.main=2, cex.axis=1.3, cex.main=1.4) 
with(subset(Brexit.6, referendum==0),lines(date, loy.rdd.pred, col="black", lty=1, lwd=4))
with(subset(Brexit.6, referendum==1),lines(date, loy.rdd.pred, col="gray", lty=1, lwd=4))
abline(v=as.Date("2016-06-24"), col="black", lwd=2, lty=2)
text(as.Date("2016-05-31"), 3, "-0.015(0.004)\n p<.001", cex = 1.3)

# Plot 7: Cause 7
plot(Brexit.7$date,Brexit.7$cause_7, 
     pch=1, frame.plot = FALSE,col="grey90",
     xaxs="i",yaxs="i",
     ylim=c(0.8,5.2), 
     main = "Illegitimate rule from Westminster",
     xlab = "", ylab ="",
     font.main=2, cex.axis=1.3, cex.main=1.4) 
with(subset(Brexit.7, referendum==0),lines(date, ill.rdd.pred, col="black", lty=1, lwd=4))
with(subset(Brexit.7, referendum==1),lines(date, ill.rdd.pred, col="gray", lty=1, lwd=4))
abline(v=as.Date("2016-06-24"), col="black", lwd=2, lty=2)

# Plot 8: Cause 8
plot(Brexit.8$date,Brexit.8$cause_8, 
     pch=1, frame.plot = FALSE,col="grey90",
     xaxs="i",yaxs="i",
     ylim=c(0.8,5.2), 
     main = "The partition of Ireland",
     xlab = "", ylab ="",
     font.main=2, cex.axis=1.3, cex.main=1.4) 
with(subset(Brexit.8, referendum==0),lines(date, part.rdd.pred, col="black", lty=1, lwd=4))
with(subset(Brexit.8, referendum==1),lines(date, part.rdd.pred, col="gray", lty=1, lwd=4))
abline(v=as.Date("2016-06-24"), col="black", lwd=2, lty=2)
text(as.Date("2016-05-31"), 3, "0.010(0.004)\n p=.009", cex = 1.3)

### Save plot
dev.off()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### Preferences for the Future ########  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### 3. RDDs: Preferences for the future ####
W.out <- weightit(referendum ~ age + employment_1 + exposure,
                    data = Brexit, estimand = "ATT", method = "ebal") #create weights

# Preference 1: Unification
unification.rdd = lm(unification ~ time_zero+referendum+(time_zero*referendum), data = Brexit,
                     weights = W.out$w)
Brexit$unification.rdd.pred <- predict(unification.rdd)
summary(unification.rdd)

# Preference 2: Remain
remain.rdd = lm(remain ~ time_zero+referendum+(time_zero*referendum), data = Brexit,
                weights = W.out$w)
Brexit$remain.rdd.pred <- predict(remain.rdd)
summary(remain.rdd)


#### Figure 5 ##### 
 
### Open plot environment 
jpeg("Figures/Fig5.jpeg", width = 9, height = 4, units = 'in', res = 500)

### Set margins
par(mfrow=c(1,2), mar=c(2,2,2,2))

# PLOT 1: Unification
plot(Brexit$date,Brexit$unification, 
     pch=1, frame.plot = FALSE,col="grey90",
     xaxs="i",yaxs="i",
     ylim=c(-0.09,1.1), 
     main = "Unify with Ireland",
     xlab = "", ylab ="",
     font.main=2, cex.axis=1.1, cex.main=1.3) 
with(subset(Brexit, referendum==0),lines(date, unification.rdd.pred, col="black", lty=1, lwd=4))
with(subset(Brexit, referendum==1),lines(date, unification.rdd.pred, col="gray", lty=1, lwd=4))
abline(v=as.Date("2016-06-24"), col="black", lwd=2, lty=2)
text(as.Date("2016-05-31"), 0.5, "0.003(0.001)\n p=.018", cex = 1.2)
text(as.Date("2016-07-06"), 0.5, "-0.007(0.004)\n p=.092", cex = 1.1)

# PLOT 2: Remain
plot(Brexit$date,Brexit$remain, 
     pch=1, frame.plot = FALSE,col="grey90",
     xaxs="i",yaxs="i",
     ylim=c(-0.09,1.1), 
     main = "Remain in the UK",
     xlab = "", ylab ="",
     font.main=2, cex.axis=1.1, cex.main=1.3) 
with(subset(Brexit, referendum==0),lines(date, remain.rdd.pred, col="black", lty=1, lwd=4))
with(subset(Brexit, referendum==1),lines(date, remain.rdd.pred, col="gray", lty=1, lwd=4))
abline(v=as.Date("2016-06-24"), col="black", lwd=2, lty=2)
text(as.Date("2016-05-31"), 0.5, "-0.004(0.001)\n p=.012", cex = 1.2)

### Save plot
dev.off()


