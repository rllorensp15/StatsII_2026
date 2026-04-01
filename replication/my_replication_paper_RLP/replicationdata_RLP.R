# Settings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
set.seed(1234)
Sys.setlocale("LC_TIME", "C")

library(haven)
library(dplyr)
library(tidyr)
library(forcats)
library(stargazer)
library(tidyverse)
library(readr)
library(margins)
library(sjPlot)
library(ggplot2)
library(ggeffects)
library(fixest)
library(tibble)
library(modelsummary)
install.packages("WeightIt") #I install this package that I didn't have because the authors use it
library(WeightIt)
install.packages("Hmisc") #I install this package that I didn't have because the authors use it
library(Hmisc)

setwd("C:/Users/Usuario/Documents/GitHub/StatsII_2026/replication/my_replication_paper_RLP")

#Data Replication for Replication Paper

#The Past, Brexit, and the Future in Northern Ireland: A Quasi-Experiment

#Original Authors: Amélie Godefroidt, Karin Dyrstad & Kristin Bakke

#Replication author: Rubèn Llorens Poblador

# Load and subset data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Brexit <- read_dta("C:/Users/Usuario/Documents/GitHub/StatsII_2026/replication/my_replication_paper_RLP/GodefroidtDyrstadBakke_2022_ReplicationFiles/Data/Brexit.dta")
View(Brexit)
head(Brexit)
#select the data
Brexit <- Brexit %>% dplyr::select(male, age, education, employment_1, exposure,
                                   referendum, referendum2, time_zero,
                                   cause_1,cause_2,cause_3,cause_4,cause_5,cause_6,cause_7,cause_8,
                                   remain, independence, unification)
summary(Brexit)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### Perceptions of the Causes ########  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Three model specifications

#models without controls
right.side.nc <- "~ referendum"
varlabels.nc <- "Brexit"

#models with controls or weights (to fix unbalance)
right.side.c <- "~ referendum + age + employment_1 + exposure"
varlabels <- c("Brexit", "Age", "Employed", "Conflict Exposure")

#I convert the variable to numeric (because it wasn't working):
Brexit$referendum <- as.numeric(Brexit$referendum)

#models with entropy matching
W.out <- weightit(referendum ~ age + employment_1 + exposure,
                  data = Brexit, estimand = "ATT", method = "ebal") #create weights
summary(W.out)


#### ATEs: Economic and political causes ####

rootcauses <- Cs(cause_1,cause_2,cause_3,cause_4)

#model with entropy matching
ate.rc.em <- vector(length(rootcauses), mode = "list")
names(ate.rc.em) <- rootcauses

for (i in 1:(length(rootcauses))){
  
  modelformula <- paste(rootcauses[i],right.side.nc)
  print(modelformula)
  
  ate.rc.em[[rootcauses[i]]] <- eval(substitute(lm(.modelformula, data = Brexit, weights = W.out$w), #use weights
                                                list(.modelformula = modelformula)))
  
  print(summary(ate.rc.em[[rootcauses[i]]])) #print results,
  print(confint(ate.rc.em[[rootcauses[i]]])) #including CIs
}


#model with controls
ate.rc.c <- vector(length(rootcauses), mode = "list")
names(ate.rc.c) <- rootcauses

for (i in 1:(length(rootcauses))){
  
  modelformula <- paste(rootcauses[i],right.side.c)
  print(modelformula)
  
  ate.rc.c[[rootcauses[i]]] <- eval(substitute(lm(.modelformula, data = Brexit), 
                                               list(.modelformula = modelformula)))
  
  print(summary(ate.rc.c[[rootcauses[i]]])) #print results,
  print(confint(ate.rc.c[[rootcauses[i]]])) #including CIs
  
}

#model without controls or matching
ate.rc.nc <- vector(length(rootcauses), mode = "list")
names(ate.rc.nc) <- rootcauses

for (i in 1:(length(rootcauses))){
  
  modelformula <- paste(rootcauses[i],right.side.nc)
  print(modelformula)
  
  ate.rc.nc[[rootcauses[i]]] <- eval(substitute(lm(.modelformula, data = Brexit), 
                                                list(.modelformula = modelformula)))
  
  print(summary(ate.rc.nc[[rootcauses[i]]])) #print results,
  print(confint(ate.rc.nc[[rootcauses[i]]])) #including CIs
  
}


#### ATEs: Actor-based causes ####

actorcauses <- Cs(cause_5,cause_6,cause_7,cause_8)

#model with entropy matching
ate.ac.em <- vector(length(actorcauses), mode = "list")
names(ate.ac.em) <- actorcauses

for (i in 1:(length(actorcauses))){
  
  modelformula <- paste(actorcauses[i],right.side.nc)
  print(modelformula)
  
  ate.ac.em[[actorcauses[i]]] <- eval(substitute(lm(.modelformula, data = Brexit, weights = W.out$w), #use weights
                                                 list(.modelformula = modelformula)))
  
  print(summary(ate.ac.em[[actorcauses[i]]])) #print results,
  print(confint(ate.ac.em[[actorcauses[i]]])) #including CIs
  
}

#model with controls
ate.ac.c <- vector(length(actorcauses), mode = "list")
names(ate.ac.c) <- actorcauses

for (i in 1:(length(actorcauses))){
  
  modelformula <- paste(actorcauses[i],right.side.c)
  print(modelformula)
  
  ate.ac.c[[actorcauses[i]]] <- eval(substitute(lm(.modelformula, data = Brexit), 
                                                list(.modelformula = modelformula)))
  
  print(summary(ate.ac.c[[actorcauses[i]]])) #print results,
  print(confint(ate.ac.c[[actorcauses[i]]])) #including CIs
  
}

#model without controls
ate.ac.nc <- vector(length(actorcauses), mode = "list")
names(ate.ac.nc) <- actorcauses

for (i in 1:(length(actorcauses))){
  
  modelformula <- paste(actorcauses[i],right.side.nc)
  print(modelformula)
  
  ate.ac.nc[[actorcauses[i]]] <- eval(substitute(lm(.modelformula, data = Brexit), 
                                                 list(.modelformula = modelformula)))
  
  print(summary(ate.ac.nc[[actorcauses[i]]])) #print results,
  print(confint(ate.ac.nc[[actorcauses[i]]])) #including CIs
  
}


#### Figure 1 ####

### Get b's and ci's for rootcauses
cis.rc.em <- matrix(NA,length(rootcauses),2)
b.rc.em <- matrix(NA,length(rootcauses),1)
cis.rc.c <- matrix(NA,length(rootcauses),2)
b.rc.c <- matrix(NA,length(rootcauses),1)
cis.rc.nc <- matrix(NA,length(rootcauses),2)
b.rc.nc <- matrix(NA,length(rootcauses),1)

for (i in 1:length(rootcauses))
{
  b.rc.em[i] <- ate.rc.em[[i]]$coefficients[2]
  ci.rc.em <- confint(ate.rc.em[[i]])
  print(b.rc.em[i])
  lb.rc.em <- ci.rc.em[2,1]
  ub.rc.em <- ci.rc.em[2,2]
  cis.rc.em[i,] <- cbind(lb.rc.em,ub.rc.em)
}

for (i in 1:length(rootcauses))
{
  b.rc.c[i] <- ate.rc.c[[i]]$coefficients[2]
  ci.rc.c <- confint(ate.rc.c[[i]])
  print(b.rc.c[i])
  lb.rc.c <- ci.rc.c[2,1]
  ub.rc.c <- ci.rc.c[2,2]
  cis.rc.c[i,] <- cbind(lb.rc.c,ub.rc.c)
}

for (i in 1:length(rootcauses))
{
  b.rc.nc[i] <- ate.rc.nc[[i]]$coefficients[2]
  ci.rc.nc <- confint(ate.rc.nc[[i]])
  print(b.rc.nc[i])
  lb.rc.nc <- ci.rc.nc[2,1]
  ub.rc.nc <- ci.rc.nc[2,2]
  cis.rc.nc[i,] <- cbind(lb.rc.nc,ub.rc.nc)
}

### Get b's and ci's for actor causes
cis.ac.em <- matrix(NA,length(actorcauses),2)
b.ac.em <- matrix(NA,length(actorcauses),1)
cis.ac.c <- matrix(NA,length(actorcauses),2)
b.ac.c <- matrix(NA,length(actorcauses),1)
cis.ac.nc <- matrix(NA,length(actorcauses),2)
b.ac.nc <- matrix(NA,length(actorcauses),1)


for (i in 1:length(actorcauses))
{
  b.ac.em[i] <- ate.ac.em[[i]]$coefficients[2]
  ci.ac.em <- confint(ate.ac.em[[i]])
  print(b.ac.em[i])
  lb.ac.em <- ci.ac.em[2,1]
  ub.ac.em <- ci.ac.em[2,2]
  cis.ac.em[i,] <- cbind(lb.ac.em,ub.ac.em)
}

for (i in 1:length(actorcauses))
{
  b.ac.c[i] <- ate.ac.c[[i]]$coefficients[2]
  ci.ac.c <- confint(ate.ac.c[[i]])
  print(b.ac.c[i])
  lb.ac.c <- ci.ac.c[2,1]
  ub.ac.c <- ci.ac.c[2,2]
  cis.ac.c[i,] <- cbind(lb.ac.c,ub.ac.c)
}

for (i in 1:length(actorcauses))
{
  b.ac.nc[i] <- ate.ac.nc[[i]]$coefficients[2]
  ci.ac.nc <- confint(ate.ac.nc[[i]])
  print(b.ac.nc[i])
  lb.ac.nc <- ci.ac.nc[2,1]
  ub.ac.nc <- ci.ac.nc[2,2]
  cis.ac.nc[i,] <- cbind(lb.ac.nc,ub.ac.nc)
}

### Specify y-labs and labels: rootcauses
y.lab <- seq(from = 1, to=length(rootcauses),by=1)
rc.vnames <-c(causes_1 = "Economic inequality\n and poverty",
              causes_2 = "Community-based,\n inequality", 
              causes_3 = "Discrimination\n and repression",
              causes_4 = "Lack of Democracy")

### Specify y-labs and labels: actorcauses
y.lab <- seq(from = 1, to=length(actorcauses),by=1)
ac.vnames <-c(causes_5 = "Extremist\n Republicans",
              causes_6 = "Extremist\n Loyalists",
              causes_7 = "Illegitimate rule\n from Westminster",
              causes_8 = "Partition of Ireland")

### Jitter to position coefficients
jitter.c <-  -0.15
jitter.nc <-  -0.30

getwd()

### Open plot environment 
jpeg("Fig1.jpeg", width = 8, height = 5, units = 'in', res = 500)

### Set margins
par(mfrow=c(1,2), mar = c(5, 4, 0, 2), oma = c(0.5, 3.5, 0, 0), mgp=c(2,0.5,0))

### PLOT 1 ### 
plot(b.rc.c, y.lab, 
     type="n", 
     ylab ="", xlab = "", yaxt="n", 
     xlim = c(-0.5,0.5), ylim = c(0.5,4.5), 
     cex.axis=1)
axis(2, at=y.lab, labels=rc.vnames, las = 2, cex.axis=0.9)
mtext("Difference in Means",side=1,line=2,outer=F, cex = 1)
#b[CI]s: entropy weights
points(b.rc.em, y.lab, pch=21, cex=1.2, lwd=1.5, col = "black")
segments(cis.rc.em[,1], y.lab, cis.rc.em[,2], y.lab, lty=1, lwd=1.5, col = "black")
#b[CI]s: controls
points(b.rc.c, y.lab+jitter.c, pch= 4, cex=1.2, lwd=1.5, col = "grey20")
segments(cis.rc.c[,1], y.lab+jitter.c, cis.rc.c[,2], y.lab+jitter.c, lty=2, lwd=1.5, col = "grey20")
#b[CI]s: nothing
points(b.rc.nc, y.lab+jitter.nc, pch= 6, cex=1.2, lwd=1.5, col = "grey50")
segments(cis.rc.nc[,1], y.lab+jitter.nc, cis.rc.nc[,2], y.lab+jitter.nc, lty=4, lwd=1.5, col = "grey50")
abline(v=0)

### Set margins
par(mar = c(5, 5, 0, 0.5))

### PLOT 2 ### 
plot(b.ac.c, y.lab, type="n", ylab ="", xlab = "", yaxt="n", 
     xlim = c(-0.5,0.5), ylim = c(0.5,4.5), 
     cex.axis=1)
axis(2, at=y.lab, labels=ac.vnames, las = 2, cex.axis=0.9)
mtext("Difference in Means",side=1,line=2,outer=F, cex = 1)
#b[CI]s: entropy weights
points(b.ac.em, y.lab, pch=21, cex=1.2, lwd=1.5, col = "black")
segments(cis.ac.em[,1], y.lab, cis.ac.em[,2], y.lab, lty=1, cex=1, lwd=1.5, col = "black")
#b[CI]s: controls
points(b.ac.c, y.lab+jitter.c, pch= 4, cex=1.2, lwd=1.5, col = "grey20")
segments(cis.ac.c[,1], y.lab+jitter.c, cis.ac.c[,2], y.lab+jitter.c, lty=2, cex=1, lwd=1.5, col = "grey20")
#b[CI]s: nothing
points(b.ac.nc, y.lab+jitter.nc, pch= 6, cex=1.2, lwd=1.5, col = "grey50")
segments(cis.ac.nc[,1], y.lab+jitter.nc, cis.ac.nc[,2], y.lab+jitter.nc, lty=4, cex=1, lwd=1.5, col = "grey50")
abline(v=0)

### LEGEND ### 
par(xpd = NA)
#add legend outside plot region
legend(x=-1.7, y=-0.4, horiz = T, cex = 0.85,
       legend=c("Matching", "Covariates", "No Correction"), 
       title = "Applied correction for imbalance", 
       lty = c(1,2,4), col=c("black", "grey20", "grey50"), pch=c(21,4,6), 
       box.lty=0)

### Close plot environment and, hence, save plot       
dev.off()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### Preferences for the Future ########  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Figure 2 ####
#### Barplot with percentages pre- and post-Brexit

### Crosstabs
# Unification
unification_count <- table(Brexit$unification, 
                           Brexit$referendum) # The first column is for groupings within a bar, the second is for the separate bars
unification_perc <- apply(unification_count, 2, function(x){x*100/sum(x,na.rm=T)})# Transform this data in %
unification_perc
# Remain
remain_count <- table(Brexit$remain, 
                      Brexit$referendum) # The first column is for groupings within a bar, the second is for the separate bars
remain_perc <- apply(remain_count, 2, function(x){x*100/sum(x,na.rm=T)})# Transform this data in %
remain_perc
# Independence
independence_count <- table(Brexit$independence, 
                            Brexit$referendum) # The first column is for groupings within a bar, the second is for the separate bars
independence_perc <- apply(independence_count, 2, function(x){x*100/sum(x,na.rm=T)})# Transform this data in %

### Open plot environment 
jpeg("Fig2.jpeg", width = 7, height = 3.5, units = 'in', res = 500)

### Set margins
par(mfrow=c(1,3), mar=c(2,2,3,0.5), mgp=c(3, .5, 0)) # Combine two plots (horizontal)

### PLOT 1 ###
ys.u <- apply(unification_perc, 2, function(x) c(x[1]/2, head(cumsum(x),-1) + tail(x,-1)/2))# Set position of percentages
xs.u <- barplot(unification_perc, # Plot 1
                legend.text = F,names.arg=c("Pre-Brexit","Post-Brexit"),
                main = "Unify with Ireland",
                cex.axis=1.3, cex.names=1.4, cex.main=1.4) # Make barplot, store x data
text(rep(xs.u, each=nrow(ys.u)), c(ys.u), 
     labels=(round(c(unification_perc),0)), cex = 1.3, col = c("white", "black")) # Add percentages to plot

### PLOT 2 ###
ys.r <- apply(remain_perc, 2, function(x) c(x[1]/2, head(cumsum(x),-1) + tail(x,-1)/2))# Set position of percentages
xz.r <- barplot(remain_perc, # Plot 2
                legend.text = F,names.arg=c("Pre-Brexit","Post-Brexit"),
                main = "Remain in the UK",
                cex.axis=1.3, cex.names=1.4, cex.main=1.4) # Make barplot, store x data
text(rep(xz.r, each=nrow(ys.r)), c(ys.r), 
     labels=round(c(remain_perc),0), cex = 1.3, col = c("white", "black")) # Add percentages to plot

### PLOT 3 ###
ys.r <- apply(independence_perc, 2, function(x) c(x[1]/2, head(cumsum(x),-1) + tail(x,-1)/2))# Set position of percentages
xz.r <- barplot(independence_perc, # Plot 2
                legend.text = F, names.arg=c("Pre-Brexit","Post-Brexit"),
                main = "Independent state",
                cex.axis=1.3, cex.names=1.4, cex.main=1.4) # Make barplot, store x data
text(rep(xz.r, each=nrow(ys.r)), c(ys.r), 
     labels=round(c(independence_perc),0), cex = 1.3, col = c("white", "black")) # Add percentages to plot
dev.off()



#### ATEs: Preferences for the Future ####

future <- Cs(remain,independence, unification)

#model with entropy matching
ate.f.em <- vector(length(future), mode = "list")
names(ate.f.em) <- future

for (i in 1:(length(future))){
  
  modelformula <- paste(future[i],right.side.nc)
  print(modelformula)
  
  ate.f.em[[future[i]]] <- eval(substitute(lm(.modelformula, data = Brexit, weights = W.out$w), #use weights
                                           list(.modelformula = modelformula)))
  
  print(summary(ate.f.em[[future[i]]]))
  
}

#model with controls
ate.f.c <- vector(length(future), mode = "list")
names(ate.f.c) <- future

for (i in 1:(length(future))){
  
  modelformula <- paste(future[i],right.side.c)
  print(modelformula)
  
  ate.f.c[[future[i]]] <- eval(substitute(lm(.modelformula, data = Brexit), 
                                          list(.modelformula = modelformula)))
  
  print(summary(ate.f.c[[future[i]]]))
  
}

#model without controls
ate.f.nc <- vector(length(future), mode = "list")
names(ate.f.nc) <- future

for (i in 1:(length(future))){
  
  modelformula <- paste(future[i],right.side.nc)
  print(modelformula)
  
  ate.f.nc[[future[i]]] <- eval(substitute(lm(.modelformula, data = Brexit),  
                                           list(.modelformula = modelformula)))
  
  print(summary(ate.f.nc[[future[i]]]))
  
}


#### Figure 3 ####

### Get b's and ci's for preferences for the future
cis.f.em <- matrix(NA,length(future),2)
b.f.em <- matrix(NA,length(future),1)
cis.f.c <- matrix(NA,length(future),2)
b.f.c <- matrix(NA,length(future),1)
cis.f.nc <- matrix(NA,length(future),2)
b.f.nc <- matrix(NA,length(future),1)


for (i in 1:length(future))
{
  b.f.em[i] <- ate.f.em[[i]]$coefficients[2]
  ci.f.em <- confint(ate.f.em[[i]])
  print(b.f.em[i])
  lb.f.em <- ci.f.em[2,1]
  ub.f.em <- ci.f.em[2,2]
  cis.f.em[i,] <- cbind(lb.f.em,ub.f.em)
}

for (i in 1:length(future))
{
  b.f.c[i] <- ate.f.c[[i]]$coefficients[2]
  print(ate.f.c[[i]]$coefficients[2])
  ci.f.c <- confint(ate.f.c[[i]])
  #print(ci)
  lb.f.c <- ci.f.c[2,1]
  ub.f.c <- ci.f.c[2,2]
  cis.f.c[i,] <- cbind(lb.f.c,ub.f.c)
}

for (i in 1:length(future))
{
  b.f.nc[i] <- ate.f.nc[[i]]$coefficients[2]
  ci.f.nc <- confint(ate.f.nc[[i]])
  print(b.f.nc[i])
  lb.f.nc <- ci.f.nc[2,1]
  ub.f.nc <- ci.f.nc[2,2]
  cis.f.nc[i,] <- cbind(lb.f.nc,ub.f.nc)
}

### Specify y-labs and labels: future preferences
y.lab.f <- seq(from = 1, to=length(future),by=1)
vnames.f <-c(remain = "Remain Part\n of the UK",
             independence = "Become an\n Independent State",
             unification = "Unify with the\n Rest of Ireland")

### Jitter to position coefficients
jitter.f.c <-  -0.15
jitter.f.nc <-  -0.30

### Open plot environment
jpeg("Fig3.jpeg", width = 5, height = 5, units = 'in', res = 500)

### Set margins
par(mfrow=c(1,1), mar=c(5, 4, 0, 3), oma=c(0.5,3.5,0.5,0.2), mgp=c(2,0.8,0))

### PLOT ### 
plot(b.f.c, y.lab.f, type="n", ylab ="", xlab = "", yaxt="n", 
     xlim = c(-0.5,0.5), ylim = c(0.5,3.5),
     cex.axis=1)
axis(2, at=y.lab.f, labels=vnames.f, las = 2, cex.axis=0.9)
mtext("Difference in Means",side=1,line=2,outer=F, cex = 1)
#b[CI]s: entropy weights
points(b.f.em, y.lab.f, pch=21, cex=1, lwd=1.5, col = "black")
segments(cis.f.em[,1], y.lab.f, cis.f.em[,2], y.lab.f, lty=1, cex=1, lwd=1.5, col = "black")
#b[CI]s: controls
points(b.f.c, y.lab.f+jitter.f.c, pch= 4, cex=1, lwd=1.5, col = "grey20")
segments(cis.f.c[,1], y.lab.f+jitter.f.c, cis.f.c[,2], y.lab.f+jitter.f.c, lty=2, cex=1, lwd=1.5, col = "grey20")
#b[CI]s: nothing
points(b.f.nc, y.lab.f+jitter.f.nc, pch= 6, cex=1, lwd=1.5, col = "grey50")
segments(cis.f.nc[,1], y.lab.f+jitter.f.nc, cis.f.nc[,2], y.lab.f+jitter.f.nc, lty=4, cex=1, lwd=1.5, col = "grey50")
abline(v=0)

### LEGEND ### 
par(xpd = NA)
#add legend outside plot region
legend(x=-0.75, y=-0.15, horiz = T, cex = 0.85,
       legend=c("Matching", "Covariates", "No Correction"), 
       title = "Applied correction for imbalance", 
       lty = c(1,2,4), col=c("black", "grey20", "grey50"), pch=c(21,4,6), 
       box.lty=0)
### Close plot environment and, hence, save plot  
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### Perceptions of the Causes ########  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#We select data again:

Brexit <- read_dta("C:/Users/Usuario/Documents/GitHub/StatsII_2026/replication/my_replication_paper_RLP/GodefroidtDyrstadBakke_2022_ReplicationFiles/Data/Brexit.dta")


Brexit <- Brexit %>% dplyr::select(age, employment_1, exposure, #failed balance tests
                                   referendum, referendum2, time_zero, date, #referendum indicators
                                   cause_1,cause_2,cause_3,cause_4,cause_5,cause_6,cause_7,cause_8, #DVs 1 (med.)
                                   remain, independence, unification) #DVs 2
summary(Brexit)


#### 1. RDDs: Economic and political causes ####

Brexit.1$referendum <- as.numeric(Brexit.1$referendum) #I do this again to avoid errors
Brexit.2$referendum <- as.numeric(Brexit.2$referendum)
Brexit.3$referendum <- as.numeric(Brexit.3$referendum)
Brexit.4$referendum <- as.numeric(Brexit.4$referendum)
Brexit.5$referendum <- as.numeric(Brexit.5$referendum)
Brexit.6$referendum <- as.numeric(Brexit.6$referendum)
Brexit.7$referendum <- as.numeric(Brexit.7$referendum)
Brexit.8$referendum <- as.numeric(Brexit.8$referendum)

#Or better

library(haven)
# Zap all labels from the main dataset once and for all
Brexit <- zap_labels(Brexit)

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
Brexit.4$referendum <- as.numeric(Brexit.4$referendum)

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
jpeg("Fig4.jpeg", width = 7, height = 10, units = 'in', res = 500)

### Set margins
par(mfrow=c(4,2), mar = c(3,3,3,2))

names(Brexit.1) #Because I received some error in the plot below:
unique(Brexit.1$time_zero)


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
jpeg("Fig5.jpeg", width = 9, height = 4, units = 'in', res = 500)

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### My Contribution: Interaction Models ########  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### Interaction Models: Cohort ########  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#We select data again:

Brexit.ext <- read_dta("C:/Users/Usuario/Documents/GitHub/StatsII_2026/replication/my_replication_paper_RLP/GodefroidtDyrstadBakke_2022_ReplicationFiles/Data/Brexit.dta")
View(Brexit.ext)

unique(Brexit.ext$age) 

# I create the variable 'cohort' with three age groups
Brexit.ext$cohort <- cut(Brexit.ext$age, 
                       breaks = c(18, 25, 45, Inf), 
                       labels = c("18-25", "26-45", "Over 45"),
                       include.lowest = TRUE)

# "Over 45" will be the reference group 
# Thus I compare the 'Brexit Effect' on youngsters compared to those in the conflict generation
Brexit.ext$cohort <- relevel(Brexit.ext$cohort, ref = "Over 45")

Brexit.ext <- Brexit.ext %>% dplyr::select(age, employment_1, exposure, cohort, education,
                                   referendum, referendum2, time_zero, date, #referendum indicators
                                   cause_1,cause_2,cause_3,cause_4,cause_5,cause_6,cause_7,cause_8, #DVs 1 (med.)
                                   remain, independence, unification) #DVs 2
summary(Brexit.ext)

# Now I will run a Logit Model with an interaction term with cohort,
#applying Entropy Balancing weights to ensure covariate balance.
#In this 1st model. Unification will be the outcome variable.
#First, as always I transform referndum into a numeric variable
Brexit.ext$referendum <- as.numeric(Brexit.ext$referendum)

# Then, I Re-calculate weights using the new dataset (Brexit.ext) and the new variable (cohort)
W.ext <- weightit(referendum ~ age + employment_1 + exposure + cohort, 
                  data = Brexit.ext, 
                  estimand = "ATT", 
                  method = "ebal")

# Then, I run the model using the weights that match this specific data
ext_model_cohorts_uni <- glm(unification ~ referendum * cohort + education + employment_1 + exposure, 
                         data = Brexit.ext, 
                         family = binomial(link = "logit"),
                         weights = W.ext$weights)

# Finally, I check the results
summary(ext_model_cohorts_uni)


#A second model with remain as the outcome variable:
ext_model_cohorts_rem <- glm(remain ~ referendum * cohort + education + employment_1 + exposure, 
                         data = Brexit.ext, 
                         family = binomial(link = "logit"),
                         weights = W.ext$weights)
summary(ext_model_cohorts_rem)

#A third model with independence as the outcome variable:
ext_model_cohorts_ind <- glm(independence ~ referendum * cohort + education + employment_1 + exposure, 
                             data = Brexit.ext, 
                             family = binomial(link = "logit"),
                             weights = W.ext$weights)
summary(ext_model_cohorts_ind)

#I will plot the interesting model (unification)

# I use type = "pred" to get the predicted probabilities from 0 to 1
plot_ext1 <- plot_model(ext_model_cohorts_uni, 
           type = "pred", 
           terms = c("cohort", "referendum"), 
           title = "Predicted Probability of Supporting Irish Unification by Age Cohort",
           axis.title = c("Age Cohorts", "Probability of Support"),
           legend.title = "Period") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Pre-Referendum", "Post-Referendum")) +
  # Some aesthetic retouches
  set_theme(base = theme_minimal(), 
            geom.label.size = 3,
            legend.pos = "bottom")

ggsave(
  filename = "plot_ext1.png", 
  plot = plot_ext1,
  device = "png",
  width = 10,      
  height = 6,   
  dpi = 300,      
  bg = "white"     
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### Interaction Models: Exposure ########  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Interaction term: referendum x exposure --> Where those exposed to the conflict
# more affected by the referendum outcome
ext_model_exposure_uni <- glm(unification ~ referendum * exposure + cohort + education + employment_1, 
                          data = Brexit.ext, 
                          family = binomial(link = "logit"),
                          weights = W.ext$weights)

summary(ext_model_exposure_uni)

ext_model_exposure_rem <- glm(remain ~ referendum * exposure + cohort + education + employment_1, 
                          data = Brexit.ext, 
                          family = binomial(link = "logit"),
                          weights = W.ext$weights)

summary(ext_model_exposure_rem)

ext_model_exposure_ind <- glm(independence ~ referendum * exposure + cohort + education + employment_1, 
                              data = Brexit.ext, 
                              family = binomial(link = "logit"),
                              weights = W.ext$weights)
summary(ext_model_exposure_ind)

#I'll make a plot with the interesting model: unificaton as outcome variable

# I create the plot
plot_ext2 <- plot_model(ext_model_exposure_uni, 
                        type = "pred", 
                        terms = c("exposure", "referendum"), # Exposure a l'eix X, Referendum com a color
                        title = "Effect of the Brexit Referendum by Exposure to the Troubles",
                        axis.title = c("Victim of the Troubles (0 = No, 1 = Yes)", "Probability of Supporting Irish Unification"),
                        legend.title = "Period") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Pre-Referendum", "Post-Referendum")) +
  scale_x_continuous(breaks = c(0, 1), labels = c("Not Victim", "Victim"))

ggsave(
  filename = "plot_ext2.png", 
  plot = plot_ext2,
  device = "png",
  width = 10,      
  height = 6,   
  dpi = 300,      
  bg = "white"     
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### Interaction Models: Sex ########  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#We select data again:

Brexit.ext.sex <- read_dta("C:/Users/Usuario/Documents/GitHub/StatsII_2026/replication/my_replication_paper_RLP/GodefroidtDyrstadBakke_2022_ReplicationFiles/Data/Brexit.dta")
View(Brexit.ext.sex)
Brexit.ext.sex$referendum <- as.numeric(Brexit.ext.sex$referendum)

Brexit.ext.sex <- Brexit.ext.sex %>% dplyr::select(age, employment_1, exposure, education, male,
                                           referendum, referendum2, time_zero, date, #referendum indicators
                                           cause_1,cause_2,cause_3,cause_4,cause_5,cause_6,cause_7,cause_8, #DVs 1 (med.)
                                           remain, independence, unification)

#I recalculate weights because I have included a new variable: 'male'
W.ext_sex <- weightit(referendum ~ age + employment_1 + exposure + male, 
                        data = Brexit.ext.sex, 
                        estimand = "ATT", 
                        method = "ebal")

ext_model_exposure_sex <- glm(unification ~ referendum * male + age + exposure + education + employment_1, 
                              data = Brexit.ext.sex, 
                              family = binomial(link = "logit"),
                              weights = W.ext_sex$weights)
summary(ext_model_exposure_sex)

#I create a plot with the predicted probabilities for each gender group:

#1st, I calculate the exact marginal predictions
pred_sex <- ggpredict(ext_model_exposure_sex, terms = c("male", "referendum"))

# 2. I create the plot with ggplot2 because I had previously received some
# compatibility errors
plot_ext3 <- ggplot(pred_sex, aes(x = x, y = predicted, color = group)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.1, position = position_dodge(0.3)) +
  scale_x_continuous(breaks = c(0, 1), labels = c("Female", "Male")) +
  scale_color_manual(values = c("darkblue", "darkorange"), 
                     labels = c("Pre-Referendum", "Post-Referendum")) +
  labs(title = "Probability of Supporting Irish Reunification by Gender Before and After the Brexit Referendum",
       x = "Gender",
       y = "Predicted Probability",
       color = "Period") +
  theme_minimal()

ggsave(
  filename = "plot_ext3.png", 
  plot = plot_ext3,
  device = "png",
  width = 10,      
  height = 6,   
  dpi = 300,      
  bg = "white"     
)
