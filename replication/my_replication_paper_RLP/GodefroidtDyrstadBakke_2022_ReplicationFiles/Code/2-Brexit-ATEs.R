#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# PROJECT: The Past, Brexit, and the Future in Northern Ireland: A Quasi-Experiment 
# AUTHOR: ** anonymized for review **
# CONTACT: ** anonymized for review **
# LAST MODIFIED: February 8, 2022

# INFO: This R file contains the code necessary to replicate the 
# ATEs reported in the main paper (plots)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())
setwd("C:/Users/ameliego/Dropbox/Brexit/GodefroidtDyrstadBakke_2022_ReplicationFiles")

# Installing and loading packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if(length(new.pkg)) install.packages(new.pkg, dependencies=TRUE)
sapply(pkg, require, character.only=TRUE)
}

pkgs <- c("Hmisc", "haven", "dplyr", "tidyr", "stargazer", "WeightIt", "ebal", "Matching") 
ipak(pkgs)


# Load and subset the data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Brexit <- read_dta("Data/Brexit.dta")

#select the data
Brexit <- Brexit %>% dplyr::select(age, employment_1, exposure, #failed balance tests
                                   referendum, #referendum indicator
                                   cause_1,cause_2,cause_3,cause_4,cause_5,cause_6,cause_7,cause_8, #DVs 1 (med.)
                                   remain, independence, unification) #DVs 2
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

### Open plot environment 
jpeg("Figures/Fig1.jpeg", width = 8, height = 5, units = 'in', res = 500)

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
jpeg("Figures/Fig2.jpeg", width = 7, height = 3.5, units = 'in', res = 500)

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
jpeg("Figures/Fig3.jpeg", width = 5, height = 5, units = 'in', res = 500)

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

