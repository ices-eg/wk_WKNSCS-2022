rm(list=ls())
# Load libraries
library(icesTAF)
library("stockassessment")
library("TMB")
#library("viridis")
library(reshape2)
#library(plyr) 
#library(dplyr)
library(ggplot2)
library(ggpubr)
#library(reshape)
#library(devtools)
#library(ggloop)
#library(multipanelfigure)
#library("flextable")
#help(package = stockassessment)
library(RColorBrewer)
library(colorRamps)
library(tidyverse)
#display.brewer.all()
colset      <- brewer.pal(9, "Set1")
#colset1      <- brewer.pal(9, "Blues")
#colset1      <- colset1[seq(9,1)]
#colset1      <- blue2red(9)

## Set path
setwd("C:/Users/chen072/OneDrive - WageningenUR/0_2021_plaice_benchmark/12_SAM_assessment_runs/")
project_path <- getwd()
data_path    <- paste0(project_path,"/data_processed/")
result_path  <- paste0(project_path,"/model_results/")

## functions
source(paste0(project_path,"/functions_ple27420_assessment.R"))


## settings 
## mean F ages (total&landings and for discards)
meanFages  <- c(2:6)
meanFDages <- c(2:3)

## Plusgroup age
pGrp       <- 10

## assessment year
assyear    <- 2021


########## load model output ---------------------
#newConf <- loadConf (dat , file =" model.cfg ")
load(file=paste0(result_path, "SAM_run7.RData"))
myrun  <- run7
myconf <- conf7
irun  <- 7

plot(myrun, main=paste0("run",irun), cex.main=2) 

########## AIC ---------------------
AIC(myrun)

########## residuals ---------------------
# one-observation-ahead quantile residuals
# obs - est
res <- residuals(myrun)
plot(res, main=paste0("run",irun,":one-observation-ahead quantile residuals"))
saveplotrun(res,"ob_residual", irun, result_path)
plot(res, type="summary")
# Plots between-age correlations by fleet, either estimated or empirical using residuals.
corplot(res)
# plot fit to data
fitplot(myrun)  


# joint sample residuals or process residuals 
# Single joint sample residuals of log(F) and log(N)
resp <- procres(myrun)
plot(resp, main=paste0("run",irun))
saveplotrun(resp,"pro_residual", irun, result_path)
plot(resp, type="summary")
#a <- SavePlot0("summaryProcResiduals",11,11)

save(run11, conf11, res, resp, dat, surveys, file=paste0(result_path, "SAM_run11.RData"))

########## retrospective ---------------------
re <- retro(myrun, year=5)
plot(re,main=paste0("run",irun))
saveplotrun(re,"retrospective_", irun, result_path)

########## leave-one-out ---------------------
loo <- leaveout(myrun)
plot(loo,main=paste0("run",irun))
saveplotrun(loo,"loo_", irun, result_path)

# model intepretation ---------------------

## 1. survey catchability ----
## 1. plot estimated parameters logFpar_ : survey catchability 
qtableplot(qtable(myrun))
parplot(myrun)
mytable <- as.data.frame(partable(myrun))
rownames(mytable)
names(surveys)

mytable  <- as.data.frame(partable(myrun))
mytable1 <- mytable[grep("logFpar_", rownames(mytable)), ]
mytable1$survey <- NA
mytable1$age    <- NA
fleets   <- c(names(surveys))
mydat           <- NA
for (isurvey in 1:length(fleets)) {
  myparind <- myconf$keyLogFpar[isurvey+1,][myconf$keyLogFpar[isurvey+1,]!=-1]
  iage <- 1
  for (ind2 in myparind){
    temp <- NA
    temp <- mytable1[rownames(mytable1) %in% paste0("logFpar_",ind2),]
    temp$survey <- fleets[isurvey]
    temp$age    <- iage
    iage        <- iage+1
    mydat       <- rbind(mydat, temp)
  }
}
mydat <- mydat[-1,]
# plot
f1 <- ggplot(mydat, aes(x = age, y = par)) +
  geom_path(lwd = 1.5) +
  geom_ribbon(aes(ymin = par-`sd(par)`, ymax = par+`sd(par)`), alpha = 0.2) +
  labs(y  = "", x = "",
       title = paste0("run ", irun, ": logFpar+SD, estimated survey catchability")) +
  scale_x_continuous(name="age", breaks=seq(1, 10, 1), labels=seq(1, 10, 1)) +
  theme_bw()+
  theme(plot.title = element_text(color="black", size=15, face="bold"),
        plot.subtitle = element_text(color="black", size=12, face="bold"),
        legend.position = "bottom", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=10, face="plain"),
        axis.text.x = element_text(color = "black", size = 14, angle = 0),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14, angle = 90),
        strip.text.x = element_text(size = 12, colour = "black", angle = 0),
        strip.text.y = element_text(size = 10, colour = "black", angle = 0))
f2 <- facet(f1, facet.by = c("survey"), ncol = 2, scales = "free_y", panel.labs.font.y = list(size = 12, angle=0), panel.labs.font.x = list(size = 12))
print(f2)

## 2. keyVarObs/logSdLogN ----
## 2. plot estimated observation/proces_N variance:keyVarObs/logSdLogN 
## survey weights proportional to: 1/exp(2*logSdlogObs_x)
mytable <- as.data.frame(partable(myrun))
rownames(mytable)
names(surveys)

mytable1 <- mytable[grep("logSdLogObs_", rownames(mytable)), ]
mytable1$survey <- NA
mytable1$age    <- NA
fleets <- c("catch", names(surveys))
mydat           <- NA
for (isurvey in 1:length(fleets)) {
  myparind <- myconf$keyVarObs[isurvey,][myconf$keyVarObs[isurvey,]!=-1]
  iage <- 1
  for (ind2 in myparind){
    temp <- NA
    temp <- mytable1[rownames(mytable1) %in% paste0("logSdLogObs_",ind2),]
    temp$survey <- fleets[isurvey]
    temp$age    <- iage
    iage        <- iage+1
    mydat       <- rbind(mydat, temp)
  }
}
mydat <- mydat[-1,]

# plot
f1 <- ggplot(mydat, aes(x = age, y = `exp(par)`)) +
  geom_path(lwd = 1.5) +
  geom_ribbon(aes(ymin = Low, ymax = High), alpha = 0.2) +
  labs(y  = "", x = "",
       title = paste0("run ", irun, ":SdLogObs+CI, estimated observation sd")) +
  scale_x_continuous(name="age", breaks=seq(1, 10, 1), labels=seq(1, 10, 1)) +
  theme_bw()+
  theme(plot.title = element_text(color="black", size=15, face="bold"),
        plot.subtitle = element_text(color="black", size=12, face="bold"),
        legend.position = "bottom", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=10, face="plain"),
        axis.text.x = element_text(color = "black", size = 14, angle = 0),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14, angle = 90),
        strip.text.x = element_text(size = 12, colour = "black", angle = 0),
        strip.text.y = element_text(size = 10, colour = "black", angle = 0))
f2 <- facet(f1, facet.by = c("survey"), ncol = 2, scales = "fixed", panel.labs.font.y = list(size = 12, angle=0), panel.labs.font.x = list(size = 12))
print(f2)

## add process_N variance
mytable1 <- mytable[grep("logSdLogN_", rownames(mytable)), ]
mytable1$survey <- NA
mytable1$age    <- NA
mydat1           <- NA
myparind <- myconf$keyVarLogN
iage <- 1
for (ind2 in myparind){
  temp1 <- NA
  temp1 <- mytable1[rownames(mytable1) %in% paste0("logSdLogN_",ind2),]
  temp1$survey <- "process_N"
  temp1$age    <- iage
  iage        <- iage+1
  mydat1       <- rbind(mydat1, temp1)
}
mydat1 <- mydat1[-1,]

mydat1 <- rbind(mydat, mydat1)
# plot
f1 <- ggplot(mydat1, aes(x = age, y = `exp(par)`)) +
  geom_path(lwd = 1.5) +
  geom_ribbon(aes(ymin = Low, ymax = High), alpha = 0.2) +
  labs(y  = "", x = "",
       title = paste0("run ", irun, ":SdLogObs+CI, estimated observation/process sd")) +
  scale_x_continuous(name="age", breaks=seq(1, 10, 1), labels=seq(1, 10, 1)) +
  theme_bw()+
  theme(plot.title = element_text(color="black", size=15, face="bold"),
        plot.subtitle = element_text(color="black", size=12, face="bold"),
        legend.position = "bottom", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=10, face="plain"),
        axis.text.x = element_text(color = "black", size = 14, angle = 0),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14, angle = 90),
        strip.text.x = element_text(size = 12, colour = "black", angle = 0),
        strip.text.y = element_text(size = 10, colour = "black", angle = 0))
f2 <- facet(f1, facet.by = c("survey"), ncol = 2, scales = "fixed", panel.labs.font.y = list(size = 12, angle=0), panel.labs.font.x = list(size = 12))
print(f2)

## 3. correlation among age: if obsCorStruct=AR ----
## 3. Plots the estimated correlation matrices by fleet: if obsCorStruct=AR
obscorrplot(myrun)
#observation covariance matrices from a SAM fit
obscov(myrun)

## Plots between-age correlations by fleet, either estimated or empirical using residuals.
corplot(myrun)
corplot(res)
empirobscorrplot(res)

## 4. F process variance ----
## 4. Plots the process variance of F
mytable1 <- mytable[grep("logSdLogFsta", rownames(mytable)), ]
mytable1$survey <- NA
mytable1$age    <- NA
mydat1           <- NA
myparind <- myconf$keyVarF[1,]
iage <- 1
for (ind2 in myparind){
  temp1 <- NA
  temp1 <- mytable1[rownames(mytable1) %in% paste0("logSdLogFsta_",ind2),]
  temp1$survey <- "process_F"
  temp1$age    <- iage
  iage        <- iage+1
  mydat1       <- rbind(mydat1, temp1)
}
mydat1 <- mydat1[-1,]
# plot
f1 <- ggplot(mydat1, aes(x = age, y = `exp(par)`)) +
  geom_path(lwd = 1.5) +
  geom_ribbon(aes(ymin = Low, ymax = High), alpha = 0.2) +
  labs(y  = "", x = "",
       title = paste0("run ", irun, ":SdLogObs+CI, estimated observation sd")) +
  scale_x_continuous(name="age", breaks=seq(1, 10, 1), labels=seq(1, 10, 1)) +
  theme_bw()+
  theme(plot.title = element_text(color="black", size=15, face="bold"),
        plot.subtitle = element_text(color="black", size=12, face="bold"),
        legend.position = "bottom", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=10, face="plain"),
        axis.text.x = element_text(color = "black", size = 14, angle = 0),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14, angle = 90),
        strip.text.x = element_text(size = 12, colour = "black", angle = 0),
        strip.text.y = element_text(size = 10, colour = "black", angle = 0))
f2 <- facet(f1, facet.by = c("survey"), ncol = 2, scales = "fixed", panel.labs.font.y = list(size = 12, angle=0), panel.labs.font.x = list(size = 12))
print(f2)

## 5. N process variance ----
## 5. Plots the process variance of N
mytable <- as.data.frame(partable(myrun))
rownames(mytable)
mytable1 <- mytable[grep("logSdLogN", rownames(mytable)), ]

## 6. F ----
##  Fbar
fbarplot(myrun)

## F@age
matplot(faytable(myrun) , type = "l") # F at age 
#mytable <- as.data.frame(faytable(myrun))
mytable <- melt(faytable(myrun))
names(mytable) <- c("year", "age","f")
mytable$age <- factor(mytable$age)
ggplot(data=mytable[mytable$age %in% c(1:10),], aes(x=year, y=f, color=age)) +
  geom_line(size=1) + 
  ylab("f")+ xlab("year") +
  scale_colour_brewer(palette = "Spectral") +
  #scale_colour_gradientn(colours =  RColorBrewer::brewer.pal(10, "Spectral")) +
  #ggtitle("XX") +
  theme_bw() +
  theme(plot.title = element_text(color="black", size=14, face="bold"), 
        legend.position = "bottom", 
        legend.title = element_text(colour="black", size=14, face="bold"),
        legend.text = element_text(colour="black", size=14, face="plain"),
        axis.text.x = element_text(color = "black", size = 14, angle = 0),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.x = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14, angle = 90)) 

##
fselectivityplot(myrun)   

##check correations of estimated F across ages
cor(faytable(myrun))       

## 7. SSB ----
##  
ssbplot(myrun)
res1 <- ssbtable(myrun)
plot(res1[,"Estimate"])

## 8. recruitment ----
## recruits
recplot(myrun)                        
# stock recruitment plot
srplot(myrun)                         

## 9. n@age ----
## stock numbers at age
ntable(myrun)                         
matplot(ntable(myrun), type = "l")   

## 10. catch ----
## cross-observed
catchplot(myrun)     

## 11. data ----
## data availability
dataplot(myrun)

## compare result run3 and run7
load(file=paste0(result_path, "SAM_run11.RData"))
ssbplot(run11)
load(file=paste0(result_path, "SAM_run7.RData"))
ssbplot(run7, add=T, col="red")
fbarplot(run3)  
fbarplot(run7, add=T, col="red")
recplot(myrun)                        # recruits
srplot(myrun)                         # stock recruitment plot


           

            



## estimated parameters (fixed effects)
parplot(myrun)
mytable <- as.data.frame(partable(myrun))
## plot
#temp <- mytable[grep("logFpar_", rownames(mytable)),]
#BTS+IBTSQ3
temp <- mytable[rownames(mytable) %in% paste0("logFpar_", 9:18),]
par(mfrow=c(1,1))
plot(temp$par, type="o",main="BTS+IBTSQ3")
#IBTSQ1
temp <- mytable[rownames(mytable) %in% paste0("logFpar_", 19:26),]
plot(temp$par, type="o", col="red", main="IBTSQ1")
#SNS1
temp <- mytable[rownames(mytable) %in% paste0("logFpar_", 27:33),]
plot(temp$par, type="o", col="blue", main="SNS1")
#SNS2
temp <- mytable[rownames(mytable) %in% paste0("logFpar_", 34:40),]
plot(temp$par, type="o", col="green",main="SNS2")
par(mfrow=c(1,1))
plot(temp$par, type="o")
# run7
#BTS+IBTSQ3
temp <- mytable[rownames(mytable) %in% paste0("logFpar_", 9:18),]
par(mfrow=c(1,1))
plot(temp$par, type="o",main="BTS+IBTSQ3-1", ylim=c(-3.5, -2.3))
temp <- mytable[rownames(mytable) %in% paste0("logFpar_", 19:28),]
lines(temp$par, type="o",main="BTS+IBTSQ3-2", col="red")

# Compare runs ----
## 1. residual ----
load(file=paste0(result_path, "SAM_run1.RData"))
myfleet <- c("catch", names(surveys))
## catch res
dd1 <- data.frame(resi = res$residual, 
                  age=res$age,
                  fleet=myfleet[res$fleet],
                  run=1)
load(file=paste0(result_path, "SAM_run4.RData"))
## catch res
dd3 <- data.frame(resi = res$residual, 
                  age=res$age,
                  fleet=myfleet[res$fleet],
                  run=3)
dd <- rbind(dd1, dd3)
dd$run <- factor(dd$run)
dd$age <- paste0("age ", dd$age)
dd$age <- factor(dd$age, levels=paste0("age ", 1:10))
f1  <- ggplot() + 
  geom_boxplot(data=dd, 
               aes(x=run, y=resi),fill="steelblue", varwidth=TRUE)+
  geom_hline(yintercept = 0, color="red", 
             linetype="dashed", size=1) +
  #facet_wrap( ~ vessel, nrow = 4, scales="fixed") +
  xlab("")+ ylab("") +
  ggtitle("ob residual") +
  theme_bw()+
  theme(plot.title = element_text(color="black", size=20, face="bold"), 
        legend.position =  "none", 
        legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=20, face="plain"),
        axis.text.x = element_text(color = "black", size = 14, angle = 0),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.x = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14, angle = 90),
        strip.text.x = element_text(size = 20, colour = "black", angle = 0),
        strip.text.y = element_text(size = 12, colour = "black", angle = 0))
f2  <- facet(f1, facet.by = c("age", "fleet"), ncol = 4, scales = "free_y", panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14))
print(f2)


## 2. survey catchability ----
load(file=paste0(result_path, "SAM_run3.RData"))
myrun  <- run3
#dd <- qtable(myrun)
myconf <- conf3

dd1     <- extract_catchability(myrun, myconf, surveys)
dd1$run <- "run3"

load(file=paste0(result_path, "SAM_run4.RData"))
myrun  <- run4
myconf <- conf4
dd3     <- extract_catchability(myrun, myconf, surveys)
dd3$run <- "run4"

datall <- rbind(dd1, dd3)
datall$run <- factor(datall$run)

# plot
f1 <- ggplot(datall, aes(x = age, y = par, color=run)) +
  geom_path(lwd = 1.5) +
  geom_ribbon(aes(ymin = par-`sd(par)`, ymax = par+`sd(par)`,fill=run), linetype=0,alpha = 0.2) +
  labs(y  = "", x = "",
       title = "compare catchability") +
  scale_x_continuous(name="age", breaks=seq(1, 10, 1), labels=seq(1, 10, 1)) +
  theme_bw()+
  theme(plot.title = element_text(color="black", size=15, face="bold"),
        plot.subtitle = element_text(color="black", size=12, face="bold"),
        legend.position = "bottom", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=10, face="plain"),
        axis.text.x = element_text(color = "black", size = 14, angle = 0),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14, angle = 90),
        strip.text.x = element_text(size = 12, colour = "black", angle = 0),
        strip.text.y = element_text(size = 10, colour = "black", angle = 0))
f2 <- facet(f1, facet.by = c("survey"), ncol = 2, scales = "free_y", panel.labs.font.y = list(size = 12, angle=0), panel.labs.font.x = list(size = 12))
print(f2)


## 3. ob/pro variance ----

load(file=paste0(result_path, "SAM_run6.RData"))
myrun  <- run6
myconf <- conf6
dd1 <- extract_ob_pro_variance(myrun, myconf, surveys)
dd1$run <- "run6"

load(file=paste0(result_path, "SAM_run7.RData"))
myrun  <- run7
myconf <- conf7
dd3 <- extract_ob_pro_variance(myrun, myconf, surveys)
dd3$run <- "run7"

datall <- rbind(dd1, dd3)
datall$run <- factor(datall$run)

# plot
f1 <- ggplot(datall, aes(x = age, y = `exp(par)`, color=run)) +
  geom_path(lwd = 1.5) +
  #geom_ribbon(aes(ymin = Low, ymax = High,fill=run), linetype=0,alpha = 0.2) +
  labs(y  = "", x = "",
       title = "Compare estimated sd of ob/process_N error") +
  scale_x_continuous(name="age", breaks=seq(1, 10, 1), labels=seq(1, 10, 1)) +
  theme_bw()+
  theme(plot.title = element_text(color="black", size=15, face="bold"),
        plot.subtitle = element_text(color="black", size=12, face="bold"),
        legend.position = "bottom", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=10, face="plain"),
        axis.text.x = element_text(color = "black", size = 14, angle = 0),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14, angle = 90),
        strip.text.x = element_text(size = 12, colour = "black", angle = 0),
        strip.text.y = element_text(size = 10, colour = "black", angle = 0))
f2 <- facet(f1, facet.by = c("survey"), ncol = 3, scales = "fixed", panel.labs.font.y = list(size = 12, angle=0), panel.labs.font.x = list(size = 12))
print(f2)

## 4. F pro variance ----

load(file=paste0(result_path, "SAM_run7.RData"))
myrun  <- run7
myconf <- conf7
dd1 <- extract_proF_variance(myrun, myconf)
dd1$run <- "run7"

load(file=paste0(result_path, "SAM_run8.RData"))
myrun  <- run8
myconf <- conf8
dd3 <- extract_proF_variance(myrun, myconf)
dd3$run <- "run8"

datall <- rbind(dd1, dd3)
datall$run <- factor(datall$run)

# plot
f1 <- ggplot(datall, aes(x = age, y = `exp(par)`, color=run)) +
  geom_path(lwd = 1.5) +
  geom_ribbon(aes(ymin = Low, ymax = High), alpha = 0.2) +
  labs(y  = "", x = "",
       title = "compare F process_error") +
  scale_x_continuous(name="age", breaks=seq(1, 10, 1), labels=seq(1, 10, 1)) +
  theme_bw()+
  theme(plot.title = element_text(color="black", size=15, face="bold"),
        plot.subtitle = element_text(color="black", size=12, face="bold"),
        legend.position = "bottom", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=10, face="plain"),
        axis.text.x = element_text(color = "black", size = 14, angle = 0),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14, angle = 90),
        strip.text.x = element_text(size = 12, colour = "black", angle = 0),
        strip.text.y = element_text(size = 10, colour = "black", angle = 0))
f2 <- facet(f1, facet.by = c("survey"), ncol = 2, scales = "fixed", panel.labs.font.y = list(size = 12, angle=0), panel.labs.font.x = list(size = 12))
print(f2)
