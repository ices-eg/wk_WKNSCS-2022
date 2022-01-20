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
source(paste0(project_path,"functions_ple27420_assessment.R"))


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
myrun <- run7
irun  <- 7

plot(myrun, main=paste0("run",irun), cex.main=2) 

########## AIC ---------------------
AIC(myrun)

########## residuals ---------------------
# one-observation-ahead quantile residuals
# obs - est
res <- residuals(myrun)
plot(res, main=paste0("run",irun,":one-observation-ahead quantile residuals"))
a <- SavePlot0("Residuals",11,11)
plot(res, type="summary")
a <- SavePlot0("summaryResiduals",11,11)
# plot fit to data
fitplot(myrun)  

# joint sample residuals or process residuals 
# Single joint sample residuals of log(F) and log(N)
resp <- procres(myrun)
plot(resp, main=paste0("run",irun))
a <- SavePlot0("ProcResiduals",11,11)
plot(resp, type="summary")
a <- SavePlot0("summaryProcResiduals",11,11)

save(run7, conf7, res, resp, dat, file=paste0(result_path, "SAM_run7.RData"))

########## retrospective ---------------------
re <- retro(myrun, year=5)
plot(re,main=paste0("run",irun))

########## leave-one-out ---------------------
loo <- leaveout(myrun)
plot(loo,main=paste0("run",irun))


########## model intepretation ---------------------

## compare result run3 and run7
load(file=paste0(result_path, "SAM_run3.RData"))
ssbplot(run3)
load(file=paste0(result_path, "SAM_run7.RData"))
ssbplot(run7, add=T, col="red")
fbarplot(run3)  
fbarplot(run7, add=T, col="red")

ssbplot(myrun)
res1 <- ssbtable(myrun)
plot(res1[,"Estimate"])

ntable(myrun)                         # stock numbers at age
matplot(ntable(myrun), type = "l")    # plot van stock numbers at age

recplot(myrun)                        # recruits
srplot(myrun)                         # stock recruitment plot
a <- SavePlot0("srplot",11,11)

catchplot(myrun)                      # catch plot (observed = kruisjes)
ssbplot(myrun)                        # ssb plot
fbarplot(myrun)                       # Fbar plot
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

##check correations of estimated F across ages
cor(faytable(myrun))                  

##??
fselectivityplot(myrun)               

## data availability
dataplot(myrun)

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
