rm(list=ls())
# Load libraries
library(icesTAF)
library("stockassessment")
library("TMB")
#library("viridis")
#library(reshape2)
#library(plyr) 
#library(dplyr)
#library(ggplot2)
#library(reshape)
#library(devtools)
#library(ggloop)
#library(multipanelfigure)
#library("flextable")



## Set path
setwd("C:/Users/chen072/OneDrive - WageningenUR/0_2021_plaice_benchmark/12_SAM_assessment_runs/")
project_path <- getwd()
data_path    <- paste0(project_path,"/data_processed/")
result_path  <- paste0(project_path,"/model_results/")

## functions
source(paste0(project_path,"/functions_ple27420_assessment.R"))


## settings 
## mean F ages (total&landings and for discards)
meanFages  <- c(2,6)
meanFDages <- c(2,3)

## minimum age
minAge     <- 1

## Plusgroup age
pGrp       <- 10

## assessment year
assyear    <- 2021


# import data -----------------------------------------------------

#file_names        <- list.files(data_path)
file_names        <- c("cn.dat", "cw.dat", "dw.dat", "lf.dat", "lw.dat", 
                       "mo.dat", "nm.dat", "pf.dat", "pm.dat", "sw.dat", 
                       "fleet_WKNSCS_2022.dat")
stock_data        <- lapply(file_names,function(x)read.ices(file.path(data_path,x))) 
# stock_data      <- lapply(file_names, read.ices.from.disk, stock_input_path = file.path("bootstrap/data/"))
names(stock_data) <- unlist(strsplit(file_names,".dat"))

## change fleet name
names(stock_data)[names(stock_data)== "fleet_WKNSCS_2022"] <- "survey"

## select survey and age
names(stock_data$survey)
surveys <- stock_data$survey[c(1,7,8,10,11)]
names(surveys)

## set SAM readable object
dat <- setup.sam.data(surveys=surveys,
                    residual.fleet=stock_data$cn, 
                    prop.mature=stock_data$mo, 
                    stock.mean.weight=stock_data$sw, 
                    catch.mean.weight=stock_data$cw, 
                    dis.mean.weight=stock_data$dw, 
                    land.mean.weight=stock_data$lw,
                    prop.f=stock_data$pf, 
                    prop.m=stock_data$pm, 
                    natural.mortality=stock_data$nm, 
                    land.frac=stock_data$lf)
names(dat)

# data exploration ----

########## run1 - default conf---------------------

# Create a default parameter configuration object
# make a configuration file for the data, filled in with default.
conf <- defcon(dat)
names(conf)

# //1// MinAge, maxAge, maxAgePlusGroup
conf$minAge          <- minAge                  # minimum age class in assessment
conf$maxAge          <- pGrp                 # maximum age class in assessment
# here it might needs to be changed
names(surveys)
conf$maxAgePlusGroup <- c(1,0,1,1,0,0)  # last age group considered a plus group for each fleet (1 = yes, 0 =no) -> hier dus enkel catch bevat plusgroep
# first is catch
# SNS age 7 is not plus age
# IBTSQ1 age 8 is plus age
# BTS-IBTSQ3 age 10 is plus age
# //15// Define the fbar range
conf$fbarRange       <- meanFages

########## F@age in catch
# //2// Number of parameters describing F-at-age (voor de catch) = coupling of the fishing mortality states
# default
conf$keyLogFsta[1,]

# //3// # Correlation of fishing mortality across ages (0 independent, 1 compound symmetry, or 2 AR(1)
# Correlation of fishing mortality across ages
# not possible to decouple it for the time series
# (0 independent, 1 compound symmetry (= zelfde trends between age groups through time), 2 AR(1) (= age groups close together have similar F trend, when distance between ages is larger, then correlation declines),
# 3 seperable AR(1) (= parallel, one up, all up))
conf$corFlag         <- 2                     
# Compound Symmetry. This structure has constant variance and constant covariance.
# This is a first-order autoregressive structure with homogenous variances. The correlation between any two elements is equal to rho for adjacent elements, rho2 for elements that are separated by a third, and so on. is constrained so that -1<<1.


########## selectivity@age parameters in survey
# //4// Number of parameters in the survey processes - coupling of the survey catchability parameters (1st row not used - ze keyLogFsta)
# Coupling of the survey catchability parameters (nomally first row is not used, as that is covered by fishing mortality). 
conf$keyLogFpar 


# //5// Density dependent catchability power parameters (if any) 
conf$keyQpow  


########## process variance for F@age parameters
# //6// Variance of parameters on F - use a single parameter: catch process variance per age
# Coupling of process variance parameters for log(F)-process (normally only first row is used)
conf$keyVarF[1,]                          



########## process variance for N@age parameters
# //7// Coupling of process variance parameters for log(N)-process -
conf$keyVarLogN 


########## Coupling of the variance parameters for the observations.
# //8// Coupling of the variance parameters on the observations
conf$keyVarObs

# Covariance structure for each fleet
# //9// Correlation at age between observations -> Covariance structure for each fleet (within fleet)
# ID = independent; AR = AR(1), US = unstructured (= they are correlated, but not in structured way, vb age 1 can correlate with age 7)
conf$obsCorStruct 
# first is catch, and then rest are tuneing series

# //10// Coupling of correlation parameters can only be specified if the AR(1) structure is chosen above. 
# NA's indicate where correlation parameters can be specified (-1 where they cannot)
conf$keyCorObs

# //11// Stock recruitment code (0 = plain random walk; 1 = Ricker; 2 = Beverton-Holt; 3 = piece-wise constant)
conf$stockRecruitmentModelCode

# //12// Number of years where catch scaling is applied
conf$noScaledYears

# //13// A vector of the years where catch scaling is applied
conf$keyScaledYears

# //14// A matrix specifying the couplings of scale parameters (nrow = no scaled years; ncols = no ages)
conf$keyParScaledYA

# //16// To be defined only if a biomass survey is used -- Biomass or catch survey for the tuning series
#H 0 = SSB index; 1 = catch index; 2 = FSB index; 3 = total catch; 4 = total landings; 5 = TSB index 
conf$keyBiomassTreat 

# //17// Option for observational likelihood (LN or ALN)
conf$obsLikelihoodFlag

# //18// if weight attribute is supplied for observations this option sets the treatment (0 = relative weight; 1 = fix variance to weight) -~keyVarObs? 
conf$fixVarToWeight

# //19// The fraction of t(3) distribution used in log F increment distribution
conf$fracMixF

# //20// The fraction of t(3) distribution used in log N increment distribution
conf$fracMixN

# //21// A vector with same length as number of fleets, where each element is the fraction of t(3) distribution used in the distribution of that fleet
conf$fracMixObs

# //22// Vector of break years between which recruitment is at constant level. The break year is included in the left interval. (This option is only used in combination with stock recruitment code =3)
conf$constRecBreaks

# //23// Coupling of parameters used in a prediction-variance link for observations
conf$predVarObsLink


## Fit the model
par       <- defpar(dat, conf)
conf1     <- conf
run1      <- sam.fit(dat,conf1, par)  ## WGNSSK2021 base run

# Convergence checks
run1$opt$convergence          # 0 = convergence
run1$opt$message              # 4 = ok
AIC(run1)


# save config and result
saveConf(conf1 , file = paste0(result_path, "SAM_run1.cfg"))
save(run1, dat, conf1, surveys, file=paste0(result_path, "SAM_run1.RData"))



########## run2 ---------------------
## not done! seperate F@age parameter for age 9 and age 10 (tested, not good) 
## decoupling survey catchability parameter for last 2 ages

rm(conf)
# Create a default parameter configuration object
# make a configuration file for the data, filled in with default.
conf <- defcon(dat)
names(conf)

# //1// MinAge, maxAge, maxAgePlusGroup
conf$minAge          <- minAge                  # minimum age class in assessment
conf$maxAge          <- pGrp                 # maximum age class in assessment
# here it might needs to be changed
names(surveys)
conf$maxAgePlusGroup <- c(1,0,1,1,0,0)  # last age group considered a plus group for each fleet (1 = yes, 0 =no) -> hier dus enkel catch bevat plusgroep
# first is catch
# SNS age 7 is not plus age
# IBTSQ1 age 8 is plus age
# BTS-IBTSQ3 age 10 is plus age
# //15// Define the fbar range
conf$fbarRange       <- meanFages

########## F@age in catch
# //2// Number of parameters describing F-at-age (voor de catch) = coupling of the fishing mortality states
# default
conf$keyLogFsta[1,]
# [1,]    0    1    2    3    4    5    6    7    8     8
#conf$keyLogFsta[1,] <- c(0,1,2,3,4,5,6,7,8,9)

# //3// # Correlation of fishing mortality across ages (0 independent, 1 compound symmetry, or 2 AR(1)
# Correlation of fishing mortality across ages
# not possible to decouple it for the time series
# (0 independent, 1 compound symmetry (= zelfde trends between age groups through time), 2 AR(1) (= age groups close together have similar F trend, when distance between ages is larger, then correlation declines),
# 3 seperable AR(1) (= parallel, one up, all up))
conf$corFlag         <- 2                     
# Compound Symmetry. This structure has constant variance and constant covariance.
# This is a first-order autoregressive structure with homogenous variances. The correlation between any two elements is equal to rho for adjacent elements, rho2 for elements that are separated by a third, and so on. is constrained so that -1<<1.


########## selectivity@age parameters in survey
# //4// Number of parameters in the survey processes - coupling of the survey catchability parameters (1st row not used - ze keyLogFsta)
# Coupling of the survey catchability parameters (nomally first row is not used, as that is covered by fishing mortality). 
conf$keyLogFpar 
names(surveys)
## BTS-Isis-early 1-9
conf$keyLogFpar[2,] <- c(0,1,2,3,4,5,6,7,8,-1)
## BTS+IBTSQ3: 1-10+
conf$keyLogFpar[3,] <- c(9,10,11,12,13,14,15,16,17,18)
## IBTSQ1: 1-8+
conf$keyLogFpar[4,] <- c(19,20,21,22,23,24,25,26,-1,-1)
## SNS1: 1-7
conf$keyLogFpar[5,] <- c(27,28,29,30,31,32,33,-1,-1,-1)
## SNS2: 1-7
conf$keyLogFpar[6,] <- c(34,35,36,37,38,39,40,-1,-1,-1)
conf$keyLogFpar

# //5// Density dependent catchability power parameters (if any) 
conf$keyQpow  

########## process variance for F@age parameters
# //6// Variance of parameters on F - use a single parameter: catch process variance per age
# Coupling of process variance parameters for log(F)-process (normally only first row is used)
conf$keyVarF[1,]                          
# default:[1] 0 0 0 0 0 0 0 0 0 0

########## process variance for N@age parameters
# //7// Coupling of process variance parameters for log(N)-process -
conf$keyVarLogN 
# default: [1] 0 1 1 1 1 1 1 1 1 1
  
########## Coupling of the variance parameters for the observations.
# //8// Coupling of the variance parameters on the observations
conf$keyVarObs
names(surveys)

# Covariance structure for each fleet
# //9// Correlation at age between observations -> Covariance structure for each fleet (within fleet)
# ID = independent; AR = AR(1), US = unstructured (= they are correlated, but not in structured way, vb age 1 can correlate with age 7)
conf$obsCorStruct 
# first is catch, and then rest are tuneing series

# //10// Coupling of correlation parameters can only be specified if the AR(1) structure is chosen above. 
# NA's indicate where correlation parameters can be specified (-1 where they cannot)
conf$keyCorObs

# //11// Stock recruitment code (0 = plain random walk; 1 = Ricker; 2 = Beverton-Holt; 3 = piece-wise constant)
conf$stockRecruitmentModelCode


## Fit the model
conf2     <- conf
par       <- defpar(dat, conf2)
run2      <- sam.fit(dat,conf2, par)  

# Convergence checks
run2$opt$convergence          # 0 = convergence
run2$opt$message              # 4 = ok
AIC(run2)


# save config and result
saveConf(conf2 , file = paste0(result_path, "SAM_run2.cfg"))
save(run2, dat, conf2, surveys, file=paste0(result_path, "SAM_run2.RData"))


########## run3 ---------------------
## coupling survey catchability parameter per age
## coupling oberservation variance per age
rm(conf)
# Create a default parameter configuration object
# make a configuration file for the data, filled in with default.
conf <- defcon(dat)
names(conf)

# //1// MinAge, maxAge, maxAgePlusGroup
conf$minAge          <- minAge                  # minimum age class in assessment
conf$maxAge          <- pGrp                 # maximum age class in assessment
# here it might needs to be changed
names(surveys)
conf$maxAgePlusGroup <- c(1,0,1,1,0,0)  # last age group considered a plus group for each fleet (1 = yes, 0 =no) -> hier dus enkel catch bevat plusgroep
# first is catch
# SNS age 7 is not plus age
# IBTSQ1 age 8 is plus age
# BTS-IBTSQ3 age 10 is plus age
# //15// Define the fbar range
conf$fbarRange       <- meanFages

########## F@age in catch
# //2// Number of parameters describing F-at-age (voor de catch) = coupling of the fishing mortality states
# default
conf$keyLogFsta[1,]
# [1,]    0    1    2    3    4    5    6    7    8     8


# //3// # Correlation of fishing mortality across ages (0 independent, 1 compound symmetry, or 2 AR(1)
# Correlation of fishing mortality across ages
# not possible to decouple it for the time series
# (0 independent, 1 compound symmetry (= zelfde trends between age groups through time), 2 AR(1) (= age groups close together have similar F trend, when distance between ages is larger, then correlation declines),
# 3 seperable AR(1) (= parallel, one up, all up))
conf$corFlag         <- 2                     
# Compound Symmetry. This structure has constant variance and constant covariance.
# This is a first-order autoregressive structure with homogenous variances. The correlation between any two elements is equal to rho for adjacent elements, rho2 for elements that are separated by a third, and so on. is constrained so that -1<<1.


########## selectivity@age parameters in survey
# //4// Number of parameters in the survey processes - coupling of the survey catchability parameters (1st row not used - ze keyLogFsta)
# Coupling of the survey catchability parameters (nomally first row is not used, as that is covered by fishing mortality). 
conf$keyLogFpar 
names(surveys)
## BTS-Isis-early 1-9
conf$keyLogFpar[2,] <- c(0,1,2,3,4,5,6,7,8,-1)
## BTS+IBTSQ3: 1-10+
conf$keyLogFpar[3,] <- c(9,10,11,12,13,14,15,16,17,18)
## IBTSQ1: 1-8+
conf$keyLogFpar[4,] <- c(19,20,21,22,23,24,25,26,-1,-1)
## SNS1: 1-7
conf$keyLogFpar[5,] <- c(27,28,29,30,31,32,33,-1,-1,-1)
## SNS2: 1-7
conf$keyLogFpar[6,] <- c(34,35,36,37,38,39,40,-1,-1,-1)
conf$keyLogFpar

# //5// Density dependent catchability power parameters (if any) 
conf$keyQpow  

########## process variance for F@age parameters
# //6// Variance of parameters on F - use a single parameter: catch process variance per age
# Coupling of process variance parameters for log(F)-process (normally only first row is used)
conf$keyVarF[1,]                          
# default:[1] 0 0 0 0 0 0 0 0 0 0

########## process variance for N@age parameters
# //7// Coupling of process variance parameters for log(N)-process -
conf$keyVarLogN 
# default: [1] 0 1 1 1 1 1 1 1 1 1

########## Coupling of the variance parameters for the observations.
# //8// Coupling of the variance parameters on the observations
conf$keyVarObs
names(surveys)
## catch 1-10+
conf$keyVarObs[1,] <- c(0,1,2,2,2,2,3,3,4,5)
## BTS-Isis-early 1-9
conf$keyVarObs[2,] <- c(6,7,8,8,8,8,9,9,9,-1)
## BTS+IBTSQ3: 1-10+
conf$keyVarObs[3,] <- c(10,11,12,12,12,12,13,13,14,15)
## IBTSQ1: 1-8+
conf$keyVarObs[4,] <- c(16,17,17,17,17,17,18,18,-1,-1)
## SNS1: 1-7
conf$keyVarObs[5,] <- c(19,20,21,21,21,21,21,-1,-1,-1)
## SNS2: 1-7
conf$keyVarObs[6,] <- c(22,23,24,24,24,24,24,-1,-1,-1)
conf$keyVarObs

# Covariance structure for each fleet
# //9// Correlation at age between observations -> Covariance structure for each fleet (within fleet)
# ID = independent; AR = AR(1), US = unstructured (= they are correlated, but not in structured way, vb age 1 can correlate with age 7)
conf$obsCorStruct 
# first is catch, and then rest are tuneing series

# //10// Coupling of correlation parameters can only be specified if the AR(1) structure is chosen above. 
# NA's indicate where correlation parameters can be specified (-1 where they cannot)
conf$keyCorObs

# //11// Stock recruitment code (0 = plain random walk; 1 = Ricker; 2 = Beverton-Holt; 3 = piece-wise constant)
conf$stockRecruitmentModelCode


## Fit the model
conf3     <- conf
par       <- defpar(dat, conf3)
run3      <- sam.fit(dat,conf3, par)  

# Convergence checks
run3$opt$convergence          # 0 = convergence
run3$opt$message              # 4 = ok
AIC(run3)


# save config and result
saveConf(conf3 , file = paste0(result_path, "SAM_run3.cfg"))
save(run3, dat, conf3, surveys, file=paste0(result_path, "SAM_run3.RData"))


########## run4 ---------------------
## coupling survey catchability parameter per age
## coupling oberservation variance per age
## AR covariance structure for each fleet
rm(conf)
# Create a default parameter configuration object
# make a configuration file for the data, filled in with default.
conf <- defcon(dat)
names(conf)

# //1// MinAge, maxAge, maxAgePlusGroup
conf$minAge          <- minAge                  # minimum age class in assessment
conf$maxAge          <- pGrp                 # maximum age class in assessment
# here it might needs to be changed
names(surveys)
conf$maxAgePlusGroup <- c(1,0,1,1,0,0)  # last age group considered a plus group for each fleet (1 = yes, 0 =no) 
# first is catch
# SNS age 7 is not plus age
# IBTSQ1 age 8 is plus age
# BTS-IBTSQ3 age 10 is plus age
# //15// Define the fbar range
conf$fbarRange       <- meanFages

########## F@age in catch
# //2// Number of parameters describing F-at-age (voor de catch) = coupling of the fishing mortality states
# default
conf$keyLogFsta[1,]
# [1,]    0    1    2    3    4    5    6    7    8     8

# //3// # Correlation of fishing mortality across ages (0 independent, 1 compound symmetry, or 2 AR(1)
# Correlation of fishing mortality across ages
# not possible to decouple it for the time series
# (0 independent, 1 compound symmetry (= zelfde trends between age groups through time), 2 AR(1) (= age groups close together have similar F trend, when distance between ages is larger, then correlation declines),
# 3 seperable AR(1) (= parallel, one up, all up))
conf$corFlag         <- 2                     
# Compound Symmetry. This structure has constant variance and constant covariance.
# This is a first-order autoregressive structure with homogenous variances. The correlation between any two elements is equal to rho for adjacent elements, rho2 for elements that are separated by a third, and so on. is constrained so that -1<<1.


########## selectivity@age parameters in survey
# //4// Number of parameters in the survey processes - coupling of the survey catchability parameters (1st row not used - ze keyLogFsta)
# Coupling of the survey catchability parameters (nomally first row is not used, as that is covered by fishing mortality). 
conf$keyLogFpar 
names(surveys)
## BTS-Isis-early 1-9
conf$keyLogFpar[2,] <- c(0,1,2,3,4,5,6,7,8,-1)
## BTS+IBTSQ3: 1-10+
conf$keyLogFpar[3,] <- c(9,10,11,12,13,14,15,16,17,18)
## IBTSQ1: 1-8+
conf$keyLogFpar[4,] <- c(19,20,21,22,23,24,25,26,-1,-1)
## SNS1: 1-7
conf$keyLogFpar[5,] <- c(27,28,29,30,31,32,33,-1,-1,-1)
## SNS2: 1-7
conf$keyLogFpar[6,] <- c(34,35,36,37,38,39,40,-1,-1,-1)
conf$keyLogFpar

# //5// Density dependent catchability power parameters (if any) 
conf$keyQpow  

########## process variance for F@age parameters
# //6// Variance of parameters on F - use a single parameter: catch process variance per age
# Coupling of process variance parameters for log(F)-process (normally only first row is used)
conf$keyVarF[1,]                          
# default:[1] 0 0 0 0 0 0 0 0 0 0
#conf$keyVarF[1,] <- c(0,1,1,1,1,2,2,2,2,2)

########## process variance for N@age parameters
# //7// Coupling of process variance parameters for log(N)-process -
conf$keyVarLogN 
# default: [1] 0 1 1 1 1 1 1 1 1 1
#conf$keyVarLogN <- c(0,1,2,3,4,5,6,7,8,8)

########## Coupling of the variance parameters for the observations.
# //8// Coupling of the variance parameters on the observations
conf$keyVarObs
names(surveys)
## catch 1-10+
conf$keyVarObs[1,] <- c(0,1,2,2,2,2,3,3,4,5)
## BTS-Isis-early 1-9
conf$keyVarObs[2,] <- c(6,7,8,8,8,8,9,9,9,-1)
## BTS+IBTSQ3: 1-10+
conf$keyVarObs[3,] <- c(10,11,12,12,12,12,13,13,14,15)
## IBTSQ1: 1-8+
conf$keyVarObs[4,] <- c(16,17,17,17,17,17,18,18,-1,-1)
## SNS1: 1-7
conf$keyVarObs[5,] <- c(19,20,21,21,21,21,21,-1,-1,-1)
## SNS2: 1-7
conf$keyVarObs[6,] <- c(22,23,24,24,24,24,24,-1,-1,-1)
conf$keyVarObs

# Covariance structure for each fleet
# //9// Correlation at age between observations -> Covariance structure for each fleet (within fleet)
# ID = independent; AR = AR(1), US = unstructured (= they are correlated, but not in structured way, vb age 1 can correlate with age 7)
conf$obsCorStruct
levels(conf$obsCorStruct)
conf$obsCorStruct  <- c("ID", "ID", "ID", "ID", "AR", "AR")
conf$obsCorStruct  <- factor(conf$obsCorStruct, levels=c("ID", "AR", "US"))
# first is catch, and then rest are tuneing series

# //10// Coupling of correlation parameters can only be specified if the AR(1) structure is chosen above. 
# NA's indicate where correlation parameters can be specified (-1 where they cannot)
conf$keyCorObs
conf$keyCorObs[5,1:6] <- 0
conf$keyCorObs[6,1:6] <- 1

## start with one para per survey
## may reduces the observation error variance, downweight the survey
## likely to solve the year effect
## AIC will go down
## it's all about whether the survey gives age specific information,
## in an extreme case, all ages indicates the same signal, it is equivalent to a single age indices

## check survey internal consistency, if the internal consistency is high, 


# //11// Stock recruitment code (0 = plain random walk; 1 = Ricker; 2 = Beverton-Holt; 3 = piece-wise constant)
conf$stockRecruitmentModelCode


## Fit the model
conf4     <- conf
par       <- defpar(dat, conf4)
run4      <- sam.fit(dat,conf4, par)  

# Convergence checks
run4$opt$convergence          # 0 = convergence
run4$opt$message              # 4 = ok
AIC(run4)


# save config and result
saveConf(conf4 , file = paste0(result_path, "SAM_run4.cfg"))
save(run4, dat, conf4, surveys,file=paste0(result_path, "SAM_run4.RData"))



########## run5 ---------------------
## coupling survey catchability parameter per age
## coupling oberservation variance per age
## AR covariance structure for SNS
## coupling f process error
rm(conf)
# Create a default parameter configuration object
# make a configuration file for the data, filled in with default.
conf <- defcon(dat)
names(conf)

# //1// MinAge, maxAge, maxAgePlusGroup
conf$minAge          <- minAge                  # minimum age class in assessment
conf$maxAge          <- pGrp                 # maximum age class in assessment
# here it might needs to be changed
names(surveys)
conf$maxAgePlusGroup <- c(1,0,1,1,0,0)  # last age group considered a plus group for each fleet (1 = yes, 0 =no) 
# first is catch
# SNS age 7 is not plus age
# IBTSQ1 age 8 is plus age
# BTS-IBTSQ3 age 10 is plus age
# //15// Define the fbar range
conf$fbarRange       <- meanFages

########## F@age in catch
# //2// Number of parameters describing F-at-age (voor de catch) = coupling of the fishing mortality states
# default
conf$keyLogFsta[1,]
# [1,]    0    1    2    3    4    5    6    7    8     8

# //3// # Correlation of fishing mortality across ages (0 independent, 1 compound symmetry, or 2 AR(1)
# Correlation of fishing mortality across ages
# not possible to decouple it for the time series
# (0 independent, 1 compound symmetry (= zelfde trends between age groups through time), 2 AR(1) (= age groups close together have similar F trend, when distance between ages is larger, then correlation declines),
# 3 seperable AR(1) (= parallel, one up, all up))
conf$corFlag         <- 2                     
# Compound Symmetry. This structure has constant variance and constant covariance.
# This is a first-order autoregressive structure with homogenous variances. The correlation between any two elements is equal to rho for adjacent elements, rho2 for elements that are separated by a third, and so on. is constrained so that -1<<1.


########## selectivity@age parameters in survey
# //4// Number of parameters in the survey processes - coupling of the survey catchability parameters (1st row not used - ze keyLogFsta)
# Coupling of the survey catchability parameters (nomally first row is not used, as that is covered by fishing mortality). 
conf$keyLogFpar 
names(surveys)
## BTS-Isis-early 1-9
conf$keyLogFpar[2,] <- c(0,1,2,3,4,5,6,7,8,-1)
## BTS+IBTSQ3: 1-10+
conf$keyLogFpar[3,] <- c(9,10,11,12,13,14,15,16,17,18)
## IBTSQ1: 1-8+
conf$keyLogFpar[4,] <- c(19,20,21,22,23,24,25,26,-1,-1)
## SNS1: 1-7
conf$keyLogFpar[5,] <- c(27,28,29,30,31,32,33,-1,-1,-1)
## SNS2: 1-7
conf$keyLogFpar[6,] <- c(34,35,36,37,38,39,40,-1,-1,-1)
conf$keyLogFpar

# //5// Density dependent catchability power parameters (if any) 
conf$keyQpow  

########## process variance for F@age parameters
# //6// Variance of parameters on F - use a single parameter: catch process variance per age
# Coupling of process variance parameters for log(F)-process (normally only first row is used)
conf$keyVarF[1,]                          
# default:[1] 0 0 0 0 0 0 0 0 0 0
conf$keyVarF[1,] <- c(0,1,1,1,1,2,2,2,2,2)

########## process variance for N@age parameters
# //7// Coupling of process variance parameters for log(N)-process -
conf$keyVarLogN 
# default: [1] 0 1 1 1 1 1 1 1 1 1
#conf$keyVarLogN <- c(0,1,2,3,4,5,6,7,8,8)

########## Coupling of the variance parameters for the observations.
# //8// Coupling of the variance parameters on the observations
conf$keyVarObs
names(surveys)
## catch 1-10+
conf$keyVarObs[1,] <- c(0,1,2,2,2,2,3,3,4,5)
## BTS-Isis-early 1-9
conf$keyVarObs[2,] <- c(6,7,8,8,8,8,9,9,9,-1)
## BTS+IBTSQ3: 1-10+
conf$keyVarObs[3,] <- c(10,11,12,12,12,12,13,13,14,15)
## IBTSQ1: 1-8+
conf$keyVarObs[4,] <- c(16,17,17,17,17,17,18,18,-1,-1)
## SNS1: 1-7
conf$keyVarObs[5,] <- c(19,20,21,21,21,21,21,-1,-1,-1)
## SNS2: 1-7
conf$keyVarObs[6,] <- c(22,23,24,24,24,24,24,-1,-1,-1)
conf$keyVarObs

# Covariance structure for each fleet
# //9// Correlation at age between observations -> Covariance structure for each fleet (within fleet)
# ID = independent; AR = AR(1), US = unstructured (= they are correlated, but not in structured way, vb age 1 can correlate with age 7)
conf$obsCorStruct
levels(conf$obsCorStruct)
conf$obsCorStruct  <- c("ID", "ID", "ID", "ID", "AR", "AR")
conf$obsCorStruct  <- factor(conf$obsCorStruct, levels=c("ID", "AR", "US"))
# first is catch, and then rest are tuneing series

# //10// Coupling of correlation parameters can only be specified if the AR(1) structure is chosen above. 
# NA's indicate where correlation parameters can be specified (-1 where they cannot)
conf$keyCorObs
conf$keyCorObs[5,1:6] <- 0
conf$keyCorObs[6,1:6] <- 1

# //11// Stock recruitment code (0 = plain random walk; 1 = Ricker; 2 = Beverton-Holt; 3 = piece-wise constant)
conf$stockRecruitmentModelCode


## Fit the model
conf5     <- conf
par       <- defpar(dat, conf5)
run5      <- sam.fit(dat,conf5, par)  

# Convergence checks
run5$opt$convergence          # 0 = convergence
run5$opt$message              # 4 = ok
AIC(run5)


# save config and result
saveConf(conf5 , file = paste0(result_path, "SAM_run5.cfg"))
save(run5, dat, conf5, surveys, file=paste0(result_path, "SAM_run5.RData"))



########## run6 ---------------------
## coupling survey catchability parameter per age
## coupling oberservation variance per age
## AR covariance structure for SNS
## coupling N(survival) process error -1
rm(conf)
# Create a default parameter configuration object
# make a configuration file for the data, filled in with default.
conf <- defcon(dat)
names(conf)

# //1// MinAge, maxAge, maxAgePlusGroup
conf$minAge          <- minAge                  # minimum age class in assessment
conf$maxAge          <- pGrp                 # maximum age class in assessment
# here it might needs to be changed
names(surveys)
conf$maxAgePlusGroup <- c(1,0,1,1,0,0)  # last age group considered a plus group for each fleet (1 = yes, 0 =no) 
# first is catch
# SNS age 7 is not plus age
# IBTSQ1 age 8 is plus age
# BTS-IBTSQ3 age 10 is plus age
# //15// Define the fbar range
conf$fbarRange       <- meanFages

########## F@age in catch
# //2// Number of parameters describing F-at-age (voor de catch) = coupling of the fishing mortality states
# default
conf$keyLogFsta[1,]
# [1,]    0    1    2    3    4    5    6    7    8     8

# //3// # Correlation of fishing mortality across ages (0 independent, 1 compound symmetry, or 2 AR(1)
# Correlation of fishing mortality across ages
# not possible to decouple it for the time series
# (0 independent, 1 compound symmetry (= zelfde trends between age groups through time), 2 AR(1) (= age groups close together have similar F trend, when distance between ages is larger, then correlation declines),
# 3 seperable AR(1) (= parallel, one up, all up))
conf$corFlag         <- 2                     
# Compound Symmetry. This structure has constant variance and constant covariance.
# This is a first-order autoregressive structure with homogenous variances. The correlation between any two elements is equal to rho for adjacent elements, rho2 for elements that are separated by a third, and so on. is constrained so that -1<<1.


########## selectivity@age parameters in survey
# //4// Number of parameters in the survey processes - coupling of the survey catchability parameters (1st row not used - ze keyLogFsta)
# Coupling of the survey catchability parameters (nomally first row is not used, as that is covered by fishing mortality). 
conf$keyLogFpar 
names(surveys)
## BTS-Isis-early 1-9
conf$keyLogFpar[2,] <- c(0,1,2,3,4,5,6,7,8,-1)
## BTS+IBTSQ3: 1-10+
conf$keyLogFpar[3,] <- c(9,10,11,12,13,14,15,16,17,18)
## IBTSQ1: 1-8+
conf$keyLogFpar[4,] <- c(19,20,21,22,23,24,25,26,-1,-1)
## SNS1: 1-7
conf$keyLogFpar[5,] <- c(27,28,29,30,31,32,33,-1,-1,-1)
## SNS2: 1-7
conf$keyLogFpar[6,] <- c(34,35,36,37,38,39,40,-1,-1,-1)
conf$keyLogFpar

# //5// Density dependent catchability power parameters (if any) 
conf$keyQpow  

########## process variance for F@age parameters
# //6// Variance of parameters on F - use a single parameter: catch process variance per age
# Coupling of process variance parameters for log(F)-process (normally only first row is used)
conf$keyVarF[1,]                          
# default:[1] 0 0 0 0 0 0 0 0 0 0

########## process variance for N@age parameters
# //7// Coupling of process variance parameters for log(N)-process -
conf$keyVarLogN 
# default: [1] 0 1 1 1 1 1 1 1 1 1
conf$keyVarLogN <- c(0,1,2,2,2,3,3,4,5,5)
# [1] 0 1 2 2 3 3 3 4 4 4
#conf$keyVarLogN <- c(0,1,1,1,1,1,2,3,4,5)

########## Coupling of the variance parameters for the observations.
# //8// Coupling of the variance parameters on the observations
conf$keyVarObs
names(surveys)
## catch 1-10+
conf$keyVarObs[1,] <- c(0,1,2,2,2,2,3,3,4,5)
## BTS-Isis-early 1-9
conf$keyVarObs[2,] <- c(6,7,8,8,8,8,9,9,9,-1)
## BTS+IBTSQ3: 1-10+
conf$keyVarObs[3,] <- c(10,11,12,12,12,12,13,13,14,15)
## IBTSQ1: 1-8+
conf$keyVarObs[4,] <- c(16,17,17,17,17,17,18,18,-1,-1)
## SNS1: 1-7
conf$keyVarObs[5,] <- c(19,20,21,21,21,21,21,-1,-1,-1)
## SNS2: 1-7
conf$keyVarObs[6,] <- c(22,23,24,24,24,24,24,-1,-1,-1)
conf$keyVarObs

# Covariance structure for each fleet
# //9// Correlation at age between observations -> Covariance structure for each fleet (within fleet)
# ID = independent; AR = AR(1), US = unstructured (= they are correlated, but not in structured way, vb age 1 can correlate with age 7)
conf$obsCorStruct
levels(conf$obsCorStruct)
conf$obsCorStruct  <- c("ID", "ID", "ID", "ID", "AR", "AR")
conf$obsCorStruct  <- factor(conf$obsCorStruct, levels=c("ID", "AR", "US"))
# first is catch, and then rest are tuneing series

# //10// Coupling of correlation parameters can only be specified if the AR(1) structure is chosen above. 
# NA's indicate where correlation parameters can be specified (-1 where they cannot)
conf$keyCorObs
conf$keyCorObs[5,1:6] <- 0
conf$keyCorObs[6,1:6] <- 1

# //11// Stock recruitment code (0 = plain random walk; 1 = Ricker; 2 = Beverton-Holt; 3 = piece-wise constant)
conf$stockRecruitmentModelCode


## Fit the model
conf6     <- conf
par       <- defpar(dat, conf6)
run6      <- sam.fit(dat,conf6, par)  

# Convergence checks
run6$opt$convergence          # 0 = convergence
run6$opt$message              # 4 = ok
AIC(run6)


# save config and result
saveConf(conf6 , file = paste0(result_path, "SAM_run6.cfg"))
save(run6, dat, conf6, surveys, file=paste0(result_path, "SAM_run6.RData"))

########## run7 best---------------------
## coupling survey catchability parameter per age
## coupling oberservation variance per age
## AR covariance structure for SNS
## coupling N(survival) process error  -2
rm(conf)
# Create a default parameter configuration object
# make a configuration file for the data, filled in with default.
conf <- defcon(dat)
names(conf)

# //1// MinAge, maxAge, maxAgePlusGroup
conf$minAge          <- minAge                  # minimum age class in assessment
conf$maxAge          <- pGrp                 # maximum age class in assessment
# here it might needs to be changed
names(surveys)
conf$maxAgePlusGroup <- c(1,0,1,1,0,0)  # last age group considered a plus group for each fleet (1 = yes, 0 =no) 
# first is catch
# SNS age 7 is not plus age
# IBTSQ1 age 8 is plus age
# BTS-IBTSQ3 age 10 is plus age
# //15// Define the fbar range
conf$fbarRange       <- meanFages

########## F@age in catch
# //2// Number of parameters describing F-at-age (voor de catch) = coupling of the fishing mortality states
# default
conf$keyLogFsta[1,]
# [1,]    0    1    2    3    4    5    6    7    8     8

# //3// # Correlation of fishing mortality across ages (0 independent, 1 compound symmetry, or 2 AR(1)
# Correlation of fishing mortality across ages
# not possible to decouple it for the time series
# (0 independent, 1 compound symmetry (= zelfde trends between age groups through time), 2 AR(1) (= age groups close together have similar F trend, when distance between ages is larger, then correlation declines),
# 3 seperable AR(1) (= parallel, one up, all up))
conf$corFlag         <- 2                     
# Compound Symmetry. This structure has constant variance and constant covariance.
# This is a first-order autoregressive structure with homogenous variances. The correlation between any two elements is equal to rho for adjacent elements, rho2 for elements that are separated by a third, and so on. is constrained so that -1<<1.


########## selectivity@age parameters in survey
# //4// Number of parameters in the survey processes - coupling of the survey catchability parameters (1st row not used - ze keyLogFsta)
# Coupling of the survey catchability parameters (nomally first row is not used, as that is covered by fishing mortality). 
conf$keyLogFpar 
names(surveys)
## BTS-Isis-early 1-9
conf$keyLogFpar[2,] <- c(0,1,2,3,4,5,6,7,8,-1)
## BTS+IBTSQ3: 1-10+
conf$keyLogFpar[3,] <- c(9,10,11,12,13,14,15,16,17,18)
## IBTSQ1: 1-8+
conf$keyLogFpar[4,] <- c(19,20,21,22,23,24,25,26,-1,-1)
## SNS1: 1-7
conf$keyLogFpar[5,] <- c(27,28,29,30,31,32,33,-1,-1,-1)
## SNS2: 1-7
conf$keyLogFpar[6,] <- c(34,35,36,37,38,39,40,-1,-1,-1)
conf$keyLogFpar

# //5// Density dependent catchability power parameters (if any) 
conf$keyQpow  

########## process variance for F@age parameters
# //6// Variance of parameters on F - use a single parameter: catch process variance per age
# Coupling of process variance parameters for log(F)-process (normally only first row is used)
conf$keyVarF[1,]                          
# default:[1] 0 0 0 0 0 0 0 0 0 0

########## process variance for N@age parameters
# //7// Coupling of process variance parameters for log(N)-process -
conf$keyVarLogN 
# default: [1] 0 1 1 1 1 1 1 1 1 1
#conf$keyVarLogN <- c(0,1,2,2,2,3,3,4,5,5)
# [1] 0 1 2 2 3 3 3 4 4 4
conf$keyVarLogN <- c(0,1,1,1,1,1,2,2,3,4)

########## Coupling of the variance parameters for the observations.
# //8// Coupling of the variance parameters on the observations
conf$keyVarObs
names(surveys)
## catch 1-10+
conf$keyVarObs[1,] <- c(0,1,2,2,2,2,3,3,4,5)
## BTS-Isis-early 1-9
conf$keyVarObs[2,] <- c(6,7,8,8,8,8,9,9,9,-1)
## BTS+IBTSQ3: 1-10+
conf$keyVarObs[3,] <- c(10,11,12,12,12,12,13,13,14,15)
## IBTSQ1: 1-8+
conf$keyVarObs[4,] <- c(16,17,17,17,17,17,18,18,-1,-1)
## SNS1: 1-7
conf$keyVarObs[5,] <- c(19,20,21,21,21,21,21,-1,-1,-1)
## SNS2: 1-7
conf$keyVarObs[6,] <- c(22,23,24,24,24,24,24,-1,-1,-1)
conf$keyVarObs

# Covariance structure for each fleet
# //9// Correlation at age between observations -> Covariance structure for each fleet (within fleet)
# ID = independent; AR = AR(1), US = unstructured (= they are correlated, but not in structured way, vb age 1 can correlate with age 7)
conf$obsCorStruct
levels(conf$obsCorStruct)
conf$obsCorStruct  <- c("ID", "ID", "ID", "ID", "AR", "AR")
conf$obsCorStruct  <- factor(conf$obsCorStruct, levels=c("ID", "AR", "US"))
# first is catch, and then rest are tuneing series

# //10// Coupling of correlation parameters can only be specified if the AR(1) structure is chosen above. 
# NA's indicate where correlation parameters can be specified (-1 where they cannot)
conf$keyCorObs
conf$keyCorObs[5,1:6] <- 0
conf$keyCorObs[6,1:6] <- 1

# //11// Stock recruitment code (0 = plain random walk; 1 = Ricker; 2 = Beverton-Holt; 3 = piece-wise constant)
conf$stockRecruitmentModelCode


## Fit the model
conf7     <- conf
par       <- defpar(dat, conf7)
run7      <- sam.fit(dat,conf7, par)  

# Convergence checks
run7$opt$convergence          # 0 = convergence
run7$opt$message              # 4 = ok
AIC(run7)


# save config and result
saveConf(conf7 , file = paste0(result_path, "SAM_run7.cfg"))
save(run7, dat, conf7, surveys, file=paste0(result_path, "SAM_run7.RData"))


########## run8 ---------------------
## coupling survey catchability parameter per age
## coupling oberservation variance per age
## AR covariance structure for SNS
## coupling N(survival) process error  -2
## coupling f process error
rm(conf)
# Create a default parameter configuration object
# make a configuration file for the data, filled in with default.
conf <- defcon(dat)
names(conf)

# //1// MinAge, maxAge, maxAgePlusGroup
conf$minAge          <- minAge                  # minimum age class in assessment
conf$maxAge          <- pGrp                 # maximum age class in assessment
# here it might needs to be changed
names(surveys)
conf$maxAgePlusGroup <- c(1,0,1,1,0,0)  # last age group considered a plus group for each fleet (1 = yes, 0 =no) 
# first is catch
# SNS age 7 is not plus age
# IBTSQ1 age 8 is plus age
# BTS-IBTSQ3 age 10 is plus age
# //15// Define the fbar range
conf$fbarRange       <- meanFages

########## F@age in catch
# //2// Number of parameters describing F-at-age (voor de catch) = coupling of the fishing mortality states
# default
conf$keyLogFsta[1,]
# [1,]    0    1    2    3    4    5    6    7    8     8

# //3// # Correlation of fishing mortality across ages (0 independent, 1 compound symmetry, or 2 AR(1)
# Correlation of fishing mortality across ages
# not possible to decouple it for the time series
# (0 independent, 1 compound symmetry (= zelfde trends between age groups through time), 2 AR(1) (= age groups close together have similar F trend, when distance between ages is larger, then correlation declines),
# 3 seperable AR(1) (= parallel, one up, all up))
conf$corFlag         <- 2                     
# Compound Symmetry. This structure has constant variance and constant covariance.
# This is a first-order autoregressive structure with homogenous variances. The correlation between any two elements is equal to rho for adjacent elements, rho2 for elements that are separated by a third, and so on. is constrained so that -1<<1.


########## selectivity@age parameters in survey
# //4// Number of parameters in the survey processes - coupling of the survey catchability parameters (1st row not used - ze keyLogFsta)
# Coupling of the survey catchability parameters (nomally first row is not used, as that is covered by fishing mortality). 
conf$keyLogFpar 
names(surveys)
## BTS-Isis-early 1-9
conf$keyLogFpar[2,] <- c(0,1,2,3,4,5,6,7,8,-1)
## BTS+IBTSQ3: 1-10+
conf$keyLogFpar[3,] <- c(9,10,11,12,13,14,15,16,17,18)
## IBTSQ1: 1-8+
conf$keyLogFpar[4,] <- c(19,20,21,22,23,24,25,26,-1,-1)
## SNS1: 1-7
conf$keyLogFpar[5,] <- c(27,28,29,30,31,32,33,-1,-1,-1)
## SNS2: 1-7
conf$keyLogFpar[6,] <- c(34,35,36,37,38,39,40,-1,-1,-1)
conf$keyLogFpar

# //5// Density dependent catchability power parameters (if any) 
conf$keyQpow  

########## process variance for F@age parameters
# //6// Variance of parameters on F - use a single parameter: catch process variance per age
# Coupling of process variance parameters for log(F)-process (normally only first row is used)
conf$keyVarF[1,]                          
# default:[1] 0 0 0 0 0 0 0 0 0 0
conf$keyVarF[1,] <- c(0,1,1,1,1,2,2,2,3,3)
  
########## process variance for N@age parameters
# //7// Coupling of process variance parameters for log(N)-process -
conf$keyVarLogN 
# default: [1] 0 1 1 1 1 1 1 1 1 1
#conf$keyVarLogN <- c(0,1,2,2,2,3,3,4,5,5)
# [1] 0 1 2 2 3 3 3 4 4 4
conf$keyVarLogN <- c(0,1,1,1,1,1,2,2,3,4)

########## Coupling of the variance parameters for the observations.
# //8// Coupling of the variance parameters on the observations
conf$keyVarObs
names(surveys)
## catch 1-10+
conf$keyVarObs[1,] <- c(0,1,2,2,2,2,3,3,4,5)
## BTS-Isis-early 1-9
conf$keyVarObs[2,] <- c(6,7,8,8,8,8,9,9,9,-1)
## BTS+IBTSQ3: 1-10+
conf$keyVarObs[3,] <- c(10,11,12,12,12,12,13,13,14,15)
## IBTSQ1: 1-8+
conf$keyVarObs[4,] <- c(16,17,17,17,17,17,18,18,-1,-1)
## SNS1: 1-7
conf$keyVarObs[5,] <- c(19,20,21,21,21,21,21,-1,-1,-1)
## SNS2: 1-7
conf$keyVarObs[6,] <- c(22,23,24,24,24,24,24,-1,-1,-1)
conf$keyVarObs

# Covariance structure for each fleet
# //9// Correlation at age between observations -> Covariance structure for each fleet (within fleet)
# ID = independent; AR = AR(1), US = unstructured (= they are correlated, but not in structured way, vb age 1 can correlate with age 7)
conf$obsCorStruct
levels(conf$obsCorStruct)
conf$obsCorStruct  <- c("ID", "ID", "ID", "ID", "AR", "AR")
conf$obsCorStruct  <- factor(conf$obsCorStruct, levels=c("ID", "AR", "US"))
# first is catch, and then rest are tuneing series

# //10// Coupling of correlation parameters can only be specified if the AR(1) structure is chosen above. 
# NA's indicate where correlation parameters can be specified (-1 where they cannot)
conf$keyCorObs
conf$keyCorObs[5,1:6] <- 0
conf$keyCorObs[6,1:6] <- 1

# //11// Stock recruitment code (0 = plain random walk; 1 = Ricker; 2 = Beverton-Holt; 3 = piece-wise constant)
conf$stockRecruitmentModelCode


## Fit the model
conf8     <- conf
par       <- defpar(dat, conf8)
run8      <- sam.fit(dat,conf8, par)  

# Convergence checks
run8$opt$convergence          # 0 = convergence
run8$opt$message              # 4 = ok
AIC(run8)


# save config and result
saveConf(conf8 , file = paste0(result_path, "SAM_run8.cfg"))
save(run8, dat, conf8, surveys, file=paste0(result_path, "SAM_run8.RData"))


########## run9 does not run---------------------
## coupling survey catchability parameter per age
## coupling oberservation variance per age
## AR covariance structure for SNS
## coupling N(survival) process error  -2
## AR covariance structure for BTS+IBTSQ3
rm(conf)
# Create a default parameter configuration object
# make a configuration file for the data, filled in with default.
conf <- defcon(dat)
names(conf)

# //1// MinAge, maxAge, maxAgePlusGroup
conf$minAge          <- minAge                  # minimum age class in assessment
conf$maxAge          <- pGrp                 # maximum age class in assessment
# here it might needs to be changed
names(surveys)
conf$maxAgePlusGroup <- c(1,0,1,1,0,0)  # last age group considered a plus group for each fleet (1 = yes, 0 =no) 
# first is catch
# SNS age 7 is not plus age
# IBTSQ1 age 8 is plus age
# BTS-IBTSQ3 age 10 is plus age
# //15// Define the fbar range
conf$fbarRange       <- meanFages

########## F@age in catch
# //2// Number of parameters describing F-at-age (voor de catch) = coupling of the fishing mortality states
# default
conf$keyLogFsta[1,]
# [1,]    0    1    2    3    4    5    6    7    8     8

# //3// # Correlation of fishing mortality across ages (0 independent, 1 compound symmetry, or 2 AR(1)
# Correlation of fishing mortality across ages
# not possible to decouple it for the time series
# (0 independent, 1 compound symmetry (= zelfde trends between age groups through time), 2 AR(1) (= age groups close together have similar F trend, when distance between ages is larger, then correlation declines),
# 3 seperable AR(1) (= parallel, one up, all up))
conf$corFlag         <- 2                     
# Compound Symmetry. This structure has constant variance and constant covariance.
# This is a first-order autoregressive structure with homogenous variances. The correlation between any two elements is equal to rho for adjacent elements, rho2 for elements that are separated by a third, and so on. is constrained so that -1<<1.


########## selectivity@age parameters in survey
# //4// Number of parameters in the survey processes - coupling of the survey catchability parameters (1st row not used - ze keyLogFsta)
# Coupling of the survey catchability parameters (nomally first row is not used, as that is covered by fishing mortality). 
conf$keyLogFpar 
names(surveys)
## BTS-Isis-early 1-9
conf$keyLogFpar[2,] <- c(0,1,2,3,4,5,6,7,8,-1)
## BTS+IBTSQ3: 1-10+
conf$keyLogFpar[3,] <- c(9,10,11,12,13,14,15,16,17,18)
## IBTSQ1: 1-8+
conf$keyLogFpar[4,] <- c(19,20,21,22,23,24,25,26,-1,-1)
## SNS1: 1-7
conf$keyLogFpar[5,] <- c(27,28,29,30,31,32,33,-1,-1,-1)
## SNS2: 1-7
conf$keyLogFpar[6,] <- c(34,35,36,37,38,39,40,-1,-1,-1)
conf$keyLogFpar

# //5// Density dependent catchability power parameters (if any) 
conf$keyQpow  

########## process variance for F@age parameters
# //6// Variance of parameters on F - use a single parameter: catch process variance per age
# Coupling of process variance parameters for log(F)-process (normally only first row is used)
conf$keyVarF[1,]                          
# default:[1] 0 0 0 0 0 0 0 0 0 0

########## process variance for N@age parameters
# //7// Coupling of process variance parameters for log(N)-process -
conf$keyVarLogN 
# default: [1] 0 1 1 1 1 1 1 1 1 1
#conf$keyVarLogN <- c(0,1,2,2,2,3,3,4,5,5)
# [1] 0 1 2 2 3 3 3 4 4 4
#conf$keyVarLogN <- c(0,1,1,1,1,1,2,2,3,4)

########## Coupling of the variance parameters for the observations.
# //8// Coupling of the variance parameters on the observations
conf$keyVarObs
names(surveys)
## catch 1-10+
conf$keyVarObs[1,] <- c(0,1,2,2,2,2,3,3,4,5)
## BTS-Isis-early 1-9
conf$keyVarObs[2,] <- c(6,7,8,8,8,8,9,9,9,-1)
## BTS+IBTSQ3: 1-10+
conf$keyVarObs[3,] <- c(10,11,12,12,12,12,13,13,14,15)
## IBTSQ1: 1-8+
conf$keyVarObs[4,] <- c(16,17,17,17,17,17,18,18,-1,-1)
## SNS1: 1-7
conf$keyVarObs[5,] <- c(19,20,21,21,21,21,21,-1,-1,-1)
## SNS2: 1-7
conf$keyVarObs[6,] <- c(22,23,24,24,24,24,24,-1,-1,-1)
conf$keyVarObs

# Covariance structure for each fleet
# //9// Correlation at age between observations -> Covariance structure for each fleet (within fleet)
# ID = independent; AR = AR(1), US = unstructured (= they are correlated, but not in structured way, vb age 1 can correlate with age 7)
conf$obsCorStruct
levels(conf$obsCorStruct)
conf$obsCorStruct  <- c("ID", "AR", "ID", "ID", "AR", "AR")
conf$obsCorStruct  <- factor(conf$obsCorStruct, levels=c("ID", "AR", "US"))
# first is catch, and then rest are tuneing series

# //10// Coupling of correlation parameters can only be specified if the AR(1) structure is chosen above. 
# NA's indicate where correlation parameters can be specified (-1 where they cannot)
conf$keyCorObs
conf$keyCorObs[3,1:9] <- 0
conf$keyCorObs[5,1:6] <- 1
conf$keyCorObs[6,1:6] <- 2

# //11// Stock recruitment code (0 = plain random walk; 1 = Ricker; 2 = Beverton-Holt; 3 = piece-wise constant)
conf$stockRecruitmentModelCode


## Fit the model
conf9     <- conf
par       <- defpar(dat, conf9)
run9      <- sam.fit(dat,conf9, par)  

# Convergence checks
run9$opt$convergence          # 0 = convergence
run9$opt$message              # 4 = ok
AIC(run9)


# save config and result
saveConf(conf8 , file = paste0(result_path, "SAM_run8.cfg"))
save(run8, dat, conf8, surveys, file=paste0(result_path, "SAM_run8.RData"))


########## run10 does not run---------------------
## coupling survey catchability parameter per age
## coupling oberservation variance per age
## AR covariance structure for SNS
## coupling N(survival) process error  -2
## seperate f@age para in age 9 and 10
rm(conf)
# Create a default parameter configuration object
# make a configuration file for the data, filled in with default.
conf <- defcon(dat)
names(conf)

# //1// MinAge, maxAge, maxAgePlusGroup
conf$minAge          <- minAge                  # minimum age class in assessment
conf$maxAge          <- pGrp                 # maximum age class in assessment
# here it might needs to be changed
names(surveys)
conf$maxAgePlusGroup <- c(1,0,1,1,0,0)  # last age group considered a plus group for each fleet (1 = yes, 0 =no) 
# first is catch
# SNS age 7 is not plus age
# IBTSQ1 age 8 is plus age
# BTS-IBTSQ3 age 10 is plus age
# //15// Define the fbar range
conf$fbarRange       <- meanFages

########## F@age in catch
# //2// Number of parameters describing F-at-age (voor de catch) = coupling of the fishing mortality states
# default
conf$keyLogFsta[1,]
conf$keyLogFsta[1,] <- c(0,1,2,3,4,5,6,7,8,9)
# [1,]    0    1    2    3    4    5    6    7    8     8

# //3// # Correlation of fishing mortality across ages (0 independent, 1 compound symmetry, or 2 AR(1)
# Correlation of fishing mortality across ages
# not possible to decouple it for the time series
# (0 independent, 1 compound symmetry (= zelfde trends between age groups through time), 2 AR(1) (= age groups close together have similar F trend, when distance between ages is larger, then correlation declines),
# 3 seperable AR(1) (= parallel, one up, all up))
conf$corFlag         <- 2                     
# Compound Symmetry. This structure has constant variance and constant covariance.
# This is a first-order autoregressive structure with homogenous variances. The correlation between any two elements is equal to rho for adjacent elements, rho2 for elements that are separated by a third, and so on. is constrained so that -1<<1.


########## selectivity@age parameters in survey
# //4// Number of parameters in the survey processes - coupling of the survey catchability parameters (1st row not used - ze keyLogFsta)
# Coupling of the survey catchability parameters (nomally first row is not used, as that is covered by fishing mortality). 
conf$keyLogFpar 
names(surveys)
## BTS-Isis-early 1-9
conf$keyLogFpar[2,] <- c(0,1,2,3,4,5,6,7,8,-1)
## BTS+IBTSQ3: 1-10+
conf$keyLogFpar[3,] <- c(9,10,11,12,13,14,15,16,17,18)
## IBTSQ1: 1-8+
conf$keyLogFpar[4,] <- c(19,20,21,22,23,24,25,26,-1,-1)
## SNS1: 1-7
conf$keyLogFpar[5,] <- c(27,28,29,30,31,32,33,-1,-1,-1)
## SNS2: 1-7
conf$keyLogFpar[6,] <- c(34,35,36,37,38,39,40,-1,-1,-1)
conf$keyLogFpar

# //5// Density dependent catchability power parameters (if any) 
conf$keyQpow  

########## process variance for F@age parameters
# //6// Variance of parameters on F - use a single parameter: catch process variance per age
# Coupling of process variance parameters for log(F)-process (normally only first row is used)
conf$keyVarF[1,]                          
# default:[1] 0 0 0 0 0 0 0 0 0 0

########## process variance for N@age parameters
# //7// Coupling of process variance parameters for log(N)-process -
conf$keyVarLogN 
# default: [1] 0 1 1 1 1 1 1 1 1 1
#conf$keyVarLogN <- c(0,1,2,2,2,3,3,4,5,5)
# [1] 0 1 2 2 3 3 3 4 4 4
conf$keyVarLogN <- c(0,1,1,1,1,1,2,2,3,4)

########## Coupling of the variance parameters for the observations.
# //8// Coupling of the variance parameters on the observations
conf$keyVarObs
names(surveys)
## catch 1-10+
conf$keyVarObs[1,] <- c(0,1,2,2,2,2,3,3,4,5)
## BTS-Isis-early 1-9
conf$keyVarObs[2,] <- c(6,7,8,8,8,8,9,9,9,-1)
## BTS+IBTSQ3: 1-10+
conf$keyVarObs[3,] <- c(10,11,12,12,12,12,13,13,14,15)
## IBTSQ1: 1-8+
conf$keyVarObs[4,] <- c(16,17,17,17,17,17,18,18,-1,-1)
## SNS1: 1-7
conf$keyVarObs[5,] <- c(19,20,21,21,21,21,21,-1,-1,-1)
## SNS2: 1-7
conf$keyVarObs[6,] <- c(22,23,24,24,24,24,24,-1,-1,-1)
conf$keyVarObs

# Covariance structure for each fleet
# //9// Correlation at age between observations -> Covariance structure for each fleet (within fleet)
# ID = independent; AR = AR(1), US = unstructured (= they are correlated, but not in structured way, vb age 1 can correlate with age 7)
conf$obsCorStruct
levels(conf$obsCorStruct)
conf$obsCorStruct  <- c("ID", "ID", "ID", "ID", "AR", "AR")
conf$obsCorStruct  <- factor(conf$obsCorStruct, levels=c("ID", "AR", "US"))
# first is catch, and then rest are tuneing series

# //10// Coupling of correlation parameters can only be specified if the AR(1) structure is chosen above. 
# NA's indicate where correlation parameters can be specified (-1 where they cannot)
conf$keyCorObs
conf$keyCorObs[5,1:6] <- 0
conf$keyCorObs[6,1:6] <- 1

# //11// Stock recruitment code (0 = plain random walk; 1 = Ricker; 2 = Beverton-Holt; 3 = piece-wise constant)
conf$stockRecruitmentModelCode


## Fit the model
conf10     <- conf
par       <- defpar(dat, conf10)
run10      <- sam.fit(dat,conf10, par)  

# Convergence checks
run10$opt$convergence          # 0 = convergence
run10$opt$message              # 4 = ok
AIC(run10)


# save config and result
saveConf(conf10 , file = paste0(result_path, "SAM_run10.cfg"))
save(run10, dat, conf10, surveys, file=paste0(result_path, "SAM_run10.RData"))


########## run11---------------------
## coupling survey catchability parameter per age
## coupling oberservation variance per age
## coupling N(survival) process error  -2
## seperate BTS+IBTSQ3 into 3 periods: 1996-2010, 2011-2020

file_names        <- c("cn.dat", "cw.dat", "dw.dat", "lf.dat", "lw.dat", 
                       "mo.dat", "nm.dat", "pf.dat", "pm.dat", "sw.dat", 
                       "fleet_WKNSCS_2022.dat")
stock_data        <- lapply(file_names,function(x)read.ices(file.path(data_path,x))) 
# stock_data      <- lapply(file_names, read.ices.from.disk, stock_input_path = file.path("bootstrap/data/"))
names(stock_data) <- unlist(strsplit(file_names,".dat"))

## change fleet name
names(stock_data)[names(stock_data)== "fleet_WKNSCS_2022"] <- "survey"

## select survey and age
names(stock_data$survey)
#surveys <- stock_data$survey[c(1,7,8,10,11)]
surveys <- stock_data$survey[c(1,12,13,8,10,11)]
names(surveys)

## set SAM readable object
dat <- setup.sam.data(surveys=surveys,
                      residual.fleet=stock_data$cn, 
                      prop.mature=stock_data$mo, 
                      stock.mean.weight=stock_data$sw, 
                      catch.mean.weight=stock_data$cw, 
                      dis.mean.weight=stock_data$dw, 
                      land.mean.weight=stock_data$lw,
                      prop.f=stock_data$pf, 
                      prop.m=stock_data$pm, 
                      natural.mortality=stock_data$nm, 
                      land.frac=stock_data$lf)
names(dat)

rm(conf)
# Create a default parameter configuration object
# make a configuration file for the data, filled in with default.
conf <- defcon(dat)
names(conf)

# //1// MinAge, maxAge, maxAgePlusGroup
conf$minAge          <- minAge                  # minimum age class in assessment
conf$maxAge          <- pGrp                 # maximum age class in assessment
# here it might needs to be changed
names(surveys)
conf$maxAgePlusGroup <- c(1,0,1,1,1,0,0)  # last age group considered a plus group for each fleet (1 = yes, 0 =no) 
# first is catch
# SNS age 7 is not plus age
# IBTSQ1 age 8 is plus age
# BTS-IBTSQ3 age 10 is plus age
# //15// Define the fbar range
conf$fbarRange       <- meanFages

########## F@age in catch
# //2// Number of parameters describing F-at-age (voor de catch) = coupling of the fishing mortality states
# default
conf$keyLogFsta[1,]
# [1,]    0    1    2    3    4    5    6    7    8     8

# //3// # Correlation of fishing mortality across ages (0 independent, 1 compound symmetry, or 2 AR(1)
# Correlation of fishing mortality across ages
# not possible to decouple it for the time series
# (0 independent, 1 compound symmetry (= zelfde trends between age groups through time), 2 AR(1) (= age groups close together have similar F trend, when distance between ages is larger, then correlation declines),
# 3 seperable AR(1) (= parallel, one up, all up))
conf$corFlag         <- 2                     
# Compound Symmetry. This structure has constant variance and constant covariance.
# This is a first-order autoregressive structure with homogenous variances. The correlation between any two elements is equal to rho for adjacent elements, rho2 for elements that are separated by a third, and so on. is constrained so that -1<<1.


########## selectivity@age parameters in survey
# //4// Number of parameters in the survey processes - coupling of the survey catchability parameters (1st row not used - ze keyLogFsta)
# Coupling of the survey catchability parameters (nomally first row is not used, as that is covered by fishing mortality). 
conf$keyLogFpar 
names(surveys)
## BTS-Isis-early 1-9
conf$keyLogFpar[2,] <- c(0,1,2,3,4,5,6,7,8,-1)
## BTS+IBTSQ3-1: 1-10+
conf$keyLogFpar[3,] <- c(9,10,11,12,13,14,15,16,17,18)
## BTS+IBTSQ3-2: 1-10+
conf$keyLogFpar[4,] <- c(19,20,21,22,23,24,25,26,27,28)
## IBTSQ1: 1-8+
conf$keyLogFpar[5,] <- c(29,30,31,32,33,34,35,36,-1,-1)
## SNS1: 1-7
conf$keyLogFpar[6,] <- c(37,38,39,40,41,42,43,-1,-1,-1)
## SNS2: 1-7
conf$keyLogFpar[7,] <- c(44,45,46,47,48,49,50,-1,-1,-1)
conf$keyLogFpar

# //5// Density dependent catchability power parameters (if any) 
conf$keyQpow  

########## process variance for F@age parameters
# //6// Variance of parameters on F - use a single parameter: catch process variance per age
# Coupling of process variance parameters for log(F)-process (normally only first row is used)
conf$keyVarF[1,]                          
# default:[1] 0 0 0 0 0 0 0 0 0 0

########## process variance for N@age parameters
# //7// Coupling of process variance parameters for log(N)-process -
conf$keyVarLogN 
# default: [1] 0 1 1 1 1 1 1 1 1 1

########## Coupling of the variance parameters for the observations.
# //8// Coupling of the variance parameters on the observations
conf$keyVarObs
names(surveys)
## catch 1-10+
conf$keyVarObs[1,] <- c(0,1,2,2,2,2,3,3,4,5)
## BTS-Isis-early 1-9
conf$keyVarObs[2,] <- c(6,7,8,8,8,8,9,9,9,-1)
## BTS+IBTSQ3-1: 1-10+
conf$keyVarObs[3,] <- c(10,11,12,12,12,12,13,13,14,15)
## BTS+IBTSQ3-2: 1-10+
conf$keyVarObs[4,] <- c(16,17,18,18,18,18,19,19,20,21)
## IBTSQ1: 1-8+
conf$keyVarObs[5,] <- c(22,23,23,23,23,23,24,24,-1,-1)
## SNS1: 1-7
conf$keyVarObs[6,] <- c(25,26,27,27,27,27,27,-1,-1,-1)
## SNS2: 1-7
conf$keyVarObs[7,] <- c(28,28,30,30,30,30,30,-1,-1,-1)
conf$keyVarObs


# Covariance structure for each fleet
# //9// Correlation at age between observations -> Covariance structure for each fleet (within fleet)
# ID = independent; AR = AR(1), US = unstructured (= they are correlated, but not in structured way, vb age 1 can correlate with age 7)
conf$obsCorStruct 
# first is catch, and then rest are tuneing series

# //10// Coupling of correlation parameters can only be specified if the AR(1) structure is chosen above. 
# NA's indicate where correlation parameters can be specified (-1 where they cannot)
conf$keyCorObs

# //11// Stock recruitment code (0 = plain random walk; 1 = Ricker; 2 = Beverton-Holt; 3 = piece-wise constant)
conf$stockRecruitmentModelCode


## Fit the model
conf11     <- conf
par       <- defpar(dat, conf11)
run11      <- sam.fit(dat,conf11, par)  

# Convergence checks
run11$opt$convergence          # 0 = convergence
run11$opt$message              # 4 = ok
AIC(run11)


# save config and result
saveConf(conf11 , file = paste0(result_path, "SAM_run11.cfg"))
save(run11, dat, conf11, surveys, file=paste0(result_path, "SAM_run11.RData"))


########## run12 does not run---------------------
## coupling survey catchability parameter per age
## coupling oberservation variance per age
## AR covariance structure for SNS
## coupling N(survival) process error  -2
## seperate BTS+IBTSQ3 into 3 periods: 1996-2010, 2011-2020

file_names        <- c("cn.dat", "cw.dat", "dw.dat", "lf.dat", "lw.dat", 
                       "mo.dat", "nm.dat", "pf.dat", "pm.dat", "sw.dat", 
                       "fleet_WKNSCS_2022.dat")
stock_data        <- lapply(file_names,function(x)read.ices(file.path(data_path,x))) 
# stock_data      <- lapply(file_names, read.ices.from.disk, stock_input_path = file.path("bootstrap/data/"))
names(stock_data) <- unlist(strsplit(file_names,".dat"))

## change fleet name
names(stock_data)[names(stock_data)== "fleet_WKNSCS_2022"] <- "survey"

## select survey and age
names(stock_data$survey)
#surveys <- stock_data$survey[c(1,7,8,10,11)]
surveys <- stock_data$survey[c(1,12,13,8,10,11)]
names(surveys)

## set SAM readable object
dat <- setup.sam.data(surveys=surveys,
                      residual.fleet=stock_data$cn, 
                      prop.mature=stock_data$mo, 
                      stock.mean.weight=stock_data$sw, 
                      catch.mean.weight=stock_data$cw, 
                      dis.mean.weight=stock_data$dw, 
                      land.mean.weight=stock_data$lw,
                      prop.f=stock_data$pf, 
                      prop.m=stock_data$pm, 
                      natural.mortality=stock_data$nm, 
                      land.frac=stock_data$lf)
names(dat)

rm(conf)
# Create a default parameter configuration object
# make a configuration file for the data, filled in with default.
conf <- defcon(dat)
names(conf)

# //1// MinAge, maxAge, maxAgePlusGroup
conf$minAge          <- minAge                  # minimum age class in assessment
conf$maxAge          <- pGrp                 # maximum age class in assessment
# here it might needs to be changed
names(surveys)
conf$maxAgePlusGroup <- c(1,0,1,1,1,0,0)  # last age group considered a plus group for each fleet (1 = yes, 0 =no) 
# first is catch
# SNS age 7 is not plus age
# IBTSQ1 age 8 is plus age
# BTS-IBTSQ3 age 10 is plus age
# //15// Define the fbar range
conf$fbarRange       <- meanFages

########## F@age in catch
# //2// Number of parameters describing F-at-age (voor de catch) = coupling of the fishing mortality states
# default
conf$keyLogFsta[1,]
# [1,]    0    1    2    3    4    5    6    7    8     8

# //3// # Correlation of fishing mortality across ages (0 independent, 1 compound symmetry, or 2 AR(1)
# Correlation of fishing mortality across ages
# not possible to decouple it for the time series
# (0 independent, 1 compound symmetry (= zelfde trends between age groups through time), 2 AR(1) (= age groups close together have similar F trend, when distance between ages is larger, then correlation declines),
# 3 seperable AR(1) (= parallel, one up, all up))
conf$corFlag         <- 2                     
# Compound Symmetry. This structure has constant variance and constant covariance.
# This is a first-order autoregressive structure with homogenous variances. The correlation between any two elements is equal to rho for adjacent elements, rho2 for elements that are separated by a third, and so on. is constrained so that -1<<1.


########## selectivity@age parameters in survey
# //4// Number of parameters in the survey processes - coupling of the survey catchability parameters (1st row not used - ze keyLogFsta)
# Coupling of the survey catchability parameters (nomally first row is not used, as that is covered by fishing mortality). 
conf$keyLogFpar 
names(surveys)
## BTS-Isis-early 1-9
conf$keyLogFpar[2,] <- c(0,1,2,3,4,5,6,7,8,-1)
## BTS+IBTSQ3-1: 1-10+
conf$keyLogFpar[3,] <- c(9,10,11,12,13,14,15,16,17,18)
## BTS+IBTSQ3-2: 1-10+
conf$keyLogFpar[4,] <- c(19,20,21,22,23,24,25,26,27,28)
## IBTSQ1: 1-8+
conf$keyLogFpar[5,] <- c(29,30,31,32,33,34,35,36,-1,-1)
## SNS1: 1-7
conf$keyLogFpar[6,] <- c(37,38,39,40,41,42,43,-1,-1,-1)
## SNS2: 1-7
conf$keyLogFpar[7,] <- c(44,45,46,47,48,49,50,-1,-1,-1)
conf$keyLogFpar

# //5// Density dependent catchability power parameters (if any) 
conf$keyQpow  

########## process variance for F@age parameters
# //6// Variance of parameters on F - use a single parameter: catch process variance per age
# Coupling of process variance parameters for log(F)-process (normally only first row is used)
conf$keyVarF[1,]                          
# default:[1] 0 0 0 0 0 0 0 0 0 0

########## process variance for N@age parameters
# //7// Coupling of process variance parameters for log(N)-process -
conf$keyVarLogN 
# default: [1] 0 1 1 1 1 1 1 1 1 1

########## Coupling of the variance parameters for the observations.
# //8// Coupling of the variance parameters on the observations
conf$keyVarObs
names(surveys)
## catch 1-10+
conf$keyVarObs[1,] <- c(0,1,2,2,2,2,2,2,4,4)
## BTS-Isis-early 1-9
conf$keyVarObs[2,] <- c(6,7,8,8,8,8,9,9,9,-1)
## BTS+IBTSQ3-1: 1-10+
conf$keyVarObs[3,] <- c(10,11,11,11,11,11,11,11,12,12)
## BTS+IBTSQ3-2: 1-10+
conf$keyVarObs[4,] <- c(16,17,17,17,17,17,17,17,18,18)
## IBTSQ1: 1-8+
conf$keyVarObs[5,] <- c(22,23,23,23,23,23,24,24,-1,-1)
## SNS1: 1-7
conf$keyVarObs[6,] <- c(25,26,27,27,27,27,27,-1,-1,-1)
## SNS2: 1-7
conf$keyVarObs[7,] <- c(28,28,30,30,30,30,30,-1,-1,-1)
conf$keyVarObs


# Covariance structure for each fleet
# //9// Correlation at age between observations -> Covariance structure for each fleet (within fleet)
# ID = independent; AR = AR(1), US = unstructured (= they are correlated, but not in structured way, vb age 1 can correlate with age 7)
conf$obsCorStruct 
# first is catch, and then rest are tuneing series

# //10// Coupling of correlation parameters can only be specified if the AR(1) structure is chosen above. 
# NA's indicate where correlation parameters can be specified (-1 where they cannot)
conf$keyCorObs

# //11// Stock recruitment code (0 = plain random walk; 1 = Ricker; 2 = Beverton-Holt; 3 = piece-wise constant)
conf$stockRecruitmentModelCode


## Fit the model
conf12     <- conf
par       <- defpar(dat, conf12)
run12      <- sam.fit(dat,conf12, par)  

# Convergence checks
run11$opt$convergence          # 0 = convergence
run11$opt$message              # 4 = ok
AIC(run11)


# save config and result
saveConf(conf11 , file = paste0(result_path, "SAM_run11.cfg"))
save(run11, dat, conf11, surveys, file=paste0(result_path, "SAM_run11.RData"))
