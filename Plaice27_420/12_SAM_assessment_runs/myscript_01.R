rm(list=ls())
# Load libraries
library(icesTAF)
library("stockassessment")
library("TMB")
library(FLCore)
library("viridis")
library(reshape2)
library(plyr) 
library(dplyr)
library(ggplot2)
library(reshape)
library(devtools)
library(ggloop)
library(multipanelfigure)
library("flextable")

## Set path
#setwd("~/Development/RStudio/D1VISBIO/NDGP/ICES/ASSESSMENTS/SOL_7D/WKNSEA 2021/SAM/")
#maindir <- getwd()
#data_path    <- paste0(maindir,'/INPUT')
#result_path  <- paste0(maindir,'/OUTPUT/Run4tris')
#rdir         <- paste0(maindir,'/R')

project_path <- "C:/Users/chen072/OneDrive - WageningenUR/0_2021_plaice_benchmark/12_SAM_assessment_runs/"

## functions
source(paste0(project_path,"functions_ple27420_assessment.R"))


## settings 
## mean F ages (total&landings and for discards)
meanFages  <- c(2:6)
meanFDages <- c(2:3)

## Plusgroup age
pGrp       <- 10

##############################################################
## 1. load raw data, plus age prossing, SOP correction -------------
## Units
Units      <- c("tonnes","thousands","kg") 

## Read data
stock              <- readFLStock(paste0(project_path,"data_raw/index.txt"))
units(stock)[1:17] <- as.list(c(rep(Units,4), "NA", "NA", "f", "NA", "NA"))
slotNames(stock)
years              <- as.numeric(dimnames(stock@stock.n)$year)
startyr            <- range(stock)[["minyear"]]

## SOP correction
soplan           <- sop(stock,"landings")
print(paste("Landings SOP range: ",range(soplan)[1]," - ",range(soplan)[2],sep=""))
stock@landings.n <- sweep(stock@landings.n, 2, soplan, FUN="/")  
sopc             <- sop(stock,"landings")
if(round(sum(sopc-1),1)!=0) print("SOP correction failed") else print("SOP correction successful")

## Add up totals
stock     <- totalStk(stock, Units)

## New catch runs
stock  <- setPlusGroup(stock, plusgroup=pGrp,na.rm=TRUE)
range(stock)[["minfbar"]] <- min(meanFages)
range(stock)[["maxfbar"]] <- max(meanFages)

## final stock: SOP corrected landings, plus age calculation

##############################################################
## 2. extra processing of some years catch data -------------
stock <- extra_processing_data_year_2019(stock)

##############################################################
## 3. save mean catch weight/number, landing frac, as input for SAM  -------------
extra_processing_Lowestoft_format(stock, project_path)

