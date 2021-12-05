rm(list = ls())

library(ggplot2)
library(ggpubr)
library(patchwork)
library(vmstools)  ## used under R 3.6.1
library(mapplots)
library(mapdata)
library(tidyverse)
library(lattice)
library(ggalluvial)
library(RColorBrewer)
library(colorRamps)
library(surveyIndex)
library(maps)
#display.brewer.all()
colset      <- c(brewer.pal(9, "Set1"), "black")


load("C:/Users/chen072/OneDrive - WageningenUR/0_2021_plaice_benchmark/10_combined_BTS_IBTSQ3/Model_result/plaice_comb_BTS_IBTSQ3_run1.RData")
last_data_year <- 2020

#################################################
#################################################
## 2. delta-gam model ---------------

mysurvey       <- "BTS_IBTSQ3"

## assign biomass as fake number at age 1
mydQ3[[2]]$Nage <- matrix(mydQ3$HaulWgt, ncol=1) 
colnames( mydQ3[[2]]$Nage ) <- 1
agesQ3         <- 1

## Make ctime : a numeric time variable 
mydQ3$ctime    <- as.numeric(as.character(mydQ3$Year))

## define survey year
survey_year    <- 1996:last_data_year

#######################
## Model formulae : ship + haulduration
#######################

## non-Stationary model: 
## ship+haulduration
## k=5 for ctime
modelsNonStat1 <- rep("Year+te(ctime,lon,lat,d=c(1,2),bs=c('cs','tp'),k=c(5,25))+s(Ship, bs='re')+s(Depth,bs='ts',k=6)+offset(log(HaulDur))",length(agesQ3))

## BTS:    cutOff=0.5
## IBTSQ3: cutOff=0.15

mycutoff         <- 0.15

mc.cores         <- 1

combmodels1       <- list()
combmodels1$SI.ac <- getSurveyIdx(mydQ3,
                                  ages=agesQ3,                 
                                  myids=grid.comb[[3]],
                                  cutOff=mycutoff,
                                  fam="LogNormal",
                                  mc.cores=mc.cores,
                                  modelZ=modelsNonStat1,
                                  modelP=modelsNonStat1)

## save as Rdata
save(combmodels1, mydQ3, agesQ3, grid.comb, file=paste(my_result_path, "Model_result/plaice_comb_BTS_IBTSQ3_run1.RData", sep="")) 

## save to dat file
exportSI(combmodels1$SI.ac$idx,agesQ3,survey_year,toy=mean(mydQ3[[2]]$timeOfYear,na.rm=TRUE),
         file=paste(my_result_path, "Model_result/indices_", mysurvey,"_WGNSSK_", work_year, "_run1.dat", sep=""),
         nam=paste("NS Plaice ; Last age is plus group, calculated",Sys.time()))

## save to csv
write.csv(combmodels1$SI.ac$idx, file=paste(my_result_path, "Model_result/","indices_", mysurvey,"_WGNSSK_", work_year, "_run1_wide.csv", sep=""))

## write to csv file
myfilepath <- paste(my_result_path, "Model_result/result_", mysurvey, "_WGNSSK_", work_year, "_run1.csv", sep="")
write_to_csv(combmodels1, myfilepath)




#################################################
#################################################
## 1. our own model ---------------

dd <- mydQ3[[2]]
temp <- mydQ3[[3]]

summary(dd$HaulWgt)
dd$HaulWgt1 <- log(dd$HaulWgt+1)
hist(dd$HaulWgt1)

dd$Ship <- factor(dd$Ship)
dd$Year  <- factor(dd$Year)
dd$log_duration <- log(dd$HaulDur)
dd$year1 <- as.numeric(as.character(dd$Year))



library(mgcv)
library(mgcViz)

m1 <- gam(HaulWgt1 ~ Year + s(Depth,bs='ts',k=6)+
            s(lon, lat,bs=c('tp')) + s(Ship, bs='re') + 
            offset(log_duration),method="REML",
          data=dd)
AIC(m1)

m2 <- gam(HaulWgt1 ~ Year + s(Depth,bs='ts',k=6)+
            te(year1,lon,lat,d=c(1,2),bs=c('cs','tp'),k=c(5,25)) + 
            s(Ship, bs='re') + 
            offset(log_duration),method="REML",
          data=dd)
AIC(m2)



#################################################
#################################################
## model result

mymod <- m2

AIC(mymod)
gam.check(mymod)
summary(mymod)
gam.vcomp(mymod)
vis.gam(mymod)
plot(mymod, se=TRUE, select=1,scheme = 2)
plot(mymod, se=TRUE, select=2,scheme = 2)

## plot using mgcViz package
## https://mfasiolo.github.io/mgcViz/articles/mgcviz.html
b <- getViz(mymod)
plot(sm(b, 1)) + l_fitRaster() + l_fitContour() + l_points()
plot(sm(b, 2)) + l_fitLine(colour = 2, linetype = 2) + l_points() + 
  l_ciLine(colour = 4, linetype = 3)

aa <- data.frame(Ship=levels(mymod$model[,c("Ship")]),
                 coef =coef(mymod)[grep("Ship", names(coef(mymod)))])
ggplot(data=aa, aes(x=Ship, y=coef)) +
  geom_bar(stat="identity")+
  theme_bw()+
  theme(plot.title = element_text(color="black", size=20, face="bold"), 
        legend.position = "right", 
        legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=15, face="plain"),
        axis.text.x = element_text(color = "black", size = 15, angle = 90),
        axis.text.y = element_text(color = "black", size = 10, angle = 0),
        axis.title.x = element_text(color = "black", size = 15, angle = 0),
        axis.title.y = element_text(color = "black", size = 15, angle = 90))

## plot model coefficient
aa <- data.frame(year=levels(mymod$model[,c("Year")])[-1],
                 coef =coef(mymod)[grep("Year", names(coef(mymod)))])
ggplot(data=aa, aes(x=year, y=coef)) +
  geom_bar(stat="identity")+
  theme_bw()+
  theme(plot.title = element_text(color="black", size=20, face="bold"), 
        legend.position = "right", 
        legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=15, face="plain"),
        axis.text.x = element_text(color = "black", size = 15, angle = 90),
        axis.text.y = element_text(color = "black", size = 10, angle = 0),
        axis.title.x = element_text(color = "black", size = 15, angle = 0),
        axis.title.y = element_text(color = "black", size = 15, angle = 90))

## prediction at fixed locations: i.e. spatial effect
myxlim <- range(dat1$lon)
myylim <- range(dat1$lat)
vis.gam(m1 , view=c("lon", "lat"), plot.type="contour")  ## heatmap, white means higher value
maps::map("world",res=0,xlim=myxlim,ylim=myylim,fill=T,col="grey90", border="grey50",add=T)

myxlim <- range(dat1$lon)
myylim <- range(dat1$lat)

## prediction at fixed locations given a year
vis.gam(m3 , view=c("lon", "lat"), cond=list(gear1="PUL"),plot.type="contour",
        main="XX")
## prediction at fixed locations given a year
iyear <- 2009
vis.gam(mymod , view=c("lon", "lat"), cond=list(Year=iyear),plot.type="contour",
        main=iyear, type="response", zlim=c(0,10))


## pick up center ICES rectangles in each RA area
## predict abundance and sum up per RA

## TBB, Q3: predict total catch given the actual fishing effort allocation per RA per rect
## increase the effort in each RA




