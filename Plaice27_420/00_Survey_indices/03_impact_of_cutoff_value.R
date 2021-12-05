## investigate cut-off value
library(ggplot2)
library(ggpubr)

mysurvey       <- "IBTSQ1"
## load data after applying alk
load(file=paste(mydatapath, "Data/plaice_", mysurvey, "_",work_year, "_alk_swept_processed.RData", sep="")) 


### investigate impact of curoff value

Nage        <- mydQ.IBTSQ1[[2]]$Nage


### investigate impact of curoff value

Nage        <- mydQ.IBTSQ1[[2]]$Nage

ddd <- data.frame(Year=rep(mydQ.IBTSQ1[[2]]$Year, 8),
                  Age=c(rep(1, nrow(mydQ.IBTSQ1[[2]])), 
                        rep(2, nrow(mydQ.IBTSQ1[[2]])), 
                        rep(3, nrow(mydQ.IBTSQ1[[2]])),
                        rep(4, nrow(mydQ.IBTSQ1[[2]])),
                        rep(5, nrow(mydQ.IBTSQ1[[2]])),
                        rep(6, nrow(mydQ.IBTSQ1[[2]])),
                        rep(7, nrow(mydQ.IBTSQ1[[2]])),
                        rep(8, nrow(mydQ.IBTSQ1[[2]]))),
                  n=c(Nage[,1],Nage[,2],Nage[,3],Nage[,4],Nage[,5],Nage[,6],Nage[,7],Nage[,8])
)
                  

f1 <- ggplot(ddd[ddd$Age==7,], aes(x=log(n+0.005))) + 
  geom_histogram(color="black", fill="white")+
  geom_vline(aes(xintercept = log(0.01+0.005)),colour = "red")+
  geom_vline(aes(xintercept = log(0.1+0.005)), colour = "blue") +
  ggtitle("IBTSQ1: histogram of age 5 after alk") 

f2 <- facet(f1, facet.by = c("Year"), ncol = 2, scales = "free_y", panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) +
  theme(plot.title = element_text(color="black", size=16, face="bold"), 
        legend.position = "bottom", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=10, face="plain"),
        axis.text.x = element_text(color = "black", size = 14, angle = 90),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.x = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14, angle = 90))+
  ylab("")+ xlab("") 
#scale_x_continuous(name="", breaks=2005:2018, labels=unique(dat$Year))
plot(f2)  



ddd$zero <- 0

ddd$zero[ddd$n<0.01] <- 1

## proportion of zero
temp1 <- aggregate(zero ~ Year+Age, FUN=sum, data=ddd)
temp2 <- aggregate(zero ~ Year+Age, FUN=length, data=ddd)
temp1 <- merge(temp1, temp2, by=c("Year", "Age"))
temp1$prop <- temp1$zero.x/temp1$zero.y
temp1$cutoff <- "0.01"

## median number of non-zero
aa1 <- aggregate(n ~ Year + Age + zero, FUN=median, data=ddd)
names(aa1)[4] <- "median_non_zero"
aa1 <- aa1[aa1$zero==0,]
aa1$cutoff <- "0.01"


ddd$zero <- 0

ddd$zero[ddd$n<0.1] <- 1

temp3 <- aggregate(zero ~ Year+Age, FUN=sum, data=ddd)
temp4 <- aggregate(zero ~ Year+Age, FUN=length, data=ddd)
temp3 <- merge(temp3, temp4, by=c("Year", "Age"))
temp3$prop <- temp3$zero.x/temp3$zero.y
temp3$cutoff <- "0.1"

## median number of non-zero
bb1 <- aggregate(n ~ Year + Age + zero, FUN=median, data=ddd)
names(bb1)[4] <- "median_non_zero"
bb1 <- bb1[bb1$zero==0,]
bb1$cutoff <- "0.1"

temp1 <- rbind(temp1, temp3)
aa1   <- rbind(aa1, bb1)


f1 <- ggplot(temp1, aes(x=Year, y=prop, group=factor(cutoff), color=factor(cutoff))) +
  geom_line(size=1) +
  #scale_linetype_manual(values=c("solid", "dotted", "dotted"))+
  #scale_x_continuous(name="", breaks=unique(res$year), minor_breaks = unique(res$year), labels=unique(res$year)) +
  ggtitle("IBTSQ1: proportion of zero hauls") 
f2 <- facet(f1, facet.by = c("Age"), ncol = 2, scales = "free_y", panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) +
  theme(plot.title = element_text(color="black", size=16, face="bold"), 
        legend.position = "bottom", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=10, face="plain"),
        axis.text.x = element_text(color = "black", size = 14, angle = 90),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.x = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14, angle = 90))+
  ylab("prop zero")+ xlab("") 
#scale_x_continuous(name="", breaks=2005:2018, labels=unique(dat$Year))
plot(f2)  


f1 <- ggplot(aa1, aes(x=Year, y=median_non_zero, group=factor(cutoff), color=factor(cutoff))) +
  geom_line(size=1) +
  #scale_linetype_manual(values=c("solid", "dotted", "dotted"))+
  #scale_x_continuous(name="", breaks=unique(res$year), minor_breaks = unique(res$year), labels=unique(res$year)) +
  ggtitle("IBTSQ1: median of non-zero count per haul") 
f2 <- facet(f1, facet.by = c("Age"), ncol = 2, scales = "free_y", panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) +
  theme(plot.title = element_text(color="black", size=16, face="bold"), 
        legend.position = "bottom", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=10, face="plain"),
        axis.text.x = element_text(color = "black", size = 14, angle = 90),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.x = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14, angle = 90))+
  ylab("median non-zero count")+ xlab("") 
#scale_x_continuous(name="", breaks=2005:2018, labels=unique(dat$Year))
plot(f2)  


##################################################################
##### SNS
mysurvey       <- "SNS"
## load data after applying alk
mydatapath <-  "C:/Users/chen072/OneDrive - WageningenUR/0_2021_plaice_benchmark/00_Survey_indices/"
load(file=paste(mydatapath, "Data/plaice_2021_SNS_processed3_ALK_0_6_swept.RData", sep=""))


Nage        <- mydQ.SNS[[2]]$Nage

ddd <- data.frame(Year=rep(mydQ.SNS[[2]]$Year, 7),
                  Age=c(rep(0, nrow(mydQ.SNS[[2]])), 
                        rep(1, nrow(mydQ.SNS[[2]])), 
                        rep(2, nrow(mydQ.SNS[[2]])),
                        rep(3, nrow(mydQ.SNS[[2]])),
                        rep(4, nrow(mydQ.SNS[[2]])),
                        rep(5, nrow(mydQ.SNS[[2]])),
                        rep(6, nrow(mydQ.SNS[[2]]))),
                  n=c(Nage[,1],Nage[,2],Nage[,3],Nage[,4],Nage[,5],Nage[,6],Nage[,7])
)


f1 <- ggplot(ddd[ddd$Age==1,], aes(x=log(n+0.005))) + 
  geom_histogram(color="black", fill="white")+
  geom_vline(aes(xintercept = log(0.5+0.005)),colour = "red")+
  geom_vline(aes(xintercept = log(0.1+0.005)), colour = "blue") +
  ggtitle("IBTSQ1: histogram of age 5 after alk") 

f2 <- facet(f1, facet.by = c("Year"), ncol = 5, scales = "free_y", panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) +
  theme(plot.title = element_text(color="black", size=16, face="bold"), 
        legend.position = "bottom", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=10, face="plain"),
        axis.text.x = element_text(color = "black", size = 14, angle = 90),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.x = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14, angle = 90))+
  ylab("")+ xlab("") 
#scale_x_continuous(name="", breaks=2005:2018, labels=unique(dat$Year))
plot(f2)  

f1 <- ggplot(ddd, aes(x=log(n+0.005))) + 
  geom_histogram(color="black", fill="white")+
  geom_vline(aes(xintercept = log(0.5+0.005)),colour = "red")+
  geom_vline(aes(xintercept = log(0.1+0.005)), colour = "blue") +
  geom_vline(aes(xintercept = log(0.01+0.005)), colour = "black") +
  ggtitle("SNS: histogram of number at age per haul") 

f2 <- facet(f1, facet.by = c("Age"), ncol = 3, scales = "free_y", panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) +
  theme(plot.title = element_text(color="black", size=16, face="bold"), 
        legend.position = "bottom", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=10, face="plain"),
        axis.text.x = element_text(color = "black", size = 14, angle = 90),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.x = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14, angle = 90))+
  ylab("")+ xlab("") 
#scale_x_continuous(name="", breaks=2005:2018, labels=unique(dat$Year))
plot(f2) 

ddd$zero <- 0

ddd$zero[ddd$n<0.01] <- 1

## proportion of zero
temp1 <- aggregate(zero ~ Year+Age, FUN=sum, data=ddd)
temp2 <- aggregate(zero ~ Year+Age, FUN=length, data=ddd)
temp1 <- merge(temp1, temp2, by=c("Year", "Age"))
temp1$prop <- temp1$zero.x/temp1$zero.y
temp1$cutoff <- "0.01"

## median number of non-zero
aa1 <- aggregate(n ~ Year + Age + zero, FUN=median, data=ddd)
names(aa1)[4] <- "median_non_zero"
aa1 <- aa1[aa1$zero==0,]
aa1$cutoff <- "0.01"


ddd$zero <- 0

ddd$zero[ddd$n<0.1] <- 1

temp3 <- aggregate(zero ~ Year+Age, FUN=sum, data=ddd)
temp4 <- aggregate(zero ~ Year+Age, FUN=length, data=ddd)
temp3 <- merge(temp3, temp4, by=c("Year", "Age"))
temp3$prop <- temp3$zero.x/temp3$zero.y
temp3$cutoff <- "0.1"

## median number of non-zero
bb1 <- aggregate(n ~ Year + Age + zero, FUN=median, data=ddd)
names(bb1)[4] <- "median_non_zero"
bb1 <- bb1[bb1$zero==0,]
bb1$cutoff <- "0.1"

temp1 <- rbind(temp1, temp3)
aa1   <- rbind(aa1, bb1)


f1 <- ggplot(temp1, aes(x=Year, y=prop, group=factor(cutoff), color=factor(cutoff))) +
  geom_line(size=1) +
  #scale_linetype_manual(values=c("solid", "dotted", "dotted"))+
  #scale_x_continuous(name="", breaks=unique(res$year), minor_breaks = unique(res$year), labels=unique(res$year)) +
  ggtitle("IBTSQ1: proportion of zero hauls") 
f2 <- facet(f1, facet.by = c("Age"), ncol = 2, scales = "free_y", panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) +
  theme(plot.title = element_text(color="black", size=16, face="bold"), 
        legend.position = "bottom", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=10, face="plain"),
        axis.text.x = element_text(color = "black", size = 14, angle = 90),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.x = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14, angle = 90))+
  ylab("prop zero")+ xlab("") 
#scale_x_continuous(name="", breaks=2005:2018, labels=unique(dat$Year))
plot(f2)  


f1 <- ggplot(aa1, aes(x=Year, y=median_non_zero, group=factor(cutoff), color=factor(cutoff))) +
  geom_line(size=1) +
  #scale_linetype_manual(values=c("solid", "dotted", "dotted"))+
  #scale_x_continuous(name="", breaks=unique(res$year), minor_breaks = unique(res$year), labels=unique(res$year)) +
  ggtitle("IBTSQ1: median of non-zero count per haul") 
f2 <- facet(f1, facet.by = c("Age"), ncol = 2, scales = "free_y", panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) +
  theme(plot.title = element_text(color="black", size=16, face="bold"), 
        legend.position = "bottom", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=10, face="plain"),
        axis.text.x = element_text(color = "black", size = 14, angle = 90),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.x = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14, angle = 90))+
  ylab("median non-zero count")+ xlab("") 
#scale_x_continuous(name="", breaks=2005:2018, labels=unique(dat$Year))
plot(f2)  




##############################################
#############  DYFS

##################################################################
##### DYFS
mysurvey       <- "DYFS"
## load data after applying alk
mydatapath <-  "C:/Users/chen072/OneDrive - WageningenUR/0_2021_plaice_benchmark/00_Survey_indices/"
load(file=paste(mydatapath, "Data/plaice_2021_DYFS_processed3_ALK_swept.RData", sep=""))


Nage        <- mydQ.DYFS[[2]]$Nage

ddd <- data.frame(Year=rep(mydQ.DYFS[[2]]$Year, 4),
                  Age=c(rep(0, nrow(mydQ.DYFS[[2]])), 
                        rep(1, nrow(mydQ.DYFS[[2]])), 
                        rep(2, nrow(mydQ.DYFS[[2]])),
                        rep(3, nrow(mydQ.DYFS[[2]]))),
                  n=c(Nage[,1],Nage[,2],Nage[,3],Nage[,4])
)


f1 <- ggplot(ddd[ddd$Age==1,], aes(x=log(n+0.005))) + 
  geom_histogram(color="black", fill="white")+
  geom_vline(aes(xintercept = log(0.5+0.005)),colour = "red")+
  geom_vline(aes(xintercept = log(0.1+0.005)), colour = "blue") +
  ggtitle("IBTSQ1: histogram of age 5 after alk") 

f2 <- facet(f1, facet.by = c("Year"), ncol = 5, scales = "free_y", panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) +
  theme(plot.title = element_text(color="black", size=16, face="bold"), 
        legend.position = "bottom", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=10, face="plain"),
        axis.text.x = element_text(color = "black", size = 14, angle = 90),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.x = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14, angle = 90))+
  ylab("")+ xlab("") 
#scale_x_continuous(name="", breaks=2005:2018, labels=unique(dat$Year))
plot(f2)  

f1 <- ggplot(ddd, aes(x=log(n+0.005))) + 
  geom_histogram(color="black", fill="white")+
  geom_vline(aes(xintercept = log(0.5+0.005)),colour = "red")+
  geom_vline(aes(xintercept = log(0.1+0.005)), colour = "blue") +
  geom_vline(aes(xintercept = log(0.01+0.005)), colour = "black") +
  ggtitle("SNS: histogram of number at age per haul") 

f2 <- facet(f1, facet.by = c("Age"), ncol = 2, scales = "free_y", panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) +
  theme(plot.title = element_text(color="black", size=16, face="bold"), 
        legend.position = "bottom", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=10, face="plain"),
        axis.text.x = element_text(color = "black", size = 14, angle = 90),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.x = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14, angle = 90))+
  ylab("")+ xlab("") 
#scale_x_continuous(name="", breaks=2005:2018, labels=unique(dat$Year))
plot(f2) 