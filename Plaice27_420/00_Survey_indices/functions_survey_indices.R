## functions used in indices calculation

########################
## functions
###############################################################################
##### 1. plot raw biomass per haul
###############################################################################
mybubblePlot <- function (d, response = "HaulWgt", bycountry = FALSE, scale = NULL, col.zero = "red", 
                          pch.zero = "+", xlim=c(-3.97, 11.6367), ylim=c(51.0107, 61.1812),...) 
{
  d[[2]]$resp.var <- d[[2]][[response]]
  if (is.null(scale)) 
    scale = mean(d[[2]]$resp.var, na.rm = TRUE)/max(d[[2]]$resp.var, na.rm = TRUE)
  plot(d$lon, d$lat, type = "n", xlab = "Longitude", ylab = "Latitude", cex.axis=1.5, cex.lab=1.5, cex.main=1.5, xlim=xlim, ylim=ylim,...)
  map("worldHires", fill = TRUE, plot = TRUE, add = TRUE, col = grey(0.5))
  if (bycountry == FALSE) {
    points(d$lon, d$lat, pch = 16, cex = scale * sqrt(d[[2]]$resp.var), col=alpha("black", 0.5),...)
    zero = subset(d, resp.var == 0)
    points(zero$lon, zero$lat, pch = pch.zero, col = col.zero)
  }
  else {
    country_list <- as.character(unique(d$Country))
    match_col    <- colset[match(country_list, col_country)]
    for (k in 1:length(country_list)) {
      d_sub <- subset(d, Country == country_list[k])
      points(d_sub$lon, d_sub$lat, pch = 16, cex = scale * sqrt(d_sub[[2]]$resp.var), col=alpha(match_col[k], 0.5),
             ...)
    }
    zero = subset(d, resp.var == 0)
    points(zero$lon, zero$lat, pch = pch.zero, col = col.zero)
    legend("bottomright", country_list, col=match_col, pch=16, pt.cex=2, cex=2)
  }
  
}

mybubblePlot_by_age <- function (mysurvey, d, response = "0", bycountry = FALSE, scale = NULL, col.zero = "red", 
                                 pch.zero = "+", xlim=c(-3.97, 11.6367), ylim=c(51.0107, 61.8783),...) 
{ if (response == "10" & mysurvey != "IBTSQ1") {
  response  <- "10+"
}
  if (response == "8" & mysurvey == "IBTSQ1") {
    response  <- "8+"
  }
  d[[2]]$resp.var <- d[[2]]$Nage[,response]
  if (is.null(scale)) 
    scale = mean(d[[2]]$resp.var, na.rm = TRUE)/max(d[[2]]$resp.var, na.rm = TRUE)
  plot(d$lon, d$lat, type = "n", xlab = "Longitude", ylab = "Latitude", cex.axis=1.5, cex.lab=1.5, cex.main=1.5, xlim=c(-3.97, 11.6367), ylim=c(51.0107, 61.8783),...)
  map("worldHires", fill = TRUE, plot = TRUE, add = TRUE, col = grey(0.5))
  if (bycountry == FALSE) {
    points(d$lon, d$lat, pch = 16, cex = scale * sqrt(d[[2]]$resp.var), col=alpha("black", 0.5),...)
    zero = subset(d, resp.var <1 )
    points(zero$lon, zero$lat, pch = pch.zero, col = col.zero)
  }
  else {
    country_list <- as.character(unique(d$Country))
    match_col    <- colset[match(country_list, col_country)]
    for (k in 1:length(country_list)) {
      d_sub <- subset(d, Country == country_list[k])
      d_sub = subset(d_sub, resp.var >= 1)
      points(d_sub$lon, d_sub$lat, pch = 16, cex = scale * sqrt(d_sub[[2]]$resp.var), col=alpha(match_col[k], 0.5),
             ...)
    }
    #zero = subset(d, resp.var < 1)
    #points(zero$lon, zero$lat, pch = pch.zero, col = col.zero)
    legend("bottomright", country_list, col=match_col, pch=16, pt.cex=2, cex=2)
  }
  
}

plot_raw_biomass_haul <- function(mysurvey, mydat, myyear = last_data_year, mypath, bycountry=TRUE, scale=1/10, myxlim=c(-3.9640, 11.6333),
                                  myylim=c(51.2000, 61.8783)) {

  plot_path  <- paste(mypath, "/plot_", mysurvey, "/", sep="")
  mydat      <- subset(mydat, Year == as.character(myyear))
  
  ## save plot
  png(filename = paste(plot_path, mysurvey, "_plot_01_haul_biomass_", myyear,".png", sep=""), width = 600, height = 800)
  mybubblePlot(mydat, bycountry=TRUE, 
               scale=myscale,
               xlim=myxlim, 
               ylim=myylim,
               main=paste(mysurvey, myyear, "biomass per haul", "scale", scale, sep="  "))
  if (mysurvey %in% c("BTS-Q3","IBTS-Q3")) {
    rect(-0.7,58.24,5,62,border="gray",lwd=1.5, col="gray")
  }
  dev.off()
  
  mybubblePlot(mydat, bycountry=TRUE, 
               scale=myscale,
               xlim=c(min(mydat$lon), max(mydat$lon)), 
               ylim=c(min(mydat$lat), max(mydat$lat)),
               main=paste(mysurvey, myyear,"biomass per haul", "scale", scale, sep="  "))
  if (mysurvey %in% c("BTS-Q3","IBTS-Q3")) {
    rect(-0.7,58.24,5,62,border="gray",lwd=1.5, col="gray")
  }
}

plot_mean_length_haul <- function(mysurvey, mydat1, myyear = last_data_year, mypath) {

  plot_path  <- paste(mypath, "/plot_", mysurvey, "/", sep="")
  mydat1      <- subset(mydat1, Year == as.character(myyear))
  mydat      <- mydat1[[3]]
  
  ## check
  summary(mydat$SubFactor*mydat$HLNoAtLngt == mydat$Count)
  
  ## exclude NAs
  mydat <- mydat[!is.na(mydat$Count),]
  
  ## calculate mean length per haul
  mydat$lencount <- mydat$LngtCm * mydat$Count
  temp1          <- aggregate(lencount ~ HaulNo + Year + Survey + Quarter + Country + Ship + Gear, FUN=sum, data=mydat)
  sum(temp1$lencount)
  temp2          <- aggregate(Count ~ HaulNo + Year + Survey + Quarter + Country + Ship + Gear, FUN=sum, data=mydat)
  temp1          <- merge(temp1, temp2, by=c("HaulNo", "Year", "Survey", "Quarter", "Country", "Ship", "Gear"))
  temp1$mean_length <- temp1$lencount/temp1$Count
  
  ## link back to get lat and lon
  mydat <- mydat1[[2]]
  temp3 <- merge(temp1, mydat[,c("HaulNo", "Year", "Survey", "Quarter", "Country", "Ship", "Gear", "lon", "lat")], by=c("HaulNo", "Year", "Survey", "Quarter", "Country", "Ship", "Gear"))
  
  ## plot mean length
  #devtools::install_github("dkahle/ggmap",  ref = "tidyup", lib="D:/software/R_library")
  library(ggmap)
  library(ggplot2)
  mylat     <- c(min(temp3$lat), max(temp3$lat))                
  mylon     <- c(min(temp3$lon), max(temp3$lon))
  register_google(key = "AIzaSyD58Yx0RkL8aYeDE_2yXmu2rvTMfWOZsrA")
  #map       <- get_map(location = c(mean(mylon), mean(mylat)), zoom = 6)
  map       <- get_map(location = c(-3.2, 51, 9.5, 58.5),
                       #zoom = 1,
                       source = "google",
                       maptype= "terrian", crop=F)
  
  ## extract consistent color
  temp3$mean_L <- cut(temp3$mean_length, include.lowest =T,breaks=quantile(temp3$mean_length,  prob=seq(0,1,0.1)))
  
  temp4     <- temp3[temp3$Year == myyear,]
  p1        <- ggmap(map) + 
    geom_point(aes(x = lon, y = lat,  colour=mean_L), size = 3, alpha=0.9, data = temp4) + 
    scale_colour_manual(values=colset1) + 
    ggtitle(paste(mysurvey, myyear, "mean length per haul", sep="  ")) +
    theme(plot.title = element_text(color="black", size=20, face="bold"), 
          legend.position = c(0.1, 0.2), legend.title = element_text(colour="black", size=15, face="bold"),
          legend.text = element_text(colour="black", size=12, face="bold")) +
    xlab("") + ylab("")
  ## save plot
  png(filename = paste(plot_path, mysurvey, "_plot_02_haul_meanlength_", myyear,".png", sep=""), width = 600, height = 800)
  print(p1)
  dev.off()
  ## plot
  print(p1)
}


run_alk_BTS_IBTS <- function(myalk) {
  if (myalk == "BTSQ3") {
    dd      <- mydQ.BTS
  }
  if (myalk == "IBTSQ3") {
    dd      <- mydQ.IBTSQ3
  }
  if (myalk == "BTS+IBTSQ3") {
    dd      <- dAll
  }
  ## check if all countries has submitted age samples
  if (myalk %in% c("BTSQ3", "BTS+IBTSQ3")) {
  if(length(unique(subset(dd, Year==last_data_year)[[1]]$Country))<4){
    stop("some countries did not submit age samples")
  }
  }
  ## select non-NA age samples in CA
  removeAgeNAs<-function(x) {
    x[[1]]=subset(x[[1]],!is.na(x[[1]]$Age))
    x[[1]]=subset(x[[1]],!is.na(x[[1]]$NoAtALK))
    x
  }
  dd      <- removeAgeNAs(dd)
  
  ## Declare settings for ALK model
  mf       = "" 
  ack      = TRUE;
  useBICs  = TRUE;
  varCofs  = FALSE;
  maxKs    = 50;
  mc.cores = 1
  
  add.ALK<-function(d){
    
    ages=agesQ3
    if(d$Quarter[1]=="1"){   ### for Q1 data, 
      d[[1]]=subset(d[[1]],Age>0)
      d=fixAgeGroup(d,1)
      ages=agesQ1
    }
    spectrumMax    <- 70
    cmSize         <- 1
    d=addSpectrum(d,cm.breaks=seq(0,spectrumMax,by=cmSize))
    
    d.ysplit = split(d,d$Year)
    
    d.ALK= mclapply(d.ysplit,fitALK,minAge=min(ages),maxAge=max(ages),autoChooseK=ack,useBIC=useBICs,varCof=varCofs,maxK=maxKs,mc.cores=mc.cores)
    
    d.Nage=mclapply(d.ALK,predict,mc.cores=mc.cores)
    for(i in 1:length(d.ALK)) d.ysplit[[i]]$Nage=d.Nage[[i]];
    dd <- do.call("c",d.ysplit)
    dd    
  }
  
  dd.alk   <- add.ALK(dd)   ## apply alk
  
  grid.dd  <- getGrid(dd,nLon=40) 
  
  
  ##
  return(list(
    mydd=dd.alk,
    mygrid=grid.dd
  ))
}

run_alk_SNS_DYFS <- function(mydat, myalk, myage) {
  
  dd      <- mydat
  
  ## check if all countries has submitted age samples
  if (myalk %in% c("BTSQ3", "BTS+IBTSQ3")) {
    if(length(unique(subset(dd, Year==last_data_year)[[1]]$Country))<4){
      stop("some countries did not submit age samples")
    }
  }
  ## select non-NA age samples in CA
  removeAgeNAs<-function(x) {
    x[[1]]=subset(x[[1]],!is.na(x[[1]]$Age))
    x[[1]]=subset(x[[1]],!is.na(x[[1]]$NoAtALK))
    x
  }
  dd      <- removeAgeNAs(dd)
  
  ## Declare settings for ALK model
  mf       <- "" 
  mc.cores <- 1 # Windows users should use mc.cores = 1
  ack      <- TRUE #automatic choice of the maximum dimension for the basis used to represent the smooth term for spatial ALK
  useBICs  <- TRUE #use BIC instead of AIC
  varCofs  <- FALSE #using varying coefficients model for spatial effect
  maxKs    <- 50 #maximum k to use
  
  # Age-length key: fit continuation-ratio logit model for age given length for each year separately
  add.ALK<-function(d){
    
    ages <- myage
    
    if(d$Quarter[1]=="1"){   ### for Q1 data, 
      d[[1]]=subset(d[[1]],Age>0)
      d=fixAgeGroup(d,1)
      ages=myage
    }
    spectrumMax    <- 70
    cmSize         <- 1
    d              <- addSpectrum(d,cm.breaks=seq(0,spectrumMax,by=cmSize))
    
    # fit ALK
    d.ysplit  <- split(d,d$Year)
    
    d.ALK     <- mclapply(d.ysplit,
                          fitALK1,  ## here i used adjusted function to check
                          minAge=min(ages),
                          maxAge=max(ages),
                          autoChooseK=ack,
                          useBIC=useBICs,
                          varCof=varCofs,
                          maxK=maxKs,
                          mc.cores=mc.cores,
                          verbose=TRUE)
    # predict numbers-at-age
    d.Nage <- mclapply(d.ALK, predict, mc.cores=mc.cores)
    for(i in 1:length(d.ALK)) d.ysplit[[i]]$Nage=d.Nage[[i]] #add N at age to data of each year
    # merge back into one object
    dd <- do.call("c",d.ysplit)
    dd    
  }
  
  dd.alk   <- add.ALK(dd)   ## apply alk
  
  ##
  return(dd.alk)
}

################# script in ALK checking for SNS/DFYS
fitALKone1 <- function (a, ages, AL, model, gamma, autoChooseK = FALSE, useBIC = FALSE, 
                        varCof = FALSE, maxK = 100, verbose = FALSE, ...) 
{
  
  if (length(model) > 1) {
    idx = which(ages == a)
    f <- as.formula(model[idx])
  }
  else {
    f <- as.formula(model)
  }
  require(mgcv, quietly = TRUE)
  myd = subset(AL, Age >= a)
  myd$cra = as.factor(myd$Age > a)
  print("#####################")
  #print("Year")
  #print(unique(myd$Year))
  print("age")
  print(a)
  print("sample size")
  print(sum(myd$NoAtALK))
  print("nrow")
  print(nrow(myd))
  #print("mymodel")
  #model
  if (autoChooseK) {
    uniqueCovs = length(unique(paste(myd$lon, myd$lat)))
    
    if (!varCof) { ## this is the one we normally choose: no lengh-spatial interaction
      k = min(maxK, uniqueCovs - 1)
      f = as.formula(paste("cra~LngtCm+s(lon,lat,k=", 
                           k, ",bs='ts')"))
      if (uniqueCovs < 10) 
        f = as.formula("cra~LngtCm")
    }
    else { ## with length spatial interaction
      k = min(maxK, uniqueCovs/2 - 1)
      f = as.formula(paste("cra~s(lon,lat,by=LngtCm,k=", 
                           k, ",bs='ts')+s(lon,lat,k=", k, ",bs='ts')"))
      if (uniqueCovs < 10) 
        f = as.formula("cra~LngtCm")
    }
    if (useBIC) 
      gamma = log(sum(myd$NoAtALK))/2
  }
  print("uniqueCovs")
  print(uniqueCovs)
  m <- DATRAS:::tryCatch.W.E(gam(f, data = myd, family = "binomial", 
                        weights = NoAtALK, gamma = gamma, ...))$value
  if (class(m)[2] == "error") {
    print(m)
    stop("Error occured for age ", a, "\n", "Try reducing the number of age groups or decrease the basis dimension of the smooths, k\n")
  }
  if (verbose) {
    print(summary(m))
  }
  return(m)
}


fitALK1 <- function (x, minAge, maxAge, mc.cores = 1, model = c("cra~LngtCm", 
                                                                "cra~poly(LngtCm,2)", "cra~LngtCm+s(lon,lat,bs='ts')")[method], 
                     method = 1, autoChooseK = FALSE, useBIC = FALSE, varCof = FALSE, 
                     maxK = 100, gamma = 1.4, verbose = FALSE, ...) 
{
  ## here method argument is not used, the actual model formula depends on the number unique locations (uniqueCovs) and sample size in the year+age setting
  print(method)
  checkSpectrum(x)
  if ((minAge + 1) > maxAge) 
    stop("Invalid age selection.")
  ages = minAge:maxAge                   ## fixed age ranges across years
  #ages = min(x[[1]]$Age):max(x[[1]]$Age)  ## different age range per year
  #ages = 0:6
  print("#####################################################")
  print("year")
  print(unique(x[[1]]$Year))
  print("age range")
  print(ages)
  nAges = length(ages)
  lastAge = ages[nAges]
  ages = ages[-nAges]
  
  extraVars = unlist(lapply(lapply(model, as.formula), DATRAS:::xtraVars, 
                        x = x))
  
  
  if (length(extraVars) == 0) 
    extraVars = NULL
  x[[1]] = merge(x[[1]], x[[2]][c("lon", "lat", "haul.id", 
                                  extraVars)], by = "haul.id", all.x = TRUE, sort = FALSE, 
                 suffixes = c("", ".y"))
  x[[1]] = subset(x[[1]], !is.na(Year) & !is.na(Age) & !is.na(LngtCm))
  mylapply <- function(...) {
    hasmc = (mc.cores > 1 && require(multicore, quietly = TRUE))
    if (!hasmc) 
      return(lapply(...))
    else return(mclapply(..., mc.cores = mc.cores))
  }
  if (verbose) 
    cat("Fitting model...")
  models = mylapply(ages, fitALKone1, ages = ages, AL = x[[1]], 
                    model = model, gamma = gamma, autoChooseK = autoChooseK, 
                    useBIC = useBIC, varCof = varCof, maxK = maxK, verbose = verbose, 
                    ...)
  class(models) <- "ALKmodel"
  attr(models, "data") <- x
  attr(models, "ALKformula") <- model
  attr(models, "ages") <- ages
  models
}

## retro function, every year, re-factor the ship levels
retro.surveyIdx.ship <- function (model, d, grid, npeels = 5, predD = NULL, ...) 
{
  if (is.null(predD)) {
    predD = subset(d, haul.id %in% grid[[3]])
    predD = predD[[2]]
  }
  ages = as.numeric(colnames(model$idx))
  dataAges = model$dataAges
  famVec = model$family
  cutOff = model$cutOff
  predfix = model$predfix
  predfix$Gear = model$refGear
  lastY = max(model$yearNum)
  lastQ = max(as.numeric(as.character(d$Quarter[d$Year == lastY])))
  yearRange = min(model$yearNum):max(model$yearNum)
  mp <- mz <- character(length(ages))
  for (aa in 1:length(ages)) {
    mp[aa] = as.character(model$pModels[[aa]]$formula)[3]
    if (length(model$zModels) > 0) {
      mz[aa] = as.character(model$zModels[[aa]]$formula)[3]
    }
    else {
      mz = NULL
    }
  }
  res = list()
  for (i in 1:npeels) {
    curd = subset(d, Year %in% as.character(head(yearRange, 
                                                 length(yearRange) - (i + 1))) | (Year %in% as.character(head(yearRange, 
                                                                                                              length(yearRange) - i)) & Quarter %in% as.character(1:lastQ)))
    ## re-level ship
    curd[[2]]$Ship <- factor(curd[[2]]$Ship)
    curd[[3]]$Ship <- factor(curd[[3]]$Ship, levels=levels(curd[[2]]$Ship))
    curd[[1]]$Ship <- factor(curd[[1]]$Ship, levels=levels(curd[[1]]$Ship))
    
    cat("Peel ", i, ": re-fitting using years ", 
        levels(curd$Year), "\n")
    res[[i]] = surveyIndex::getSurveyIdx(curd, ages, myids = NULL, predD = predD, 
                                         cutOff = cutOff, fam = famVec, method = model$pModels[[1]]$method, 
                                         knotsP = model$knotsP, knotsZ = model$knotsZ, predfix = predfix, 
                                         nBoot = 0, modelP = mp, modelZ = mz, ...)
  }
  class(res) <- "SIlist"
  res
}

plot_number_per_age_haul <- function(mysurvey, mydat, iage="0", myyear = last_data_year, mypath, bycountry=TRUE, scale=1/10) {
  
  plot_path  <- paste(mypath, "/plot_", mysurvey, "/", sep="")
  mydat      <- subset(mydat, Year == as.character(myyear))
  
  
  ## save plot
  png(filename = paste(plot_path, mysurvey, "_plot_03_haul_N_at_age_", myyear,"_age", iage, ".png", sep=""), width = 600, height = 800)
  mybubblePlot_by_age(mysurvey, mydat, response = iage, bycountry=TRUE, scale=myscale,xlim=c(min(mydat$lon), max(mydat$lon)), ylim=c(min(mydat$lat), max(mydat$lat)),
                      main=paste(mysurvey, myyear, "age", iage, "scale", scale, sep="  "))
  #if (mysurvey %in% c("BTSQ3","IBTSQ3")) {
  #  rect(-0.7,58.24,5,62,border="gray",lwd=1.5, col="gray")
  #}
  dev.off()
  
  mybubblePlot_by_age(mysurvey,mydat, response = iage, bycountry=TRUE, scale=myscale,xlim=c(min(mydat$lon), max(mydat$lon)), ylim=c(min(mydat$lat), max(mydat$lat)),
                      main=paste(mysurvey, myyear, "age", iage, "scale", scale, sep="  "))
  #if (mysurvey %in% c("BTSQ3","IBTSQ3")) {
  #  rect(-0.7,58.24,5,62,border="gray",lwd=1.5, col="gray")
  #}
}

## export indices
exportSI <- function(x,ages,years,toy,file,nam="",exclude=c()){
  cat(nam,"\n",file=file)
  cat(range(as.numeric(as.character(years))),"\n",file=file,append=TRUE)
  cat("1 1 ",rep(toy,2),"\n",file=file,append=TRUE)
  cat(min(ages),max(ages),"\n",file=file,append=TRUE)
  write.table(round(cbind(1,x[,]),4),file=file,row.names=FALSE,col.names=FALSE,append=TRUE)
}

## collect historical indices
collect_historical_indices <- function(mysurvey, myage, mywork_year) {
  my_year <- 2017:(mywork_year-1)
  indices <- NA
  if (mysurvey != "IBTSQ1") {
    yearstart <- 1996
  } else {
    yearstart <- 2007
  }
  ## historical output
  for (iyear in my_year) {
    
    temp        <- read.table(paste("Model_result/historical_results/indices_", mysurvey, "_WGNSSK_", iyear, "_stationary.dat", sep=""), header=FALSE, skip=4)

    temp$year   <- yearstart:(iyear-1)
    temp$work_year <- iyear
    indices        <- rbind(indices, temp)
  }
  ## current year output
  temp        <- read.table(paste("Model_result/indices_", mysurvey, "_WGNSSK_", mywork_year, "_stationary.dat", sep=""), header=FALSE, skip=4)


  temp$year   <- yearstart:(mywork_year-1)
  temp$work_year <- mywork_year
  indices        <- rbind(indices, temp)
  
  indices <- indices[-1,]
  indices <- indices[,-1]
  names(indices)[1:length(myage)] <- myage
  indices$survey <- mysurvey
  indices$method <- "delta-gam"
  indices$work_year <- factor(indices$work_year)
  
  ## reshape
  indices <- reshape(indices, idvar = c("year", "work_year", "survey", "method"), varying = list(1:length(myage)),
                v.names = "indices", timevar="age",times = myage, direction = "long")
  rownames(indices) <- c()
  return(indices)
}

write_to_csv <- function(mymodel, myfilepath) {
  r       <- mymodel[[1]]
  idx     <- r$idx
  idx.m   <- melt(idx,value.name = "data", varnames = c("year","age"))
  lo      <- r$lo
  colnames(lo) <- colnames(idx)
  rownames(lo) <- rownames(idx)
  lo      <- melt(lo,value.name = "lo", varnames = c("year","age"))
  up      <- r$up
  colnames(up) <- colnames(idx)
  rownames(up) <- rownames(idx)
  up      <- melt(up,value.name = "up", varnames = c("year","age"))
  idx.res <- cbind(idx.m,lo$lo,up$up)
  colnames(idx.res)[4] <- "lo" 
  colnames(idx.res)[5] <- "up"
  ## write to csv
  write.csv(idx.res, file=myfilepath, row.names = F)
}