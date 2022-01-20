# --------------------------------------------------------------------------------------
# Functions for north sea plaice
#
# Author  : Chun chen
# --------------------------------------------------------------------------------------


## totalStk
## Adds up totals for stock objects - computes landings, discards, catch, stock totals and sets units
totalStk <- function(stk, Units){
  landings(stk) <- computeLandings(stk)
  discards(stk) <- computeDiscards(stk)
  catch.n(stk)  <- landings.n(stk)+discards.n(stk)
  catch.wt(stk) <-(landings.n(stk)*landings.wt(stk)+discards.n(stk)*discards.wt(stk))/catch.n(stk)
  catch(stk)    <- computeCatch(stk)
  stock(stk)    <- computeStock(stk)
  units(stk)[1:17] <- as.list(c(rep(Units,4), "NA", "NA", "f", "NA", "NA"))
  return(stk)
}

## extra_processing_data_year_2019
## manually add 2.9763 t of sweden in area 4
#############################################################################  
## special processing in WG2020, manually add 2.9763 t of sweden in area 4
## add on landing.n, landing, catch.n and catch

extra_processing_data_year_2019 <- function(mystock) {
  ## landing
  landings_old <- mystock@landings[,"2019"]  ## 48742 old landing
  landings_new <- round(landings_old + 2.9763, digit=0)
  mystock@landings[,"2019"]   <- landings_new
  mystock@landings.n[,"2019"] <- as.vector(landings_new)/as.vector(landings_old)*as.vector(mystock@landings.n[,"2019"])
  sum(mystock@landings.n[,"2019"]*mystock@landings.wt[,"2019"])
  ## discards
  ## disacards rate in area 4
  aa <- read.table("C:/Users/chen072/OneDrive - WageningenUR/0_2020_WGNSSK/01_Data North Sea and Ska/Intercatch/Intercatch_submitted_files/StockOverview.txt",header=TRUE,sep="\t")
  aa <- aa[aa$Area %in% c("27.4", "27.4.a", "27.4.b", "27.4.c"),]
  #table(aa$Area)
  aa <- aa[aa$Catch.Cat. %in% c("Discards", "Landings"),]
  #table(aa$Catch.Cat.)
  temp <- aggregate(Catch..kg~Catch.Cat., FUN=sum, data=aa)
  dis_ratio_4  <- temp[1,2]/temp[2,2]
  discards_old <- round(mystock@discards[,"2019"], digit=1)  
  discards_new <- round(mystock@discards[,"2019"] + 2.9763*dis_ratio_4, digit=1)
  mystock@discards[,"2019"]   <- discards_new
  mystock@discards.n[,"2019"] <- as.vector(discards_new)/as.vector(discards_old)*as.vector(mystock@discards.n[,"2019"])
  sum(mystock@discards.n[,"2019"]*mystock@discards.wt[,"2019"])
  ## catch
  catch_old   <- mystock@catch[,"2019"]
  catch_new   <- round(catch_old + (discards_new-discards_old) + (landings_new-landings_old), digit=0)
  mystock@catch[,"2019"]      <- catch_new
  mystock@catch.n[,"2019"]    <- as.vector(catch_new)/as.vector(catch_old)*as.vector(mystock@catch.n[,"2019"])
  sum(mystock@catch.n[,"2019"]*mystock@catch.wt[,"2019"])
  
  return(mystock)
  
}

## write cn, cw, lf, dw, sw files in lowestoft format

write_to_Lowestoft <- function (mydat, file, data_index, format_identifier=1, mydigit, nam = "") 
{
  dat1 <- reshape(mydat, v.names = "value", idvar = "year",
                  timevar = "age", direction = "wide")
  dat1 <- dat1[order(dat1$year, decreasing = F),]
  cat(nam, "\n", file = file)
  cat(paste0("1 ",  data_index, " "),  "\n", file = file, append = TRUE)
  cat(range(mydat$year), "\n", file = file, 
      append = TRUE)
  cat(min(mydat$age), max(mydat$age), "\n", file = file, append = TRUE)
  cat(format_identifier, "\n", file = file, append = TRUE)
  write.table(round(dat1[,6:ncol(dat1)], digit=mydigit), file = file, row.names = FALSE, 
              col.names = FALSE, append = TRUE)
}

extra_processing_Lowestoft_format <- function(mystock, project_path) {
  ## catch number: cn
  dat <- melt(catch.n(mystock))
  write_to_Lowestoft(dat, file=paste0(project_path, "data_processed/cn.dat"), 
                     data_index=2, format_identifier=1, mydigit=1, 
                     nam = paste("Plaice in IV(incl SK and VIId): Catch in numbers (thousands)", Sys.time(), sep=" ")) 
  
  ## catch mean weight: cw
  dat <- melt(catch.wt(mystock))
  write_to_Lowestoft(dat, file=paste0(project_path, "data_processed/cw.dat"), 
                     data_index=3, format_identifier=1, mydigit=3,
                     nam = paste("Plaice in IV+IIIa: Mean weight of catches (kg)", Sys.time(), sep=" ")) 
  
  
  ## landing fraction in number: lf
  temp <- landings.n(stock)/catch.n(mystock)
  dat  <- melt(temp)
  write_to_Lowestoft(dat, file=paste0(project_path, "data_processed/lf.dat"), 
                     data_index=4, format_identifier=1, mydigit=4,
                     nam = paste("Plaice in IV(incl SK and VIId): Landings fraction in numbers", Sys.time(), sep=" ")) 
  
  ## discards mean weight: dw
  dat <- melt(discards.wt(mystock))
  write_to_Lowestoft(dat, file=paste0(project_path, "data_processed/dw.dat"), 
                     data_index=5, format_identifier=1, mydigit=3,
                     nam = paste("Plaice in IV+IIIa: Mean weight of discards (kg)", Sys.time(), sep=" ")) 
  
  ## landing mean weight: lw
  dat <- melt(landings.wt(mystock))
  write_to_Lowestoft(dat, file=paste0(project_path, "data_processed/lw.dat"), 
                     data_index=6, format_identifier=1, mydigit=3,
                     nam = paste("Plaice in IV+IIIa: Mean weight of discards (kg)", Sys.time(), sep=" ")) 
  
  ## stock mean weight: sw
  dat <- melt(landings.wt(mystock))
  write_to_Lowestoft(dat, file=paste0(project_path, "data_processed/sw.dat"), 
                     data_index=7, format_identifier=1, mydigit=3,
                     nam = paste("Plaice in IV+IIIa: Mean weight of discards (kg)", Sys.time(), sep=" ")) 
  
}

