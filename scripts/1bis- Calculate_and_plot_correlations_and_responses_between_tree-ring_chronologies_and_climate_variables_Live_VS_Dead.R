######################################################
# Purpose: Calculate correlations and response (and plot correlations) between tree-ring chronologies and climate variables, looking separately at trees cored while alive and trees cored while dead
# Developped by: Valentine Herrmann - HerrmannV@si.edu 10/18/2018
# R version 3.5.1 (2018-07-02)
######################################################

# Clean environment ####
rm(list = ls())

# Set working directory as Shenandoah main folder ####
setwd(".")

# Load libraries ####
library(dplR)
library(bootRes)
library(caTools)
library(RCurl)

source("scripts/0-My_dplR_functions.R")

# Define parameters and variables ####

## saving or not saving outputs ? ####
save.plots <- TRUE
save.result.table <- TRUE

## Define order of the species in the  plots, based on ANPP contribution####
ANPP_contribution <- read.csv(text=getURL("https://raw.githubusercontent.com/EcoClimLab/SCBI-ForestGEO-Data_private/master/SCBI_numbers_and_facts/ANPP_total_and_by_species.csv?token=ASwxIcI15ZV1vVyd84R5gnaI0Wo3zpBpks5b0g_2wA%3D%3D"), header=T) # this URL might change because it is a private repository. If it does, update if by copying the URL direcltly from github: go to https://github.com/EcoClimLab/SCBI-ForestGEO-Data_private/master/SCBI_numbers_and_facts/ANPP_total_and_by_species.csv, click on Raw, copy the URL and paste it in place of the current URL here, inbetween the quotes of this line of code.
SPECIES_IN_ORDER <- toupper(ANPP_contribution$species[ ANPP_contribution$species %in% c("litu", "qual", "quru", "quve", "qupr", "fram", "cagl", "caco", "cato", "juni", "fagr", "caovl", "pist", "frni")]) #toupper(c("litu", "qual", "quru", "quve", "qupr", "fram", "cagl", "caco", "cato", "juni", "fagr", "caov", "pist", "frni"))
SPECIES_IN_ORDER <- gsub("CAOVL", "CAOV", SPECIES_IN_ORDER)

SPECIES_IN_ORDER <- SPECIES_IN_ORDER[grepl("LITU|QURU|FRAM|QUVE", SPECIES_IN_ORDER)] # special for live vs dead only

SPECIES_IN_ORDER <- paste0(rep(SPECIES_IN_ORDER, each =2), c("_live", "_dead"))

## Define sets of climate data to use ####

climate.data.types <- c("PRISM_SCBI_1930_2015_30second", "CRU_SCBI_1901_2016", "NOAA_PDSI_Northern_Virginia_1895_2017")

## Define sets of methods to run ####

methods.to.run <- c("correlation") # c("correlation", "response", "moving_correlation")

## Define how to run it regarding the starting year ####
type.of.start.date <- c("Going_back_as_far_as_possible", "Going_back_to_1920", "Going_back_to_1980") # Going_back_at_earliest_common_year")


## Define sss threshold ####
sss.threshold = 0.75
## Define start and end month for anlaysis ####
start <- -4 # April of previous year
end <- 8 # August of current year

start.frs <- -10 # october of previous year (for freeze days variable only - otherwise error because all 0 in other months)
end.frs <- 5 # may of current year (for freeze days variable only)

# Load and prepare core data ####

filenames <- list.dirs("data", full.names = F, recursive = F  ) # filenames <- list.files("raw_data/cores/")
filenames <- filenames[!grepl("[a-z]", filenames) | grepl("dead|live", filenames)] # keep only all caps names


filenames <- filenames[grepl("dead|live", filenames)] # special for live vs dead only

filenames <- gsub("_", "_drop_", filenames)

all_sss <- NULL

for(f in filenames) {
  # get the raw data
  core_raw <- read.rwl(paste0("raw_data/cores/", tolower(f), ".rwl"))
  
  # get the detrended data
  core <- read.table(paste0("data/", gsub("_drop", "", f),"/ARSTANfiles/", tolower(f), ".rwl_tabs.txt"), sep = "\t", h = T)
  core <- data.frame(res = core$res,  samp.depth = core$num, row.names = core$year)
  
  # get the Subsample Signal Strength (sss as function of the number of trees in sample, the last one appearing in the "xxx_drop.rxl_out.txt files)
  
  sss <- readLines(paste0("data/", gsub("_drop", "", f),"/ARSTANfiles/", tolower(f), ".rwl_out.txt"))
  sss <- sss[grep("sss", sss)]
  
  sss <- sss[grep("  sss:   ", sss)[c(rep(FALSE, 3*length(seq(grep("  sss:   ", sss)))/4), rep(TRUE, 1*length(seq(grep("  sss:   ", sss)))/4))]] # keep only last rows that have sss: in them
  
  sss <- sub("  sss:   ", "", sss)
  sss <- as.numeric(unlist(strsplit(sss, " " ))) # keep only numbers and store them as a vector
  
  sss <- data.frame(Species = f, "Num_of_trees" = 1:length(sss), sss)
  
  Year_to_Num_of_trees <- apply(core_raw, 1, function(x) sum(!is.na(x)))
  Year_to_Num_of_trees <- data.frame(Species = f, Year = as.numeric(names(Year_to_Num_of_trees)), Num_of_trees= Year_to_Num_of_trees)
  
  match(Year_to_Num_of_trees$Num_of_trees, sss$Num_of_trees)
  
  Year_to_Num_of_trees$sss <- NA
  for(i in 1:nrow(Year_to_Num_of_trees)) {
    
    Year_to_Num_of_trees$sss[i] <- rev(sss[sss$Num_of_trees <= Year_to_Num_of_trees$Num_of_trees[i],]$sss)[1]
    
  }
  
  sss <- Year_to_Num_of_trees
  
  # for(i in 1:nrow(sss)) {
  #   year_with_x_trees <- Year_to_Num_of_trees[Year_to_Num_of_trees >= sss$Num_of_trees[i]]
  #   sss$Year[i] <- as.numeric(names(year_with_x_trees)[1])
  # }
  
  
  # core <- read.rwl(paste0("raw_data/cores/", f))
  # core <- detrend(core, f = 0.5, nyrs = 32, method = "Spline", make.plot = TRUE) # detrend/smooth the time series
  # core <- chron(core)
  
  assign(f, core)
  assign(paste0(f, "_sss"), sss)
  
  all_sss <- rbind(all_sss, sss)
  
}

# save SSS for all species 

write.csv(all_sss, file = "results/live_vs_dead/SSS_as_a_function_of_the_number_of_trees_in_sample.csv", row.names = F)


## Define start and end year for analysis, common to all species and one for each species ####

start.years.sss <- NULL # species specific
for(f in filenames) {
  sss <- get(paste0(f, "_sss"))
  start.years.sss <- c(start.years.sss, sss[sss$sss >= sss.threshold, ]$Year[1])
}

end.year = 2009  # common to all species

# Plot SSS for the the decided threshold ####

if(save.plots) tiff("results/live_vs_dead/SSS_as_a_function_of_the_number_of_trees_in_sample.tiff", res = 150, width = 169, height = 169, units = "mm", pointsize = 10)

op <- par(mfrow = c(2, 1), oma = c(5, 5, 2, 0), mar = c(0, 0, 0, 1))

cols <- data.frame(col = rainbow(length(filenames)), row.names = filenames, stringsAsFactors = F)

years <- NULL
for(sp in levels(all_sss$Species)){
  x = all_sss[all_sss$Species %in% sp,]
  year <- x$Year[x$sss > sss.threshold][1]
  years <- c(years, year)
}

plot.nb <- 1

for(sp in levels(all_sss$Species)){
  
  x <- all_sss[all_sss$Species %in% sp,]
  x <- x[x$Year <= end.year,] 
  # n.core <- x$Num_of_trees[x$sss > sss.threshold][1]
  
  if(plot.nb %in% 1) {
    plot(Num_of_trees ~ Year, data = x, type = "l", col = cols[sp,], xlim = c(min(all_sss$Year), end.year), ylim = range(all_sss$Num_of_trees), lwd = 2, log = "y", las = 1, ylab = "", xaxt = "n")
    mtext(side= 2 , "log(No. cores)", line = 3)
    axis(1, labels = F, tcl = 0.5)
    axis(1, labels = F, tcl = -0.5)
    # mtext(side= 1 , "Year", line = 3, outer = T)
    # abline(v = n.core, lty = 3, col = cols[sp,])
    # abline(h = 0.75, lty = 2)
    # axis(2, tck = 0.01, labels = F)
  } else {
    lines(Num_of_trees ~ Year, data = x, col = cols[sp,], lwd = 2)
    # abline(v = x$Num_of_trees[x$sss > sss.threshold][1], lty = 3, col = cols[sp,])
  }
  # n.cores <- c(n.cores, n.core)
  plot.nb <- plot.nb +1
}

abline(v =years,  col = cols$col, lty = 2)
# abline(h =n.cores,  col = cols$col, lty = 3)
# legend("topleft", col = cols$col, lty = 1, bty = "n", legend = levels(all_sss$Species), lwd = 2, cex = 0.8)
legend("topleft", col = cols$col, lty = 1, bty = "n", legend = paste(levels(all_sss$Species), years, sep = " - "), lwd = 2, cex = 0.8)

plot.nb <- 1

for(sp in levels(all_sss$Species)){
  x <- all_sss[all_sss$Species %in% sp,]
  x <- x[x$Year <= end.year,] 
  
  year <- x$Year[x$sss > sss.threshold][1]
  
  if(plot.nb %in% 1) {
    plot(sss ~ Year, data = x, type = "l", col = cols[sp,], xlim = c(min(all_sss$Year), end.year), lwd = 2, las = 1, xaxt = "n")
    abline(v = year, lty = 2, col = cols[sp,])
    abline(h = 0.75, lty = 3)
    # axis(2, tck = 0.01, las = 1)
    axis(1, labels = T, tcl = 0.5)
    axis(1, labels = F, tcl = -0.5)
    mtext(side= 2 , "sss", line = 3)
  } else {
    lines(sss ~ Year, data = x, col = cols[sp,], lwd = 2)
    abline(v = x$Year[x$sss > sss.threshold][1], lty = 2, col = cols[sp,])
  }
  # years <- c(years, year)
  plot.nb <- plot.nb +1
}

# legend("topleft", col = cols$col, lty = 1, bty = "n", legend = paste(levels(all_sss$Species), years, sep = " - "), lwd = 2, cex = 0.8)


title(paste("SSS threshold =", sss.threshold), outer = T)
par(op)

if(save.plots) dev.off()
par(op)

# Run analysis for all types of climate data with all variables ####

for(c in climate.data.types) {
  print(c)
  
  ## Load climate data ####
  
  clim <- read.csv(paste0("raw_data/climate/Formated_", c, ".csv"))
  
  ### crop first and last year of NOAA data because outliers
  if(c %in% "NOAA_PDSI_Northern_Virginia_1895_2017") {
    clim <- clim[!(clim$year %in% min(clim$year) | clim$year %in% max(clim$year)), ]
  }
  
  ### Pre_whiten PDSI of NOAA data because autocorrelated by definitiaon
  if(c %in% "NOAA_PDSI_Northern_Virginia_1895_2017") {
    clim$PDSI_prewhiten <- ar(clim$PDSI)$resid
    clim$PHDI_prewhiten <- ar(clim$PHDI)$resid
    clim$PMDI_prewhiten <- ar(clim$PMDI)$resid
  }
  
  ### start NOAA in 1901 only (to be able to compare PDSI with CRU data later) ####
  if(c %in% "NOAA_PDSI_Northern_Virginia_1895_2017") {
    clim <- clim[clim$year >= 1901, ]
  }
  
  ### remove climate variables we don't care about
  if(c %in% "NOAA_PDSI_Northern_Virginia_1895_2017") {
    clim  <- clim[, !(colnames(clim) %in% c("SP02", "SP03", "SP06", "SP09", "SP12", "SP24"))]
  }
  if(c %in% "NOAA_PDSI_Northern_Virginia_1895_2017") {
    clim  <- clim[, !(colnames(clim) %in% c("pet_sum"))]
  }
  
  ### get a moving average and sd of climate varibales, by month
  
  clim.moving.avg <- NULL
  clim.moving.sd <- NULL
  
  for(mth.i in (unique(clim$month))) {
    mth <- tolower(month.abb[mth.i])
    x.clim <- clim[clim$month %in% mth.i, ]
    x.clim.ma <- apply(x.clim, 2, runmean, k = 25, endrule = "NA", align = "center")
    x.clim.msd <- apply(x.clim, 2, runsd, k = 25, endrule = "NA", align = "center")
    
    rownames(x.clim.ma) <- paste(x.clim$year-12, x.clim$year + 12, sep = "-")
    rownames(x.clim.msd) <- paste(x.clim$year-12, x.clim$year + 12, sep = "-")
    
    clim.moving.avg[[mth]] <- x.clim.ma
    clim.moving.sd[[mth]] <- x.clim.msd
  }
  
  
  ## Run analysis on core data ####
  
  for(type.start in type.of.start.date) {
    
    print(type.start)
    
    dir.create(paste0("results/live_vs_dead/", type.start), showWarnings = F)
    dir.create(paste0("results/live_vs_dead/", type.start, "/figures"), showWarnings = F)
    dir.create(paste0("results/live_vs_dead/", type.start, "/tables"), showWarnings = F)
    
    
    if(type.start %in% "Going_back_as_far_as_possible") start.years <- start.years.sss
    if(type.start %in% "Going_back_to_1920") start.years <- ifelse(start.years.sss > 1920, start.years.sss, 1920)
    if(type.start %in% "Going_back_to_1980") start.years <- ifelse(start.years.sss > 1980, start.years.sss, 1980)
    if(type.start %in% "Going_back_at_earliest_common_year") start.years <- ifelse(start.years.sss > max(start.years.sss), max(start.years.sss), max(start.years.sss))
    # common to all species
    
    
    
    for(method.to.run in methods.to.run) {
      
      print(method.to.run)
      
      dir.create(paste0("results/live_vs_dead/", type.start, "/tables/monthly_", method.to.run ), showWarnings = F)
      
      
      all.dcc.output <- NULL
      
      ### run analysis ###
      for(f in filenames) {
        print(f)
        
        core <- get(f)
        
        core <- core[rownames(core) %in% clim$year, ] # trim to use only years for which with have clim data 
        
        start.year <- max(min(clim$year), start.years[which(filenames %in% f)])
        
        dcc.output <- NULL
        
        for (v in names(clim)[-c(1:2)]) {
          print(v)
          
          if(method.to.run %in% c("correlation", "response")) {
            dcc.output <- rbind(dcc.output, my.dcc(core, clim[, c("year", "month", v)], method = method.to.run, start = ifelse(v %in% "frs", start.frs, start), end = ifelse(v %in% "frs", end.frs, end), timespan = c(start.year, end.year), ci = 0.05, ci2 = 0.002))
          }
          
          if(method.to.run %in% "moving_correlation" & type.start %in% "Going_back_as_far_as_possible") {
            all.dcc.output[[f]][[v]] <- bootRes::mdcc(core, clim[, c("year", "month", v)], method = "corr", start = ifelse(v %in% "frs", start.frs, start), end = ifelse(v %in% "frs", end.frs, end), timespan = c(start.year, end.year), win.size = 25, win.offset = 1, startlast = T,  boot = TRUE, ci = 0.05)
          }
          
        }# for (v in names(clim)[-c(1:2)])
        
        if(method.to.run %in% c("correlation", "response")) {
          # all.dcc.output <- rbind(all.dcc.output, data.frame(cbind(Species = substr(f, 1, 4), dcc.output)))
          all.dcc.output <- rbind(all.dcc.output, data.frame(cbind(Species = gsub("_drop", "", f), dcc.output)))
        }
        
      } # for(f in filenames)
      
      ### clean and save results###
      if(method.to.run %in% c("correlation", "response")) {  
        all.dcc.output$variable <- sapply(strsplit(row.names(all.dcc.output), "\\."), function(x) x[1])
        all.dcc.output$month <- sapply(strsplit(row.names(all.dcc.output), "\\."), function(x) paste(x[2], x[3], sep ="."))
        all.dcc.output$month <- gsub("[0-9]", "",   all.dcc.output$month)
        
        if(save.result.table) write.csv(all.dcc.output, file = paste0("results/live_vs_dead/", type.start, "/tables/monthly_", method.to.run, "/", method.to.run, ifelse(grepl("corr", method.to.run), "_with_", "_to_"), c, "_climate_data.csv"), row.names = F)
      }
      
      if(method.to.run %in% c("moving_correlation") & type.start %in% "Going_back_as_far_as_possible") {
        if(save.result.table) save(all.dcc.output, file = paste0("results/live_vs_dead/", type.start, "/tables/monthly_moving_correlation/moving_correlation_with_", c, "_climate_data.Rdata"))
      }
      
      ## Plot results ####
      
      if(method.to.run %in% c("correlation")) {
        for(v in names(clim)[-c(1,2)]) {
          print(v)
          
          X <- all.dcc.output[all.dcc.output$variable %in% v, ]
          
          x <- data.frame(reshape(X[, c("month", "Species", "coef")], idvar = "month", timevar = "Species", direction = "wide"))
          rownames(x) <- ifelse(grepl("curr",  rownames(x)), toupper(rownames(x)), tolower( rownames(x)))
          rownames(x) <- gsub(".*curr.|.*prev.", "",   rownames(x), ignore.case = T)
          
          x.sig <- reshape(X[, c("month", "Species", "significant")], idvar = "month", timevar = "Species", direction = "wide")
          x.sig2 <- reshape(X[, c("month", "Species", "significant2")], idvar = "month", timevar = "Species", direction = "wide")
          
          colnames(x) <- gsub("coef.", "", colnames(x))
          colnames(x.sig) <- gsub("significant.", "", colnames(x.sig))
          colnames(x.sig2) <- gsub("significant2.", "", colnames(x.sig2))
          
          x <- x[, -1]
          x.sig <- x.sig[, -1]
          x.sig2 <- x.sig2[, -1]
          
          x <- x[, rev(SPECIES_IN_ORDER)]
          x.sig <- x.sig[, rev(SPECIES_IN_ORDER)]
          x.sig2 <- x.sig2[, rev(SPECIES_IN_ORDER)]
          
          if(save.plots)  {
            dir.create(paste0("results/live_vs_dead/", type.start, "/figures/monthly_", method.to.run), showWarnings = F)
            dir.create(paste0("results/live_vs_dead/", type.start, "/figures/monthly_", method.to.run, "/", c), showWarnings = F)
            tiff(paste0("results/live_vs_dead/", type.start, "/figures/monthly_", method.to.run, "/", c, "/", v, ".tif"), res = 150, width = 169, height = 169, units = "mm", pointsize = 10)
          }
          
          v <-  toupper(v)
          v <- gsub("PDSI_PREWHITEN" , "PDSI", v)
          
          my.dccplot(x = as.data.frame(t(x)), sig = as.data.frame(t(x.sig)), sig2 = as.data.frame(t(x.sig2)),  main = v, method = method.to.run)
          
          if(save.plots) dev.off()
        } #   for(v in names(clim)[-c(1,2)])
      } # if(method.to.run %in% c("correlation") 
      
      if(method.to.run %in% c("moving_correlation") & type.start %in% "Going_back_as_far_as_possible") {
        
        ## plot by SPECIES and by Climate variable ####
        for(f in filenames) {
          print(f)
          for(v in names(clim)[-c(1,2)]) {
            print(v)
            
            X <- all.dcc.output[[f]][[v]]
            
            X <- lapply(X, function(x) {
              rownames(x) <- gsub(v,  "", rownames(x))
              rownames(x) <- ifelse(grepl("curr",  rownames(x)), toupper(rownames(x)), tolower( rownames(x)))
              rownames(x) <- gsub(".*curr.|.*prev.", "",   rownames(x), ignore.case = T)
              return(x)
            })
            
            if(save.plots)  {
              dir.create(paste0("results/live_vs_dead/", type.start, "/figures/monthly_moving_correlation"), showWarnings = F)
              dir.create(paste0("results/live_vs_dead/", type.start, "/figures/monthly_moving_correlation/by_species_and_by_month"), showWarnings = F)
              dir.create(paste0("results/live_vs_dead/", type.start, "/figures/monthly_moving_correlation/by_species_and_by_month/", c), showWarnings = F)
              tiff(paste0("results/live_vs_dead/", type.start, "/figures/monthly_moving_correlation/by_species_and_by_month/", c, "/", f, "_", v, ".tif"), res = 150, width = 169, height = 169, units = "mm", pointsize = 10)
            }
            
            # op <- par(oma = c(1, 3, 5, 3), mai = c(1, 0.5, 0.2, 1)) #par(oma = c(0, 3, 5, 0), mai = c(0.5, 0.8, 0.2, 2))
            # op <- par(mfrow = c(2, 1), oma = c(5, 5, 5, 5), mai = c(1, 0.5, 0.2, 1), mar = c(0,0,0,0))
            
            my.mdccplot(x = X, main = paste(f, v, sep = " - "))
            
            # par(op)
            
            if(save.plots) dev.off()
          } #   for(v in names(clim)[-c(1,2)])
        } # for(f in filenames)
        
        
        ## plot all Species together for each Climate variable and each month of the growing season ####
        
        for(v in names(clim)[-c(1,2)]) {
          print(v)
          for(mth in switch(v,frs = c("apr", "may"), c("apr", "may", "jun", "jul"))) {
            print(mth)
            
            X <- lapply(lapply(all.dcc.output, "[[", v), function(x){
              x <- x[["coef"]][paste0(v, ".curr.", mth),]
              x <- x[,c(grep(overall.start.year + 1, substr(names(x), 1, 4)):ncol(x))] # this won't work, need to adapt if we get back to this analysis
              return(x)
            })
            X <- do.call(rbind, X)
            
            Sig <- lapply(lapply(all.dcc.output, "[[", v), function(x){
              x <- x[["significant"]][paste0(v, ".curr.", mth),]
              x <- x[,c(grep(overall.start.year + 1, substr(names(x), 1, 4)):ncol(x))]  # this won't work, need to adapt if we get back to this analysis
              return(x)
            })
            
            Sig <- do.call(rbind, Sig)
            
            X <- list(coef = X, significant = Sig)
            
            if(save.plots) {
              dir.create(paste0("results/live_vs_dead/", type.start, "/figures/monthly_moving_correlation/by_curr_season_month_all_sp_together"), showWarnings = F)
              dir.create(paste0("results/live_vs_dead/", type.start, "/figures/monthly_moving_correlation/by_curr_season_month_all_sp_together/", c), showWarnings = F)
              tiff(paste0("results/live_vs_dead/", type.start, "/figures/monthly_moving_correlation/by_curr_season_month_all_sp_together/", c, "/", v, "_", mth, ".tif"), res = 150, width = 169, height = 169, units = "mm", pointsize = 10)
            }
            
            my.mdccplot(X, main = paste(v, mth, sep = " - "), clim.ma = clim.moving.avg[[mth]][, v][colnames(X$coef)], clim.sd = clim.moving.sd[[mth]][, v][colnames(X$coef)])
            
            if(save.plots) dev.off()
          } #  for(m in c( c("apr", "may", "jun", "jul")))
          
        } #  for(v in names(clim)[-c(1,2)]) 
        
        
      } #  if(method.to.run %in% c(""moving_correlation""))
      
      
    } #  for(method.to.run in methods.to.run)
  } # for(type.start in type.of.start.date) 
  
  
  
} # for(c in climate.data.types)
