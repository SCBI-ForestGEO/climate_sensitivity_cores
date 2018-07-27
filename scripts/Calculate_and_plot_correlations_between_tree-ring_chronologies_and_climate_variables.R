######################################################
# Purpose: Calculate and plot correlations (or response functions) between tree-ring chronologies and climate variables 
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.4 (2018-03-15)
######################################################

# Clean environment ####
rm(list = ls())

# Set working directory as Shenandoah main folder ####
setwd(".")

# Load libraries ####
library(dplR)
# library(treeclim)
library(bootRes)

source("scripts/my.mdcplot.R")

save.plots <- TRUE
save.result.table <- TRUE

# Load and prepare core data ####

filenames <- list.files("raw_data/cores/")

for(f in filenames) {
  core <- read.rwl(paste0("raw_data/cores/", f))
  core <- detrend(core, f = 0.5, method = "Spline", make.plot = TRUE ) # detrend/smooth the time series
  core <- chron(core)
  
  assign(f, core)
}


# Define sets of climate data to use ####

climate.data.types <- c("PRISM_SCBI_1930_2015_30second", "CRU_SCBI_1901_2014", "NOAA_PDSI_Northern_Virginia_1895_2017")


# Define start and end year for analysis ####
start.year = 1901
end.year = 2009

# Define start and end month for anlaysis ####
start <- -4 # April of previous year
end <- 8 # Augnust of current year

start.frs <- -10 # october of previous year (for freeze days variable only - otherwise error because all 0 in other months)
end.frs <- 5 # may of current year (for freeze days variable only)
  

# Run analysis for all types of climate data with all variables ####

for( c in climate.data.types) {
  print(c)
  
  ## Load climate data ####
  
  clim <- read.csv(paste0("raw_data/climate/Formated_", c, ".csv"))
  
  # crop first and last year of NOAA data because outliers
  if(c %in% "NOAA_PDSI_Northern_Virginia_1895_2017") {
    clim <- clim[!(clim$year %in% min(clim$year) | clim$year %in% max(clim$year)), ]
  }
  
  ## Run analysis on core data ####

  all.dc.corr <- NULL
  
  for(f in filenames) {
    print(f)
    
    core <- get(f)
    
    core <- core[rownames(core) %in% clim$year, ] # trim to use only years for which with have clim data
    
    dc.corr <- NULL
    
    for (v in names(clim)[-c(1:2)]) {
      print(v)
      dc.corr <- rbind(dc.corr, bootRes::dcc(core, clim[, c("year", "month", v)], method = "corr", start = ifelse(v %in% "frs", start.frs, start), end = ifelse(v %in% "frs", end.frs, end))) # , timespan = c(start.year, end.year)))
    }
    
    # dc.corr <- bootRes::dcc(core, clim, method = "corr", start = -4, end = 8) # , timespan = c(start.year, end.year))
    all.dc.corr <- rbind(all.dc.corr, data.frame(cbind(Species = substr(f, 1, 4), dc.corr)))
    
  }

  all.dc.corr$Variable <- sapply(strsplit(row.names(all.dc.corr), "\\."), function(x) x[1])
  all.dc.corr$month <- sapply(strsplit(row.names(all.dc.corr), "\\."), function(x) paste(x[2], x[3], sep ="."))
  all.dc.corr$month <- gsub("[0-9]", "",   all.dc.corr$month)

  if(save.result.table) write.csv(all.dc.corr, file = paste0("results/tables/monthly_correlations_all_speciess_and_climate_variables/Correlation_with_", c, "_climate_data.csv"), row.names = F)
  
  
  ## Plot results ####
  
  for(v in names(clim)[-c(1,2)]) {
    print(v)
    
    X <- all.dc.corr[all.dc.corr$Variable %in% v, ]
    
    x <- data.frame(reshape(X[, c("month", "Species", "coef")], idvar = "month", timevar = "Species", direction = "wide"))
    rownames(x) <- ifelse(grepl("curr",  rownames(x)), toupper(rownames(x)), tolower( rownames(x)))
    rownames(x) <- gsub(".*curr.|.*prev.", "",   rownames(x), ignore.case = T)
    
    x.sig <- reshape(X[, c("month", "Species", "significant")], idvar = "month", timevar = "Species", direction = "wide")
    
    colnames(x) <- gsub("coef.", "", colnames(x))
    colnames(x.sig) <- gsub("significant.", "", colnames(x))
    
    x <- x[, -1]
    x.sig <- x.sig[, -1]
    
    x <- x[, rev(c("litu", "qual", "quru", "quve", "qupr", "fram", "cagl", "caco", "cato", "juni", "fagr", "caov", "pist", "frni"))]
    x.sig <- x.sig[, rev(c("litu", "qual", "quru", "quve", "qupr", "fram", "cagl", "caco", "cato", "juni", "fagr", "caov", "pist", "frni"))]
    
    if(save.plots)  {
      dir.create(paste0("results/figures/monthly_correlations_all_speciess_and_climate_variables/", c), showWarnings = F)
      tiff(paste0("results/figures/monthly_correlations_all_speciess_and_climate_variables/", c, "/", v, ".tif"), res = 300, width = 169, height = 169, units = "mm", pointsize = 10)
    }
    
    my.mdcplot(x = as.data.frame(t(x)), sig = as.data.frame(t(x.sig)), main = v)
    
    if(save.plots) dev.off()
  }
  
}
