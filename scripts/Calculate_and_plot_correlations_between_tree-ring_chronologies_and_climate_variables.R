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
library(treeclim)
library(bootRes)

source("scripts/my.mdcplot.R")

save.plots <- TRUE

# Load and prepare climate data ####

clim.raw <- read.csv("raw_data/climate/PRISM_SCBI_1930_2015_30second.csv")
head(clim.raw)

clim <- data.frame(year = as.integer(substr(clim.raw$Variable, 2, 5)),
                   month = as.integer(substr(clim.raw$Variable, 7, 9)))

clim <- cbind(clim, clim.raw[, -1]) # clim <- cbind(clim, PPT = clim.raw$PPT)
head(clim)

# Load, prepapre and run analysis on core data ####


filenames <- list.files("raw_data/cores/")

all.dc.corr <- NULL


for(f in filenames) {
  core <- read.rwl(paste0("raw_data/cores/", f))
  core <- detrend(core, f = 0.5, method = "Spline", make.plot = TRUE ) # detrend/smooth the time series
  core <- core[rownames(core) %in% clim$year, ] # trim to use only years for which with have clim data
  core <- chron(core)
  
  dc.corr <- bootRes::dcc(core, clim, method = "corr")
  dcplot(dc.corr)
  
  all.dc.corr <- rbind(all.dc.corr, data.frame(cbind(Species = substr(f, 1, 4), dc.corr)))
  
}


for(v in names(clim.raw)[-1]) {
  print(v)
  
  X <- all.dc.corr[grepl(v, row.names(all.dc.corr)), ]
  # row.names(X) <-  gsub("[0-9]", "", row.names(X))
  (X$month <- gsub("[0-9]", "", row.names(X)))
  
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
  
  if(save.plots)  tiff(paste0("results/monthly_correlations_all_speciess_and_climate_variables/", v, ".tif"), res = 300, width = 169, height = 169, units = "mm", pointsize = 10)

  my.mdcplot(x = as.data.frame(t(x)), sig = as.data.frame(t(x.sig)), main = v)
  
  if(save.plots) dev.off()
}
