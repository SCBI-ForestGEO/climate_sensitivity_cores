######################################################
# Purpose: Calculate correlations and response (and plot correlations) between tree-ring chronologies and climate variables 
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

filenames <- list.dirs("data", full.names = F, recursive = F  ) # filenames <- list.files("raw_data/cores/")
filenames <- filenames[!grepl("[a-z]", filenames)] # keep only all caps names

all_sss <- NULL

for(f in filenames) {
  # get the raw data
  core_raw <- read.rwl(paste0("raw_data/cores/", tolower(f), "_drop.rwl"))
  
  # get the detrended data
  core <- read.table(paste0("data/", f,"/ARSTANfiles/", tolower(f), "_drop.rwl_tabs.txt"), sep = "\t", h = T)
  core <- data.frame(res = core$res,  samp.depth = core$num, row.names = core$year)
  
  # get the Subsample Signal Strength (sss as function of the number of trees in sample, the last one appearing in the "xxx_drop.rxl_out.txt files)
  
  sss <- readLines(paste0("data/", f,"/ARSTANfiles/", tolower(f), "_drop.rwl_out.txt"))
  sss <- sss[grep("sss", sss)]
  
  sss <- sss[grep("  sss:   ", sss)[c(rep(FALSE, 3*length(seq(grep("  sss:   ", sss)))/4), rep(TRUE, 1*length(seq(grep("  sss:   ", sss)))/4))]] # keep only last rows that have sss: in them
  
  sss <- sub("  sss:   ", "", sss)
  sss <- as.numeric(unlist(strsplit(sss, " " ))) # keep only numbers and store them as a vector

  sss <- data.frame(Species = f, "Num_of_trees" = 1:length(sss), sss)
  
  Year_to_Num_of_trees <- apply(core_raw, 1, function(x) sum(!is.na(x)))

  for(i in 1:nrow(sss)) {
    year_with_x_trees <- Year_to_Num_of_trees[Year_to_Num_of_trees >= sss$Num_of_trees[i]]
    sss$Year[i] <- as.numeric(names(year_with_x_trees)[1])
  }

  
    # core <- read.rwl(paste0("raw_data/cores/", f))
  # core <- detrend(core, f = 0.5, nyrs = 32, method = "Spline", make.plot = TRUE) # detrend/smooth the time series
  # core <- chron(core)
  
  assign(f, core)
  assign(paste0(f, "_sss"), sss)
  
  all_sss <- rbind(all_sss, sss)
  
}

# save SSS for all species 

write.csv(all_sss, file = "results/tables/SSS_as_a_function_of_the_number_of_trees_in_sample.csv", row.names = F)


SPECIES_IN_ORDER <- toupper(c("litu", "qual", "quru", "quve", "qupr", "fram", "cagl", "caco", "cato", "juni", "fagr", "caov", "pist", "frni"))

# core.eps <- NULL
# for(f in filenames) {
#   print(f)
#   core <- read.rwl(paste0("raw_data/cores/", f))
#   core.ids <- read.ids(core, stc = c("auto"))
#   rwi.stats.core <- rwi.stats.running(core, core.ids, prewhiten = T)
#   core.eps <- rbind(core.eps, data.frame(Species = substr(f, 1,4), rwi.stats.core))
# }


# Define sets of climate data to use ####

climate.data.types <- c("PRISM_SCBI_1930_2015_30second", "CRU_SCBI_1901_2014", "NOAA_PDSI_Northern_Virginia_1895_2017")


# Define start and end year for analysis, common to all species and one for each species ####
sss.threshold = 0.75

start.years <- NULL # species specific
for(f in filenames) {
  sss <- get(paste0(f, "_sss"))
  start.years <- c(start.years, sss[sss$sss >= sss.threshold, ]$Year[1])
}

overall.start.year <- max(start.years) # common to all species

end.year = 2009  # common to all species


# Define start and end month for anlaysis ####
start <- -4 # April of previous year
end <- 8 # August of current year

start.frs <- -10 # october of previous year (for freeze days variable only - otherwise error because all 0 in other months)
end.frs <- 5 # may of current year (for freeze days variable only)
  


# Plot SSS for the the decided threshold ####

if(save.plots) tiff("results/figures/SSS_as_a_function_of_the_number_of_trees_in_sample.tiff", res = 150, width = 169, height = 169, units = "mm", pointsize = 10)


cols <- data.frame(col = rainbow(length(filenames)), row.names = filenames, stringsAsFactors = F)
years <- NULL

plot.nb <- 1

for(sp in levels(all_sss$Species)){
  x = all_sss[all_sss$Species %in% sp,]
  year <- x$Year[x$sss > sss.threshold][1]
  
  if(plot.nb %in% 1) {
    plot(sss ~ Year, data = x, type = "l", col = cols[sp,], xlim = range(all_sss$Year), lwd = 2, main = paste("SSS threshold =", sss.threshold))
    abline(v = year, lty = 3, col = cols[sp,])
    abline(h = 0.75, lty = 2)
  } else {
    lines(sss ~ Year, data = x, col = cols[sp,], lwd = 2)
    abline(v = x$Year[x$sss > sss.threshold][1], lty = 3, col = cols[sp,])
  }
  years <- c(years, year)
  plot.nb <- plot.nb +1
}

legend("topleft", col = cols$col, lty = 1, bty = "n", legend = paste(levels(all_sss$Species), years, sep = " - "), lwd = 2)


if(save.plots) dev.off()

# Run analysis for all types of climate data with all variables ####

for( c in climate.data.types) {
  print(c)
  
  ## Load climate data ####
  
  clim <- read.csv(paste0("raw_data/climate/Formated_", c, ".csv"))
  
  # crop first and last year of NOAA data because outliers
  if(c %in% "NOAA_PDSI_Northern_Virginia_1895_2017") {
    clim <- clim[!(clim$year %in% min(clim$year) | clim$year %in% max(clim$year)), ]
  }
  
  # Pre_chiten PDSI of NOAA data because autocorrelated by definitiaon
  if(c %in% "NOAA_PDSI_Northern_Virginia_1895_2017") {
    clim$PDSI_prewhiten <- ar(clim$PDSI)$resid
    clim$PHDI_prewhiten <- ar(clim$PHDI)$resid
    clim$PMDI_prewhiten <- ar(clim$PMDI)$resid
  }
  
  ## Run analysis on core data ####

  all.dc.corr <- NULL
  all.dc.resp <- NULL
  
  for(f in filenames) {
    print(f)
    
    core <- get(f)
    
    core <- core[rownames(core) %in% clim$year, ] # trim to use only years for which with have clim data
    
    start.year <- max(min(clim$year), start.years[which(filenames %in% f)])
    
    dc.corr <- NULL
    dc.resp <- NULL
    
    for (v in names(clim)[-c(1:2)]) {
      print(v)
      dc.corr <- rbind(dc.corr, bootRes::dcc(core, clim[, c("year", "month", v)], method = "corr", start = ifelse(v %in% "frs", start.frs, start), end = ifelse(v %in% "frs", end.frs, end), timespan = c(start.year, end.year)))
      
      dc.resp <- rbind(dc.resp, bootRes::dcc(core, clim[, c("year", "month", v)], method = "response", start = ifelse(v %in% "frs", start.frs, start), end = ifelse(v %in% "frs", end.frs, end), timespan = c(start.year, end.year)))
    }
    
    # dc.corr <- bootRes::dcc(core, clim, method = "corr", start = -4, end = 8) # , timespan = c(start.year, end.year))
    all.dc.corr <- rbind(all.dc.corr, data.frame(cbind(Species = substr(f, 1, 4), dc.corr)))
    all.dc.resp <- rbind(all.dc.resp, data.frame(cbind(Species = substr(f, 1, 4), dc.resp)))
    
  }

  all.dc.corr$variable <- sapply(strsplit(row.names(all.dc.corr), "\\."), function(x) x[1])
  all.dc.corr$month <- sapply(strsplit(row.names(all.dc.corr), "\\."), function(x) paste(x[2], x[3], sep ="."))
  all.dc.corr$month <- gsub("[0-9]", "",   all.dc.corr$month)
  
  
  all.dc.resp$variable <- sapply(strsplit(row.names(all.dc.resp), "\\."), function(x) x[1])
  all.dc.resp$month <- sapply(strsplit(row.names(all.dc.resp), "\\."), function(x) paste(x[2], x[3], sep ="."))
  all.dc.resp$month <- gsub("[0-9]", "",   all.dc.resp$month)

  if(save.result.table) {
    write.csv(all.dc.corr, file = paste0("results/tables/monthly_correlations_all_speciess_and_climate_variables/Correlation_with_", c, "_climate_data.csv"), row.names = F)
    write.csv(all.dc.resp, file = paste0("results/tables/monthly_responses_all_speciess_and_climate_variables/Response_to_", c, "_climate_data.csv"), row.names = F)
  }
  
  
  ## Plot results ####
  
  for(v in names(clim)[-c(1,2)]) {
    print(v)
    
    X <- all.dc.corr[all.dc.corr$variable %in% v, ]
    
    x <- data.frame(reshape(X[, c("month", "Species", "coef")], idvar = "month", timevar = "Species", direction = "wide"))
    rownames(x) <- ifelse(grepl("curr",  rownames(x)), toupper(rownames(x)), tolower( rownames(x)))
    rownames(x) <- gsub(".*curr.|.*prev.", "",   rownames(x), ignore.case = T)
    
    x.sig <- reshape(X[, c("month", "Species", "significant")], idvar = "month", timevar = "Species", direction = "wide")
    
    colnames(x) <- gsub("coef.", "", colnames(x))
    colnames(x.sig) <- gsub("significant.", "", colnames(x))
    
    x <- x[, -1]
    x.sig <- x.sig[, -1]
    
    x <- x[, rev(SPECIES_IN_ORDER)]
    x.sig <- x.sig[, rev(SPECIES_IN_ORDER)]
    
    if(save.plots)  {
      dir.create(paste0("results/figures/monthly_correlations_all_speciess_and_climate_variables/", c), showWarnings = F)
      tiff(paste0("results/figures/monthly_correlations_all_speciess_and_climate_variables/", c, "/", v, ".tif"), res = 300, width = 169, height = 169, units = "mm", pointsize = 10)
    }
    
    my.mdcplot(x = as.data.frame(t(x)), sig = as.data.frame(t(x.sig)), main = v)
    
    if(save.plots) dev.off()
  }
  
}
