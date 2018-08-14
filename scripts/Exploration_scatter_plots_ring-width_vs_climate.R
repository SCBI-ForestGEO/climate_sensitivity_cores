######################################################
# Purpose: Explore relationship between ring width and climate variable depending on start date
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.4 (2018-03-15)
######################################################

# Clean environment ####
rm(list = ls())

# Set working directory as Shenandoah main folder ####
setwd(".")

# Load libraries ####
library(dplR)
library(caTools)

# Define parameters and variables ####

## saving or not saving outputs ? ####
save.plots <- TRUE


## Define order of the species in the  plots ####
SPECIES_IN_ORDER <- toupper(c("litu", "qual", "quru", "quve", "qupr", "fram", "cagl", "caco", "cato", "juni", "fagr", "caov", "pist", "frni"))

## Define sets of climate data to use ####

climate.data.types <- "CRU_SCBI_1901_2014" # c("PRISM_SCBI_1930_2015_30second", "CRU_SCBI_1901_2014", "NOAA_PDSI_Northern_Virginia_1895_2017")

## Define sss threshold ####
sss.threshold = 0.75


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

## Define start and end year for analysis, common to all species and one for each species ####

start.years <- NULL # species specific
for(f in filenames) {
  sss <- get(paste0(f, "_sss"))
  start.years <- c(start.years, sss[sss$sss >= sss.threshold, ]$Year[1])
}

overall.start.year <- max(start.years) # common to all species

end.year = 2009  # common to all species

# Plot ####
for(c in climate.data.types) {
  print(c)
  
  ## Load climate data ####
  
  clim <- read.csv(paste0("raw_data/climate/Formated_", c, ".csv"))
  
  ### crop first and last year of NOAA data because outliers
  if(c %in% "NOAA_PDSI_Northern_Virginia_1895_2017") {
    clim <- clim[!(clim$year %in% min(clim$year) | clim$year %in% max(clim$year)), ]
  }
  
  ### Pre_chiten PDSI of NOAA data because autocorrelated by definitiaon
  if(c %in% "NOAA_PDSI_Northern_Virginia_1895_2017") {
    clim$PDSI_prewhiten <- ar(clim$PDSI)$resid
    clim$PHDI_prewhiten <- ar(clim$PHDI)$resid
    clim$PMDI_prewhiten <- ar(clim$PMDI)$resid
  }
  
  ### remove climate variables we don't care about
  if(c %in% "NOAA_PDSI_Northern_Virginia_1895_2017") {
    clim  <- clim[, !(colnames(clim) %in% c("SP02", "SP03", "SP06", "SP09", "SP12", "SP24"))]
  }
  if(c %in% "NOAA_PDSI_Northern_Virginia_1895_2017") {
    clim  <- clim[, !(colnames(clim) %in% c("pet_sum"))]
  }


  for(f in toupper(SPECIES_IN_ORDER[c(1:3)])) {
    print(f)
    
    core <- get(f)
    
    core <- core[rownames(core) %in% clim$year, ] # trim to use only years for which with have clim data 
    
    start.year <- max(min(clim$year), start.years[which(filenames %in% f)])
    
    for (v in c("pet", "tmx")) { #names(clim)[-c(1:2)]) {
      print(v)
      
      for(mth in c("apr", "may", "jun", "jul", "aug")) {
        print(mth)
        mth.i <- which(tolower(month.abb) %in% mth)
        
        x.all.years <- clim[clim$month %in% mth.i & clim$year >= start.year &  clim$year <= end.year, v]
        y.all.years <- core$res[row.names(core) >= start.year &  row.names(core) <= end.year]
        
        x.later.years <- clim[clim$month %in% mth.i & clim$year >= overall.start.year &  clim$year <= end.year, v]
        y.later.years <-  core$res[row.names(core) >= overall.start.year &  row.names(core) <= end.year]
  
        
        
        if(save.plots) tiff(paste0("results/Exploration_scatter_plots_ring-width_vs_climate/", paste(f, v, mth, sep = "_"), ".tiff"), res = 100, width = 169, height = 169, units = "mm", pointsize = 12)

        plot(x.all.years, y.all.years, main = paste(f, v, mth, sep = "-"), ylab = "ring-width index", xlab = v, las = 1)
        points(x.later.years, y.later.years, col = "grey")
    
        abline(lm(y.all.years ~ x.all.years))
        abline(lm(y.later.years ~ x.later.years), col = "grey")
        
        legend("topright", lty = 1, col = c("black", "grey"), legend = c(paste(start.year, end.year, sep = "-"), paste(overall.start.year, end.year, sep = "-")), bty = "n")
        
        if(save.plots) dev.off()
      } # for(mth in c("apr", "may", "jun", "jul", "aug"))) 
    } #  for (v in c("pet", "tmx")
    
  } # for(f in toupper(SPECIES_IN_ORDER[c(1:3)])) 
} # for(c in climate.data.types)