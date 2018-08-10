######################################################
# Purpose: Summarize correlations (or response functions) between tree-ring chronologies and climate variables 
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

# Define sets of climate data to use ###3

climate.data.types <- c("PRISM_SCBI_1930_2015_30second", "CRU_SCBI_1901_2014", "NOAA_PDSI_Northern_Virginia_1895_2017")

# Summarize tables of results for each climate data ####

for( c in climate.data.types) {
  print(c)
 
  ## Load tables of results ####

  all.dc.corr <- read.csv(paste0("results/tables/monthly_correlation_all_species_and_climate_variables/correlation_with_", c, "_climate_data.csv"))
  
  ## Summarize by Variable and by species  ####
  
  ### Take the sum of all correlations (absolute) ####
  
  summary_of_correlations_all <- rbind(tapply(all.dc.corr$coef, all.dc.corr$variable, function(x) sum(abs(x))))
  rownames(summary_of_correlations_all) <- "All"
  
  summary_of_correlations_by_sepcies <- tapply(all.dc.corr$coef, list(all.dc.corr$Species, all.dc.corr$variable) , function(x) sum(abs(x)), simplify = T)
  
  summary_of_correlations <- data.frame(Summary_type = "Sum of all absolute correlations", Species = c(rownames(summary_of_correlations_all), rownames(summary_of_correlations_by_sepcies)), rbind(summary_of_correlations_all, summary_of_correlations_by_sepcies))
  
  ### Count the number of significant correlations ####
  
  summary_of_significance_all <- rbind(tapply(all.dc.corr$significant, all.dc.corr$variable, function(x) sum(x)))
  rownames(summary_of_significance_all) <- "All"
  
  summary_of_significance_by_sepcies <- tapply(all.dc.corr$significant, list(all.dc.corr$Species, all.dc.corr$variable) , function(x) sum(x), simplify = T)
  
  summary_of_significance <- data.frame(Summary_type = "Count of Significant correlations", Species = c(rownames(summary_of_significance_all), rownames(summary_of_significance_by_sepcies)), rbind(summary_of_significance_all, summary_of_significance_by_sepcies))
  
  
  ### Take the sum of all correlations (absolute) that are significant
  
  idx <- all.dc.corr$significant
  
  summary_of_significant_correlations_all <- rbind(tapply(all.dc.corr$coef[idx], all.dc.corr$variable[idx], function(x) sum(abs(x))))
  rownames(summary_of_significant_correlations_all) <- "All"
  
  summary_of_significant_correlations_by_sepcies <- tapply(all.dc.corr$coef[idx], list(all.dc.corr$Species[idx], all.dc.corr$variable[idx]) , function(x) sum(abs(x)), simplify = T)
  
  summary_of_significant_correlations <- data.frame(Summary_type = "Sum of absolute significant correlations", Species = c(rownames(summary_of_significant_correlations_all), rownames(summary_of_significant_correlations_by_sepcies)), rbind(summary_of_significant_correlations_all, summary_of_significant_correlations_by_sepcies))
  
    
  
  # Save ####
  
  
  write.csv(rbind(summary_of_correlations, summary_of_significance, summary_of_significant_correlations),
        file = paste0("results/tables/monthly_correlation_all_species_and_climate_variables//SUMMARY_Correlation_with_", c, "_climate_data.csv"), row.names = F)
  
    
  }
  

