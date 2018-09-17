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

save.plots <- TRUE
save.result.table <- TRUE

# Define sets of climate data to use ###3

climate.data.types <- c("PRISM_SCBI_1930_2015_30second", "CRU_SCBI_1901_2016", "NOAA_PDSI_Northern_Virginia_1895_2017")

## Define how to run it regarding the starting year ####
type.of.start.date <- c("Going_back_as_far_as_possible", "Going_back_to_1920", "Going_back_to_1980") # Going_back_at_earliest_common_year")


# Summarize tables of results for each climate data ####


for(type.start in type.of.start.date) {
  
  print(type.start)
  
  
  All_species_combined_both_season_subsets_all_climates <- NULL
  
  
  for( c in climate.data.types) {
    print(c)

    
    All_species_combined_both_season_subsets <- NULL
    
    for(season in c("all_seasons", "curr_growing_season_only")) {
      ## Load tables of results ####
      
      all.dc.corr <- read.csv(paste0("results/", type.start, "/tables/monthly_correlation/correlation_with_", c, "_climate_data.csv"))
      
      if(season %in% "curr_growing_season_only") all.dc.corr <- all.dc.corr[all.dc.corr$month %in% paste0("curr.", c("may", "jun", "jul", "aug")),]
      
      ## Summarize by Variable and by species  ####
      
      ### Take the sum of all correlations (absolute) ####
      
      summary_of_correlations_all <- rbind(tapply(all.dc.corr$coef, all.dc.corr$variable, function(x) sum(abs(x))))
      rownames(summary_of_correlations_all) <- "All"
      
      summary_of_correlations_by_species <- tapply(all.dc.corr$coef, list(all.dc.corr$Species, all.dc.corr$variable) , function(x) sum(abs(x)), simplify = T)
      
      summary_of_correlations <- data.frame(Summary_type = "Sum of all absolute correlations", Species = c(rownames(summary_of_correlations_all), rownames(summary_of_correlations_by_species)), rbind(summary_of_correlations_all, summary_of_correlations_by_species))
      
      ### Count the number of significant correlations ####
      
      summary_of_significance_all <- rbind(tapply(all.dc.corr$significant, all.dc.corr$variable, function(x) sum(x)))
      rownames(summary_of_significance_all) <- "All"
      
      summary_of_significance_by_species <- tapply(all.dc.corr$significant, list(all.dc.corr$Species, all.dc.corr$variable) , function(x) sum(x), simplify = T)
      
      summary_of_significance <- data.frame(Summary_type = "Count of Significant correlations", Species = c(rownames(summary_of_significance_all), rownames(summary_of_significance_by_species)), rbind(summary_of_significance_all, summary_of_significance_by_species))
      
      
      ### Take the sum of all correlations (absolute) that are significant
      
      idx <- all.dc.corr$significant
      
      summary_of_significant_correlations_all <- rbind(tapply(all.dc.corr$coef[idx], all.dc.corr$variable[idx], function(x) sum(abs(x))))
      rownames(summary_of_significant_correlations_all) <- "All"
      
      summary_of_significant_correlations_by_species <- tapply(all.dc.corr$coef[idx], list(all.dc.corr$Species[idx], all.dc.corr$variable[idx]) , function(x) sum(abs(x)), simplify = T)
      
      summary_of_significant_correlations <- data.frame(Summary_type = "Sum of absolute significant correlations", Species = c(rownames(summary_of_significant_correlations_all), rownames(summary_of_significant_correlations_by_species)), rbind(summary_of_significant_correlations_all, summary_of_significant_correlations_by_species))
      
      
      
      # Save ####
      summary_to_save <- rbind(summary_of_correlations, summary_of_significance, summary_of_significant_correlations)
    
      # if(save.result.table) write.csv(summary_to_save, file = paste0("results/", type.start, "/tables/monthly_correlation/SUMMARY_Correlation_with_", c, "_", season, ".csv"), row.names = F)
      
      All_species_combined_both_season_subsets <- rbind(All_species_combined_both_season_subsets, data.frame(season, summary_to_save[grepl("All", rownames(summary_to_save)), ]))
      
       } #   for(season in c("all_seasons", "curr_growing_season_only")) 
    
    # if(save.result.table) write.csv(All_species_combined_both_season_subsets, file = paste0("results/", type.start, "/tables/monthly_correlation/SUMMARY_Correlation_with_", c, "_both_seasons_species_combined.csv"), row.names = F)
    
    if( c %in% climate.data.types[1]) All_species_combined_both_season_subsets_all_climates <- All_species_combined_both_season_subsets
    if( !c %in% climate.data.types[1]) All_species_combined_both_season_subsets_all_climates <- cbind(All_species_combined_both_season_subsets_all_climates, All_species_combined_both_season_subsets[, -c(1:3)])
   
  } # for( c in climate.data.types)
  

  
  if(save.result.table) write.csv(All_species_combined_both_season_subsets_all_climates, file = paste0("results/", type.start, "/tables/monthly_correlation/SUMMARY_Correlation_with_all_climates_both_seasons_species_combined.csv"), row.names = F)
  
  

}  # for(type.start in type.of.start.date)

