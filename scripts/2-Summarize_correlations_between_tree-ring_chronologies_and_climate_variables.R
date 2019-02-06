######################################################
# Purpose: Summarize correlations (or response functions) between tree-ring chronologies and climate variables 
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.5.1 (2018-07-02)
######################################################

# Clean environment ####
rm(list = ls())

# Set working directory as Shenandoah main folder ####
setwd(".")

# Load libraries ####
library(dplR)
library(bootRes)

save.plots <- TRUE
save.result.table <- TRUE

# Define sets of climate data to use ###3

climate.data.types <- c("CRU_SCBI_1901_2016", "NOAA_PDSI_Northern_Virginia_1895_2017")

## Define how to run it regarding the starting year ####
type.of.start.date <- c("1901_2009", "1980_2009")


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
      
      All_species_combined_both_season_subsets <- rbind(All_species_combined_both_season_subsets, data.frame(season, summary_to_save[grepl("All", rownames(summary_to_save)), ]))
      
    } #   for(season in c("all_seasons", "curr_growing_season_only")) 
    
    if( c %in% climate.data.types[1]) All_species_combined_both_season_subsets_all_climates <- All_species_combined_both_season_subsets
    if( !c %in% climate.data.types[1]) All_species_combined_both_season_subsets_all_climates <- cbind(All_species_combined_both_season_subsets_all_climates, All_species_combined_both_season_subsets[, -c(1:3)])
    
  } # for( c in climate.data.types)
  
  
  
  if(save.result.table) write.csv(All_species_combined_both_season_subsets_all_climates, file = paste0("results/", type.start, "/tables/monthly_correlation/SUMMARY_Correlation_with_all_climates_both_seasons_species_combined.csv"), row.names = F)
  
  
  
}  # for(type.start in type.of.start.date)



# compute statistics on the consistency of climate responses across species ####


climate.data.types <- c("CRU_SCBI_1901_2016", "NOAA_PDSI_Northern_Virginia_1895_2017")



for(type.start in type.of.start.date) {
  
  print(type.start)
  
  n_positive_corr <- NULL
  n_significant_corr <- NULL
  mean_corr <- NULL
  min_corr <- NULL
  max_corr <- NULL
  
  for( c in climate.data.types) {
    print(c)
    
    ## Load tables of results ####
    
    all.dc.corr <- read.csv(paste0("results/", type.start, "/tables/monthly_correlation/correlation_with_", c, "_climate_data.csv"))
    
    ### keep only variable we want
    if(c %in% "NOAA_PDSI_Northern_Virginia_1895_2017") all.dc.corr <- droplevels(all.dc.corr[all.dc.corr$variable %in% "PDSI_prewhiten", ])
    if(c %in% "CRU_SCBI_1901_2016") all.dc.corr <- droplevels(all.dc.corr[!all.dc.corr$variable %in% c("pet_sum", "frs"), ])
    
    ## Summarize by Variable and by month ####
    
    n_positive_corr <- rbind(n_positive_corr, tapply(all.dc.corr$coef, list(all.dc.corr$variable, all.dc.corr$month), function(x) sum(x>=0, na.rm = T)))
    
    n_significant_corr <- rbind(n_significant_corr, tapply(all.dc.corr$significant, list(all.dc.corr$variable, all.dc.corr$month), function(x) sum(x)))
    
    mean_corr <- rbind(mean_corr, tapply(all.dc.corr$coef,  list(all.dc.corr$variable, all.dc.corr$month), function(x) mean(x, na.rm = T)))
    
    min_corr <- rbind(min_corr, tapply(all.dc.corr$coef, list(all.dc.corr$variable, all.dc.corr$month), function(x) min(x, na.rm = T)))
    
    max_corr <- rbind(max_corr, tapply(all.dc.corr$coef,  list(all.dc.corr$variable, all.dc.corr$month), function(x) max(x, na.rm = T)))
    
    
  } # for( c in climate.data.types)
  
  # re-order the months
  columns.in.order <- c(paste0("prev.", tolower(month.abb[4:12])), paste0("curr.", tolower(month.abb[1:8])))
  
  n_positive_corr <- n_positive_corr[, columns.in.order]
  n_significant_corr <- n_significant_corr[, columns.in.order]
  mean_corr <- mean_corr[, columns.in.order]
  min_corr <- min_corr[, columns.in.order]
  max_corr <- max_corr[, columns.in.order]
  
  
  # give variable column
  n_positive_corr <- data.frame(variable = row.names(n_positive_corr), n_positive_corr)
  n_significant_corr <- data.frame(variable = row.names(n_significant_corr), n_significant_corr)
  mean_corr <- data.frame(variable = row.names(mean_corr), mean_corr)
  min_corr <- data.frame(variable = row.names(min_corr), min_corr)
  max_corr <- data.frame(variable = row.names(max_corr), max_corr)
  
  if(save.result.table) {
    write.csv(n_positive_corr, file = paste0("results/", type.start, "/tables/monthly_correlation/SUMMARY_n_positive_correlation.csv"), row.names = F)
    write.csv(n_significant_corr, file = paste0("results/", type.start, "/tables/monthly_correlation/SUMMARY_n_significant_correlation.csv"), row.names = F)
    write.csv(mean_corr, file = paste0("results/", type.start, "/tables/monthly_correlation/SUMMARY_mean_correlation.csv"), row.names = F)
    write.csv(min_corr, file = paste0("results/", type.start, "/tables/monthly_correlation/SUMMARY_min_correlation.csv"), row.names = F)
    write.csv(max_corr, file = paste0("results/", type.start, "/tables/monthly_correlation/SUMMARY_max_correlation.csv"), row.names = F)
    
  }
  
  
  
}  # for(type.start in type.of.start.date)


# Supplementary tables ####

## reload libraries and data to be able to run this section only ###

library(officer)
library(flextable)


for(type.start in c("1901_2009", "1980_2009")) {
  assign(paste("n_positive_corr", type.start, sep = "_"), read.csv(file = paste0("results/", type.start, "/tables/monthly_correlation/SUMMARY_n_positive_correlation.csv")))
  assign(paste("n_significant_corr", type.start, sep = "_"), read.csv(file = paste0("results/", type.start, "/tables/monthly_correlation/SUMMARY_n_significant_correlation.csv")))
  assign(paste("mean_corr", type.start, sep = "_"), read.csv(file = paste0("results/", type.start, "/tables/monthly_correlation/SUMMARY_mean_correlation.csv")))
  assign(paste("min_corr", type.start, sep = "_"), read.csv(file = paste0("results/", type.start, "/tables/monthly_correlation/SUMMARY_min_correlation.csv")))
  assign(paste("max_corr", type.start, sep = "_"), read.csv(file = paste0("results/", type.start, "/tables/monthly_correlation/SUMMARY_max_correlation.csv")))
}

doc <- read_docx()

for(v in levels(n_positive_corr_1901_2009$variable)) {
  
  supp_table_v <- NULL
  
  for(type.start in c("1901_2009", "1980_2009")) {
    
    n_positive_corr <- get(paste("n_positive_corr", type.start, sep = "_"))
    n_significant_corr <- get(paste("n_significant_corr", type.start, sep = "_"))
    mean_corr <- get(paste("mean_corr", type.start, sep = "_"))
    min_corr <- get(paste("min_corr", type.start, sep = "_"))
    max_corr<- get(paste("max_corr", type.start, sep = "_"))
    
    
    
    supp_table_v <- as.data.frame(cbind(supp_table_v,
                                        cbind(t(mean_corr[mean_corr$variable %in% v, -1]),
                                              t(min_corr[min_corr$variable %in% v, -1]),
                                              t(max_corr[max_corr$variable %in% v, -1]),
                                              t(n_positive_corr[n_positive_corr$variable %in% v, -1]),
                                              t(n_significant_corr[n_significant_corr$variable %in% v, -1])))
    )
  } # for(type.start in c("1901_2009", "1980_2009"))
  
  supp_table_v <- cbind(rownames(supp_table_v), supp_table_v)
  colnames(supp_table_v) <-  letters[1:11]
  
  ft <- flextable(supp_table_v)
  ft <- set_header_labels(x = ft,
                          a = "", b = "mean", c = "min", d = "max", e = "n positive", f = "n significant",
                          g = "mean", h = "min", i = "max", j = "n positive", k = "n significant", top = FALSE )
  ft <- add_header(ft,  a = "month", b = "1901-2009", c = "1901-2009", d = "1901-2009", e = "1901-2009", f = "1901-2009",
                   g = "1980-2009", h = "1980-2009", i = "1980-2009", j = "1980-2009", k = "1980-2009")
  ft <- merge_h(ft, part = "header")
  ft <- theme_booktabs(ft)
  ft <- autofit(ft, add_w = 0, add_h = 0)
  
  doc <- body_add_par(doc, paste0("table ", which(levels(n_positive_corr$variable) %in% v), ": Summary of Pearson correlations across species for ", v), style = "table title", pos = "after")
  doc <- body_add_flextable(doc, ft, align = "center")
  
} # for(v in levels(n_positive_corr$variable))



print(doc, target = "results/tables_for_manuscript/Supplementary_tables_Pearson_correlation_summary_per_variable.docx")





