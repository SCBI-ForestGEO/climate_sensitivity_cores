######################################################
# Purpose: Run all relevant script for the prject, looking separately at trees cored while alive and trees cored while 
# Developped by: Valentine Herrmann - HerrmannV@si.edu 10/18/2018
# R version 3.5.1 (2018-07-02)
######################################################

# Clean environment ####
rm(list = ls())

# Set working directory  ####
setwd(".")

# settings ###

# run all necessary script ####

##** WARNING** ###
##** If script #1 does not run. open it and update URL like explained in line 28. **##


source("scripts/1bis- Calculate_and_plot_correlations_and_responses_between_tree-ring_chronologies_and_climate_variables_Live_VS_Dead.R")
source("scripts/2bis-Summarize_correlations_between_tree-ring_chronologies_and_climate_variables_Live_VS_Dead.R")
