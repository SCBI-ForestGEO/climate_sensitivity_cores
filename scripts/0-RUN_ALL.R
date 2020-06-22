######################################################
# Purpose: Run all relevant script for the prject
# Developped by: Valentine Herrmann - HerrmannV@si.edu 10/18/2018
# R version 3.5.1 (2018-07-02)
######################################################

# Clean environment ####
rm(list = ls())

# Set working directory ####
setwd(".")

# settings ###

re_create_climate_data <- FALSE # this should be true only if climate data has been updated.

# run all necessary script ####

## load functions(adapted from existing packages) ####

source("scripts/0-My_dplR_functions.R")

## prepare climate data (probabebly not necessary to re-run)

if(re_create_climate_data) source("scripts/0-Prepare_Climate_Data.R")


## run analysis ####

##** WARNING** ###
##** If script #1 does not run. open and update URL like explained in line 28. **##

source("scripts/1-Calculate_and_plot_correlations_and_responses_between_tree-ring_chronologies_and_climate_variables.R")
source("scripts/2-Summarize_correlations_between_tree-ring_chronologies_and_climate_variables.R")
source("scripts/3-Scale_cores_to_ANPP.R")
source("scripts/4-PLOT_figures_for_manuscript.R")
source("scripts/5-Pull_out_and_map_census_data_for_cored_trees.R")
