######################################################
# Purpose: pull out census data for cored trees
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.4 (2018-03-15)
######################################################

# Clean environment ####
rm(list = ls())

# Set working directory as Shenandoah main folder ####
setwd(".")

# Load libraries ####
library(dplR)
library(bootRes)

save.result.table <- TRUE


# Load core data ####

filenames <- list.files("raw_data/cores/") # filenames <- list.files("raw_data/cores/")

for(f in filenames) {
  print(f)
  core <- read.rwl(paste0("raw_data/cores/", f))
  assign(f, core)
}


# Load 2008 census data ####
load("data/scbi.stem1.rdata")
head(scbi.stem1)



# pull out census data for cored trees ####


## build up the data frame that link radius increment in 2008 and dbh in 2008
census.data.for.cored.trees <- NULL

for(f in filenames) {
  print(f)
  
  core <- get(f)
  
  for (t in names(core)) {

    # get census data in 2008
    tag <- sub("[a-z]{1,}", "", t, ignore.case = T) # remove last letter
    tag <- sub("^0", "", tag ) # remove first zero if any
    
    if(tag %in% scbi.stem1$tag) {
      census.data.for.cored.trees <- rbind(census.data.for.cored.trees, scbi.stem1[scbi.stem1$tag %in% tag & scbi.stem1$StemTag %in% 1, ][1,]) # adding [,1] because issue with tag 40873...
    }  
    
  } #  for (t in names(core))
  
  
} # for(f in filenames)


# remove duplicates that occur when there has been 2 cores on the same tree ####
census.data.for.cored.trees <- census.data.for.cored.trees[!duplicated(census.data.for.cored.trees$tag), ]

head(census.data.for.cored.trees)
sum(duplicated(census.data.for.cored.trees$tag))


# save ####

if(save.result.table) write.csv(census.data.for.cored.trees, file = "data/census_data_for_cored_trees.csv", row.names = F)
