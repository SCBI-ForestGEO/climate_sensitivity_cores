######################################################
# Purpose: pull out and map census data for cored trees
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
library(RCurl)

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


# load list of tree censused in 2010 (live trees) and trees censused im 20106-2017 (dead trees)
trees_censused_live <- read.csv(text=getURL("https://raw.githubusercontent.com/EcoClimLab/SCBI-ForestGEO-Data_private/master/tree_cores/measurement_files/measurement_notes_2010.csv?token=ASwxIayClVq88MBfIrm1jp185idBT_TVks5bq7F1wA%3D%3D"), header = T)
trees_censused_dead <- read.csv(text=getURL("https://raw.githubusercontent.com/EcoClimLab/SCBI-ForestGEO-Data_private/master/tree_cores/measurement_files/measurement_notes_2016_17.csv?token=ASwxISBg8ayLKfIRfIDp5jZeLzdjo2xNks5bq7GKwA%3D%3D"), header = T)

# pull out census data for cored trees ####


## build up the data frame that link radius increment in 2008 and dbh in 2008
census.data.for.cored.trees <- NULL
live.trees.2010.2011 <- NULL
dead.trees.2016.2017 <- NULL

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
    
    if(tag %in% trees_censused_live$Tag) live.trees.2010.2011 <- c(live.trees.2010.2011, tag)
    if(tag %in% trees_censused_dead$Tag) dead.trees.2016.2017 <- c(dead.trees.2016.2017, tag)
    if(tag %in% trees_censused_live & tag %in% trees_censused_dead) stop("tree in both coring periods")
    
  } #  for (t in names(core))
  
  
} # for(f in filenames)


# remove duplicates that occur when there has been 2 cores on the same tree ####
census.data.for.cored.trees <- census.data.for.cored.trees[!duplicated(census.data.for.cored.trees$tag), ]

head(census.data.for.cored.trees)
sum(duplicated(census.data.for.cored.trees$tag))

# order by treeID ####
census.data.for.cored.trees <- census.data.for.cored.trees[order(census.data.for.cored.trees$treeID), ]

# save census data####

if(save.result.table) write.csv(census.data.for.cored.trees, file = "data/census_data_for_cored_trees.csv", row.names = F)


# plot ####

all(census.data.for.cored.trees$tag %in% c(live.trees.2010.2011, dead.trees.2016.2017)) # should be TRUE
census.data.for.cored.trees$tag[!census.data.for.cored.trees$tag %in% c(live.trees.2010.2011, dead.trees.2016.2017)]

colors <- colorRamp(c("red4", "red",  "gold", "chartreuse4", "dodgerblue", "blue", "purple4", "magenta", "grey", "black"))
colors <- colors(c(0:(length(unique(census.data.for.cored.trees$sp))-1))/length(unique(census.data.for.cored.trees$sp)))
rownames(colors) <- sort(unique(census.data.for.cored.trees$sp))

tiff(paste0("results/figures_for_manuscript/MAP.tif"), res = 300, width = 124, height = 150, units = "mm", pointsize = 10)


par(mar = c(3, 0, 0, 9), xpd = T)
plot.new( )
plot.window( xlim=c(0,400), ylim=c(0,620))
segments(x0 = 0, x1= 400, y0 = seq(0, 640, by = 20), y1 = seq(0, 640, by = 20), col = "grey")
segments(x0 = seq(0, 400, by = 20), x1= seq(0, 400, by = 20), y0 = 0, y1 = 640, col = "grey")
axis(1, at = c(0, 100), labels = c("0", "100m"))

points(census.data.for.cored.trees$gx, census.data.for.cored.trees$gy, col = rgb(colors[census.data.for.cored.trees$sp,], maxColorValue = 255), pch =  ifelse(census.data.for.cored.trees$tag %in% live.trees.2010.2011, 16, 10), cex = ifelse(census.data.for.cored.trees$tag %in% live.trees.2010.2011, 0.8, 1))

legend(x = 425, y = 400, pch = 16, col = rgb(colors[sort(unique(census.data.for.cored.trees$sp)),], maxColorValue = 255), legend = sort(unique(census.data.for.cored.trees$sp)), bty = "n", pt.cex = 0.8)
legend(x = 425, y = 550, pch = c(16, 10), pt.cex = c(0.8, 1), col = "black", legend = c("live (2010-2011)", "dead (2016-2017)"), bty = "n")

dev.off()
