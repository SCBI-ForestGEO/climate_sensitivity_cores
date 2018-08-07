######################################################
# Purpose: Scale cores to ANPP
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

save.plots <- TRUE
save.result.table <- FALSE

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


# Look at relationship between growth in 2008 and DBH in 2008, for each species


## build up the data frame that link radius increment in 2008 and dbh in 2008
DF <- NULL
unknown_trees <- NULL
for(f in filenames) {
  print(f)
  core <- get(f)
  
  for (t in names(core)) {
    
    # get radius increment (take the average of 2007-2009)
    r_inc_2008 <- mean(core[c("2007", "2008", "2009"), t], na.rm = T) # core["2008", t] # 

    # get dbh in 2008
    tag <- sub("[a-z]{1,}", "", t, ignore.case = T) # remove last letter
    tag <- sub("^0", "", tag ) # remove first zero if any
    
    if(tag %in% scbi.stem1$tag) {
      dbh_2008 <- scbi.stem1[scbi.stem1$tag %in% tag, ]$dbh
    } else {
      unknown_trees <- rbind(unknown_trees, data.frame(Species = substr(f, 1, 4), tag = t))
    }    
    
    # append to big dataframe
    
    DF <- rbind(DF, data.frame(Species = substr(f, 1, 4), r_inc_2008, dbh_2008))
    
  }
    

}

## look at relationship by species (+ plot) + predict on all other trees ####
scbi.stem1$dbh_2009 <- NA

for(f in filenames) {
  print(f)
  df <- DF[DF$Species %in% substr(f, 1, 4),]
  
  ### look at relationship by species (+ plot)
  if(save.plots)  tiff(paste0("results/figures/Scaling_cores_to_ANPP/", substr(f, 1, 4), ".tif"), res = 300, width = 169, height = 169, units = "mm", pointsize = 10)
  
  plot(df$r_inc_2008 ~ df$dbh_2008, main = substr(f, 1, 4), xlab = "dbh in 2008 (mm)", ylab = "radius increment in 2008 (mm)", xlim = c(0, 1500), ylim = c(0, 7))
  lm1 <- lm(r_inc_2008  ~ dbh_2008, data =df)
  abline(lm1, lty = ifelse(summary(lm1)$coefficients[2,4] < 0.05, 1, 2))
  mtext(side = 3, adj = 0.95, paste("r_inc_2008 =", round(summary(lm1)$coefficients[1],2), "+", round(summary(lm1)$coefficients[2], 4), "dbh_2008"), line = -1)
  mtext(side = 3, adj = 0.95, paste("P-value =", round(summary(lm1)$coefficients[2,4],4)), line = -2)
  
  if(save.plots)  dev.off()
  
  ### predict on all other trees
  idx <- substr(scbi.stem1$sp, 1, 4) %in% substr(f, 1, 4)
  scbi.stem1$dbh_2009[idx] <- scbi.stem1$dbh[idx] + c(2 * predict(object = lm1, newdata = data.frame(dbh_2008 = scbi.stem1$dbh[idx])))
}

# calculate AGB in 2008 and 2009 ####

## 2008
x <- scbi.stem1
source("scripts/scbi_Allometries.R")
scbi.stem1$agb_2008 <- x$agb * .47 # convert to Mg of C

## 2009
x <- scbi.stem1
x$dbh <- x$dbh_2009
source("scripts/scbi_Allometries.R")
scbi.stem1$agb_2009 <- x$agb * .47 # convert to Mg of C



# Calculate ANPP ####
idx <- scbi.stem1$DFstatus %in% "alive"
AGB_diff <- scbi.stem1$agb_2009 - scbi.stem1$agb_2008
AGB_diff_Alive <- AGB_diff[idx]

sum_AGB_diff_by_species <- tapply(AGB_diff_Alive, scbi.stem1$sp[idx], sum, na.rm = T)
ANPP_stem_by_species <- c(sum_AGB_diff_by_species / 25.6) [ sum_AGB_diff_by_species > 0] # per hectare
