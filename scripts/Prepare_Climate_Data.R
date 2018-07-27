######################################################
# Purpose: Prepare climate data for for correlations (or response functions) between tree-ring chronologies and climate variables 
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.4 (2018-03-15)
######################################################

# Clean environment ####
rm(list = ls())

# Set working directory as Shenandoah main folder ####
setwd(".")

# Load libraries ####
library(RCurl)

# Load and prepare climate data ####

save.plots <- TRUE

## PRISM ####

clim.raw <- read.csv("raw_data/climate/PRISM_SCBI_1930_2015_30second.csv")
head(clim.raw)

clim <- data.frame(year = as.integer(substr(clim.raw$Variable, 2, 5)),
                   month = as.integer(substr(clim.raw$Variable, 7, 9)))

clim <- cbind(clim, clim.raw[, -1]) # clim <- cbind(clim, PPT = clim.raw$PPT)
head(clim)

write.csv(clim, file = "raw_data/climate/Formated_PRISM_SCBI_1930_2015_30second.csv", row.names = F)

## CRU data ####
### https://github.com/forestgeo/Climate/tree/master/Gridded_Data_Products/Historical%20Climate%20Data

url <- "https://github.com/forestgeo/Climate/tree/master/Gridded_Data_Products/Historical%20Climate%20Data"
x <- readLines(url)
grep("drt", x)
x[677]

goodlines = '\\.1901\\.2014\\.csv'
try = grep(goodlines,x,value=TRUE)
try <- gsub('"', "", try)
try.regexec <- regexec('title=(.*) id=', try, perl = T)
try.regmatches <- regmatches(try, try.regexec)

good.files <- sapply(try.regmatches, function(x) x[2])

for(f in good.files) {
  print(f)
  
  clim.raw <- read.csv(text=getURL(paste0("https://raw.githubusercontent.com/forestgeo/Climate/master/Gridded_Data_Products/Historical%20Climate%20Data/", f)), header=T)
  
  clim.raw <- clim.raw[clim.raw$sites.sitename %in% "Smithsonian_Conservation_Biology_Institute_(SCBI)", -1]
  
  if(f %in% good.files[1]) {
    clim <- data.frame(year = as.integer(substr(colnames(clim.raw), 2, 5)),
                       month = as.integer(substr(colnames(clim.raw), 7, 9)))
  } 
  
  clim.to.add <- t(clim.raw)
  colnames(clim.to.add) <- strsplit(f, "\\.")[[1]][1]
  
  clim <- cbind(clim, clim.to.add) # clim <- cbind(clim, PPT = clim.raw$PPT)
  head(clim)
  
}

clim <- clim[, !names(clim) %in% c("frs")]

write.csv(clim, file = "raw_data/climate/Formated_CRU_SCBI_1901_2014.csv", row.names = F)


### Palmer Drought Severity Index ####
### https://github.com/forestgeo/Climate/tree/master/Gridded_Data_Products/NOAA%20Divisional%20Data%20(USA)/Virginia_04_Northern_Virginia

clim.raw <- read.csv(text=getURL("https://raw.githubusercontent.com/forestgeo/Climate/master/Gridded_Data_Products/NOAA%20Divisional%20Data%20(USA)/Virginia_04_Northern_Virginia/NOAA_Divisional_data_(N_VA)_through_11_2017.csv"), header=T)

head(clim.raw)

clim <- data.frame(year = as.integer(substr(clim.raw$YearMonth, 1, 4)),
                   month = as.integer(substr(clim.raw$YearMonth, 5, 6)))

clim <- cbind(clim, clim.raw[, -c(1:3)]) # clim <- cbind(clim, PPT = clim.raw$PPT)
head(clim)

clim <- clim[, !names(clim) %in% c("CDD", "HDD")]

write.csv(clim, file = "raw_data/climate/Formated_NOAA_PDSI_Northern_Virginia_1895_2017.csv", row.names = F)








# plot ####

climate.data.types <- c("PRISM_SCBI_1930_2015_30second", "CRU_SCBI_1901_2014", "NOAA_PDSI_Northern_Virginia_1895_2017")

for( c in climate.data.types) {
  print(c)
  
  ## Load climate data ####
  
  clim <- read.csv(paste0("raw_data/climate/Formated_", c, ".csv"))

  ## plot data ####
  for(v in names(clim)[-c(1:2)]) {
    
    if(save.plots)  {
      dir.create(paste0("raw_data/climate/Graphs_raw_climate_data/", c), showWarnings = F)
      tiff(paste0("raw_data/climate/Graphs_raw_climate_data/", c, "/", v, ".tif"), res = 150, width = 169, height = 169, units = "mm", pointsize = 10)
    }
  
    plot(clim[, v] ~ as.Date(paste(clim$year, clim$month, "01", sep = "-")), main = v, type = "l", xlab = "year", ylab = v, las = 1)
    
    if(save.plots) dev.off()
  }
  
  
}
