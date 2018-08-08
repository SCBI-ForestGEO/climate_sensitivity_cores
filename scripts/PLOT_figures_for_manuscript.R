######################################################
# Purpose: Plot figures for publications
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.4 (2018-03-15)
######################################################

# Clean environment ####
rm(list = ls())

# Set working directory as Shenandoah main folder ####
setwd(".")

# Load libraries ####

source("scripts/my.mdcplot.R")

# set parameters ####
save.plots <- TRUE
save.result.table <- TRUE



# Load data ####

ANPP_response_total <- read.csv("results/tables/monthly_responses_ANPP_to_climate_variables/Total_ANPP_response_climate_variable_and_month.csv")



# ANPP response to CRU data and PDSI_pre-whitened ####

X <- droplevels(ANPP_response_total[(ANPP_response_total$Climate_data %in% "CRU_SCBI_1901_2014" & ! ANPP_response_total$variable %in% "pet_sum")| (ANPP_response_total$Climate_data %in% "NOAA_PDSI_Northern_Virginia_1895_2017" & ANPP_response_total$variable %in% "PDSI_prewhiten"), ])


x <- data.frame(reshape(X[, c("month", "variable", "ANPP_response")], idvar = "month", timevar = "variable", direction = "wide"))
rownames(x) <- ifelse(grepl("curr",  x$month), toupper(x$month), tolower( x$month))
rownames(x) <- gsub(".*curr.|.*prev.", "",   rownames(x), ignore.case = T)

colnames(x) <- gsub("ANPP_response.", "", colnames(x))

x <- x[c(tolower(month.abb)[4:12],toupper(month.abb)[1:8]),]# order the months correctly


x <- x[, -1]
x.sig <- x
x.sig[] <- FALSE

# remove frs
if("frs" %in% names(x)) x <- x[,-which(names(x) %in% "frs")]

# order by influence on ANPP (defined as predicted changes summed across all months).
x <- x[, order(abs(apply(x, 2, sum)))]


if(save.plots)  {
  # dir.create(paste0("results/figures/monthly_responses_all_speciess_and_climate_variables/", c), showWarnings = F)
  tiff(paste0("results/figures/for_manuscript/ANPP_response.tif"), res = 300, width = 169, height = 140, units = "mm", pointsize = 10)
}

my.mdcplot(x = as.data.frame(t(x)), sig = as.data.frame(t(x.sig)), main = "")

if(save.plots) dev.off()


