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
save.result.table <- TRUE

source("scripts/0-My_dplR_functions.R")


## Define how to run it regarding the starting year ####
type.of.start.date <- c("Going_back_as_far_as_possible", "Going_back_to_1980") #  "Going_back_to_1920"


# Load core data ####

filenames <- list.files("raw_data/cores/") # filenames <- list.files("raw_data/cores/")

filenames <- filenames[!grepl("live|dead", filenames, ignore.case = T)] # this is to remove dead vs live data because we don't want to look at it here.



for(f in filenames) {
  print(f)
  core <- read.rwl(paste0("raw_data/cores/", f))
  assign(f, core)
}

# for(f in filenames) {
#   print(f)
#   core <- get(f)
#   print(sum(duplicated(gsub("[^0-9]", "", names(core)))))
# }
# 
# tail(core[, duplicated(gsub("[^0-9]", "", names(core))) | duplicated(gsub("[^0-9]", "", names(core)), fromLast=TRUE)], 100)

# Load 2008 census data ####
# load("data/scbi.stem1.rdata")

url <- "https://raw.github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/scbi.stem1.rdata"
download.file(url, "scbi.stem1.rdata", mode = "wb")
load("scbi.stem1.rdata")
file.remove("scbi.stem1.rdata")

head(scbi.stem1)

# Define sets of climate data to use ####
climate.data.types <- c("CRU_SCBI_1901_2016", "NOAA_PDSI_Northern_Virginia_1895_2017") #"PRISM_SCBI_1930_2015_30second", 



# Get the linear relationship between growth in 2008 and DBH in 2008, for each species ####


## build up the data frame that link radius increment in 2008 and dbh in 2008
DF <- NULL
unknown_trees <- NULL
for(f in filenames) {
  print(f)
  
  core <- get(f)
  
  for (t in names(core)) {
    
    print(t)
    # get radius increment (take the average of 2007-2009)
    r_inc_2008 <- mean(core[c("2007", "2008", "2009"), t], na.rm = T) # core["2008", t] # 

    # get dbh in 2008
    tag <- sub("[a-z]{1,}", "", t, ignore.case = T) # remove last letter
    tag <- sub("^0", "", tag ) # remove first zero if any
    
    if(tag %in% scbi.stem1$tag) {
      dbh_2008 <- scbi.stem1[scbi.stem1$tag %in% tag & scbi.stem1$StemTag %in% 1, ]$dbh[1] # adding [1] because issue with tag 40873...
      if(length(dbh_2008)>1) readline(print(dbh_2008))
    } else {
      unknown_trees <- rbind(unknown_trees, data.frame(Species = substr(f, 1, 4), tag = t))
    }    
    
    # append to big dataframe
    
    DF <- rbind(DF, data.frame(Species = substr(f, 1, 4), r_inc_2008, dbh_2008))
    
  }
    

}



for(type.start in type.of.start.date) {
  
  print(type.start)
  
## Get the linear models definitions to be able to predict later
DBH_to_r_inc_lms <- list()

for(f in filenames) {
  print(f)
  df <- DF[DF$Species %in% substr(f, 1, 4),]
  
  ### look at relationship by species (+ plot)
  if(save.plots)  {
    dir.create(paste0("results/", type.start, "/figures/Scaling_DBH_to_radius_increment"), showWarnings = F)
    tiff(paste0("results/", type.start, "/figures/Scaling_DBH_to_radius_increment/", substr(f, 1, 4), ".tif"), res = 300, width = 169, height = 169, units = "mm", pointsize = 10)
  }
  
  plot(df$r_inc_2008 ~ df$dbh_2008, main = substr(f, 1, 4), xlab = "dbh in 2008 (mm)", ylab = "radius increment in 2008 (mm)", xlim = c(0, 1500), ylim = c(0, 7))
  lm1 <- lm(r_inc_2008  ~ dbh_2008, data =df)
  abline(lm1, lty = ifelse(summary(lm1)$coefficients[2,4] < 0.05, 1, 2))
  mtext(side = 3, adj = 0.95, paste("r_inc_2008 =", round(summary(lm1)$coefficients[1],2), "+", round(summary(lm1)$coefficients[2], 4), "dbh_2008"), line = -1)
  mtext(side = 3, adj = 0.95, paste("P-value =", round(summary(lm1)$coefficients[2,4],4)), line = -2)
  
  if(save.plots)  dev.off()
  
  ### save the model
  DBH_to_r_inc_lms[[substr(f, 1, 4)]] <- lm1 
  
}


# Calculate ANPP response for each species, each climate variable and each month ####
## For this:
# 1- Predict DBH in 2009 for all trees >10cm in full census 2008 of the species that were cored using the models defined in previous step
# 2- Do the same as 1) but for a year where there would be 1 unit increase in the climate varible (using the models defined in previous step + the response (calulated in script Calculate_and_plot_responses_between_tree-ring_chronologies_and_climate_variables.R)
# 3- Change DBH into AGB using SCBI allometries
# 4- Calculate ANPP for 2008 on a regular year (using values in 1)
# 5- Calculate ANPP for 2008 on a year with 1 unit increase in climate variable (using values in 2)
# 6- get the difference betwen 5 and 4 to get the ANPP response to climate variable
# 7- sum 6 per climate variable and month of the year
# 8- plot the quilt

ANPP_response <- NULL

for(c in climate.data.types) {
  print(c)
  # 
  # ## Load climate data + calculate SD of each variable
  # 
  # clim <- read.csv(paste0("raw_data/climate/Formated_", c, ".csv"))
  # 
  # ### crop first and last year of NOAA data because outliers
  # if(c %in% "NOAA_PDSI_Northern_Virginia_1895_2017") {
  #   clim <- clim[!(clim$year %in% min(clim$year) | clim$year %in% max(clim$year)), ]
  # }
  # 
  # ### Pre_chiten PDSI of NOAA data because autocorrelated by definitiaon
  # if(c %in% "NOAA_PDSI_Northern_Virginia_1895_2017") {
  #   clim$PDSI_prewhiten <- ar(clim$PDSI)$resid
  #   clim$PHDI_prewhiten <- ar(clim$PHDI)$resid
  #   clim$PMDI_prewhiten <- ar(clim$PMDI)$resid
  # }
  # 
  # ### get SD
  # SDs <- apply(clim, 2, sd, na.rm = T)
  # assign(paste("SDs", c, sep = "_"), SDs)
  
  ## load the slopes calculated from correlations coefficients of chronologies to climate variables (output of script 1- Calculate_and_plot_correlations_and_responses_between_tree-ring_chronologies_and_climate_variables.R)
  
  Results_correlation_climate <- read.csv(paste0("results/", type.start, "/tables/monthly_correlation/correlation_with_", c, "_climate_data.csv"), stringsAsFactors = F)
  
  
  ## do steps 1 through 6 ####
  pb <- txtProgressBar(style = 3, min = 1, max = nrow(Results_correlation_climate))

  ANPP_stem_core <- NULL # (this is for Table S3 Comparison of ANPP_stem estimates from census data and cores.) 
  
  for(i in 1:nrow(Results_correlation_climate)) {
    
    setTxtProgressBar(pb,i)

    v <- Results_correlation_climate$variable[i]
    m <- Results_correlation_climate$month[i]
    sp <- tolower(Results_correlation_climate$Species[i])
    
    rad_plus <- Results_correlation_climate$chg_rad_inc_1SD_clim[i] # coef <- Results_correlation_climate$coef[i]
    # sd.v <- SDs[[v]] 
    
    lm1 <- DBH_to_r_inc_lms[[sp]]
    
    
    # get the index of trees of the right species that were alive in 2008
    idx <- substr(scbi.stem1$sp, 1, 4) %in% sp & scbi.stem1$DFstatus %in% "alive" & scbi.stem1$dbh >= 100  & !is.na(scbi.stem1$dbh)

    # steps 1,2,3 - get the AGB for 2008, 2009 and 2009 if there were one unit of increase in climate variable ####
    
    sp_complete <- ifelse(sp %in% "caov", "caovl", sp)
    
    ## AGB 2008
    x <- data.frame(sp = sp_complete, dbh = scbi.stem1$dbh[idx])
    source("scripts/0-scbi_Allometries.R")
    agb_2008 <- x$agb * .47
    
    ## AGB 2009 (using DBH in 2009 predicted using linear model)
    x <- data.frame(sp = sp_complete, dbh = scbi.stem1$dbh[idx] + c(2 * predict(object = lm1, newdata = data.frame(dbh_2008 = scbi.stem1$dbh[idx]))))
    source("scripts/0-scbi_Allometries.R")
    agb_2009 <- x$agb * .47
    
    ## AGB 2009 with one unit of increase in climate variable  (using DBH in 2009 predicted using  linear model + response coeficient)
     x <- data.frame(sp = sp_complete, dbh = scbi.stem1$dbh[idx] + c(2 * (predict(object = lm1, newdata = data.frame(dbh_2008 = scbi.stem1$dbh[idx])) + rad_plus * 2))) # rad_plus is the absolute change in radius increment for 1SD increase in climate variable
    source("scripts/0-scbi_Allometries.R")
    agb_2009_plus <- x$agb * .47
    
    if(any(is.na(agb_2009_plus))) stop("Problem of trees becoming < 0 mm")
    
    # 4- get ANPP_2008 on a normal year ####
    ANPP_2008 <- sum(agb_2009 - agb_2008) / 25.6
    ANPP_stem_core <- rbind(ANPP_stem_core, data.frame(sp = sp_complete, ANPP_2008)) # this is for Table S3 Comparison of ANPP_stem estimates from census data and cores. 
    
    # 5- get ANPP_2008 on a year with one unit of increase in climate variable ####
    ANPP_2008_plus <- sum(agb_2009_plus - agb_2008) / 25.6
    
    # 6 - get the difference between ANPP_2008 of a normal year and a year with one unit of increase in climate variable ####
    
    ANPP_diff <- ANPP_2008_plus - ANPP_2008
    
  
    # save ANPP response to 1 unit increase in climate variable into table
    
    ANPP_response <- rbind(ANPP_response,
                           data.frame(Species = sp, 
                                      Climate_data = c, 
                                      variable = v, 
                                      month = m, 
                                      ANPP_response = ANPP_diff))
  } #  for(i in 1:nrow(Results_correlation_climate))
  
  close(pb)
} # for(c in climate.data.types

head(ANPP_response)

# 7- Sum ANPP response per climate variable and per month ####
ANPP_response[is.na(ANPP_response$ANPP_response), ]

X <- data.frame(ANPP_response = tapply(ANPP_response$ANPP_response, list(paste(ANPP_response$Climate_data, ANPP_response$variable, ANPP_response$month)), sum))

ANPP_response_total <- data.frame(Climate_data = sapply(strsplit(rownames(X), " "), "[[", 1),
                                  variable = sapply(strsplit(rownames(X), " "), "[[", 2),
                                  month = sapply(strsplit(rownames(X), " "), "[[", 3),
                                  X)

## save ####

dir.create(paste0("results/", type.start, "/tables/monthly_responses_ANPP_to_climate_variables"), showWarnings = F)

###  save ANPP_response by species, climate variable and by month ####
write.csv(ANPP_response, file = paste0("results/", type.start, "/tables/monthly_responses_ANPP_to_climate_variables/ANPP_response_by_species_climate_variable_and_month.csv"), row.names = F)

### save ANPP_response total, climate variable and by month ####
write.csv(ANPP_response_total, file = paste0("results/", type.start, "/tables/monthly_responses_ANPP_to_climate_variables/Total_ANPP_response_climate_variable_and_month.csv"), row.names = F)

# 8- plot the quilt ####

for( c in climate.data.types) {
  print(c)
  
  # SDs <- get(paste("SDs", c, sep = "_"))
  
  X <- ANPP_response_total[ANPP_response_total$Climate_data %in% c, ]

  x <- data.frame(reshape(X[, c("month", "variable", "ANPP_response")], idvar = "month", timevar = "variable", direction = "wide"))
 rownames(x) <- ifelse(grepl("curr",  x$month), toupper(x$month), tolower( x$month))
 rownames(x) <- gsub(".*curr.|.*prev.", "",   rownames(x), ignore.case = T)
  
 colnames(x) <- gsub("ANPP_response.", "", colnames(x))
 colnames(x) <- gsub("PETminusPRE", "pet-pre",  colnames(x))
 
 x <- x[c(tolower(month.abb)[4:12],toupper(month.abb)[1:8]),]# order the months correctly

 
 x <- x[, -1]
 x.sig <- x.sig2 <- x
 x.sig[] <- x.sig2[] <- FALSE
 
 # remove frs
 if("frs" %in% names(x)) x <- x[,-which(names(x) %in% "frs")]

 # get the SD in the right order
 # SDs <- SDs[colnames(x)] # put in right order

 


 if(save.plots)  {
  dir.create(paste0("results/", type.start, "/figures/monthly_responses_ANPP_to_climate_variables"), showWarnings = F)
  tiff(paste0("results/", type.start, "/figures/monthly_responses_ANPP_to_climate_variables/response_to_", c, ".tif"), res = 300, width = 169, height = 169, units = "mm", pointsize = 10)
}

  my.dccplot(x = as.data.frame(t(x)), sig = as.data.frame(t(x.sig)), sig2 = as.data.frame(t(x.sig2)), main = "", method = "response")
 # axis(2, at = c(1:ncol(x))- ncol(x)/40, paste0("SD=", round(SDs,2)), las = 1, tick = 0, line = -0.5,  cex.axis = 0.8)

 if(save.plots) dev.off()


}



} # for(type.start in type.of.start.date)

# Supplementary tables ####
##  format and save Table S2: Species-specific allometries between radial increment and DBH ####
### Allometries are the same for both type of start so we use the last one.

library(officer)
library(flextable)

Table_S2 <- as.data.frame(t(sapply(DBH_to_r_inc_lms, function(x) return(c(summary(x)$coefficients[,1], P_value_slope = summary(x)$coefficients[2,4])))))
Table_S2 <- cbind(Species = toupper(rownames(Table_S2)), Table_S2)
colnames(Table_S2)[2:3] <- c("a", "b")
Table_S2[, 2:4] <- round(Table_S2[, 2:4], 4)
Table_S2 <- Table_S2[c("litu", "qual", "quru", "quve", "qupr", "fram", "cagl", "juni", 
  "cato", "caco", "fagr", "caovl", "pist", "frni"), ]
# Table_S2$Species <- gsub("CAOV", "CAOVL", Table_S2$Species)



doc <- read_docx()

ft <- flextable(Table_S2)
ft <- set_header_labels(x = ft, Species = "Species code", P_value_slope = "P-value")

doc <- body_add_par(doc, "Table S2: Species-specific allometries between radial increment and DBH with  P-value of the slope. Radial increment = a + b * DBH.", style = "table title", pos = "after")
doc <- body_add_flextable(doc, ft, align = "center")

print(doc, target = paste0("results/tables_for_manuscript/Table_S2.docx"))

##  format and save Table S3 Comparison of ANPP_stem estimates from census data and cores. ####
### ANPP stem is the same for both type of start and all climate variable and month so we need to reduce the table and keep one line per species.

Table_S3 <- ANPP_stem_core[!duplicated(ANPP_stem_core),]

ANPP_stem_census <- read.csv(text = getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/summary_data/ANPP_total_and_by_species.csv"))
ANPP_stem_census <- ANPP_stem_census[ANPP_stem_census$census_interval %in% "census_1_2" & ANPP_stem_census$species %in% Table_S3$sp, c("species", 
                                                                                                                                       "ANPP_Mg.C.ha1.y1_10cm")]

Table_S3 <- cbind(Table_S3,  ANPP_stem_census = ANPP_stem_census[match(Table_S3$sp, ANPP_stem_census$species), 2])
Table_S3$sp <- toupper(Table_S3$sp)

# Table_S3 <- Table_S3[match(Table_S3$sp, c("litu", "qual", "quru", "quve", "qupr", "fram", "cagl", "juni", "cato", "caco", "fagr", "caovl", "pist", "frni")), ]
Table_S3 <- Table_S3[order(Table_S3$ANPP_stem_census, decreasing = T), ]

doc <- read_docx()

ft <- flextable(Table_S3)
ft <- set_header_labels(x = ft, sp = "Species code")

ft <- display( ft, col_key = "ANPP_2008", 
                 part = "header",
                 pattern = "ANPP {{my_message}} core data", 
                 formatters = list(
                   my_message ~ sprintf("stem") 
                 ), 
                 fprops = list(
                   my_message = fp_text(vertical.align = "subscript")
                 ) 
)
ft <- display( ft, col_key = "ANPP_stem_census", 
               part = "header",
               pattern = "ANPP {{my_message}} census data", 
               formatters = list(
                 my_message ~ sprintf("stem") 
               ), 
               fprops = list(
                 my_message = fp_text(vertical.align = "subscript")
               ) 
)
ft <- autofit(ft)
ft

doc <- body_add_par(doc, "Table S3 Comparison of ANPP_stem estimates from census data and cores", style = "table title", pos = "after")
doc <- body_add_flextable(doc, ft, align = "center")

print(doc, target = paste0("results/tables_for_manuscript/Table_S3.docx"))
