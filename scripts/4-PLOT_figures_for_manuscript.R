######################################################
# Purpose: Plot figures for publications
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.5.1 (2018-07-02)
######################################################

# Clean environment ####
rm(list = ls())

# Set working directory as Shenandoah main folder ####
setwd(".")

# Load libraries ####
library(RCurl)

source("scripts/0-My_dplR_functions.R")

# set parameters ####
save.plots <- T
save.result.table <- T


# ANPP_response_total for each type of starting year (not realy for manuscript as is) ####

## Define how to run it regarding the starting year ####
type.of.start.date <- c("1901_2009", "1977_2009") 


## plot ####

for(type.start in type.of.start.date) {
  
  print(type.start)
  
  
  # Load data ####
  
  ANPP_response_total <- read.csv(paste0("results/", type.start, "/tables/monthly_responses_ANPP_to_climate_variables/Total_ANPP_response_climate_variable_and_month.csv"))
  
  
  
  # ANPP response to CRU data and PDSI_pre-whitened ####
  
  X <- droplevels(ANPP_response_total[(ANPP_response_total$Climate_data %in% "CRU_SCBI_1901_2016" & ! ANPP_response_total$variable %in% "pet_sum")| (ANPP_response_total$Climate_data %in% "NOAA_PDSI_Northern_Virginia_1895_2017" & ANPP_response_total$variable %in% "PDSI_prewhiten"), ])
  
  
  x <- data.frame(reshape(X[, c("month", "variable", "ANPP_response")], idvar = "month", timevar = "variable", direction = "wide"))
  rownames(x) <- ifelse(grepl("curr",  x$month), toupper(x$month), tolower( x$month))
  rownames(x) <- gsub(".*curr.|.*prev.", "",   rownames(x), ignore.case = T)
  
  colnames(x) <- gsub("ANPP_response.", "", colnames(x))
  
  x <- x[c(tolower(month.abb)[4:12],toupper(month.abb)[1:8]),]# order the months correctly
  
  
  x <- x[, -1]
  x.sig <- x.sig2 <- x
  x.sig[] <- x.sig2[] <- FALSE
  
  # remove frs
  if("frs" %in% names(x)) x <- x[,-which(names(x) %in% "frs")]
  
  # order by influence on ANPP (defined as predicted changes summed across all months) in analysis going back as far as possible
  
  if(type.start %in% "1901_2009") {
    
    ENERGY_VARIABLES.IN.ORDER <- names(sort(apply(x[c("MAY", "JUN", "JUL", "AUG"), c("pet", "dtr", "tmp", "tmn", "tmx")], 2, sum), decreasing = F))
    DEFICIT_VARIABLES.IN.ORDER <- "PETminusPRE"
    ENERGY_WATER_BALANCE_VARIABLES_IN_ORDER <- "PDSI_prewhiten"
    MOISTURE_VARIABLES.IN.ORDER <- names(sort(apply(x[c("MAY", "JUN", "JUL", "AUG"), c("cld", "pre", "vap", "wet")], 2, sum), decreasing = F))
    
    VARIABLES.IN.ORDER <- c(ENERGY_VARIABLES.IN.ORDER, DEFICIT_VARIABLES.IN.ORDER, ENERGY_WATER_BALANCE_VARIABLES_IN_ORDER, MOISTURE_VARIABLES.IN.ORDER)
  }
  
  x <- x[, rev(VARIABLES.IN.ORDER)]
  colnames(x) <- toupper(colnames(x))
  colnames(x) <- gsub("PDSI_PREWHITEN" , "PDSI", colnames(x))
  colnames(x) <- gsub("PETMINUSPRE" , "PET-PRE", colnames(x))
  
  
  
  
  if(save.plots)  {
    tiff(paste0("results/", type.start, "/figures/ANPP_response.tif"), res = 300, width = 169, height = 140, units = "mm", pointsize = 10)
  }
  
  my.dccplot(x = as.data.frame(t(x)), sig = as.data.frame(t(x.sig)), sig2 = as.data.frame(t(x.sig2)), main = "", method = "response")
  
  if(save.plots) dev.off()
  
  
  
  
} # for(type.start in type.of.start.date)


# # Figure 1 ####
# ## see this issue: https://github.com/EcoClimLab/climate_sensitivity_cores/issues/31
# 
# method.to.run = "correlation"
# climate_data = "CRU_SCBI_1901_2016"
# 
# type.of.start.date <- c("1901_2009", "1977_2009") # Going_back_at_earliest_common_year")
# 
# ANPP_contribution <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/summary_data/ANPP_total_and_by_species.csv"), header=T)
# 
# SPECIES_IN_ORDER <- toupper(ANPP_contribution$species[ ANPP_contribution$species %in% c("litu", "qual", "quru", "quve", "qupr", "fram", "cagl", "caco", "cato", "juni", "fagr", "caovl", "pist", "frni")])
# SPECIES_IN_ORDER <- gsub("CAOVL", "CAOV", SPECIES_IN_ORDER)
# 
# # plot ####
# if(save.plots)  {
#   tiff("results/figures_for_manuscript/Figure_1.tif", res = 150, width = 150, height = 169, units = "mm", pointsize = 10)
# }
# 
# nf <- layout(mat = matrix(c(1,2,7,3,4,7,5,6,7), ncol = 3, byrow = T), widths = c(1,1,0.4))
# # layout.show(nf)
# 
# plot.nb = 0
# 
# for(v in c("pet", "wet", "PETminusPRE")) {
#   
#   print(v)
#   for(type.start in type.of.start.date) {
#     
#     plot.nb <- plot.nb + 1
#     
#     all.dcc.output <- read.csv(paste0("results/", type.start, "/tables/monthly_", method.to.run, "/", method.to.run, ifelse(grepl("corr", method.to.run), "_with_", "_to_"), climate_data, "_climate_data.csv"), stringsAsFactors = F)
#     
#     X <- all.dcc.output[all.dcc.output$variable %in% v, ]
#     
#     x <- data.frame(reshape(X[, c("month", "Species", "coef")], idvar = "month", timevar = "Species", direction = "wide"))
#     rownames(x) <- ifelse(grepl("curr",  x$month), toupper( x$month), tolower(  x$month))
#     rownames(x) <- gsub(".*curr.|.*prev.", "",   rownames(x), ignore.case = T)
#     
#     x.sig <- reshape(X[, c("month", "Species", "significant")], idvar = "month", timevar = "Species", direction = "wide")
#     x.sig2 <- reshape(X[, c("month", "Species", "significant2")], idvar = "month", timevar = "Species", direction = "wide")
#     
#     colnames(x) <- gsub("coef.", "", colnames(x))
#     colnames(x.sig) <- gsub("significant.", "", colnames(x.sig))
#     colnames(x.sig2) <- gsub("significant2.", "", colnames(x.sig2))
#     
#     x <- x[, -1]
#     x.sig <- x.sig[, -1]
#     x.sig2 <- x.sig2[, -1]
#     
#     x <- x[, rev(SPECIES_IN_ORDER)]
#     x.sig <- x.sig[, rev(SPECIES_IN_ORDER)]
#     x.sig2 <- x.sig2[, rev(SPECIES_IN_ORDER)]
#     
#     
#     # plot (adapted my.dccplot function)
#     x = as.data.frame(t(x))
#     sig = as.data.frame(t(x.sig))
#     sig2 = as.data.frame(t(x.sig2))
#     main = ifelse(grepl("1980", type.start), "1980-2009", "1901-2009") # "[1901-1938]-2009"
#     ylab = toupper(v) ; ylab <- gsub("PETMINUSPRE", "PET-PRE", ylab)
#     rescale = T
#     
#     if (!is.data.frame(x)) {
#       x <- x$coef
#     }
#     
#     blues <- colorRamp(c("#FFFFFF", "#4B9EF2", "blue4"))
#     reds <- colorRamp(c("#FFFFFF", "#F25757", "red4"))
#     
#     m <- dim(x)[1]
#     n <- dim(x)[2]
#     
#     pos.max <- 0.65 #max(x)
#     neg.max <- 0.65 #abs(min(x))
#     
#     if(plot.nb %in% 1 ) par(oma = c(1.5, 4, 0, 0))
#     if(plot.nb %in% c(1,2)) par(mar = c(0, 1.5, 6, 0))
#     if(!plot.nb %in% c(1,2)) par(mar = c(0, 1.5, 4, 0))
#     plot(c(0.5, n + 0.5), c(0.5, m + 0.5), type = "n", xaxt = "n", 
#          yaxt = "n", ylab = "", xlab = "")
#     
#     # x-axis ####
#     axis(side = 3, at = 1:n, labels = colnames(x), las = 2) # change here
#     
#     
#     
#     # y-axis ####
#     if(plot.nb %in% c(1,3,5)) {
#       axis(side = 2, at = 1:m, labels = rownames(x), las = 1)
#       mtext(side = 2, ylab, line = 4)
#     } else {
#       axis(side = 2, at = 1:m, labels = FALSE, las = 1)
#     } 
#     # title ####
#     if(plot.nb %in% c(1,2)) title(main, line = 5, outer = F)
#     
#     
#     # plot quilt ####
#     X.left <- X.right <- Y.bottom <- Y.top <- x
#     
#     X.left[] <- rep((1:n - 0.5), each = m)
#     X.right[] <- rep((1:n + 0.5), each = m)
#     Y.bottom[] <- rep(1:m - 0.5, n)
#     Y.top[] <- rep(1:m + 0.5, n)
#     
#     
#     x.left <- unlist(c(X.left))
#     x.right <- unlist(c(X.right))
#     y.bottom <- unlist(c(Y.bottom))
#     y.top <- unlist(c(Y.top))
#     
#     xs <- unlist(c(x))
#     xs.sig <- unlist(c(sig))
#     xs.sig2 <- unlist(c(sig2))
#     
#     color <- xs
#     color[xs <= 0] <- rgb(reds(abs(xs[xs <= 0])/ neg.max), maxColorValue = 255)
#     color[xs > 0] <- rgb(blues(xs[xs > 0]/ pos.max), maxColorValue = 255)
#     
#     rect(x.left, y.bottom , x.right, y.top, col = color, border = "white")
#     
#     points((x.left + x.right) /2 , (y.bottom + y.top) /2, bg = ifelse(xs.sig,  "white", "transparent"), col = ifelse(xs.sig, "black", "transparent"), pch = 21) 
#     points((x.left + x.right) /2 , (y.bottom + y.top) /2, bg = ifelse(xs.sig2,  "white", "transparent"), col = ifelse(xs.sig2, "black", "transparent"), pch = 24) 
#     
#     
#     
#     
#     
#     # current vs previous year bars ####
#     
#     par(xpd= NA)
#     if(plot.nb %in% c(1,2)) {
#       lines(x = 1:9, y = rep(18.5, 9), col = "grey", lwd = 2)
#       lines(x = 10:17, y = rep(18.5, 8), lwd = 2)
#       text(x = 5, y = 18.5, labels = "previous year", col = "grey", pos = 3)
#       text(x = 14, y = 18.5, labels = "current year", pos = 3)
#     } else {
#       lines(x = 1:9, y = rep(18, 9), col = "grey", lwd = 2)
#       lines(x = 10:17, y = rep(18, 8), lwd = 2)
#     }
#     
#     
#     # add letter ####
#     text(x = -1, y = 18, paste0(letters[plot.nb], ")"), font = 2)
#     
#   } #  for(type.start in type.of.start.date[c(1,3)])
# } # for(v in c("pet", "cld", "PETminusPRE"))
# 
# 
# # legend ####
# par(mar = c(0,0,0,0))
# plot.new( )
# plot.window( xlim=c(0,10), ylim=c(0,100) )
# 
# leg.unit <- 2
# start.unit <- 30
# right.pos <- 1
# leg.width <- 2
# values <- seq(-1, 1, length = 11)
# 
# neg.rescaled.values <- round(seq(-neg.max, 0, length = 6), 
#                              2)
# pos.rescaled.values <- rev(round(seq(pos.max, 0, length = 6), 
#                                  2)[-6])
# rescaled.values <- c(neg.rescaled.values, pos.rescaled.values)
# 
# for (i in 1:11) {
#   if (values[i] <= 0) {
#     polygon(c(right.pos, right.pos + leg.width, right.pos + 
#                 leg.width, right.pos), c(start.unit + ((i - 1) * 
#                                                          leg.unit), start.unit + ((i - 1) * leg.unit), 
#                                          start.unit + (i * leg.unit), start.unit + (i * 
#                                                                                       leg.unit)), col = rgb(reds(abs(values[i])), 
#                                                                                                             maxColorValue = 255), lty = 0)
#     text(right.pos + leg.width , start.unit + (i * 
#                                                  leg.unit) - leg.unit/2, ifelse(rescale, rescaled.values[i], 
#                                                                                 values[i]), pos = 4)
#   }
#   else {
#     polygon(c(right.pos, right.pos + leg.width, right.pos + 
#                 leg.width, right.pos), c(start.unit + ((i - 1) * 
#                                                          leg.unit), start.unit + ((i - 1) * leg.unit), 
#                                          start.unit + (i * leg.unit), start.unit + (i * 
#                                                                                       leg.unit)), col = rgb(blues(values[i]), maxColorValue = 255), 
#             lty = 0)
#     text(right.pos + leg.width, start.unit + (i * 
#                                                 leg.unit) - leg.unit/2, ifelse(rescale, rescaled.values[i], 
#                                                                                values[i]), pos = 4)
#   }
# } #  for (i in 1:11) 
# 
# text(x = 5, y = start.unit + (i * leg.unit) + 3, labels = "Correlation", font = 2)
# 
# legend(x = 1, y = 25 , pch =  c(21, 24), bg = "white", col = "black", legend = c( "0.05", "0.0002"), bty = "n")
# text(x = 5, y = 26, labels = "Significance", font = 2)
# 
# # dev.off ####
# if(save.plots) dev.off()
# 
# 
# # Figure 1 _longer (4 variables) ####
# ## see this issue: https://github.com/EcoClimLab/climate_sensitivity_cores/issues/31
# 
# method.to.run = "correlation"
# climate_data = "CRU_SCBI_1901_2016"
# 
# type.of.start.date <- c("1901_2009", "1980_2009") # Going_back_at_earliest_common_year")
# 
# ANPP_contribution <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/summary_data/ANPP_total_and_by_species.csv"), header=T) # this URL might change because it is a private repository. If it does, update if by copying the URL direcltly from github: go to https://github.com/EcoClimLab/SCBI-ForestGEO-Data_private/master/SCBI_numbers_and_facts/ANPP_total_and_by_species.csv, click on Raw, copy the URL and paste it in place of the current URL here, inbetween the quotes of this line of code.
# SPECIES_IN_ORDER <- toupper(ANPP_contribution$species[ ANPP_contribution$species %in% c("litu", "qual", "quru", "quve", "qupr", "fram", "cagl", "caco", "cato", "juni", "fagr", "caovl", "pist", "frni")])
# SPECIES_IN_ORDER <- gsub("CAOVL", "CAOV", SPECIES_IN_ORDER)
# 
# # plot ####
# if(save.plots)  {
#   tiff("results/figures_for_manuscript/Figure_1_longer.tif", res = 150, width = 140, height = 190, units = "mm", pointsize = 10)
# }
# 
# nf <- layout(mat = matrix(c(1,2,9,3,4,9,5,6,9,7, 8, 9), ncol = 3, byrow = T), widths = c(1,1,0.4))
# # layout.show(nf)
# 
# plot.nb = 0
# 
# for(v in c("pet", "wet", "PETminusPRE", "tmx")) {
#   
#   print(v)
#   for(type.start in type.of.start.date) {
#     
#     plot.nb <- plot.nb + 1
#     
#     all.dcc.output <- read.csv(paste0("results/", type.start, "/tables/monthly_", method.to.run, "/", method.to.run, ifelse(grepl("corr", method.to.run), "_with_", "_to_"), climate_data, "_climate_data.csv"), stringsAsFactors = F)
#     
#     X <- all.dcc.output[all.dcc.output$variable %in% v, ]
#     
#     x <- data.frame(reshape(X[, c("month", "Species", "coef")], idvar = "month", timevar = "Species", direction = "wide"))
#     rownames(x) <- ifelse(grepl("curr",  x$month), toupper( x$month), tolower(  x$month))
#     rownames(x) <- gsub(".*curr.|.*prev.", "",   rownames(x), ignore.case = T)
#     
#     x.sig <- reshape(X[, c("month", "Species", "significant")], idvar = "month", timevar = "Species", direction = "wide")
#     x.sig2 <- reshape(X[, c("month", "Species", "significant2")], idvar = "month", timevar = "Species", direction = "wide")
#     
#     colnames(x) <- gsub("coef.", "", colnames(x))
#     colnames(x.sig) <- gsub("significant.", "", colnames(x.sig))
#     colnames(x.sig2) <- gsub("significant2.", "", colnames(x.sig2))
#     
#     x <- x[, -1]
#     x.sig <- x.sig[, -1]
#     x.sig2 <- x.sig2[, -1]
#     
#     x <- x[, rev(SPECIES_IN_ORDER)]
#     x.sig <- x.sig[, rev(SPECIES_IN_ORDER)]
#     x.sig2 <- x.sig2[, rev(SPECIES_IN_ORDER)]
#     
#     
#     # plot (adapted my.dccplot function)
#     x = as.data.frame(t(x))
#     sig = as.data.frame(t(x.sig))
#     sig2 = as.data.frame(t(x.sig2))
#     main = ifelse(grepl("1980", type.start), "1980-2009", "1901-2009")
#     ylab = toupper(v) ; ylab <- gsub("PETMINUSPRE", "PET-PRE", ylab)
#     rescale = T
#     
#     if (!is.data.frame(x)) {
#       x <- x$coef
#     }
#     
#     blues <- colorRamp(c("#FFFFFF", "#4B9EF2", "blue4"))
#     reds <- colorRamp(c("#FFFFFF", "#F25757", "red4"))
#     
#     m <- dim(x)[1]
#     n <- dim(x)[2]
#     
#     pos.max <- 0.65 #max(x)
#     neg.max <- 0.65 #abs(min(x))
#     
#     # op <- par(no.readonly = TRUE)
#     
#     if(plot.nb %in% 1 ) par(oma = c(1.5, 4, 5, 0))
#     if(plot.nb %in% c(1,2)) par(mar = c(0, 1.5, 1.5, 0))
#     if(!plot.nb %in% c(1,2)) par(mar = c(0, 1.5, 0.5, 0))
#     plot(c(0.5, n + 0.5), c(0.5, m + 0.5), type = "n", xaxt = "n", 
#          yaxt = "n", ylab = "", xlab = "")
#     
#     # x-axis ####
#     if(plot.nb %in% c(1,2)) {
#       axis(side = 3, at = 1:n, labels = colnames(x), las = 2)
#     }
#     
#     # y-axis ####
#     if(plot.nb %in% c(1,3,5,7)) {
#       axis(side = 2, at = 1:m, labels = rownames(x), las = 1)
#       mtext(side = 2, ylab, line = 4)
#     } else {
#       axis(side = 2, at = 1:m, labels = FALSE, las = 1)
#     } 
#     # title ####
#     if(plot.nb %in% c(1,2)) title(main, line = 4, outer = T, adj = ifelse(plot.nb %in% 1, 0.18, 0.67))
#     
#     
#     # plot quilt ####
#     X.left <- X.right <- Y.bottom <- Y.top <- x
#     
#     X.left[] <- rep((1:n - 0.5), each = m)
#     X.right[] <- rep((1:n + 0.5), each = m)
#     Y.bottom[] <- rep(1:m - 0.5, n)
#     Y.top[] <- rep(1:m + 0.5, n)
#     
#     
#     x.left <- unlist(c(X.left))
#     x.right <- unlist(c(X.right))
#     y.bottom <- unlist(c(Y.bottom))
#     y.top <- unlist(c(Y.top))
#     
#     xs <- unlist(c(x))
#     xs.sig <- unlist(c(sig))
#     xs.sig2 <- unlist(c(sig2))
#     
#     color <- xs
#     color[xs <= 0] <- rgb(reds(abs(xs[xs <= 0])/ neg.max), maxColorValue = 255)
#     color[xs > 0] <- rgb(blues(xs[xs > 0]/ pos.max), maxColorValue = 255)
#     
#     rect(x.left, y.bottom , x.right, y.top, col = color, border = "white")
#     
#     points((x.left + x.right) /2 , (y.bottom + y.top) /2, bg = ifelse(xs.sig,  "white", "transparent"), col = ifelse(xs.sig, "black", "transparent"), pch = 21) 
#     points((x.left + x.right) /2 , (y.bottom + y.top) /2, bg = ifelse(xs.sig2,  "white", "transparent"), col = ifelse(xs.sig2, "black", "transparent"), pch = 24) 
#     
#     
#     
#     
#     
#     # current vs previous year bars ####
#     
#     par(xpd= NA)
#     if(plot.nb %in% c(1,2)) {
#       lines(x = 1:9, y = rep(19.2, 9), col = "grey", lwd = 2)
#       lines(x = 10:17, y = rep(19.2, 8), lwd = 2)
#       text(x = 5, y = 19.2, labels = "previous year", col = "grey", pos = 3)
#       text(x = 14, y = 19.2, labels = "current year", pos = 3)
#     }
#     
#     
#     # add letter ####
#     text(x = -1, y = 15, paste0(letters[plot.nb], ")"), font = 2)
#   } #  for(type.start in type.of.start.date[c(1,3)])
# } # for(v in c("pet", "cld", "PETminusPRE"))
# 
# 
# # legend ####
# par(mar = c(0,0,0,0))
# plot.new( )
# plot.window( xlim=c(0,10), ylim=c(0,100) )
# 
# leg.unit <- 2
# start.unit <- 30
# right.pos <- 1
# leg.width <- 2
# values <- seq(-1, 1, length = 11)
# 
# neg.rescaled.values <- round(seq(-neg.max, 0, length = 6), 
#                              2)
# pos.rescaled.values <- rev(round(seq(pos.max, 0, length = 6), 
#                                  2)[-6])
# rescaled.values <- c(neg.rescaled.values, pos.rescaled.values)
# 
# for (i in 1:11) {
#   if (values[i] <= 0) {
#     polygon(c(right.pos, right.pos + leg.width, right.pos + 
#                 leg.width, right.pos), c(start.unit + ((i - 1) * 
#                                                          leg.unit), start.unit + ((i - 1) * leg.unit), 
#                                          start.unit + (i * leg.unit), start.unit + (i * 
#                                                                                       leg.unit)), col = rgb(reds(abs(values[i])), 
#                                                                                                             maxColorValue = 255), lty = 0)
#     text(right.pos + leg.width , start.unit + (i * 
#                                                  leg.unit) - leg.unit/2, ifelse(rescale, rescaled.values[i], 
#                                                                                 values[i]), pos = 4)
#   }
#   else {
#     polygon(c(right.pos, right.pos + leg.width, right.pos + 
#                 leg.width, right.pos), c(start.unit + ((i - 1) * 
#                                                          leg.unit), start.unit + ((i - 1) * leg.unit), 
#                                          start.unit + (i * leg.unit), start.unit + (i * 
#                                                                                       leg.unit)), col = rgb(blues(values[i]), maxColorValue = 255), 
#             lty = 0)
#     text(right.pos + leg.width, start.unit + (i * 
#                                                 leg.unit) - leg.unit/2, ifelse(rescale, rescaled.values[i], 
#                                                                                values[i]), pos = 4)
#   }
# } #  for (i in 1:11) 
# 
# text(x = 5, y = start.unit + (i * leg.unit) + 3, labels = "Correlation", font = 2)
# 
# legend(x = 1, y = 25 , pch =  c(21, 24), bg = "white", col = "black", legend = c( "0.05", "0.0002"), bty = "n")
# text(x = 5, y = 26, labels = "Significance", font = 2)
# 
# # dev.off ####
# if(save.plots) dev.off()
# 

# Figure 1B _ with Percent change of ANPP _longer (4 variables) ####
## see this issue: https://github.com/EcoClimLab/climate_sensitivity_cores/issues/31

climate_data = "CRU_SCBI_1901_2016"

type.start <- c("1901_2009")

ANPP_contribution <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/summary_data/ANPP_total_and_by_species.csv"), header=T) # this URL might change because it is a private repository. If it does, update if by copying the URL direcltly from github: go to https://github.com/EcoClimLab/SCBI-ForestGEO-Data_private/master/SCBI_numbers_and_facts/ANPP_total_and_by_species.csv, click on Raw, copy the URL and paste it in place of the current URL here, inbetween the quotes of this line of code.
SPECIES_IN_ORDER <- toupper(ANPP_contribution$species[ ANPP_contribution$species %in% c("litu", "qual", "quru", "quve", "qupr", "fram", "cagl", "caco", "cato", "juni", "fagr", "caovl", "pist", "frni")])
SPECIES_IN_ORDER <- gsub("CAOVL", "CAOV", SPECIES_IN_ORDER)



# plot ####
if(save.plots)  {
  tiff("results/figures_for_manuscript/Figure_1_longer.tif", res = 150, width = 140, height = 190, units = "mm", pointsize = 10)
}

nf <- layout(mat = matrix(c(1,9,2,10,3,9,4,10,5,9,6,10,7,9,8,10), ncol = 4, byrow = T), widths = c(1,0.4, 1,0.4))
# layout.show(nf)

plot.nb = 0

for(v in c("pet", "wet", "PETminusPRE", "tmx")) {
  
  print(v)
  for(corr_or_ANPP in c("correlation", "ANPP_response")) {
    
    plot.nb <- plot.nb + 1
    
    
    if(corr_or_ANPP %in% "correlation") {
      method.to.run = "correlation"
      
      all.dcc.output <- read.csv(paste0("results/", type.start, "/tables/monthly_", method.to.run, "/", method.to.run, ifelse(grepl("corr", method.to.run), "_with_", "_to_"), climate_data, "_climate_data.csv"), stringsAsFactors = F)
      
      X <- all.dcc.output[all.dcc.output$variable %in% v, ]
      
      x <- data.frame(reshape(X[, c("month", "Species", "coef")], idvar = "month", timevar = "Species", direction = "wide"))
      rownames(x) <- ifelse(grepl("curr",  x$month), toupper( x$month), tolower(  x$month))
      rownames(x) <- gsub(".*curr.|.*prev.", "",   rownames(x), ignore.case = T)
      
      x.sig <- reshape(X[, c("month", "Species", "significant")], idvar = "month", timevar = "Species", direction = "wide")
      x.sig2 <- reshape(X[, c("month", "Species", "significant2")], idvar = "month", timevar = "Species", direction = "wide")
      
      colnames(x) <- gsub("coef.", "", colnames(x))
      colnames(x.sig) <- gsub("significant.", "", colnames(x.sig))
      colnames(x.sig2) <- gsub("significant2.", "", colnames(x.sig2))
      
      x <- x[, -1]
      x.sig <- x.sig[, -1]
      x.sig2 <- x.sig2[, -1]
      
      x <- x[, rev(SPECIES_IN_ORDER)]
      x.sig <- x.sig[, rev(SPECIES_IN_ORDER)]
      x.sig2 <- x.sig2[, rev(SPECIES_IN_ORDER)]
      
    }
    
    if(corr_or_ANPP %in% "ANPP_response") {
    ANPP_response <- read.csv(paste0("results/", type.start, "/tables/monthly_responses_ANPP_to_climate_variables/ANPP_response_by_species_climate_variable_and_month.csv"), stringsAsFactors = F)
    
    X <- ANPP_response[ANPP_response$variable %in% v, ]

    x <- data.frame(reshape(X[, c("month", "Species", "ANPP_response")], idvar = "month", timevar = "Species", direction = "wide"))
    rownames(x) <- ifelse(grepl("curr",  x$month), toupper( x$month), tolower(  x$month))
    rownames(x) <- gsub(".*curr.|.*prev.", "",   rownames(x), ignore.case = T)
    colnames(x) <- gsub("ANPP_response.", "", toupper(colnames(x)), ignore.case = T)
    
    x <- x[, -1]
    x <- x[, rev(SPECIES_IN_ORDER)]
    
    x.sig <- x; x.sig[] <- FALSE
    x.sig2 <- x.sig

    }
    
    # plot (adapted my.dccplot function)
    x = as.data.frame(t(x))

    main = "1901-2009"
    ylab = toupper(v) ; ylab <- gsub("PETMINUSPRE", "PET-PRE", ylab)
    rescale = T
    
    if (!is.data.frame(x)) {
      x <- x$coef
    }
    
    blues <- colorRamp(c("#FFFFFF", "#4B9EF2", "blue4"))
    reds <- colorRamp(c("#FFFFFF", "#F25757", "red4"))
    
    m <- dim(x)[1]
    n <- dim(x)[2]
    
    pos.max <- ifelse(corr_or_ANPP %in% "correlation", 1.2, 0.04) #max(x)
    neg.max <- ifelse(corr_or_ANPP %in% "correlation", 0.65, 0.04) #abs(min(x))
    
    # op <- par(no.readonly = TRUE)
    
    if(plot.nb %in% 1 ) par(oma = c(1.5, 4, 5, 0))
    if(plot.nb %in% c(1,2)) par(mar = c(0, 1.5, 1.5, 0))
    if(!plot.nb %in% c(1,2)) par(mar = c(0, 1.5, 0.5, 0))
    plot(c(0.5, n + 0.5), c(0.5, m + 0.5), type = "n", xaxt = "n", 
         yaxt = "n", ylab = "", xlab = "")
    
    # x-axis ####
    if(plot.nb %in% c(1,2)) {
      axis(side = 3, at = 1:n, labels = colnames(x), las = 2)
    }
    
    # y-axis ####
    if(plot.nb %in% c(1,3,5,7)) {
      axis(side = 2, at = 1:m, labels = rownames(x), las = 1)
      mtext(side = 2, ylab, line = 4)
    } else {
      axis(side = 2, at = 1:m, labels = FALSE, las = 1)
    } 
    # title ####
    # if(plot.nb %in% c(1,2)) title(main, line = 4, outer = T, adj = ifelse(plot.nb %in% 1, 0.18, 0.67))
    if(plot.nb %in% c(1)) title(main, line = 4, outer = T, adj = 0.45)
    
    
    # plot quilt ####
    X.left <- X.right <- Y.bottom <- Y.top <- x
    
    X.left[] <- rep((1:n - 0.5), each = m)
    X.right[] <- rep((1:n + 0.5), each = m)
    Y.bottom[] <- rep(1:m - 0.5, n)
    Y.top[] <- rep(1:m + 0.5, n)
    
    
    x.left <- unlist(c(X.left))
    x.right <- unlist(c(X.right))
    y.bottom <- unlist(c(Y.bottom))
    y.top <- unlist(c(Y.top))
    
    xs <- unlist(c(x))

    color <- xs
    color[xs <= 0] <- rgb(reds(abs(xs[xs <= 0])/ neg.max), maxColorValue = 255)
    color[xs > 0] <- rgb(blues(xs[xs > 0]/ pos.max), maxColorValue = 255)
    
    rect(x.left, y.bottom , x.right, y.top, col = color, border = "white")

    
    
    
    
    # current vs previous year bars ####
    
    par(xpd= NA)
    if(plot.nb %in% c(1,2)) {
      lines(x = 1:9, y = rep(19.2, 9), col = "grey", lwd = 2)
      lines(x = 10:17, y = rep(19.2, 8), lwd = 2)
      text(x = 5, y = 19.2, labels = "previous year", col = "grey", pos = 3)
      text(x = 14, y = 19.2, labels = "current year", pos = 3)
    }
    
    
    # add letter ####
    text(x = -1, y = 15, paste0(letters[plot.nb], ")"), font = 2)
  } #  for(type.start in type.of.start.date[c(1,3)])
} # for(v in c("pet", "cld", "PETminusPRE"))


# legend corelation####
corr_or_ANPP = "correlation"
pos.max <- ifelse(corr_or_ANPP %in% "correlation", 1.2, 0.04) #max(x)
neg.max <- ifelse(corr_or_ANPP %in% "correlation", 0.65, 0.04) #abs(min(x))

par(mar = c(0,0,0,0))
plot.new( )
plot.window( xlim=c(0,10), ylim=c(0,100) )

leg.unit <- 2
start.unit <- 30
right.pos <- 1
leg.width <- 2
values <- seq(-1, 1, length = 11)

neg.rescaled.values <- round(seq(-neg.max, 0, length = 6), 
                             2)
pos.rescaled.values <- rev(round(seq(pos.max, 0, length = 6), 
                                 2)[-6])
rescaled.values <- c(neg.rescaled.values, pos.rescaled.values)

for (i in 1:11) {
  if (values[i] <= 0) {
    polygon(c(right.pos, right.pos + leg.width, right.pos + 
                leg.width, right.pos), c(start.unit + ((i - 1) * 
                                                         leg.unit), start.unit + ((i - 1) * leg.unit), 
                                         start.unit + (i * leg.unit), start.unit + (i * 
                                                                                      leg.unit)), col = rgb(reds(abs(values[i])), 
                                                                                                            maxColorValue = 255), lty = 0)
    text(right.pos + leg.width , start.unit + (i * 
                                                 leg.unit) - leg.unit/2, ifelse(rescale, rescaled.values[i], 
                                                                                values[i]), pos = 4)
  }
  else {
    polygon(c(right.pos, right.pos + leg.width, right.pos + 
                leg.width, right.pos), c(start.unit + ((i - 1) * 
                                                         leg.unit), start.unit + ((i - 1) * leg.unit), 
                                         start.unit + (i * leg.unit), start.unit + (i * 
                                                                                      leg.unit)), col = rgb(blues(values[i]), maxColorValue = 255), 
            lty = 0)
    text(right.pos + leg.width, start.unit + (i * 
                                                leg.unit) - leg.unit/2, ifelse(rescale, rescaled.values[i], 
                                                                               values[i]), pos = 4)
  }
} #  for (i in 1:11) 

text(x = 5, y = start.unit + (i * leg.unit) + 3, labels = "Correlation", font = 2)

legend(x = 1, y = 25 , pch =  c(21, 24), bg = "white", col = "black", legend = c( "0.05", "0.0002"), bty = "n")
text(x = 5, y = 26, labels = "Significance", font = 2)

# legend ANPP####

corr_or_ANPP = "ANPP_response"
pos.max <- ifelse(corr_or_ANPP %in% "correlation", 1.2, 0.04) #max(x)
neg.max <- ifelse(corr_or_ANPP %in% "correlation", 0.65, 0.04) #abs(min(x))


par(mar = c(0,0,0,0))
plot.new( )
plot.window( xlim=c(0,10), ylim=c(0,100) )

leg.unit <- 2
start.unit <- 30
right.pos <- 1
leg.width <- 2
values <- seq(-1, 1, length = 11)

neg.rescaled.values <- round(seq(-neg.max, 0, length = 6), 
                             2)
pos.rescaled.values <- rev(round(seq(pos.max, 0, length = 6), 
                                 2)[-6])
rescaled.values <- c(neg.rescaled.values, pos.rescaled.values)

for (i in 1:11) {
  if (values[i] <= 0) {
    polygon(c(right.pos, right.pos + leg.width, right.pos + 
                leg.width, right.pos), c(start.unit + ((i - 1) * 
                                                         leg.unit), start.unit + ((i - 1) * leg.unit), 
                                         start.unit + (i * leg.unit), start.unit + (i * 
                                                                                      leg.unit)), col = rgb(reds(abs(values[i])), 
                                                                                                            maxColorValue = 255), lty = 0)
    text(right.pos + leg.width , start.unit + (i * 
                                                 leg.unit) - leg.unit/2, ifelse(rescale, rescaled.values[i], 
                                                                                values[i]), pos = 4)
  }
  else {
    polygon(c(right.pos, right.pos + leg.width, right.pos + 
                leg.width, right.pos), c(start.unit + ((i - 1) * 
                                                         leg.unit), start.unit + ((i - 1) * leg.unit), 
                                         start.unit + (i * leg.unit), start.unit + (i * 
                                                                                      leg.unit)), col = rgb(blues(values[i]), maxColorValue = 255), 
            lty = 0)
    text(right.pos + leg.width, start.unit + (i * 
                                                leg.unit) - leg.unit/2, ifelse(rescale, rescaled.values[i], 
                                                                               values[i]), pos = 4)
  }
} #  for (i in 1:11) 

text(x = 5, y = start.unit + (i * leg.unit) + c(5, 3), labels = c(expression(bold(Delta*"ANPP")), expression(bold("response"))), font = 2)

# dev.off ####
if(save.plots) dev.off()


# Figure 2 ####

## Define how to run it regarding the starting year ####
type.of.start.date <- c("1901_2009") # c("1901_2009","1980_2009")


## plot ####

if(save.plots)  {
  tiff(paste0("results/figures_for_manuscript/Figure_2.tif"), res = 300, width = 169, height = 120, units = "mm", pointsize = 10)
}


nf <- layout(mat = matrix(c(1,2), ncol = 2, byrow = T), widths = c(1,0.4)) # nf <- layout(mat = matrix(c(1,2,3), ncol = 3, byrow = T), widths = c(1,1,0.4))

# layout.show(nf)

plot.nb = 0
for(type.start in type.of.start.date) {
  
  print(type.start)
  
  plot.nb = plot.nb + 1
  # Load data ####
  
  ANPP_response_total <- read.csv(paste0("results/", type.start, "/tables/monthly_responses_ANPP_to_climate_variables/Total_ANPP_response_climate_variable_and_month.csv"))
  
  X <- droplevels(ANPP_response_total[(ANPP_response_total$Climate_data %in% "CRU_SCBI_1901_2016" & ! ANPP_response_total$variable %in% "pet_sum")| (ANPP_response_total$Climate_data %in% "NOAA_PDSI_Northern_Virginia_1895_2017" & ANPP_response_total$variable %in% "PDSI_prewhiten"), ])
  
  
  x <- data.frame(reshape(X[, c("month", "variable", "ANPP_response")], idvar = "month", timevar = "variable", direction = "wide"))
  rownames(x) <- ifelse(grepl("curr",  x$month), toupper(x$month), tolower( x$month))
  rownames(x) <- gsub(".*curr.|.*prev.", "",   rownames(x), ignore.case = T)
  
  colnames(x) <- gsub("ANPP_response.", "", colnames(x))
  
  
  x <- x[c(tolower(month.abb)[4:12],toupper(month.abb)[1:8]),]# order the months correctly
  
  
  x <- x[, -1]
  x.sig <- x.sig2 <- x
  x.sig[] <- x.sig2[] <- FALSE
  
  # remove frs
  if("frs" %in% names(x)) x <- x[,-which(names(x) %in% "frs")]
  
  # order the variables ####
  # ---- BEFORE (order by influence on ANPP) -----
  # order by influence on ANPP (defined as predicted changes summed across all months) in analysis going back as far as possible
  
  # if(type.start %in% "1901_2009") {
  #   
  #   ENERGY_VARIABLES.IN.ORDER <- names(sort(apply(x[c("MAY", "JUN", "JUL", "AUG"), c("pet", "dtr", "tmp", "tmn", "tmx")], 2, sum), decreasing = F))
  #   DEFICIT_VARIABLES.IN.ORDER <- "PETminusPRE"
  #   ENERGY_WATER_BALANCE_VARIABLES_IN_ORDER <- "PDSI_prewhiten"
  #   MOISTURE_VARIABLES.IN.ORDER <- names(sort(apply(x[c("MAY", "JUN", "JUL", "AUG"), c("cld", "pre", "wet")], 2, sum), decreasing = F))
  #   
  #   VARIABLES.IN.ORDER <- c(ENERGY_VARIABLES.IN.ORDER, DEFICIT_VARIABLES.IN.ORDER, ENERGY_WATER_BALANCE_VARIABLES_IN_ORDER, MOISTURE_VARIABLES.IN.ORDER)
  # }
  # ---- NOW (fixed order)  ----
  
  VARIABLES.IN.ORDER <- c("tmx", "tmp", "tmn", "dtr", "pet", "PETminusPRE", "PDSI_prewhiten", 
                          "pre", "wet", "cld") # , "vap"
  x <- x[, rev(VARIABLES.IN.ORDER)]
  colnames(x) <- toupper(colnames(x))
  colnames(x) <- gsub("PDSI_PREWHITEN" , "PDSI", colnames(x))
  colnames(x) <- gsub("PETMINUSPRE" , "PET-PRE", colnames(x))
  
  
  # plot (adapted my.dccplot function) ####
  x = as.data.frame(t(x))
  sig = as.data.frame(t(x.sig))
  sig2 = as.data.frame(t(x.sig2))
  main = ifelse(grepl("1980", type.start), "1980-2009", "1901-2009")
  rescale = T
  
  if (!is.data.frame(x)) {
    x <- x$coef
  }
  
  blues <- colorRamp(c("#FFFFFF", "#4B9EF2", "blue4"))
  reds <- colorRamp(c("#FFFFFF", "#F25757", "red4"))
  
  m <- dim(x)[1]
  n <- dim(x)[2]
  
  pos.max <- 0.053 # 0.12, max(x)
  neg.max <- 0.053 # 0.12, abs(min(x))
  
  if(plot.nb %in% 1 ) par(oma = c(1.5, 5, 0, 0))
  if(plot.nb %in% c(1,2)) par(mar = c(0, 1.5, 6, 0))
  
  plot(c(0.5, n + 0.5), c(0.5, m + 0.5), type = "n", xaxt = "n", 
       yaxt = "n", ylab = "", xlab = "")
  
  # x-axis ####
  axis(side = 3, at = 1:n, labels = colnames(x), las = 2) # change here
  
  
  
  # y-axis ####
  if(plot.nb %in% c(1)) {
    axis(side = 2, at = 1:m, labels =rownames(x) , las = 1) # axis(side = 2, at = 1:m, labels = ifelse(grepl("PDSI", rownames(x)), expression(PDSI^1), rownames(x)) , las = 1)
  } else {
    axis(side = 2, at = 1:m, labels = F, las = 1)
  } 
  # title ####
  if(plot.nb %in% c(1,2)) title(main, line = 5, outer = F)
  
  
  # plot quilt ####
  X.left <- X.right <- Y.bottom <- Y.top <- x
  
  X.left[] <- rep((1:n - 0.5), each = m)
  X.right[] <- rep((1:n + 0.5), each = m)
  Y.bottom[] <- rep(1:m - 0.5, n)
  Y.top[] <- rep(1:m + 0.5, n)
  
  
  x.left <- unlist(c(X.left))
  x.right <- unlist(c(X.right))
  y.bottom <- unlist(c(Y.bottom))
  y.top <- unlist(c(Y.top))
  
  xs <- unlist(c(x))
  xs.sig <- unlist(c(sig))
  xs.sig2 <- unlist(c(sig2))
  
  color <- xs
  color[xs <= 0] <- rgb(reds(abs(xs[xs <= 0])/ neg.max), maxColorValue = 255)
  color[xs > 0] <- rgb(blues(xs[xs > 0]/ pos.max), maxColorValue = 255)
  
  rect(x.left, y.bottom , x.right, y.top, col = color, border = "white")
  
  # current vs previous year bars ####
  
  par(xpd= NA)
  if(plot.nb %in% c(1,2)) {
    lines(x = 1:9, y = rep(12.5, 9), col = "grey", lwd = 2)
    lines(x = 10:17, y = rep(12.5, 8), lwd = 2)
    text(x = 5, y = 12.5, labels = "previous year", col = "grey", pos = 3)
    text(x = 14, y = 12.5, labels = "current year", pos = 3)
  } else {
    lines(x = 1:9, y = rep(12.5, 9), col = "grey", lwd = 2)
    lines(x = 10:17, y = rep(12.5, 8), lwd = 2)
  }
  
  # "energy" vs "water"' variable group bars ###
  if(plot.nb %in% c(1)) {
    lines(x = rep(-4, 3), y = 1:3, lwd = 2)
    lines(x = rep(-4, 5), y = 6:10, lwd = 2)
    text(x = -4.4, y = 1.8, labels = "Water variables", pos = 3, srt = 90)
    text(x = -4.4, y = 7.7, labels = "Energy variables", pos = 3, srt = 90)
  }
  
  
  
  # add letter ####
  text(x = -1, y = 13, paste0(letters[plot.nb], ")"), font = 2)
  
  
} # for(type.start in type.of.start.date)

# legend ####
par(mar = c(0,0,0,0))
plot.new( )
plot.window( xlim=c(0,10), ylim=c(0,50) )

leg.unit <- 3
start.unit <- 5
right.pos <- 1
leg.width <- 2
values <- seq(-1, 1, length = 11)

neg.rescaled.values <- round(seq(-neg.max, 0, length = 6), 
                             2)
pos.rescaled.values <- rev(round(seq(pos.max, 0, length = 6), 
                                 2)[-6])
rescaled.values <- c(neg.rescaled.values, pos.rescaled.values)
rescaled.values <- as.vector(sapply(rescaled.values, function(x) bquote(.(x) ~ bold(" - ") ~ .(round(x*100/ANPP_contribution$ANPP_Mg.C.ha1.y1_10cm[ANPP_contribution$species %in% "total"], 2)))))

for (i in 1:11) {
  if (values[i] <= 0) {
    polygon(c(right.pos, right.pos + leg.width, right.pos + 
                leg.width, right.pos), c(start.unit + ((i - 1) * 
                                                         leg.unit), start.unit + ((i - 1) * leg.unit), 
                                         start.unit + (i * leg.unit), start.unit + (i * 
                                                                                      leg.unit)), col = rgb(reds(abs(values[i])), 
                                                                                                            maxColorValue = 255), lty = 0)
    text(right.pos + leg.width , start.unit + (i * 
                                                 leg.unit) - leg.unit/2, ifelse(rescale, as.expression(rescaled.values[i]), 
                                                                                values[i]), pos = 4)
  }
  else {
    polygon(c(right.pos, right.pos + leg.width, right.pos + 
                leg.width, right.pos), c(start.unit + ((i - 1) * 
                                                         leg.unit), start.unit + ((i - 1) * leg.unit), 
                                         start.unit + (i * leg.unit), start.unit + (i * 
                                                                                      leg.unit)), col = rgb(blues(values[i]), maxColorValue = 255), 
            lty = 0)
    text(right.pos + leg.width, start.unit + (i * 
                                                leg.unit) - leg.unit/2, ifelse(rescale, as.expression(rescaled.values[i]), 
                                                                               values[i]), pos = 4)
  }
} #  for (i in 1:11) 

text(x = 5, y = start.unit + (i * leg.unit) + 3, labels = expression(bold(atop("ANPP Response", Mg ~ ha^-1 ~ yr^-1 ~" - %"))))

# dev.off ####
if(save.plots) dev.off()




# Supplementary figures - [climate variable]_species responses ####
## see this issue: https://github.com/SCBI-ForestGEO/climate_sensitivity_cores/issues/43

method.to.run = "correlation"

type.of.start.date <-c("1901_2009", "1911_1943", "1944_1976", "1977_2009")

ANPP_contribution <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/summary_data/ANPP_total_and_by_species.csv"), header=T) # this URL might change because it is a private repository. If it does, update if by copying the URL direcltly from github: go to https://github.com/EcoClimLab/SCBI-ForestGEO-Data_private/master/SCBI_numbers_and_facts/ANPP_total_and_by_species.csv, click on Raw, copy the URL and paste it in place of the current URL here, inbetween the quotes of this line of code.
SPECIES_IN_ORDER <- toupper(ANPP_contribution$species[ ANPP_contribution$species %in% c("litu", "qual", "quru", "quve", "qupr", "fram", "cagl", "caco", "cato", "juni", "fagr", "caovl", "pist", "frni")])
SPECIES_IN_ORDER <- gsub("CAOVL", "CAOV", SPECIES_IN_ORDER)

# plot ####

for(v in c("pet", "dtr", "tmp", "tmn", "tmx", "cld", "pre", "vap", "wet", "PDSI_prewhiten", "PETminusPRE")) {
  
  print(v)
  
  if(save.plots)  {
    tiff(paste0("results/figures_for_manuscript/supplementary_figures/", v, "_species_responses.tif"), res = 150, width = 150, height = 140, units = "mm", pointsize = 10)
  }
  
  nf <- layout(mat = matrix(c(1,2,5,3,4,5), ncol = 3, byrow = T), widths = c(1,1,0.4))
  # layout.show(nf)
  
  plot.nb = 0
  
  for(type.start in type.of.start.date) {
    
    plot.nb <- plot.nb + 1
    
    if(v %in% "PDSI_prewhiten")  climate_data <- "NOAA_PDSI_Northern_Virginia_1895_2017" else climate_data <- "CRU_SCBI_1901_2016"
    all.dcc.output <- read.csv(paste0("results/", type.start, "/tables/monthly_", method.to.run, "/", method.to.run, ifelse(grepl("corr", method.to.run), "_with_", "_to_"), climate_data, "_climate_data.csv"), stringsAsFactors = F)
    
    
    X <- all.dcc.output[all.dcc.output$variable %in% v, ]
    
    x <- data.frame(reshape(X[, c("month", "Species", "coef")], idvar = "month", timevar = "Species", direction = "wide"))
    rownames(x) <- ifelse(grepl("curr",  x$month), toupper( x$month), tolower(  x$month))
    rownames(x) <- gsub(".*curr.|.*prev.", "",   rownames(x), ignore.case = T)
    
    x.sig <- reshape(X[, c("month", "Species", "significant")], idvar = "month", timevar = "Species", direction = "wide")
    x.sig2 <- reshape(X[, c("month", "Species", "significant2")], idvar = "month", timevar = "Species", direction = "wide")
    
    colnames(x) <- gsub("coef.", "", colnames(x))
    colnames(x.sig) <- gsub("significant.", "", colnames(x.sig))
    colnames(x.sig2) <- gsub("significant2.", "", colnames(x.sig2))
    
    x <- x[, -1]
    x.sig <- x.sig[, -1]
    x.sig2 <- x.sig2[, -1]
    
    if(length(names(x)) < length(SPECIES_IN_ORDER)) {
      
      missing_species <- SPECIES_IN_ORDER[!SPECIES_IN_ORDER %in% names(x)]

      missing_species_x <- data.frame(matrix(0, nrow = nrow(x), ncol = length(missing_species)))
      missing_species_x.sig <- data.frame(matrix(FALSE, nrow = nrow(x), ncol = length(missing_species)))
      
      colnames(missing_species_x) <- missing_species
      colnames(missing_species_x.sig) <- missing_species
      
      x <- cbind(x, missing_species_x)
      x.sig <- cbind(x.sig, missing_species_x.sig)
      x.sig2 <- cbind(x.sig, missing_species_x.sig)

      x <- x[, rev(SPECIES_IN_ORDER)]
      x.sig <- x.sig[, rev(SPECIES_IN_ORDER)]
      x.sig2 <- x.sig2[, rev(SPECIES_IN_ORDER)]
      
    } else {
      
      x <- x[, rev(SPECIES_IN_ORDER)]
      x.sig <- x.sig[, rev(SPECIES_IN_ORDER)]
      x.sig2 <- x.sig2[, rev(SPECIES_IN_ORDER)]
      
    }
    

    
    
    # plot (adapted my.dccplot function)
    x = as.data.frame(t(x))
    sig = as.data.frame(t(x.sig))
    sig2 = as.data.frame(t(x.sig2))
    main = gsub("_", "-", type.start)
    ylab = toupper(v) ; ylab = gsub("PETMINUSPRE", "PET-PRE", ylab)
    rescale = T
    
    if (!is.data.frame(x)) {
      x <- x$coef
    }
    
    blues <- colorRamp(c("#FFFFFF", "#4B9EF2", "blue4"))
    reds <- colorRamp(c("#FFFFFF", "#F25757", "red4"))
    
    m <- dim(x)[1]
    n <- dim(x)[2]
    
    pos.max <- 0.65 #max(x)
    neg.max <- 0.65 #abs(min(x))
    
    if(plot.nb %in% c(1)) par(oma = c(1.5, 4, 0, 0))
    if(plot.nb %in% c(1:4)) par(mar = c(0, 1.5, 6, 0))
    if(!plot.nb %in% c(1:4)) par(mar = c(0, 1.5, 4, 0))
    plot(c(0.5, n + 0.5), c(0.5, m + 0.5), type = "n", xaxt = "n", 
         yaxt = "n", ylab = "", xlab = "")
    
    # x-axis ####
    axis(side = 3, at = 1:n, labels = colnames(x), las = 2) # change here
    
    
    
    # y-axis ####
    if(plot.nb %in% 1) mtext(side = 3, ylab, line = 4, outer = F, adj = -0.2)
    
    
    if(plot.nb %in% c(1,3,5)) {
      axis(side = 2, at = 1:m, labels = rownames(x), las = 1)
    } else {
      axis(side = 2, at = 1:m, labels = FALSE, las = 1)
    } 
    
    # title ####
    if(plot.nb %in% c(1,2)) title(main, line = 5, outer = F)
    if(plot.nb %in% c(3,4)) title(main, line = 4, outer = F)
    
    
    # plot quilt ####
    X.left <- X.right <- Y.bottom <- Y.top <- x
    
    X.left[] <- rep((1:n - 0.5), each = m)
    X.right[] <- rep((1:n + 0.5), each = m)
    Y.bottom[] <- rep(1:m - 0.5, n)
    Y.top[] <- rep(1:m + 0.5, n)
    
    
    x.left <- unlist(c(X.left))
    x.right <- unlist(c(X.right))
    y.bottom <- unlist(c(Y.bottom))
    y.top <- unlist(c(Y.top))
    
    xs <- unlist(c(x))
    xs.sig <- unlist(c(sig))
    xs.sig2 <- unlist(c(sig2))
    
    color <- xs
    color[xs <= 0] <- rgb(reds(abs(xs[xs <= 0])/ neg.max), maxColorValue = 255)
    color[xs > 0] <- rgb(blues(xs[xs > 0]/ pos.max), maxColorValue = 255)
    
    rect(x.left, y.bottom , x.right, y.top, col = color, border = "white")
    
    points((x.left + x.right) /2 , (y.bottom + y.top) /2, bg = ifelse(xs.sig,  "white", "transparent"), col = ifelse(xs.sig, "black", "transparent"), pch = 21) 
    points((x.left + x.right) /2 , (y.bottom + y.top) /2, bg = ifelse(xs.sig2,  "white", "transparent"), col = ifelse(xs.sig2, "black", "transparent"), pch = 24) 
    
    
    
    
    
    # current vs previous year bars ####
    
    par(xpd= NA)
    if(plot.nb %in% c(1,2)) {
      lines(x = 1:9, y = rep(17.7, 9), col = "grey", lwd = 2)
      lines(x = 10:17, y = rep(17.7, 8), lwd = 2)
      text(x = 5, y = 17.7, labels = "previous year", col = "grey", pos = 3)
      text(x = 14, y = 17.7, labels = "current year", pos = 3)
    } else {
      lines(x = 1:9, y = rep(17.7, 9), col = "grey", lwd = 2)
      lines(x = 10:17, y = rep(17.7, 8), lwd = 2)
    }
    
    
    # add letter ####
    text(x = -1, y = 16, paste0(letters[plot.nb], ")"), font = 2)
  } #  for(type.start in type.of.start.date[c(1,3)])
  
  
  # legend ####
  par(mar = c(0,0,0,0))
  plot.new( )
  plot.window( xlim=c(0,10), ylim=c(0,100) )
  
  leg.unit <- 3.5
  start.unit <- 30
  right.pos <- 1
  leg.width <- 2
  values <- seq(-1, 1, length = 11)
  
  neg.rescaled.values <- round(seq(-neg.max, 0, length = 6), 
                               2)
  pos.rescaled.values <- rev(round(seq(pos.max, 0, length = 6), 
                                   2)[-6])
  rescaled.values <- c(neg.rescaled.values, pos.rescaled.values)
  
  for (i in 1:11) {
    if (values[i] <= 0) {
      polygon(c(right.pos, right.pos + leg.width, right.pos + 
                  leg.width, right.pos), c(start.unit + ((i - 1) * 
                                                           leg.unit), start.unit + ((i - 1) * leg.unit), 
                                           start.unit + (i * leg.unit), start.unit + (i * 
                                                                                        leg.unit)), col = rgb(reds(abs(values[i])), 
                                                                                                              maxColorValue = 255), lty = 0)
      text(right.pos + leg.width , start.unit + (i * 
                                                   leg.unit) - leg.unit/2, ifelse(rescale, rescaled.values[i], 
                                                                                  values[i]), pos = 4)
    }
    else {
      polygon(c(right.pos, right.pos + leg.width, right.pos + 
                  leg.width, right.pos), c(start.unit + ((i - 1) * 
                                                           leg.unit), start.unit + ((i - 1) * leg.unit), 
                                           start.unit + (i * leg.unit), start.unit + (i * 
                                                                                        leg.unit)), col = rgb(blues(values[i]), maxColorValue = 255), 
              lty = 0)
      text(right.pos + leg.width, start.unit + (i * 
                                                  leg.unit) - leg.unit/2, ifelse(rescale, rescaled.values[i], 
                                                                                 values[i]), pos = 4)
    }
  } #  for (i in 1:11) 
  
  text(x = 5, y = start.unit + (i * leg.unit) + 3, labels = "Correlation", font = 2)
  
  legend(x = 1, y = 25 , pch =  c(21, 24), bg = "white", col = "black", legend = c( "0.05", "0.0002"), bty = "n")
  text(x = 5, y = 26, labels = "Significance", font = 2)
  
  # dev.off ####
  if(save.plots) dev.off()
} # for(v in c("pet", "cld", "PETminusPRE"))




# Supplementary figures - [climate variable]_SUMMARY ####
mean_and_std_of_clim <- read.csv("results/climate/mean_and_std_of_climate_variables.csv")


if(save.plots)  {
  tiff(paste0("results/climate/climate_variables_monthy_means_for_both_time_periods.tif"), res = 150, width = 120, height = 150, units = "mm", pointsize = 10)
}

par(mfrow = c(5,2))
par(mar = c(1, 4, 0, 0), oma = c(3, 0, 1, 0))

plot.nb = 1

for(v in c("pre", "wet",
           "cld", "tmx",
           "tmp", "tmn",
           "dtr", "pet",
           "PETminusPRE", "PDSI")) {
  
  ylab <- toupper(v) ; ylab <- gsub("PETMINUSPRE", "PET-PRE", ylab)
  if(ylab %in% "PDSI_PREWHITEN") ylab <- bquote(PDSI^1)
  
  X.mean <- mean_and_std_of_clim[mean_and_std_of_clim$variable %in% v, grepl("mean", colnames(mean_and_std_of_clim))][, 1:12]
  X.sd <- mean_and_std_of_clim[mean_and_std_of_clim$variable %in% v, grepl("sd", colnames(mean_and_std_of_clim))][, 1:12]
  
  plot(x = 1:12, y = c(X.mean[1,]) , type = "l", xaxt = "n", xlab = "", ylab = "", bty = "L", ylim = range(min(X.mean) - max(X.sd), max(X.mean) + max(X.sd)), yaxt = "n", col = "blue")
  
  polygon(x = c(1:12, 12:1), y = c(X.mean[1,]-X.sd[1,], rev(X.mean[1,]+X.sd[1,])), col = rgb(0,0,0,0.1), border = F)
  polygon(x = c(1:12, 12:1), y = c(X.mean[2,]-X.sd[2,], rev(X.mean[2,]+X.sd[2,])), col = rgb(1,0,0,0.1), border = F)
  polygon(x = c(1:12, 12:1), y = c(X.mean[3,]-X.sd[3,], rev(X.mean[3,]+X.sd[3,])), col = rgb(0,0,1,0.1), border = F)
  polygon(x = c(1:12, 12:1), y = c(X.mean[4,]-X.sd[4,], rev(X.mean[4,]+X.sd[4,])), col = rgb(1,0.8,0,0.1), border = F)
  
  lines(x = 1:12, y = c(X.mean[1,]), col = rgb(0,0,0), lwd = 2)
  lines(x = 1:12, y = c(X.mean[2,]), col = rgb(1,0,0))
  lines(x = 1:12, y = c(X.mean[3,]), col = rgb(0,0,1))
  lines(x = 1:12, y = c(X.mean[4,]), col = rgb(1,0.8,0))
  
  
  # x-axis
  axis(1, at = 1:12, labels = F)
  
  if(plot.nb %in% c(9, 10)) {
    axis(1, at = 1:12, substr(month.abb,1,1))
    mtext(1, text = "month", line = 2.5, cex = 0.8)
  }
  
  # y-axis
  axis(2, las = 2)
  mtext(2, text = ylab, line = 2.5, cex = 0.8)
  
  # legend
  if(plot.nb %in% 2) {
    legend("bottomleft", lty = 1, lwd = c(2,1,1,1), 
           col = c(rgb(0,0,0),
                   rgb(1,0,0),
                   rgb(0,0,1),
                   rgb(1,0.8,0)),
           legend = c("1901-2009", "1911-1943", "1944-1976", "1977-2009"), bty = "n",
           ncol = 2) # "[1901-1938]-2009"
    legend("topright", fill = rgb(0,0,0,0.1), border = "transparent", c("+/- SD"), bty = "n", x.intersp = 0.5)
  }
  
  # titles
  
  mtext(side = 3, line = -1, text = paste0(letters[plot.nb], ")"), adj = 0.01, cex = 0.8)
  
  
  plot.nb = plot.nb +1
}


if(save.plots) dev.off()


# Supplementary figures - time series for each species ####
# see this issue: https://github.com/SCBI-ForestGEO/climate_sensitivity_cores/issues/59

filenames <- list.dirs("data/cores/", full.names = F, recursive = F  )
filenames <- filenames[!grepl("[a-z]", filenames)] # keep only all caps names

all_sss <- read.csv("results/SSS_as_a_function_of_the_number_of_trees_in_sample.csv")

sss.threshold = 0.75


colors.species <- colorRampPalette(c("purple", "cadetblue", "yellow", "darkorange", "red", "brown"))(14)
clim <- read.csv("data/climate/Formated_CRU_SCBI_1901_2016.csv")

clim <- clim[clim$month %in% c(5:7), c("year", "pre", "pet_sum")]
clim <- clim[clim$year <= 2009 & clim$year >= 1901,]

clim <- data.frame(apply(clim, 2, function(x) tapply(x, clim$year, mean)))

drought_years <- clim$year[which(c(clim$pet_sum - clim$pre) >= (sort(clim$pet_sum - clim$pre, decreasing = T)[10]))]

cbind(drought_years, c(clim$pet_sum - clim$pre)[which(c(clim$pet_sum - clim$pre) >= (sort(clim$pet_sum - clim$pre, decreasing = T)[10]))])

if(save.plots)  {
  tiff(paste0("results/Time_series_for_each_species.tif"), res = 150, width = 150, height = 150, units = "mm", pointsize = 10)
}

par(mfrow = c(14 + 2, 1), mar = c(0,0,0,0), oma = c(4, 6, 0, 0))


# pet_sum ####
plot(NULL,
     axes = F,
     ann = F, 
     xlim = c(1900,2020), ylim = c(100, 160))


abline(v = drought_years, col = "grey", lty = 2) # seq(1900, 2000, by = 20)

lines(pet_sum ~ year, data = clim, col  = "red", lwd = 2)
text(x = 2010, y = 130, "PET", pos = 4, col = "red")

axis(2, at = c(110, 130, 150), las = 1)
# mtext(side = 2, text = expression("(mm mo"^-1*")"), las = 1, cex = 0.7, line = 3)

# pre ####
plot(NULL,
     axes = F,
     ann = F, 
     xlim = c(1900,2020), ylim = c(40, 180))


abline(v = drought_years, col = "grey", lty = 2)

lines(pre ~ year, data = clim, col  = "blue",lwd = 2)

text(x = 2010, y = 100, "PRE", pos = 4, col = "blue")

axis(2, at = c(60, 110, 160), las = 1)

mtext(side = 2, text = expression("(mm mo"^-1*")"), line = 3, adj = 0)
axis(1, at = c(1700, 2020), labels = F, tck = 0, col = "grey60")

# chronologies ####
for(f in SPECIES_IN_ORDER) {
  
  
  if (f %in% "CAOV") f <- "CAOVL"
  
  # get the detrended data
  core <- read.table(paste0("data/cores/", f,"/ARSTANfiles/", tolower(f), "_drop.rwl_tabs.txt"), sep = "\t", h = T)
  
  years.with.enough.sss <- all_sss[all_sss$Species %in% f & all_sss$sss >= sss.threshold, ]$Year
  
  years.with.enough.sss <- years.with.enough.sss[years.with.enough.sss >=1901 & years.with.enough.sss <= 2009]

  core <- core[core$year %in% years.with.enough.sss, ] # trim to use only years for which with have clim data 

  if (f %in% "CAOVL") f <- "CAOV"
  
  plot(NULL,
       axes = F,
       ann = F, 
       xlim = c(1900,2020), ylim = c(0.5, 1.5))
  
  abline(v = drought_years, col = "grey", lty = 2)
  
  lines(res ~ year, data = core,
       col = colors.species[which(SPECIES_IN_ORDER %in% f)])
  axis(2, at = c(0.7, 1, 1.3), las = 1)
  text(x = 2010, y = 1, f, pos = 4, col = colors.species[which(SPECIES_IN_ORDER %in% f)])

}
axis(1)
mtext(side = 1, "Year", outer = T, line = 2.5)
mtext(side = 2, "Ring Width Index", outer = T, line = 4)

if(save.plots) dev.off()



# Supplementary figures - Correlation plots between species ####
# see this issue: https://github.com/SCBI-ForestGEO/climate_sensitivity_cores/issues/64

filenames <- list.dirs("data/cores/", full.names = F, recursive = F  )
filenames <- filenames[!grepl("[a-z]", filenames)] # keep only all caps names

if(save.plots)  {
  tiff(paste0("results/Correlation_plots_between_species.tif"), res = 150, width = 150, height = 150, units = "mm", pointsize = 10)
}

bigtable_of_res <- data.frame(year = 1901:2009)

for(f in SPECIES_IN_ORDER) {
  
  
  if (f %in% "CAOV") f <- "CAOVL"
  
  # get the detrended data
  core <- read.table(paste0("data/cores/", f,"/ARSTANfiles/", tolower(f), "_drop.rwl_tabs.txt"), sep = "\t", h = T)
  
  bigtable_of_res <- cbind(bigtable_of_res,data.frame( res =  core$res[match(bigtable_of_res$year, core$year)]))
  
  
  }

names(bigtable_of_res)[-1] <- SPECIES_IN_ORDER


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor.fac=.5, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r.sign <- sign(cor(x, y, use = "pairwise.complete.obs"))
  r <- cor(x, y, use = "pairwise.complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  cex.cor <- cex.cor.fac/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * abs(r) +1, col=ifelse(r.sign > 0, "blue", "red"))
}

pairs(bigtable_of_res[-1], upper.panel = panel.cor, lower.panel = NULL, yaxt = "n", xaxt = "n")

if(save.plots) dev.off()


mean(cor(bigtable_of_res[-1], use="pairwise.complete.obs")[upper.tri(cor(bigtable_of_res[-1], use="pairwise.complete.obs"))]) # 0.41
