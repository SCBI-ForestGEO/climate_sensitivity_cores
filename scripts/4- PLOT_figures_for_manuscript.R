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
library(RCurl)

source("scripts/0-My_dplR_functions.R")

# set parameters ####
save.plots <- TRUE
save.result.table <- TRUE


# ANPP_response_total for each type of starting year (not realy for manuscript as is) ####

## Define how to run it regarding the starting year ####
type.of.start.date <- c("Going_back_as_far_as_possible", "Going_back_to_1920", "Going_back_to_1980") # Going_back_at_earliest_common_year")


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
  
  # order by influence on ANPP (defined as predicted changes summed across all months).
  x <- x[, order(abs(apply(x, 2, sum)))]
  
  
  if(save.plots)  {
    dir.create(paste0("results/", type.start, "/figures/for_manuscript"), showWarnings = F)
    tiff(paste0("results/", type.start, "/figures/for_manuscript/ANPP_response.tif"), res = 300, width = 169, height = 140, units = "mm", pointsize = 10)
  }
  
  my.dccplot(x = as.data.frame(t(x)), sig = as.data.frame(t(x.sig)), sig2 = as.data.frame(t(x.sig2)), main = "", method = "response")
  
  if(save.plots) dev.off()
  
  
  
  
} # for(type.start in type.of.start.date)


# Figure 1 ####
## see this issue: https://github.com/EcoClimLab/climate_sensitivity_cores/issues/31

method.to.run = "correlation"
climate_data = "CRU_SCBI_1901_2016"

type.of.start.date <- c("Going_back_as_far_as_possible", "Going_back_to_1980") # Going_back_at_earliest_common_year")

ANPP_contribution <- read.csv(text=getURL("https://raw.githubusercontent.com/EcoClimLab/SCBI-ForestGEO-Data_private/master/SCBI_numbers_and_facts/ANPP_total_and_by_species.csv?token=ASwxIfr3Rxt2z2lxnOnKrUmaCTxdDBQcks5bpQfewA%3D%3D"), header=T)
SPECIES_IN_ORDER <- toupper(ANPP_contribution$species[ ANPP_contribution$species %in% c("litu", "qual", "quru", "quve", "qupr", "fram", "cagl", "caco", "cato", "juni", "fagr", "caovl", "pist", "frni")]) #toupper(c("litu", "qual", "quru", "quve", "qupr", "fram", "cagl", "caco", "cato", "juni", "fagr", "caov", "pist", "frni"))
SPECIES_IN_ORDER <- gsub("CAOVL", "CAOV", SPECIES_IN_ORDER)

# plot ####
if(save.plots)  {
  tiff("results/figures_for_manuscript/Figure_1.tif", res = 150, width = 150, height = 169, units = "mm", pointsize = 10)
}

nf <- layout(mat = matrix(c(1,2,7,3,4,7,5,6,7), ncol = 3, byrow = T), widths = c(1,1,0.4))
# layout.show(nf)

plot.nb = 0

for(v in c("pet", "cld", "deficit")) {
  
  print(v)
  for(type.start in type.of.start.date) {
    
    plot.nb <- plot.nb + 1
    
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
    
    
    # plot (adapted my.dccplot function)
    x = as.data.frame(t(x))
    sig = as.data.frame(t(x.sig))
    sig2 = as.data.frame(t(x.sig2))
    main = ifelse(grepl("1980", type.start), "1980-2009", "[1901-1938]-2009")
    ylab = toupper(v)
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
    
    # op <- par(no.readonly = TRUE)
    
    if(plot.nb %in% 1 ) par(oma = c(1.5, 4, 0, 0))
    if(plot.nb %in% c(1,2)) par(mar = c(0, 1.5, 6, 0))
    if(!plot.nb %in% c(1,2)) par(mar = c(0, 1.5, 4, 0))
    plot(c(0.5, n + 0.5), c(0.5, m + 0.5), type = "n", xaxt = "n", 
         yaxt = "n", ylab = "", xlab = "")
    
    # x-axis ####
    axis(side = 3, at = 1:n, labels = colnames(x), las = 2) # change here
    
    
    
    # y-axis ####
    if(plot.nb %in% c(1,3,5)) {
      axis(side = 2, at = 1:m, labels = rownames(x), las = 1)
      mtext(side = 2, ylab, line = 4)
    } else {
      axis(side = 2, at = 1:m, labels = FALSE, las = 1)
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
    
    points((x.left + x.right) /2 , (y.bottom + y.top) /2, bg = ifelse(xs.sig,  "white", "transparent"), col = ifelse(xs.sig, "black", "transparent"), pch = 21) 
    points((x.left + x.right) /2 , (y.bottom + y.top) /2, bg = ifelse(xs.sig2,  "white", "transparent"), col = ifelse(xs.sig2, "black", "transparent"), pch = 24) 
    
    
    
    
    
    # current vs previous year bars ####
    
    par(xpd= NA)
    if(plot.nb %in% c(1,2)) {
      lines(x = 1:9, y = rep(18.5, 9), col = "grey", lwd = 2)
      lines(x = 10:17, y = rep(18.5, 8), lwd = 2)
      text(x = 5, y = 18.5, labels = "previous year", col = "grey", pos = 3)
      text(x = 14, y = 18.5, labels = "current year", pos = 3)
    } else {
      lines(x = 1:9, y = rep(18, 9), col = "grey", lwd = 2)
      lines(x = 10:17, y = rep(18, 8), lwd = 2)
    }
    
    
    # add letter ####
    text(x = -1, y = 18, paste0(letters[plot.nb], ")"), font = 2)
  } #  for(type.start in type.of.start.date[c(1,3)])
} # for(v in c("pet", "cld", "deficit"))


# legend ####
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

# dev.off ####
if(save.plots) dev.off()
