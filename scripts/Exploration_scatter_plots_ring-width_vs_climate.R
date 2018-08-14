
save.plots <- TRUE


SPECIES_IN_ORDER <- toupper(c("litu", "qual", "quru", "quve", "qupr", "fram", "cagl", "caco", "cato", "juni", "fagr", "caov", "pist", "frni"))


climate.data.types <- "CRU_SCBI_1901_2014" # c("PRISM_SCBI_1930_2015_30second", "CRU_SCBI_1901_2014", "NOAA_PDSI_Northern_Virginia_1895_2017")

start.years <- c(1916, 1891, 1895, 1920, 1930, 1896, 1916, 1910, 1919, 1937, 
             1868, 1890, 1902, 1895)

overall.start.year <- 1937

end.year = 2009 


for(c in climate.data.types) {
  print(c)
  
  ## Load climate data ####
  
  clim <- read.csv(paste0("raw_data/climate/Formated_", c, ".csv"))
  
  ### crop first and last year of NOAA data because outliers
  if(c %in% "NOAA_PDSI_Northern_Virginia_1895_2017") {
    clim <- clim[!(clim$year %in% min(clim$year) | clim$year %in% max(clim$year)), ]
  }
  
  ### Pre_chiten PDSI of NOAA data because autocorrelated by definitiaon
  if(c %in% "NOAA_PDSI_Northern_Virginia_1895_2017") {
    clim$PDSI_prewhiten <- ar(clim$PDSI)$resid
    clim$PHDI_prewhiten <- ar(clim$PHDI)$resid
    clim$PMDI_prewhiten <- ar(clim$PMDI)$resid
  }
  
  ### remove climate variables we don't care about
  if(c %in% "NOAA_PDSI_Northern_Virginia_1895_2017") {
    clim  <- clim[, !(colnames(clim) %in% c("SP02", "SP03", "SP06", "SP09", "SP12", "SP24"))]
  }
  if(c %in% "NOAA_PDSI_Northern_Virginia_1895_2017") {
    clim  <- clim[, !(colnames(clim) %in% c("pet_sum"))]
  }


  for(f in toupper(SPECIES_IN_ORDER[c(1:3)])) {
    print(f)
    
    core <- get(f)
    
    core <- core[rownames(core) %in% clim$year, ] # trim to use only years for which with have clim data 
    
    start.year <- max(min(clim$year), start.years[which(filenames %in% f)])
    
    for (v in c("pet", "tmx")) { #names(clim)[-c(1:2)]) {
      print(v)
      
      for(mth in c("apr", "may", "jun", "jul", "aug")) {
        print(mth)
        mth.i <- which(tolower(month.abb) %in% mth)
        
        x.all.years <- core$res[row.names(core) >= start.year]
        y.all.years <- clim[clim$month %in% mth.i & clim$year >= start.year &  clim$year <= max(rownames(core)), v]
        
        x.later.years <-  core$res[row.names(core) >= overall.start.year]
        y.later.years <- clim[clim$month %in% mth.i & clim$year >= overall.start.year &  clim$year <= max(rownames(core)), v]
        
        
        if(save.plots) tiff(paste0("results/Exploration_scatter_plots_ring-width_vs_climate/", paste(f, v, mth, sep = "_"), ".tiff"), res = 100, width = 169, height = 169, units = "mm", pointsize = 12)

        plot(x.all.years, y.all.years, main = paste(f, v, mth, sep = "-"), xlab = "ring-width index", ylab = v)
        points(x.later.years, y.later.years, col = "grey")
    
        abline(lm(y.all.years ~ x.all.years))
        abline(lm(y.later.years ~ x.later.years), col = "grey")
        
        legend("topright", lty = 1, col = c("black", "grey"), legend = c(paste(start.year, end.year, sep = "-"), paste(overall.start.year, end.year, sep = "-")), bty = "n")
        
        if(save.plots) dev.off()
      } # for(mth in c("apr", "may", "jun", "jul", "aug"))) 
    } #  for (v in c("pet", "tmx")
    
  } # for(f in toupper(SPECIES_IN_ORDER[c(1:3)])) 
} # for(c in climate.data.types)