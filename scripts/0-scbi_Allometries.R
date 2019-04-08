# SCBI Biomass Code
##########################################################################
## x is a table with
## - a column called 'dbh' which has the dbh of all the stems in mm
## - a column called 'SPPCODE' with species 6-letter code of species in capital letter
x$agb_ctfs  <-  x$agb

x$order <- 1:nrow(x)

list.object.before <- ls()

# list of srubs species ####

shrub.DBH.sp <- c("beth", "chvi", "coam", "crpr", "elum", "eual", "ilve", "libe", "saca") # with DBH in AGB allometric equation

shrub.BD.sp <- c("havi", "loma", "romu","rual", "rupe", "ruph", "viac", "vipr", "vire") # with basal diameter in AGB allometric equation

# calculate AGB of trees ####


x.trees <- x[!x$sp %in% c(shrub.DBH.sp, shrub.BD.sp),]

x.trees$agb <- NA

x.trees$agb <- ifelse(x.trees$sp == "acne", exp(-2.047 + 2.3852 * log(x.trees$dbh * 0.1)), NA)

x.trees$agb <- ifelse(x.trees$sp == "acpl", exp(-2.047 + 2.3852 * log(x.trees$dbh * 0.1)), x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "acru", exp(4.5835 + 2.43 * log(x.trees$dbh * 0.1)) / 1000, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "acsp", exp(4.5835 + 2.43 * log(x.trees$dbh * 0.1)) / 1000, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "aial", exp(-2.48 + 2.4835 * log(x.trees$dbh * 0.1)) * 0.36, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "amar", exp(7.217 + 1.514 * log(x.trees$dbh * 0.1)) / 1000
                      + 10^(2.5368 + 1.3197 * log10(x.trees$dbh * 0.03937)) / 1000
                      + 10^(2.0865 + 0.9449 * log10(x.trees$dbh * 0.03937)) /1000,
                      x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "astr", exp(-2.48 + 2.4835 * log(x.trees$dbh * 0.1)) * 0.36, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "caca", exp(-2.48 + 2.4835 * log(x.trees$dbh * 0.1)) * 0.36, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "caco", (1.93378 * (x.trees$dbh * 0.03937)^2.6209) * 0.45359, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "cade", exp(-2.0705 + 2.441 * log(x.trees$dbh * 0.1)), x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "cagl", (1.93378 * (x.trees$dbh * 0.03937)^2.6209) * 0.45359, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "caovl", (1.93378 * (x.trees$dbh * 0.03937)^2.6209) * 0.45359, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "cato", (1.93378 * (x.trees$dbh * 0.03937)^2.6209) * 0.45359, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "casp", (1.93378 * (x.trees$dbh * 0.03937)^2.6209) * 0.45359, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "ceca", exp(-2.5095 + 2.5437 * log(x.trees$dbh * 0.1)), x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "ceoc", exp(-2.48 + 2.4835 * log(x.trees$dbh * 0.1)) * 0.36, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "coal",  (3.08355 * ((x.trees$dbh * 0.03937)^2)^1.1492) * 0.45359,  x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "cofl",  (3.08355 * ((x.trees$dbh * 0.03937)^2)^1.1492) * 0.45359, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "crpr", exp(-2.2118 + 2.4133 * log(x.trees$dbh * 0.1)), x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "crsp", exp(-2.2118 + 2.4133 * log(x.trees$dbh * 0.1)), x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "divi", exp(-2.48 + 2.4835 * log(x.trees$dbh * 0.1)) * 0.36, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "elum", exp(-2.48 + 2.4835 * log(x.trees$dbh * 0.1)), x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "eual", exp(-2.48 + 2.4835 * log(x.trees$dbh * 0.1)), x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "fagr", (2.0394 * (x.trees$dbh * 0.03937)^2.5715) * 0.45359, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "fram", (2.3626 * (x.trees$dbh * 0.03937)^2.4798) * 0.45359, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "frni", 0.1634 * (x.trees$dbh * 0.1)^2.348, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "frpe", 0.1634 * (x.trees$dbh * 0.1)^2.348, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "frsp",  0.1634 * (x.trees$dbh * 0.1)^2.348, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "juci", exp(-2.5095 + 2.5437 * log(x.trees$dbh * 0.1)), x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "juni", exp(-2.5095 + 2.5437 * log(x.trees$dbh * 0.1)), x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "juvi", 0.1632 * (x.trees$dbh * 0.1)^2.2454, x.trees$agb)

# x.trees$agb <- ifelse(x.trees$sp == "litu", (1.0259 * (x.trees$dbh * 0.03937)^2.7324) * 0.45359, x.trees$agb)
x.trees$agb <- ifelse(x.trees$sp == "litu", (10^(-1.236 + 2.635 * (log10(x.trees$dbh * 0.1)))) * 1.008, x.trees$agb) # new equation given by Erika on Tue 4/2/2019 11:57



# x.trees$agb <- ifelse(x.trees$sp == "nysy", (1.5416 * ((x.trees$dbh * 0.03937)^2)^1.2759) * 0.45359, x.trees$agb)
x.trees$agb <- ifelse(x.trees$sp == "nysy", exp(-2.2118 + 2.4133 * log(x.trees$dbh * 0.1)) , x.trees$agb)# new equation given by Erika on Tue 4/2/2019 11:57

x.trees$agb <- ifelse(x.trees$sp == "pato", exp(-2.48 + 2.4835 * log(x.trees$dbh * 0.1)) * 0.36, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "pipu", (exp(5.2831 + 2.0369 * log(x.trees$dbh * 0.1))) / 1000, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "pist", (exp(5.2831 + 2.0369 * log(x.trees$dbh * 0.1))) / 1000, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "pivi", 10^(1.83 + 2.464 * log10(x.trees$dbh * 0.1)) / 1000, x.trees$agb)

# x.trees$agb <- ifelse(x.trees$sp == "ploc", (2.4919 * ((x.trees$dbh * 0.03937)^2)^1.1888) *  0.45359, x.trees$agb)
x.trees$agb <- ifelse(x.trees$sp == "ploc" & (x.trees$dbh * 0.1) < 24, (1.57573 * ((x.trees$dbh * 0.03937)^2) ^ 1.29005) * 0.45359, x.trees$agb) # new equation given by Erika on Tue 4/2/2019 11:57
x.trees$agb <- ifelse(x.trees$sp == "ploc" & (x.trees$dbh * 0.1) >= 24, (2.51502 * ((x.trees$dbh * 0.03937)^2) ^ 1.19256) * 0.45359, x.trees$agb) # new equation given by Erika on Tue 4/2/2019 11:57


x.trees$agb <- ifelse(x.trees$sp == "prav", (1.8082 * (x.trees$dbh * 0.03937)^2.6174) * 0.45359, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "prpe", (1.8082 * (x.trees$dbh * 0.03937)^2.6174) * 0.45359, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "prse", (1.8082 * (x.trees$dbh * 0.03937)^2.6174) * 0.45359, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "prsp", (1.8082 * (x.trees$dbh * 0.03937)^2.6174) * 0.45359, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "qual", (1.5647 * (x.trees$dbh * 0.03937)^2.6887) * 0.45359, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "quco", (2.6574 * (x.trees$dbh * 0.03937)^2.4395) * 0.45359, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "qufa" & x.trees$dbh <= 26 * 25.4, (2.3025 * ((x.trees$dbh * 0.03937)^2)^1.2580) * 0.45359, x.trees$agb)
x.trees$agb <- ifelse(x.trees$sp == "qufa" & x.trees$dbh > 26 * 25.4,  (2.2373 * ((x.trees$dbh * 0.03937)^2)^1.2639) * 0.45359, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "qumi", (1.5509 * (x.trees$dbh * 0.03937)^2.7276) * 0.45359, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "qumu", (1.5509 * (x.trees$dbh * 0.03937)^2.7276) * 0.45359, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "qupr", (1.5509 * (x.trees$dbh * 0.03937)^2.7276) * 0.45359, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "quru", (2.4601 * (x.trees$dbh * 0.03937)^2.4572) * 0.45359, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "qusp", (1.5509 * (x.trees$dbh * 0.03937)^2.7276) * 0.45359, x.trees$agb)

# x.trees$agb <- ifelse(x.trees$sp == "quve", (2.1457 * (x.trees$dbh * 0.03937)^2.5030) * 0.45359, x.trees$agb)
x.trees$agb <- ifelse(x.trees$sp == "quve" & (x.trees$dbh * 0.1) < 30, exp(-0.34052 + 2.65803 * log(x.trees$dbh * 0.03937)), x.trees$agb)
x.trees$agb <- ifelse(x.trees$sp == "quve" & (x.trees$dbh * 0.1) >= 30, (10^(1.00005 + 2.10621 * (log10(x.trees$dbh * 0.03937)))) * 0.45359, x.trees$agb)

# x.trees$agb <- ifelse(x.trees$sp == "rops", (1.04649 * ((x.trees$dbh * 0.03937)^2)^1.37539) * 0.45359, x.trees$agb)
x.trees$agb <- ifelse(x.trees$sp == "rops", exp(-2.5095 + 2.5437 * log(x.trees$dbh * 0.1)), x.trees$agb)

# x.trees$agb <- ifelse(x.trees$sp == "saal", (10^(1.3539  + 1.3412 * log10(x.trees$dbh * 0.1)^2)) / 1000 * 1.004, x.trees$agb)
x.trees$agb <- ifelse(x.trees$sp == "saal", exp(-2.2118 + 2.4133 * log(x.trees$dbh * 0.1)) , x.trees$agb) # new equation given by Erika on Tue 4/2/2019 11:57


# x.trees$agb <- ifelse(x.trees$sp == "tiam", (1.4416 * (x.trees$dbh * 0.03937)^2.7324) * 0.45359, x.trees$agb)
x.trees$agb <- ifelse(x.trees$sp == "tiam" & (x.trees$dbh * 0.1) <= 10, exp(4.29 + 2.29 * log(x.trees$dbh * 0.1)) * 0.001, x.trees$agb)# new equation given by Erika on Wed 4/3/2019 15:13
x.trees$agb <- ifelse(x.trees$sp == "tiam" & (x.trees$dbh * 0.1) > 10 & (x.trees$dbh * 0.1) <= 27, 1.74995 * ((x.trees$dbh * 0.03937)^2)^1.19103 * 0.45359, x.trees$agb)# new equation given by Erika on Wed 4/3/2019 15:13
x.trees$agb <- ifelse(x.trees$sp == "tiam" & (x.trees$dbh * 0.1) > 27, 1.49368 * ((x.trees$dbh * 0.03937)^2)^1.22405 * 0.45359, x.trees$agb)# new equation given by Erika on Wed 4/3/2019 15:13

x.trees$agb <- ifelse(x.trees$sp == "ulam", (2.17565 * ((x.trees$dbh * 0.03937)^2)^1.2481) * 0.45359, x.trees$agb)

# x.trees$agb <- ifelse(x.trees$sp == "ulru", (2.04282 * ((x.trees$dbh * 0.03937)^2)^1.2546) * 0.45359, x.trees$agb)
# x.trees$agb <- ifelse(x.trees$sp == "ulru", (0.08248 * (x.trees$dbh * 0.03937)^2.468) * 0.45359, x.trees$agb)# new equation given by Erika on Wed 4/3/2019 15:13
x.trees$agb <- ifelse(x.trees$sp == "ulru", exp(-2.2118 + 2.4133*log(x.trees$dbh * 0.1)), x.trees$agb)# Chojnacky eq in allodb for ulmaceae, equation_id f08fff


x.trees$agb <- ifelse(x.trees$sp == "ulsp", (2.04282 * ((x.trees$dbh * 0.03937)^2)^1.2546) * 0.45359, x.trees$agb)

x.trees$agb <- ifelse(x.trees$sp == "unk", exp(-2.48 + 2.4835 * log(x.trees$dbh * 0.1)) * 0.36, x.trees$agb) # mixed hardwood


# put back the subsets together ####
x.all <- rbind(x.trees) #, x.shrub.DBH, x.shrub.BD)
x.all <- x.all[order(x.all$order),]
x.all$order <- NULL

if(!identical(x.all$dbh, x$dbh)) stop("order is not he same as the begining")
x <- x.all

#Convert from kg to Mg
x$agb <- x$agb / 1000 

#remove object we don't need anymore
rm(list = ls()[!ls() %in% list.object.before])

#Plot agb vs. dbh
# png(paste0(input.files.location,  "Allometries/",  site,  "_Allometries.png"))
# plot(x$dbh,  x$agb,  main = paste(site,  "Allomteries"))
# dev.off()
