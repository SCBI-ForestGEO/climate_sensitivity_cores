
all_identical <- NULL
for(f in filenames[-13]) {
  print(f)
  sp <- toupper(substr(f, 1, 4))
  sp_dropbox <- read.rwl(paste0('C:/Users/HerrmannV/Dropbox (Smithsonian)/climate sensitivity/results/z_FinalChronologies/', sp, '/', f))
  sp_core <- read.rwl(paste0('C:/Users/HerrmannV/Dropbox (Smithsonian)/GitHub/SCBI-ForestGEO-Data_private/tree_cores/chronologies/current_chronologies/complete/', f))
  sp_raw <- read.rwl(paste0('raw_data/cores/', f))
  sp_data <- read.rwl(paste0('data/', ifelse(sp %in% "CAOV", "CAOVL", sp), '/', f))
  all_identical <- rbind(all_identical, data.frame(sp = sp, identical = all(c(identical(sp_dropbox, sp_core), identical(sp_dropbox, sp_raw),  identical(sp_dropbox, sp_data)))))
  
}

all_identical

f = filenames[2]
  sp <- toupper(substr(f, 1, 4))
  sp_dropbox <- read.rwl(paste0('C:/Users/HerrmannV/Dropbox (Smithsonian)/climate sensitivity/results/z_FinalChronologies/', sp, '/', f))
  sp_core <- read.rwl(paste0('C:/Users/HerrmannV/Dropbox (Smithsonian)/GitHub/SCBI-ForestGEO-Data_private/tree_cores/chronologies/current_chronologies/complete/', f))
  sp_raw <- read.rwl(paste0('raw_data/cores/', f))
  sp_data <- read.rwl(paste0('data/', ifelse(sp %in% "CAOV", "CAOVL", sp), '/', f))
  sp_raw <- read.rwl(paste0('C:/Users/HerrmannV/Dropbox (Smithsonian)/GitHub/', f))
  all_identical <- rbind(all_identical, data.frame(sp = sp, identical = all(c(identical(sp_dropbox, sp_core), identical(sp_dropbox, sp_raw),  identical(sp_dropbox, sp_data)))))
  
dim(sp_dropbox)
dim(sp_core)
dim(sp_raw)
dim(sp_data)
colnames(sp_dropbox)[!colnames(sp_dropbox) %in% colnames(sp_core)]
colnames(sp_raw)[!colnames(sp_raw) %in% colnames(sp_core)]


# CAOVL
f = filenames[3]
sp <- toupper(substr(f, 1, 4))
sp_dropbox <- read.rwl(paste0('C:/Users/HerrmannV/Dropbox (Smithsonian)/climate sensitivity/results/z_FinalChronologies/', sp, '/', f))
sp_core <- read.rwl(paste0('C:/Users/HerrmannV/Dropbox (Smithsonian)/GitHub/SCBI-ForestGEO-Data_private/tree_cores/chronologies/current_chronologies/complete/', f))
sp_raw <- read.rwl(paste0('raw_data/cores/', f))
sp_data <- read.rwl(paste0('data/', ifelse(sp %in% "CAOV", "CAOVL", sp), '/', f))

trees_censused_live <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data_private/master/tree_cores/measurement_files/measurement_notes_2010_chronology.csv?token=ASwxIfdeBgR-A0zbpC_2MwI0iY7o6UBmks5cLmO4wA%3D%3D"), header = T)
trees_censused_dead <- read.csv(text=getURL("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data_private/master/tree_cores/measurement_files/measurement_notes_2016_17_chronology.csv?token=ASwxIXP2rDj4rscIkWDZe_CjWZk2Z4P8ks5cLmPSwA%3D%3D"), header = T)

tag <- colnames(sp_data)
tag <- sub("[a-z]{1,}", "", tag, ignore.case = T) # remove last letter
tag <- sub("^0", "", tag ) # remove first zero if any

tag %in% c(trees_censused_live$Tag, trees_censused_dead$Tag)
