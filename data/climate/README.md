# Data/Climate folder

The data in this folder is automatically created by [0-Prepare_Climate_Data.R](https://github.com/SCBI-ForestGEO/climate_sensitivity_cores/blob/master/scripts/0-Prepare_Climate_Data.R)

**!!DO NOT EDIT THE DATA HERE!!**
# Data Origin

Climate data were originally obtained from the ForestGEO Climate Data Portal (https://github.com/forestgeo/Climate), including
- Climatic Research Unit monthly data for eight variables from 1901-2016 ([CRU TS v. 4.01](https://github.com/forestgeo/Climate/tree/master/Gridded_Data_Products/Historical%20Climate%20Data/CRU_v4_01); Harris, Jones, Osborn, & Lister, 2014) and
- NOAA Divisional Data’s Palmer Drought Severity Index (PDSI, see [here](https://github.com/forestgeo/Climate/blob/master/Gridded_Data_Products/NOAA%20Divisional%20Data%20(USA)/Virginia_04_Northern_Virginia/NOAA_Divisional_data_(N_VA)_through_11_2017.csv)) from 1895-2017. 

# Data Formating
Original data was simply converted into a format compatible with 'dplR' R package:
- First column is the year
- Second column is the month
- All other columns are the climate variables

Variable _PETminusPRE = pet_sum - pre_ was calculated and added into CRU dataset.
