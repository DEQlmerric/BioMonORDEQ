# Authors: Lesley Merrick and Shannon Hubler

# SLH 11.16.23: modified original code to make this work in new BioMonORDEQ repository,
#               goal is to bring in new data, run through all steps, write back into final files


# objective:  1) Run the function "ref_gis.screen" from 'Fun_RefScreem.R' 
#             2) Take GIS screen metric outputs and classifies stations as GIS candidate reference sites, or trashed (highest disturbances)
#             3) write into Excel file, with each worksheet being a separate (yearly?) run.  THis then feeds into GE and/or One.Table





library(readxl)
library(tidyverse)
library(openxlsx)
# update file location based on the sites you are screening - this should be an export from the GIS tool 
# this file must have a column MlocID and sites should be in the stations database 
gis_mets <- read_excel("//deqlab1/GIS_LIBRARY/Reference_Site_Selection/Siletz33360_metrics.xlsx", 
                       sheet = "Siletz33360_metrics")



gis_mets <- gis_mets %>%
  select(OBJECTID, MLocID, Area_km2,rd_km,rdden_km_km2,total_strm_km,xings,xings_km2,
         canal_km,P_canal,mines,mines_km2,grvl_mn,grvl_mn_km2,Ag_km2,P_AgLand,Urban21_km2,P_Urban21Land)




# source function, run GIS metrics through the screening thresholds to establish "MOST DISTURBED" and or "GIS Candidates".
source('Reference/Fun_RefScreen.R')
ref_gis.screen(gis_mets)

#######
#######

#                 save file with date included = ref_screen.DEQ+DATE.csv

#######
#######

library(openxlsx)

today <- Sys.Date()

worksheet.name <- paste("ref_screen.DEQ", today)


# This overwrites, but keeps all data present in new file
filepath <- "Reference/ref_screen.DEQ.xlsx"  #change filepath to original xlsx filepath

wb <- loadWorkbook(filepath)  
addWorksheet(wb,worksheet.name) #change "sheet2" to "whatever-you-want-to-name-sheet"
writeData(wb, worksheet.name, ref_screen) #change "sheet2" to whatever you named new tab and df to whatever dataframe you want
saveWorkbook(wb,filepath, overwrite = TRUE)








