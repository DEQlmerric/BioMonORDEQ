# Authors: Lesley Merrick and Shannon Hubler

# SLH 11.16.23: modified original code to make this work in new BioMonORDEQ repository,
#               goal is to bring in new data, run through all steps, write back into final files


# objective:  1) Run the function "ref_gis.screen" from 'Fun_RefScreem.R' 
#             2) Take GIS screen metric outputs and classifies stations as GIS candidate reference sites, or trashed (highest disturbances)
#             3) write into Excel file, with each worksheet being a separate (yearly?) run.  THis then feeds into GE and/or One.Table





library(readxl)

# update file location based on the sites you are screening - this should be an export from the GIS tool 
# this file must have a column MlocID and sites should be in the stations database 
gis_mets <- read_excel("//deqlab1/GIS_LIBRARY/Reference_Site_Selection/Siletz33360_metrics.xlsx", 
                       sheet = "Siletz33360_metrics")


gis_mets <- gis_mets %>%
  select(OBJECTID,MLocID,Area_km2,rd_km,rdden_km_km2,total_strm_km,xings,xings_km2,canal_km,P_canal,mines,mines_km2,grvl_mn,grvl_mn_km2,Ag_km2,P_AgLand,Urban21_km2,P_Urban21Land)


source('Reference/Fun_RefScreen.R')
ref_gis.screen(gis_mets)


# save file with date included = ref_screen.DEQ+DATE.csv
write.csv(ref_screen, "Reference/ref_screen.DEQ_1.4.22.csv")



#############

#  USU sites

############



matt <- read_excel("//deqlab1/GIS_LIBRARY/Reference_Site_Selection/Ref_Screen_Metrics_USU_Matt.xlsx", 
                       sheet = "RefScreen_Matt")


adam <- read_excel("//deqlab1/GIS_LIBRARY/Reference_Site_Selection/Ref_Screen_Metrics_USU_Adam.xlsx", 
                   sheet = "Ref_Screen_Metrics_USU_Adam")


gis_mets_USU <- matt %>%
  rbind(adam) 
gis_mets_USU<- rename(gis_mets_USU, EcoRegion3 = US_L3CODE)


source('Reference/Fun_RefScreen_Non-DEQ.R')


ref_gis.screen.nonDEQ(gis_mets_USU)

