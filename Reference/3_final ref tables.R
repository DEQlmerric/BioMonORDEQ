# Author: Shannon Hubler
# Initial date: 1/14/21
#
#
# Purpose: Bring together GIS and GE screening results into a single output file

# Do this for both DEQ and USU sites.  
# My initial thought was to bring all sites together (DEQ = 2514, USU = 929) into a single data frame/file
# But the GIS outputs for both sets of sites have different column names and dimensions (DEQ ~ 30, USU ~ 60)



# SLH 11.17.23: modify to pnly bring in new runs of GIS and GE screens, append only those records to an existing One.Table file

library(tidyverse)
library(data.table)
library(openxlsx)

# bring in GIS outputs

gis.screens <- read.xlsx('Reference/ref_screen.DEQ.xlsx', sheet = 'ref_screen.DEQ 2023-11-20')


####### bring in Google Earth screens outputs
### DEQ
ge.screens <- read.xlsx('Reference/GE_Site_sum.scores_ave_bpj.xlsx', sheet = 'GE_final.bpj_ 2023-11-20')

# add an ownership column
ge.screens <- ge.screens %>%
  mutate(owner = 'DEQ')




##### merge GIS and Ref


ge.screens_lim <- ge.screens %>%
  select(MLocID, Disturb.score, BPJ_final, Ref2020_FINAL)

gis.ge <- gis.screens %>%
  full_join(ge.screens_lim, by = c('MLocID'))

########### change final ref status field, based on conditional status in multiple columns
# 
gis.ge$Ref2020_FINAL <- ifelse(is.na(gis.ge$Ref2020_FINAL), 
                            gis.ge$GIS.status_2020, gis.ge$Ref2020_FINAL)


                                              # Do we need this?  if not, delete
                                                              write.csv(gis.ge_deq, '//deqlab1/GIS_WA/Project_WOrking_Folders/Reference/2020/_Final outputs/FINAL.reference.2020_gis.ge_DEQ.csv',
                                                                        row.names = FALSE)



########

#                       One Table To Rule Them All

########                                                              
today <- as.character.POSIXt(Sys.Date())                                                              
                                                              
d <- gis.ge %>%
  select(MLocID, StationDes, Lat_DD, Long_DD, Eco3, COMID, rdden_km_km2, xings_km2, P_AgLand, P_Urban21Land, mines, grvl_mn_km2, P_canal, 
         rd_Status, xing_Status, Ag_Status, Urb21L_Status, mines_status, gmines_status, canal_status, GIS.status_2020, WorE, Disturb.score,
         BPJ_final, Ref2020_FINAL)
d <-  d %>%
  mutate(owner ='DEQ') %>%
  mutate(date.stamp = today)


# read in the existing One.Table

one.table <- read.csv("Reference/one.table_rule.all.csv")

one.table_new <- rbind(one.table, d)


write.csv(one.table_new, 'Reference/one.table_rule.all.csv', row.names=FALSE)



                                      # already created this file.  Not necessary to repeat, but save here in case it needs to be done again, or modified in the future.

                                                                                        # Create One Summary table, covering site info + GIS screens + GE screens + FINAL REF
                                                                                        one.table_rule.all <- rbind(d, u)
                                                                                        
                                                                                        write.csv(one.table_rule.all, 'Reference/one.table_rule.all.csv', row.names=FALSE)
                                                                                        write.csv(one.table_rule.all, '//deqlab1/GIS_WA/Project_WOrking_Folders/Reference/2020/_Final outputs/one.table_rule.all.csv', row.names = FALSE)
                                                                                        
                                                                                        
                                                                                        # create a data dictionary for one.table
                                                                                        variables <- colnames(one.table_rule.all)
                                                                                        
                                                                                        definitions <- c('unique station identifier_DEQ stations table', 'station descriptive name', 'latitude_NAD83 decimal degrees', 'longitude_NAD83 decimal degrees',
                                                                                                         'Level3 Omernik Ecoregion_abbreviation', 'Common identifier of an NHD Flowline', 'road density per watershed area', 'xings per watershed area',
                                                                                                         '% Ag land use', '% of watershed assigned to Code21 landuse from NLCD', 'of mines in watershed', 'gravel mines per watershed area', '% of watershed with canals', 'Roads GIS status: 1 = Candidate ref 2 = Trashed 0 = Neither',
                                                                                                         'Road crossings GIS status', 'Ag GIS status', 'Code21 GIS status', 'Mines GIS status', 'Gravel Mines GIS status', 'Canals GIS status',
                                                                                                         'Overall GIS screen status--across all 7 GIS metrics', 'East or West of Cascades crest', 'Overall score from Google Earth Screens', 
                                                                                                         'Best professional judgement call from Reference Council', 'FINAL Reference status: combines GIS + GE + BPJ', 'Organization that provided the station'
                                                                                                        )
                                                                                        
                                                                                        
                                                                                        one.table_meta.data<-as.data.frame(cbind(variables, definitions))
                                                                                        write.csv(one.table_meta.data, 'Reference/one.table_meta.data.csv')
                                                                                        write.csv(one.table_meta.data, '//deqlab1/GIS_WA/Project_WOrking_Folders/Reference/2020/_Final outputs/one.table_meta.data.csv')



############# 
#
# # #  HUMAN DISTURBANCE INDEX
#
############

hdi <- read_excel('Reference/FC_HDI Checklist_main.xlsx',
           sheet='Sheet1')

require(RODBC)

#connect to view as a general user 
sta.sql = odbcConnect('Stations')
#pull in stations table
stations = sqlFetch(sta.sql, "VWStationsFinal") 
odbcClose(sta.sql)

stations <- stations %>%
  select(station_key, MLocID)
stations$station_key <- as.character(stations$station_key)

hdi <- hdi %>%
  left_join(stations, by=c('STATION_KEY' = 'station_key'))


hdi<- hdi %>%
  select(MLocID, LOC_NAME, DATE, Crew, AgUrb_score, Range_score, Roads_score, Silvicult_score, Misc_score, HDIreach, Ref_Site, RefSITE_COMM, 'BPJ Grade')



one_hdi <- one.table_rule.all %>%
  left_join(hdi, by=c('MLocId' = 'MLocID'))


write.csv(one_hdi, 'Reference/one.table____hdi.csv')
