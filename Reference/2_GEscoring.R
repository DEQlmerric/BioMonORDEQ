# Author: Shannon Hubler
  
# Google Earth Reference Screens: FINAL Scoring (for DEQ sites)
        
        # 1) bring in GE Screen results.
        # 2) Re-format for calculations
        # 3) Calculate final site sum scores

library(reshape2)
require(tidyverse)






# bring in data
screens <-  read_excel("Reference/GE_screens.xlsx", 
                                  sheet = "GE.screens_2023-11-17")
      # merge multiple sets of GE screens together, if necessary



# use reshape2 to make df go from wide to long (fewer columns)


screens_long <- melt(screens,
        # ID variables - all the variables to keep but not split apart on
    id.vars=c("Agency_ID", "Sample.date", "Type", "Scorer", "Timestamp"),
        # The source columns
    measure.vars=c("ag_crops", "ag_cafo",	"ag_grazing",	"ag_stockponds",	
      "ag_orchardplantation",	"built_impermeable",	"built_permeable",
      "built_residence",	"built_sewagetreatment",	"log_under5",	"log_6toMA",
      "log_overMA",	"log_landslide",	"roads",	"railroads",	"powerlines",
      "rec_hiking",	"rec_campground",	"rec_parks",	"rec_hatchery",	"hydro_dam",
      "hydro_channelization",	"hydro_riprapdikes"),
        # Name of the destination column that will identify the original
        # column that the measurement came from
    variable.name="disturbance", 
    value.name="value"
)

     

# long format does a good job, but not sure how to calculate across two rows
# what if cast back to wide format, but only by putting Extent and Proximity as columns?
# then concatenate E & P text strings into a new column,
# followed by ifelse statements for text strings: AA = 0, AL=1, AM=3, etc...

screens_wide <- dcast(screens_long, Agency_ID + Sample.date + Scorer + disturbance + Timestamp ~ Type, value.var=unique("value"))
# if get error meassage 'defaulting to length' then need to fix non-unique records across grouping variables


 
screens_wide <- screens_wide %>%
  mutate(E.P = case_when(
    E == 'A' & P == 'A' ~ 0,
    E == 'A' & P == 'L' ~ 1,
    E == 'A' & P == 'M' ~ 3,
    E == 'A' & P == 'H' ~ 10,
    E == 'L' & P == 'A' ~ 1,
    E == 'L' & P == 'L' ~ 1,
    E == 'L' & P == 'M' ~ 3,
    E == 'L' & P == 'H' ~ 10,
    E == 'M' & P == 'A' ~ 3,
    E == 'M' & P == 'L' ~ 3,
    E == 'M' & P == 'M' ~ 10,
    E == 'M' & P == 'H' ~ 10,
    E == 'H' & P == 'A' ~ 10,
    E == 'H' & P == 'L' ~ 10,
    E == 'H' & P == 'M' ~ 10,
    E == 'H' & P == 'H' ~ 10,
     TRUE ~ 999)) 




# control for max of 3 for hiking stress               
screens_wide <- screens_wide %>%        
    mutate(E.P_adj.hike = case_when(disturbance == 'rec_hiking' & E.P == 10 ~ 3, 
                                TRUE ~ E.P))    
 
 
 
 
         

#summarize the data

GE_Site_sum.scores <- screens_wide %>%
  #define what makes each group unique
  group_by(Agency_ID, Sample.date, Scorer, Timestamp ) %>%
  #summarise the stats per group
  summarize(Disturb.score = sum(E.P_adj.hike))
  
# get station and BPJ alone, with distinct values
screen.bpj <- screens %>%
  select(Agency_ID, Sample.date, Scorer, Timestamp, Scorer_BPJ)

screen.bpj <- distinct(screen.bpj) # remove duplicates


# merge BPJ into scores           
GE_Site_sum.scores <- GE_Site_sum.scores  %>% 
   left_join(screen.bpj, by = c('Agency_ID', 'Sample.date', 'Scorer', 'Timestamp' )) 




                              ##########
                              ##########
                              ##
                              # visualize and explore the final scores
                              ##
                              #########
                              ##########
                              
                              hist(GE_Site_sum.scores$Disturb.score, breaks =150, xlim=c(0,100))
                              
                              ggplot(GE_Site_sum.scores, aes(Disturb.score)) +
                                geom_freqpoly(bins = 50) + xlim(0,25) +
                                facet_wrap(~Scorer_BPJ)
                              
                              ggplot(GE_Site_sum.scores, aes(x=Scorer_BPJ, y = Disturb.score)) +
                                geom_boxplot() + ylim(0,50) +
                                facet_wrap(~Scorer)
                              
                              ggplot(GE_Site_sum.scores, aes(x=Scorer, y = Disturb.score)) +
                                geom_boxplot() + ylim(0,50) +
                                facet_wrap(~Scorer_BPJ)
                              
                              # make a table of scores by BPJ category
                              with(GE_Site_sum.scores, table(Disturb.score, Scorer_BPJ)) 
                              
                              




# pull out BPJ Y and dist score >15 and BPJ N and dist score <15
# these outputs are then taken to the Reference Council for final BPJ review
bpj_y_above<-subset(GE_Site_sum.scores, Scorer_BPJ=="Y" & Disturb.score >= 15)

bpj_n_below<-subset(GE_Site_sum.scores, Scorer_BPJ=="N" & Disturb.score < 15)

bpj_question<-subset(GE_Site_sum.scores, Scorer_BPJ=="?" )



#########
#
#
#    Combine Scores + BPJ for FINAL REF STATUS
#
#
#########

# first, get single scores for a site on any given sample date 
GE_Site_sum.scores_ave <- GE_Site_sum.scores %>%
  group_by(Agency_ID, Sample.date) %>%    #define what makes each group unique
  summarize(mean(Disturb.score))          #summarise the stats per group



 # check for duplicates
GE_Site_sum.scores_ave_repeats<- GE_Site_sum.scores_ave%>%
 group_by(Agency_ID)%>%
  filter(n()>1) 

#
  # visually look at each set of duplicates, for each station
  # if there are no significant changes in scores, thru time, then simply average across all dates
  # if there are significant changes in scores, it will require dropping certain dates from the ref pool
#

# second, no significant score changes observed, average all samples per station
GE_Site_sum.scores_ave <- GE_Site_sum.scores %>%
  group_by(Agency_ID) %>%                            #define what makes each group unique
  summarize(Disturb.score = mean(Disturb.score))     #summarise the stats per group


########
########
        ###
          ####  Bring in Final BPJ designations
        ###
########
########

# combine single disturb scores per station, with final BPJ status
bpj.final <- read.xlsx('Reference/FINAL_BPJ.xlsx', sheet = 'FINAL_BPJ_2023-11-17')

bpj.final <- bpj.final%>%
  select(Agency_ID, BPJ_final)

GE_Site_sum.scores_ave_bpj <- GE_Site_sum.scores_ave  %>% 
   left_join(bpj.final, by = c('Agency_ID')) 



GE_Site_sum.scores_ave_bpj[c("BPJ_final")][is.na(GE_Site_sum.scores_ave_bpj[c("BPJ_final")])] <- "Score"

GE_Site_sum.scores_ave_bpj$BPJ_final<-as.character(GE_Site_sum.scores_ave_bpj$BPJ_final)

GE_Site_sum.scores_ave_bpj <- GE_Site_sum.scores_ave_bpj  %>% 
   mutate(Ref2020_FINAL = case_when(BPJ_final == 'Score' & Disturb.score < 15 ~ 'REFERENCE',
                                   BPJ_final == 'Score' & Disturb.score >= 15 ~ 'NO',
                                   BPJ_final == 'Y'  ~ 'REFERENCE',
                                   BPJ_final == 'N'  ~ 'NO',
                                   TRUE ~ 'WhatchuTalkinBoutWillis'))


GE_Site_sum.scores_ave_bpj$Ref2020_FINAL # look for "WhatchuTalkinBoutWillis--if any, will need to rectify


# change Agency_ID to 'MLocID' to match with stations table
colnames(GE_Site_sum.scores_ave_bpj)[which(names(GE_Site_sum.scores_ave_bpj) == "Agency_ID")] <- "MLocID"



##############

                # join GE and station info together

##############


# bring in station level data                                    
require(RODBC)

#connect to view as a general user 
sta.sql = odbcConnect('Stations')
#pull in stations table
stations = sqlFetch(sta.sql, "VWStationsFinal") 
odbcClose(sta.sql)

# cut down to essential columns
stations <- stations %>%
  select(MLocID, StationDes, Lat_DD, Long_DD, EcoRegion3)



# merge with stations
GE_Site_sum.scores_ave_bpj <- GE_Site_sum.scores_ave_bpj %>%
  left_join(stations, by = 'MLocID')

                # SLH 11.17.23: I don't think we need to keep this piece.  Remove later if proved unnecessary 

                                                        ref2020.sites_FINAL <- GE_Site_sum.scores_ave_bpj[GE_Site_sum.scores_ave_bpj$Ref2020_FINAL=='YES',]
                                                        
                                                        
                                                        
                                                        write.csv(ref2020.sites_FINAL, '//deqlab1/GIS_WA/Project_Working_Folders/Reference/2020/_Final outputs/REF.sites.only_2020_FINAL_DEQ.csv')
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        # create an exportable table of ref sites by ecoregion, save in same location as 'ref2020.sites_FINAL'
                                                        
                                                        
                                                        ref2020.sites_by_eco <- with(GE_Site_sum.scores_ave_bpj, table(Ref2020_FINAL, EcoRegion3))
                                                        write.csv(ref2020.sites_by_eco, '//deqlab1/GIS_WA/Project_Working_Folders/Reference/2020/_Final outputs/REF.2020_FINAL_deq_by_ecoregion.csv')

                                                        
                                                        
                                                        
#####                                                        

#         export GE_Site_sum.scores_ave_bpj  --> this needs to be linked to GIS screen results (in "final ref tables.R')

#####
                                                        
#write.csv(GE_Site_sum.scores_ave_bpj, 'Reference/GE_Site_sum.scores_ave_bpj_DEQ.csv')

#######
#######

#                 save file with date included = ref_screen.DEQ+DATE.csv

#######
#######

library(openxlsx)

today <- Sys.Date()

worksheet.name <- paste("GE_final.bpj_", today)


# This overwrites, but keeps all data present in new file
filepath <- "Reference/GE_Site_sum.scores_ave_bpj.xlsx"  #change filepath to original xlsx filepath

wb <- loadWorkbook(filepath)  
addWorksheet(wb,worksheet.name) #change "sheet2" to "whatever-you-want-to-name-sheet"
writeData(wb, worksheet.name, GE_Site_sum.scores_ave_bpj) #change "sheet2" to whatever you named new tab and df to whatever dataframe you want
saveWorkbook(wb,filepath, overwrite = TRUE)



