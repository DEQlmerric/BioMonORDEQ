
# Author: Shannon Hubler, 7/30/25

# Objective: Develop reference expectations for MTTI and BSTI stressor indexes
  # 1) pull in stressor scores for all sites
  # 2) limit to ref sites only
          # -------->> should these be the same samples/sites used in OE/MMI construction?
  # 3) pull StreamCAT data for ref sites--natural gradients/factors only
  # 4) RandomForest modeling to find best predictors for indexes 
          # -------->> and get site-specific expectations for indexes
  # 5) determine thresholds 
          # to mark when deviation in Obs vs Pred index values represents departure from reference conditions


library(openxlsx)
library(StreamCatTools)

#----------------------------------------------------------------------------------------------
# read in summary index results
sum.bugs <- read.xlsx("bugs analyses/stressor/reference expectations/biocriteria_scores2025-07-30.xlsx")

# read in ref bugs used in O/E and MMI models
ref_257 <- read.xlsx('bugs analyses/RIVPACS_2022/_2024 model build/Reference sites_bug samples_total and OTU abundances.xlsx',
                     sheet = 'FINAL FINAL FINAL_265 ref samps')  

ref_257 <- ref_257 %>%
  filter(Use_spatial=='Y') %>%
  rename(MLocID = Name, StaDes = Description)

# create a list of sampleIDs to use in querying bug data

sampleIDs.ref257 <- as.vector(ref_257$act_id)

# limit sum bugs to Ref sample ids

sum.bugs_ref257 <- sum.bugs %>% filter (act_id %in% sampleIDs.ref257)



#---------------------------------------------------------------------------------------------

# STREAMCAT

# import the stream cat metrics, along with SLH's use for modeling classification
streamcat_mets.defs <- read.csv('bugs analyses/stressor/reference expectations/streamcat_variable.info.csv')

kitty.mets_use <- streamcat_mets.defs %>%
  filter(SLH_use.modeling == 'Y')

kitty.mets_use_met.names <- as.vector(kitty.mets_use$metric_to_send)



###### THINGS TO THINK ABOUT HERE ##########################################################################
### Temporary until we figure out what years ###
kitty.mets_use_met.names <- kitty.mets_use_met.names[!grepl("Year", kitty.mets_use_met.names)]


# These don't seem to be valid metrics: SOLVED BELOW
# Precip_Minus_EVT, Rckdep
# kitty.mets_use_met.names <- kitty.mets_use_met.names[!grepl("Precip_Minus_EVT", kitty.mets_use_met.names)]
# kitty.mets_use_met.names <- kitty.mets_use_met.names[!grepl("Rckdep", kitty.mets_use_met.names)]

#################################################################################################################


kitty.mets_use_met.names <- str_replace(kitty.mets_use_met.names, "Precip_Minus_EVT", "precipminusevt")

kitty.mets_use_met.names <- str_replace(kitty.mets_use_met.names, "Rckdep", "rckdep")
#area doesn't come through anymore. We get this later if we include showAreaSqKm = TRUE in sc_get_data
kitty.mets_use_met.names <-  kitty.mets_use_met.names[!grepl("area", kitty.mets_use_met.names)]



metrics_together <- str_c(kitty.mets_use_met.names, sep = "", collapse = ",")

# comids.together <- str_c(sum.bugs_ref257$COMID, sep = "", collapse = ",")


# Watershed + Other (stream temp mets)
metrics_kitty <- StreamCatTools::sc_get_data(metric=metrics_together, 
      comid = sum.bugs_ref257$COMID, aoi='cat',
      showAreaSqKm = TRUE) 





@@@@
@@@@@@@@@   NEED NHD SLOPE
@@@@




#----------------------------------------------------------------------------------

# bring sum bug data and streamcat together

# first, simplify sum bugs
sum.bugs_ref257_simp <- sum.bugs_ref257 %>%
  select(act_id, MLocID, COMID, MTTI, BSTI)

# join to streamcat mets

stress.kitty <- sum.bugs_ref257_simp %>%
  left_join(metrics_kitty, by=c('COMID' = 'comid'))


stress.kitty[!complete.cases(stress.kitty), ] # one ref site with missing predictors
# drop this one site, run models and see if these predictors are important. 
# if not, drop predictors and add ref site back in

stress.kitty_na <- na.omit(stress.kitty)

#--------------------------------------------------------------------------

# MODELING - MTTI

#---------------------------------------------------------------------------

library(randomForest)

rf.mtti <- randomForest(y=stress.kitty_na$MTTI, x=stress.kitty_na[6:59], importance=TRUE,  keep.forest=TRUE)
rf.mtti  

    #---WS+CAT: 104 preds = 58.16% variance explained, many CAT preds higher importance
    #---WS: 54 preds = 57.92 % variance explained
    #---CAT: 54 preds = 57.37 % variance explained

ref.varimp <- importance(rf.mtti, conditional = TRUE) 
rf.mtti$importance
plot(rf.mtti)
varImpPlot(rf.mtti)



