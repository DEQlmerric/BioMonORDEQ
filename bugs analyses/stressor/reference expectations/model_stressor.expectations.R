
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

kitty.mets_use_met.names <- as.vector(kitty.mets_use$METRIC_NAME)

metrics_together <- str_c(kitty.mets_use_met.names, sep = "", collapse = ",")

comids.together <- str_c(sum.bugs_ref257$COMID, sep = "", collapse = ",")


# Watershed + Other (stream temp mets)
metrics_WS.OTHER_OR.CA.NV <- StreamCatTools::sc_get_data(metric= 
     "areasqkm,wsareasqkm,wsareasqkmrp100,bfi,precip8110,precip9120,
     precip2009,tmax8110,tmax9120,tmean8110,tmean9120,tmean2009,tmin8110,
     tmin9120,Precip_Minus_EVT,Rckdep,al2o3,cao,compstrgth,elev,fe2o3,hydrlcond,k2o,
     mast2009,mgo,msst2009,mwst2009,n,na2o,p2o5,pctalkintruvol,pctalluvcoast,
     pctcarbresid,pctcoastcrs,pctcolluvsed,pcteolcrs,pcteolfine,pctextruvol,
     pctglaclakecrs,pctglaclakefine,pctglactilclay,pctglactilcrs,
     pctglactilloam,pcthydric,pctnoncarbresid,pctsallake,pctsilicic,pctwater,rockn,runoff,
     s,sio2,wetindex,clay,kffact,om,perm,rckdep,sand", 
      comid = comids.together, aoi='watershed') 
# SLH 11.20.24: precip, mast, msst, mwst codes all changed slightly









