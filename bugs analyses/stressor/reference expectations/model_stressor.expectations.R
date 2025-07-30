
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
     "wsareasqkm,wsareasqkmrp100,bfi,precip8110,precip9120,
     precip2009,tmax8110,tmax9120,tmean8110,tmean9120,tmean2009,tmin8110,
     tmin9120,Rckdep,al2o3,cao,compstrgth,elev,fe2o3,hydrlcond,k2o,
     mast2009,mgo,msst2009,mwst2009,n,na2o,p2o5,pctalkintruvol,pctalluvcoast,
     pctcarbresid,pctcoastcrs,pctcolluvsed,pcteolcrs,pcteolfine,pctextruvol,
     pctglaclakecrs,pctglaclakefine,pctglactilclay,pctglactilcrs,
     pctglactilloam,pcthydric,pctnoncarbresid,pctsallake,pctsilicic,pctwater,rockn,runoff,
     s,sio2,wetindex,clay,kffact,om,perm,rckdep,sand", 
      comid = "23821971,24193544,24026308,23822531,24072957,24075543,24073571,24057153,
     24043279,23752326,23753746,24013583,24207735,23809250,23773687,23700205,23648960,
     24013637,23781075,23735951,23737695,23752538,23773153,24013783,23721863,23437013,
     23442167,23941059,23920572,23890138,23887034,23890318,23886786,23876481,23876723,
     23876781,23946161,23949727,23700171,23718863,24504252,23720273,23700385,23700715,
     23701355,23786877,23753434,23781517,23809612,24507192,24072907,23824341,23781021,
     23872841,24224943,23414837,24224291,23417413,23671309,23671681,23824289,23681451,
     23720045,23786861,23774807,23437127,23672921,23672885,23876311,23886156,23890280,
     23945549,23737605,23737257,23887010,23890084,23876307,23886774,23737687,23886658,
     23894722,23887024,23876231,23877163,23887124,23876239,23903389,23946171,23936959,
     23949695,23774647,23752772,23773125,23894328,23805548,23753686,23886182,23681447,
     23681503,23736007,23809510,23822645,23822531,23824243,23672733,23833101,23877125,
     23810368,23810642,23810652,23736143,24505068,23809282,24506058,23737505,23737449,
     23737433,23949757,23921066,23950269,23921110,23950275,23921102,23941145,23895502,
     23893449,23941143,23946197,23681463,23894558,23936135,23890076,23430670,23903435,
     24074673,24013711,22226750,22226782,8315549,7932053,7929215,8257659,7960911,
     22227592,22226836,7960907,4439208,8315561,4439740,8257399,8257397,22227610,
     22227560,8257711,8951906,7928665,4439616,8257759,22227360,7932037,3798357,23752404,
     23774595,23923530,23930882,23923542,23774549,23780661,23781569,23780745,23752560,
     23774269,23752924,23773261,23773293,22226492,22226474,23903227,23894540,23787123,
     23901781,23901773,23736455,23781113,23736433,23809492,23809538,23759496,23759498,
     23752932,23442275,23437019,23428578,23648604,24223883,24074639,24092160,24035798,
     24057445,23638006,23429124,24224117,23823089,23822537,23700917,23722535,24207783,
     23437329,23437457,23673433,11845665,23671181,24013685,24224151,23700207,24208245,
     24504702,23438027,23437101,23442235,23442243,23637852,24223999,23437223,24208721,
     24224177,23334796,24075771,23822603,23876575,23876421,23437389,23781017,23809232,
     24224161,23352085,23350559,23923572,947090096,24072957,24072895,23935911,24058291,
     24057437,24075267,11300894,11844773,24014023,24013693,24013967,-81330,24003099,
     11845035,24075579,24075031,23773305", aoi='ws') 
# SLH 11.20.24: precip, mast, msst, mwst codes all changed slightly









