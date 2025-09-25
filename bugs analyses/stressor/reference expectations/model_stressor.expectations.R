
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
library(randomForest)
library(tidyverse)

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

# First bring in the predictors used in OE and MMI modeling to capture those things not included in STREAMCAT (Slope, LAT/Long, ecoregions, etc)
# Second, query streamcat for all NATURAL variables
# Third, tie all the predictors back together for use in initial model building

# 1) Bring in predictors from the O/E and MMI modeling exercises

load('bugs analyses/RIVPACS_2022/_2024 model build_REBUILD NEW OTUs/pred.mets_257_final.Rdata')

orig.predictors <- pred.mets_257_final %>%
  select(act_id, MLocID, COMID, Latitude, Longitude, SampleStart_Date, MSST_mean08.14, MWST_mean08.14, SLOPE, EcoRegion2, EcoRegion3)



  # 2 query streamcat directly for predictor variables


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
metrics_kitty <- StreamCatTools::sc_get_data(metric="bfi,precip8110,precip9120,precip2008,precip2009,tmax8110,tmax9120,tmean8110,tmean9120,tmean,tmin8110,tmin9120,rckdep,al2o3,cao,compstrgth,elev,fe2o3,hydrlcond,k2o,mast2008,mgo,msst2008,mwst,n,na2o,nh4,no3,p2o5,pctalkintruvol,pctalluvcoast,pctbl,pctcarbresid,pctcoastcrs,pctcolluvsed,pcteolcrs,pcteolfine,pctextruvol,pctglaclakecrs,pctglaclakefine,pctglactilclay,pctglactilcrs,pctglactilloam,pcthydric,pctice2008,pctice2009,pctice2013,pctice2014,pctnoncarbresid,pctsallake,pctsilicic,pctwater,rockn,runoff,s,sio2,wetindex,wettedwidth,wtdep,clay,kffact,om,perm,rckdep,sand", 
      comid = sum.bugs_ref257$COMID, aoi='ws, other',
      showAreaSqKm = TRUE) 


metrics_kitty$comid <- as.character(metrics_kitty$comid)


@@@@ this drops YEAR metrics, which we definitely want some of them--until Travis can help resolve, use MSST and MWST means from previous modeling for OE//MMI


                                                            
  #3) join all predictors back together

preds.all <- orig.predictors %>%
  left_join(metrics_kitty, by=c('COMID'='comid')) %>%
  select(-tmean8110ws, -tmin8110ws, -tmin9120ws, -tmean9120ws, -tmax8110ws, -precip8110ws, -na2ows, -precip2008ws, -precip2009ws, -precip8110ws)  # drop correlated predictors

preds.all <- preds.all %>%
  mutate(pct_erod = pctalluvcoastws+pctcarbresidws+pctcoastcrsws+pctcolluvsedws+pcteolcrsws+pcteolfinews+pctglaclakecrsws+pctglaclakefinews+pctglactilclayws+pctglactilcrsws+pctglactilloamws+pctnoncarbresidws+pctsallakews) %>%
  mutate(pct_resist = pctalkintruvolws+pctextruvolws+pcthydricws+pctsilicicws) %>%
  mutate(strmpow_cat = wsareasqkm+precip9120ws*SLOPE)

#----------------------------------------------------------------------------------

# bring sum bug data and streamcat together

# mtti + predictors
mtti.preds <- sum.bugs_ref257 %>%
  select(act_id, MTTI) %>%
  left_join(preds.all, by = 'act_id')

# for model building, replace NAs with mean values
mtti.preds$sandws <- na.roughfix(mtti.preds$sandws) 
mtti.preds$clayws <- na.roughfix(mtti.preds$clayws)
mtti.preds$permws <- na.roughfix(mtti.preds$permws)
mtti.preds$rckdepws <- na.roughfix(mtti.preds$rckdepws)
mtti.preds$omws <- na.roughfix(mtti.preds$omws)
mtti.preds$MSST_mean08.14 <- na.roughfix(mtti.preds$MSST_mean08.14)
mtti.preds$MWST_mean08.14 <- na.roughfix(mtti.preds$MWST_mean08.14)
mtti.preds$SLOPE <- na.roughfix(mtti.preds$SLOPE)
mtti.preds$wtdepws <- na.roughfix(mtti.preds$wtdepws)
mtti.preds$strmpow_cat <- na.roughfix(mtti.preds$strmpow_cat)


# bsti = predictors

bsti.preds <- sum.bugs_ref257 %>%
  select(act_id, BSTI) %>%
  left_join(preds.all, by = 'act_id')



bsti.preds$sandws <- na.roughfix(bsti.preds$sandws) 
bsti.preds$clayws <- na.roughfix(bsti.preds$clayws)
bsti.preds$permws <- na.roughfix(bsti.preds$permws)
bsti.preds$rckdepws <- na.roughfix(bsti.preds$rckdepws)
bsti.preds$omws <- na.roughfix(bsti.preds$omws)
bsti.preds$MSST_mean08.14 <- na.roughfix(bsti.preds$MSST_mean08.14)
bsti.preds$MWST_mean08.14 <- na.roughfix(bsti.preds$MWST_mean08.14)
bsti.preds$SLOPE <- na.roughfix(bsti.preds$SLOPE)
bsti.preds$wtdepws <- na.roughfix(bsti.preds$wtdepws)
bsti.preds$strmpow_cat <- na.roughfix(bsti.preds$strmpow_cat)



# verify all data present

mtti.preds[!complete.cases(mtti.preds), ] # verify no records
bsti.preds[!complete.cases(bsti.preds), ] # verify no records





#--------------------------------------------------------------------------

# MODELING - MTTI

#---------------------------------------------------------------------------



rf.mtti <- randomForest(y=mtti.preds$MTTI, x=mtti.preds[3:62], importance=TRUE,  keep.forest=TRUE)
rf.mtti  

    # 57 preds = 60 % variance explained

ref.varimp <- as.data.frame(importance(rf.mtti, conditional = TRUE) )
rf.mtti$importance
plot(rf.mtti) # stability reached well before 500 trees, minimal improvement beyond 200
varImpPlot(rf.mtti)

# run a rf model with reduced predictors, based on top %IncMSE and IncNodePurity

rf.mtti.reduced <- randomForest(MTTI ~ MSST_mean08.14+clayws+tmax9120ws+bfiws+kffactws+Latitude+precip9120ws+elevws,
                                  data=mtti.preds,
                                importance=TRUE,  keep.forest=TRUE)


rf.mtti.reduced
# 5 preds = 57.84 % variance explained. 

ref.varimp.redu <- as.data.frame(importance(rf.mtti.reduced, conditional = TRUE) )
rf.mtti.reduced$importance
plot(rf.mtti.reduced) # stability reached well before 500 trees, minimal improvement beyond 200, very stable past 300
varImpPlot(rf.mtti.reduced)


plot(rf.mtti.reduced$predicted ~ rf.mtti.reduced$y, ylim=c(5,30))
a <- lm(rf.mtti.reduced$predicted ~ rf.mtti.reduced$y)
summary(a) 
abline(a)

# save mtti rf model
save(ranfor.mod=rf.mtti.reduced, file='bugs analyses/stressor/reference expectations/rf.mtti.reduced.Rdata')




#--------------------------------------------------------------------------------------

# make predictions for all bug samples

# 1) bring in sites/samples to be assessed. Need: act_id, COMID, MTTI
sum.bugs

# 2) get specific streamcat predictors for all samples

metrics_kitty_all.samples <- StreamCatTools::sc_get_data(metric=
      'MSST_mean08.14,tmax9120ws,bfiws,Latitude,kffactws', 
       comid = sum.bugs_ref257$COMID, aoi='ws',
       showAreaSqKm = TRUE) 



@@@@@@@@@@@@@@@@@@ doesnt work, predictors are wrong







predict.mtti <- predict(rf.mtti.reduced, newdata=, type="response",
        norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE)






#--------------------------------------------------------------------------

# MODELING - BSTI

#---------------------------------------------------------------------------



rf.bsti <- randomForest(y=bsti.preds$BSTI, x=bsti.preds[c(3:64)], importance=TRUE,  keep.forest=TRUE)
rf.bsti  

# 38 preds = 32.52 % variance explained (eariler work on Mid-Coast = 34%, so similar performance).....maybe older work was 42-46%?????

ref.varimp <- as.data.frame(importance(rf.bsti, conditional = TRUE) )
rf.bsti$importance
plot(rf.bsti) # stability reached well before 500 trees, minimal improvement beyond 100
varImpPlot(rf.bsti)

# run a rf model with reduced predictors, based on top %IncMSE and IncNodePurity

rf.bsti.reduced <- randomForest(BSTI ~ @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@,
                                data=bsti.preds,
                                importance=TRUE,  keep.forest=TRUE)


rf.bst.reduced
# 8 preds = 57 % variance explained, verified removing additional variables results in ~3-5% reduction in variance explained

ref.varimp.redu <- as.data.frame(importance(rf.mtti.reduced, conditional = TRUE) )
rf.mtti.reduced$importance
plot(rf.mtti.reduced) # stability reached well before 500 trees, minimal improvement beyond 100, very stable past 300
varImpPlot(rf.mtti.reduced)


plot(rf.mtti.reduced$predicted ~ rf.mtti.reduced$y, ylim=c(5,30))
a <- lm(rf.mtti.reduced$predicted ~ rf.mtti.reduced$y)
summary(a) 
abline(a)

# save mtti rf model
save(ranfor.mod=rf.mtti.reduced, file='bugs analyses/stressor/reference expectations/rf.mtti.reduced.Rdata')








#--------------------------------------------------------------------------------------

# make predictions for all bug samples

# 1) bring in sites/samples to be assessed. Need: act_id, COMID, MTTI
sum.bugs

# 2) get specific streamcat predictors for all samples

metrics_kitty_all.samples <- StreamCatTools::sc_get_data(metric=
                                                           'MSST2008,TMAX8110,CLAY,KFFACT,ELEV,BFI,
      PERM,Latitude,PRECIP8110', 
                                                         comid = sum.bugs_ref257$COMID, aoi='ws',
                                                         showAreaSqKm = TRUE) 











predict.mtti <- predict(rf.mtti.reduced, newdata=, type="response",
                        norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# compare old Mid Coast predictors to StreamCAT preds

midcoast <- read.xlsx('bugs analyses/stressor/reference expectations/MIDCOAST_Biocriteria Methods - CART_DATA.xlsx')

preds.all_midcoast <- preds.all %>%
  left_join(midcoast, by = 'MLocID')

