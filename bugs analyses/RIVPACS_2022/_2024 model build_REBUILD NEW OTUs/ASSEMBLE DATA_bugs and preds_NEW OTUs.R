#
#
#   Author: SLH 2.20.24
#
#   Purpose: assemble bug and predictor data for building new O/E models
#             once the two datasets are complete, bring them into th O/E model build R code to generate models


# SLH: 11.20.24 = re-run and develop updated model based on updated OTUs for DEQ's taxonomy table

library(tidyverse)
library(openxlsx)
library(StreamCatTools)
              #library(remotes)
              #install_github("USEPA/StreamCatTools", build_vignettes=FALSE)


##############################################################################################################################

#     BUG DATA

################################################################################################################################

#     1) bring in bug sample list
#     2) query bug samples from AWQMS
#     3) assign to OTUs
#     4) subsample to 300 count
#     5) crosstab 
#     6) change counts to '1' or '0'
  
ref_257 <- read.xlsx('bugs analyses/RIVPACS_2022/_2024 model build/Reference sites_bug samples_total and OTU abundances.xlsx',
                          sheet = 'FINAL FINAL FINAL_265 ref samps')  

ref_257 <- ref_257 %>%
            filter(Use_spatial=='Y') %>%
            rename(MLocID = Name, StaDes = Description)

# create a list of sampleIDs to use in querying bug data

sampleIDs.ref257 <- as.vector(ref_257$act_id)


# bring in saved bug data, downloaded from AWQMS in early February by LAM
      # AWQMS pulls haven't been working, this was a temporary work-around, suitable for model building but not permament
load('bugs analyses/RIVPACS_2022/_2024 model build/raw_bugs.Rdata') 


# filter raw_bugs to only the 256 ref sites

ref.bugs_257 <- raw_bugs %>%
            filter(act_id %in% sampleIDs.ref257) %>%
            filter(Char_Name=='Count') %>%
            select(act_id, MLocID, StationDes, COMID, EcoRegion2, EcoRegion3, Lat_DD, Long_DD, ELEV_Ft, 
                   SampleStart_Date, Sample_Method, DEQ_Taxon, Taxonomic_Name, Result_Numeric, StageID, UniqueTaxon)

# join to taxonomy and OTUs
taxonomy <- read.xlsx('bugs analyses/Taxonomy/ODEQ_Taxonomy_dec22.xlsx') 

taxonomy.otu <- taxonomy %>%
              select(DEQ_Taxon = DEQ_TAXON, Taxon, OTU_RIV_24)

taxonomy.otu$DEQ_Taxon <- as.character(taxonomy.otu$DEQ_Taxon)


ref.bugs.taxonomy <- ref.bugs_257 %>%
  left_join(taxonomy.otu, by='DEQ_Taxon')

# sum abundances across OTUs, drop 'DNI' taxa

ref.bugs_257_OTUs <- as.data.frame(ref.bugs.taxonomy %>%
      group_by(Sample=act_id, MLocID, OTU=OTU_RIV_24) %>%
      summarise(Count=sum(Result_Numeric)) %>%
      filter(OTU != 'DNI'))
      


      #### READY for SUBSAMPLING

#####
#####
#       1.2 rarify to 300 count
#####
#####

# rarify to 300 count per sample --> this standardizes 'effort' across all samples 
# since O/E is basically 'reference taxa richness', it is highly related to total count


# load the 'reshape2' package and source in the 'rarify' script for subsampling to 300

source('bugs analyses/RIVPACS_2022/_2024 model build/rarify_w_seed.R')


b.rare.seed <- rarify.seed(na.omit(ref.bugs_257_OTUs), 'Sample', 'Count', 300) 



#####
#####
#       1.3 get total abundance of rarified samples for PREDATOR
######
######
tot.abund_notsub_257<-aggregate(ref.bugs_257_OTUs$Count, list(Sample=ref.bugs_257_OTUs$Sample, MLocID=ref.bugs_257_OTUs$MLocID), sum)
tot.abund_notsub_257 <- dplyr::rename(tot.abund_notsub_257, tot.abund_notsub_257 = x)


tot.abund.sub257<-aggregate(b.rare.seed$Count, list(Sample=b.rare.seed$Sample, MLocID=b.rare.seed$MLocID), sum)
tot.abund.sub257 <- dplyr::rename(tot.abund.sub257, tot.abund.sub257 = x)

#####
#####
#       1.4 Matrify: convert from long format (many rows per sample), to site X species matrix (one row per sample)
######
######

dm.rare <- tidyr::pivot_longer(b.rare.seed, Count, names_to = "variable", values_to = "value" )

# data.table::setDT(dm.rare)
# bugs.all <- data.table::dcast(dm.rare, Sample+MLocID+Eco2+Eco3 ~ OTU, fun.aggregate = sum) # Sample x OTU doesn't seem to be unique so you need the fun.aggregate.  
# head(bugs.all)

bugs.mat_257.ref <- dm.rare %>% pivot_wider(               # new tidy method, replacing dcast (data.table)
  names_from = OTU,
  id_cols = c(Sample),
  values_from = value,
  values_fill = 0,
  names_repair = "check_unique",
  names_sort = TRUE)

# export matrified bug data file to load directly in model building phase

save(bugs.mat_257.ref, file='bugs analyses/RIVPACS_2022/_2024 model build_REBUILD NEW OTUs/bugs.mat_257.ref.Rdata')
  

  
  
  
  
  
#######################################################################################

#           StreamCat predictors

#######################################################################################

# streamcat metrics are associated by COMIDs. Generally, we want to use WS (watershed) metrics as predictors
# But some sites--small streams not on NHD med res--don't have COMIDs
      # in these cases, Adam Thompson assigned COMID manually assigned COMID to the closest associated COMID
      # but the WS metrics will be for larger systems, so instead, for these sites we want to use the CAT (catchment) metrics
# Goal: 1) Get COMIDs for all sites, keeping track of which ones need metrics
#       2) download streamcat metrics for all COMIDs associated with hydroregions 16/17/18, which covers all reference sites
#           a) WS for sites with matching COMIDs
#           b) CAT for sites with manually assigned COMID
#       3) bring WS and CAT metrics together, based on sites and which scale is appropriate
#           a) will require renaming metrics to match, then appending together




#####

#   Bring in COMIDs that were reviewed and, as necessary, manually assigned by Adam Thompson

#####

bio.mlocids.awqms_ALT <- read.xlsx('bugs analyses/RIVPACS_2022/_2024 model build/Bio_MlocIDs_AWQMS.xlsx') 
  


mlocids_WS <- bio.mlocids.awqms_ALT %>%
                filter(COMID != 'NULL') %>%
                select(MLocID, COMID) 


mlocids_CAT <- bio.mlocids.awqms_ALT %>%
  filter(COMID == 'NULL') %>%
  select(MLocID, COMID = Nearby_COMID)



###  associate bug samples with WS or CAT COMIDs

samps.257_WS <- ref_257 %>%
  inner_join(mlocids_WS, by='MLocID') %>%
  unique() %>%
  select(-Icon, -Use_nopool.date.abund, -Use_spatial)


samps.257_CAT <- ref_257 %>%
  inner_join(mlocids_CAT, by='MLocID') %>%
  unique() %>%
  select(-Icon, -Use_nopool.date.abund, -Use_spatial)

samps.257_CAT$COMID <- as.character(samps.257_CAT$COMID)



#####

#   Query StreamCat database

#####

region_params <- sc_get_params(param='areaOfInterest')
name_params <- sc_get_params(param='name')
name_params <- sort(name_params)  


# Get data for the three Hydroregions covering: 17= OR+WA+ID, 16 = NV, 18 = CA

# Watershed + Other (stream temp mets)
metrics_WS.OTHER_OR.CA.NV <- sc_get_data(metric='al2o3,bfi,cao,clay,compstrgth,elev,fe2o3,hydrlcond,
                       inorgnwetdep_2008,k2o,kffact,mast2008,mast2009,mast2013,mast2014,
                       msst2008,msst2009,msst2013,msst2014,mwst2008,mwst2009,mwst2013,mwst2014,  
                       mgo,n,na2o,nh4_2008,no3_2008,om,p2o5,pctalkintruvol,pctalluvcoast,pctbl2001,
                       pctbl2004,pctcarbresid,pctcoastcrs,pctcolluvsed,pcteolcrs,pcteolfine,pctextruvol,
                       pctglaclakecrs,pctglaclakefine,pctglactilclay,pctglactilcrs,pctglactilloam,
                       pcthydric,pctice2001,pctice2004,pctice2006,pctice2008,pctice2011,
                       pctice2013,pctice2016,pctice2019,pctsallake,perm,precip2008,precip2009,
                       precip8110,rckdep,s,sand,sio2,tmax8110,tmean8110,tmin8110', 
                       aoi='watershed, other', region='16,17,18') 
                        # SLH 11.20.24: precip, mast, msst, mwst codes all changed slightly

# convert multiple years to means
metrics_WS.OTHER_OR.CA.NV <- metrics_WS.OTHER_OR.CA.NV %>%
  mutate(MAST_mean08.14 = (MAST2008+MAST2009+MAST2013+MAST2014)/4) %>%
  mutate(MSST_mean08.14 = (MSST2008+MSST2009+MSST2013+MSST2014)/4) %>% 
  mutate(MWST_mean08.14 = (MWST2008+MWST2009+MWST2013+MWST2014)/4) %>%
  mutate(PCTICE_mean01.19 = (PCTICE2001WS+PCTICE2004WS+PCTICE2006WS+PCTICE2008WS+PCTICE2011WS+
                               PCTICE2013WS+PCTICE2016WS+PCTICE2019WS)/8) %>%
  select(-MAST2008, -MAST2009,-MAST2013,-MAST2014,-MSST2008,-MSST2009,
         -MSST2013,-MSST2014,-MWST2008,-MWST2009,-MWST2013,-MWST2014, 
         -PCTICE2001WS,-PCTICE2004WS,-PCTICE2006WS,-PCTICE2008WS,-PCTICE2011WS,
         -PCTICE2013WS,-PCTICE2016WS,-PCTICE2019WS)


# Catchment + Other (stream temp mets)

metrics_CAT.OTHER_OR.CA.NV <- sc_get_data(metric='al2o3,bfi,cao,clay,compstrgth,elev,fe2o3,hydrlcond,
                       inorgnwetdep_2008,k2o,kffact,mast2008,mast2009,mast2013,mast2014,
                       msst2008,msst2009,msst2013,msst2014,mwst2008,mwst2009,mwst2013,mwst2014,  
                       mgo,n,na2o,nh4_2008,no3_2008,om,p2o5,pctalkintruvol,pctalluvcoast,pctbl2001,
                       pctbl2004,pctcarbresid,pctcoastcrs,pctcolluvsed,pcteolcrs,pcteolfine,pctextruvol,
                       pctglaclakecrs,pctglaclakefine,pctglactilclay,pctglactilcrs,pctglactilloam,
                       pcthydric,pctice2001,pctice2004,pctice2006,pctice2008,pctice2011,
                       pctice2013,pctice2016,pctice2019,pctsallake,perm,precip2008,precip2009,
                       precip8110,rckdep,s,sand,sio2,tmax8110,tmean8110,tmin8110', 
                       aoi='catchment, other', region='16,17,18')


# convert multiple years to means
metrics_CAT.OTHER_OR.CA.NV <- metrics_CAT.OTHER_OR.CA.NV %>%
  mutate(MAST_mean08.14 = (MAST2008+MAST2009+MAST2013+MAST2014)/4) %>%
  mutate(MSST_mean08.14 = (MSST2008+MSST2009+MSST2013+MSST2014)/4) %>% 
  mutate(MWST_mean08.14 = (MWST2008+MWST2009+MWST2013+MWST2014)/4) %>%
  mutate(PCTICE_mean01.19 = (PCTICE2001CAT+PCTICE2004CAT+PCTICE2006CAT+PCTICE2008CAT+PCTICE2011CAT+
          PCTICE2013CAT+PCTICE2016CAT+PCTICE2019CAT)/8) %>%
  select(-MAST2008, -MAST2009,-MAST2013,-MAST2014,-MSST2008,-MSST2009,
         -MSST2013,-MSST2014,-MWST2008,-MWST2009,-MWST2013,-MWST2014, 
         -PCTICE2001CAT,-PCTICE2004CAT,-PCTICE2006CAT,-PCTICE2008CAT,-PCTICE2011CAT,
           -PCTICE2013CAT,-PCTICE2016CAT,-PCTICE2019CAT, -WSAREASQKM)

# Convert COMID to character to allow join with COMIDs in our excel files
metrics_WS.OTHER_OR.CA.NV$COMID <- as.character(metrics_WS.OTHER_OR.CA.NV$COMID)
metrics_CAT.OTHER_OR.CA.NV$COMID <- as.character(metrics_CAT.OTHER_OR.CA.NV$COMID)


##
#   save streamcat metrics for loading in other applications
##
save(metrics_WS.OTHER_OR.CA.NV, file = 'bugs analyses/RIVPACS_2022/_2024 model build_REBUILD NEW OTUs/metrics_WS.OTHER_OR.CA.NV.Rdta' )
save(metrics_CAT.OTHER_OR.CA.NV, file = 'bugs analyses/RIVPACS_2022/_2024 model build_REBUILD NEW OTUs/metrics_CAT.OTHER_OR.CA.NV.Rdta' )



# join WS metrics to appropriate bug samples

samps.257_WS_mets <- samps.257_WS %>%
        left_join(metrics_WS.OTHER_OR.CA.NV, by='COMID') %>%
        mutate(met.type = 'WS')

samps.257_CAT_mets <- samps.257_CAT %>%
  left_join(metrics_CAT.OTHER_OR.CA.NV, by='COMID')%>%
  mutate(met.type = 'WS')



# rename to match columns, then rbind

samps.257_WS_mets <- samps.257_WS_mets %>%
      rename(AREASQKM = WSAREASQKM, SAND = SANDWS, CLAY = CLAYWS, ELEV = ELEVWS,            
             BFI = BFIWS, KFFACT = KFFACTWS, PCTGLACLAKEFINE = PCTGLACLAKEFINEWS,
             PCTGLACTILCLAY = PCTGLACTILCLAYWS, PCTGLACTILLOAM = PCTGLACTILLOAMWS,  
             PCTCOASTCRS = PCTCOASTCRSWS, PCTGLACLAKECRS = PCTGLACLAKECRSWS,
             PCTEXTRUVOL = PCTEXTRUVOLWS, PCTEOLFINE = PCTEOLFINEWS, PCTEOLCRS = PCTEOLCRSWS, 
             PCTGLACTILCRS = PCTGLACTILCRSWS, PCTSALLAKE = PCTSALLAKEWS, 
             PCTCARBRESID = PCTCARBRESIDWS, PCTALLUVCOAST = PCTALLUVCOASTWS,      
             PCTALKINTRUVOL = PCTALKINTRUVOLWS, PCTHYDRIC = PCTHYDRICWS,
             PCTCOLLUVSED = PCTCOLLUVSEDWS, TMAX8110 = TMAX8110WS, TMIN8110 = TMIN8110WS,
             PRECIP8110 = PRECIP8110WS, TMEAN8110 = TMEAN8110WS, COMPSTRGTH = COMPSTRGTHWS,        
             PCTBL2004 = PCTBL2004WS, # INORGNWETDEP_2008 = INORGNWETDEP_2008WS,
             N = NWS,  # NO3_2008 = NO3_2008WS,  NH4_2008 = NH4_2008WS, 
             HYDRLCOND = HYDRLCONDWS, MGO = MGOWS, K2O = K2OWS, AL2O3 = AL2O3WS,
             NA2O = NA2OWS, SIO2 = SIO2WS, CAO = CAOWS, P2O5 = P2O5WS,             
             S = SWS, FE2O3 = FE2O3WS, PERM = PERMWS, RCKDEP = RCKDEPWS, OM = OMWS,
             PCTBL2001 = PCTBL2001WS, PRECIP2008 = PRECIP2008WS, PRECIP2009 = PRECIP2009WS )    

samps.257_CAT_mets <- samps.257_CAT_mets %>%
  rename(AREASQKM = CATAREASQKM, SAND = SANDCAT, CLAY = CLAYCAT, ELEV = ELEVCAT,            
         BFI = BFICAT, KFFACT = KFFACTCAT, PCTGLACLAKEFINE = PCTGLACLAKEFINECAT,
         PCTGLACTILCLAY = PCTGLACTILCLAYCAT, PCTGLACTILLOAM = PCTGLACTILLOAMCAT,  
         PCTCOASTCRS = PCTCOASTCRSCAT, PCTGLACLAKECRS = PCTGLACLAKECRSCAT,
         PCTEXTRUVOL = PCTEXTRUVOLCAT, PCTEOLFINE = PCTEOLFINECAT, PCTEOLCRS = PCTEOLCRSCAT, 
         PCTGLACTILCRS = PCTGLACTILCRSCAT, PCTSALLAKE = PCTSALLAKECAT, 
         PCTCARBRESID = PCTCARBRESIDCAT, PCTALLUVCOAST = PCTALLUVCOASTCAT,      
         PCTALKINTRUVOL = PCTALKINTRUVOLCAT, PCTHYDRIC = PCTHYDRICCAT,
         PCTCOLLUVSED = PCTCOLLUVSEDCAT, TMAX8110 = TMAX8110CAT, TMIN8110 = TMIN8110CAT,
         PRECIP8110 = PRECIP8110CAT, TMEAN8110 = TMEAN8110CAT, COMPSTRGTH = COMPSTRGTHCAT,        
         PCTBL2004 = PCTBL2004CAT, # INORGNWETDEP_2008 = INORGNWETDEP_2008CAT,
         N = NCAT, # NO3_2008 = NO3_2008CAT, NH4_2008 = NH4_2008CAT,
         HYDRLCOND = HYDRLCONDCAT, MGO = MGOCAT, K2O = K2OCAT, AL2O3 = AL2O3CAT,
         NA2O = NA2OCAT, SIO2 = SIO2CAT, CAO = CAOCAT, P2O5 = P2O5CAT,             
         S = SCAT, FE2O3 = FE2O3CAT, PERM = PERMCAT, RCKDEP = RCKDEPCAT, OM = OMCAT,
         PCTBL2001 = PCTBL2001CAT, PRECIP2008 = PRECIP2008CAT, PRECIP2009 = PRECIP2009CAT )    

pred.mets_257 <- bind_rows(samps.257_WS_mets, samps.257_CAT_mets)


# predictors together for all 265 sites

summary(pred.mets_257)
      # most metrics with 4-5 missing values--this is good enough to replace with imputation
      # however, MAST (mean annual stream temp) has 29 missing values =---> need to drop this variable
      # variables with very little spread = drop
              # PCTGLACLAKEFINE, PCTGLACTILCLAY, PCTGLACTILLOAM, PCTCOASTCRS, PCTGLACLAKECRS, PCTEXTRUVOL, PCTEOLFINE,PCTEOLCRS, PCTSALLAKE, PCTHYDRIC,

pred.mets_257 <- pred.mets_257 %>% select(-MAST_mean08.14, -PCTGLACLAKEFINE, -PCTGLACTILCLAY, -PCTGLACTILLOAM, -PCTCOASTCRS, -PCTGLACLAKECRS, 
                                          -PCTEXTRUVOL, -PCTEOLFINE, -PCTEOLCRS, -PCTSALLAKE, -PCTHYDRIC)

##########

#  CORRELATIONS of PREDICTORS

##########

# we don't want to build models off of predictors that are highly correlated
# run correlations, then retain a single metric from correlated metrics

pred.corr <- pred.mets_257 %>%
      select(17:56)


results_corr <- cor(pred.corr, use='complete.obs')

write.table(results_corr, 'clipboard', sep='\t') # pastes to clipboard, go into Excel, and paste in cell



# SLH set a threshold of r > 0.895 for identifying highly correlated variables
# drop the following variables from future analyses

pred.mets_257_final <- pred.mets_257 %>%
  select(-TMIN8110, -TMEAN8110, -PRECIP2008, PRECIP2009, -PCTBL2001,  -AL2O3) # -NO3_2008, -NH4_2008,


# bring in NHD Slope


nhd.or <- read.csv('bugs analyses/RIVPACS_2022/_2024 model build/nhd.slope/NHD_Slope.csv')
nhd.ca <- read.csv('bugs analyses/RIVPACS_2022/_2024 model build/nhd.slope/CA_nhdslope.csv')
nhd.gb <- read.csv('bugs analyses/RIVPACS_2022/_2024 model build/nhd.slope/GB_nhdslope.csv')

  

nhd_all <- bind_rows(nhd.or, nhd.ca, nhd.gb) %>%
  select(COMID, SLOPE)

nhd_all$COMID <- as.character(nhd_all$COMID)


pred.mets_257_final <- pred.mets_257_final %>%
        left_join(nhd_all, by='COMID')


# still missing Ecoregion from preds file
ecos <- bio.mlocids.awqms_ALT %>%
  select(MLocID, EcoRegion2, EcoRegion3) %>%
  unique()


pred.mets_257_final <- pred.mets_257_final %>%
      left_join(ecos, by='MLocID')


# export final predictors file

save(pred.mets_257_final, file='bugs analyses/RIVPACS_2022/_2024 model build_REBUILD NEW OTUs/pred.mets_257_final.Rdata')









