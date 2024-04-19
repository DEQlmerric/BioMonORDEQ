#
#
#   Author: SLH 3.12.24
#
#   Purpose: assemble bug and predictor data for building new MMI models
#             once the two datasets are complete, bring them into the MMI model build R code to generate models


library(tidyverse)
library(openxlsx)
library(StreamCatTools)
library(BioMonTools)
          # library(remotes)
          # install_github("USEPA/StreamCatTools", build_vignettes=FALSE)



##############################################################################################################################

#     REFERENCE and MOST DISTURBED: sites and samples

################################################################################################################################


####
    ######  REFERENCE
####

ref_256 <- read.xlsx('bugs analyses/MMI/_2024 model build/Reference sites_bug samples_total and OTU abundances.xlsx',
                     sheet = 'FINAL FINAL FINAL_265 ref samps')  

ref_256 <- ref_256 %>%
  filter(Use_spatial=='Y') %>%
  rename(MLocID = Name, StaDes = Description)

# create a list of sampleIDs to use in querying bug data

sampleIDs.ref256 <- as.vector(ref_256$act_id)




####
    ######  MOST DISTURBED
####

                                                                                  # all possible bio stations with ref status
                                                                                 # all.stations <- read.xlsx('bugs analyses/MMI/_2024 model build/Bio_MlocIDs_AWQMS_Most disturbed.SYB.xlsx')
                                                                                  
                                                                                #  most.disturbed <- all.stations %>%
                                                                                #    filter(ReferenceSite == 'MOST DISTURBED') %>% # limit to sites of interest
                                                                                #    unique()

# which samples to use? --SLH took the "MOST.DISTURBED_bug.samples_site.info_total.abundances.xlsx" file 
# and identified which sample to use when more than one sample existed for a MostDisturbed station

most.dist_USE <- read.csv('bugs analyses/MMI/_2024 model build/MOST.DISTURBED_bug.samples_site.info_total.abundances_USE2.csv')

most.dist_USE <- most.dist_USE %>%
  filter(Use_FINAL == 'Y')           # 'n' = 167 most disturbed samples to use for modeling
  
sampleIDs.most.dist167 <- as.vector(most.dist_USE$act_id) 
  

###
    ####### BUG DATA
###

load('bugs analyses/MMI/_2024 model build/raw_bugs2.Rdata')



# get month
raw_bugs <- raw_bugs %>%
  mutate(month = month(SampleStart_Date))



# drop: NOT reference or most disturbed, old samples (diff methods/taxonomy), pools/other, density, non-summer
bugs_ref.most <- raw_bugs %>%
  filter(act_id %in% sampleIDs.ref256 | act_id %in% sampleIDs.most.dist167) %>%
  filter(Result_Unit == 'count') %>%
  select(act_id, MLocID, StationDes, DEQ_Taxon,  Result_Numeric, ReferenceSite) 

bugs_ref.most$DEQ_Taxon <- as.numeric(bugs_ref.most$DEQ_Taxon)

# calculate total abundance per sample
tot.abund_ref.most <- bugs_ref.most %>%
  group_by(act_id, MLocID, StationDes, ReferenceSite) %>%
  summarize( tot.abund = sum(Result_Numeric))




##############################################################################################################################

#     BUG DATA

################################################################################################################################

#     1) assocaite raw_bugs with OTUs, then attributes
#     2) rarify to 300 count
#     3) calculate metrics using BioMonTools

# join to taxonomy and OTUs
taxonomy <- read.xlsx('bugs analyses/Taxonomy/ODEQ_Taxonomy_dec22.xlsx') 

taxonomy<- taxonomy %>%
    filter(Phylum != 'Chordata') %>%
    select(DEQ_Taxon = DEQ_TAXON, Taxon)

# bring in OTUs from taxa translator created by Sean Sullivan
translator <- read.csv('bugs analyses/MMI/_2024 model build/ORWA_TaxaTranslator_20240417.csv')

# join taxonomy and translator: bug data has taxon codes only, translator has taxon, taxonomy has both

taxon.translate <- taxonomy %>%
  left_join(translator, by = 'Taxon') %>%
  unique()



# join with bugs, sum abundances across OTUs, drop 'DNI' taxa

bugs_ref.most_OTUs <- bugs_ref.most %>%
  left_join(taxon.translate, by='DEQ_Taxon') %>%
  select(act_id, OTU_MetricCalc, Count = Result_Numeric, ReferenceSite) %>%
  filter(OTU_MetricCalc != 'DNI') 
  
# sum across OTUs

bugs_ref.most_OTUs_sum <- bugs_ref.most_OTUs %>%
  group_by(act_id, OTU_MetricCalc) %>%
  summarise(sum.count = sum(Count))%>%
  filter( sum.count > 0)
  # need to drop ReferenceSite to work with rarify


#####
#       1.2 rarify to 300 count
#####


# rarify to 300 count per sample --> this standardizes 'effort' across all samples 
# technically, this is only required for richness (# taxa/sample) metrics
# start with a single subsampled dataset for all metrics
        #### if model performance poor, explore subsampled bugs ONLY FOR RICHNESS
        #     ---> requires two datasets and running metrics function separately for each

# use BioMonTools 'rarify' function (JVS original code)
mySize <- 300
Seed_OR <- 18590214

bugs_ref.most_OTUs_sum <- as.data.frame(bugs_ref.most_OTUs_sum) 

bugs_ref.most_rare <- rarify(inbug = bugs_ref.most_OTUs_sum, sample.ID='act_id', abund='sum.count', subsiz = mySize, mySeed = Seed_OR)%>%
  filter(sum.count > 0)




#######

#         ATTRIBUTES

#######

# join OTUs and counts with attributes
attributes <- read.csv('bugs analyses/MMI/_2024 model build/ORWA_Attributes_20240417.csv')

attributes <- attributes %>%
  rename(AIRBREATHER = Air) %>%
  rename(OTU_MetricCalc = Taxon)


bugs_ref.most_OTUs_sum_attr <- bugs_ref.most_rare %>%
  left_join(attributes, by = 'OTU_MetricCalc')%>%
  rename(SAMPLEID = act_id, TAXAID = OTU_MetricCalc, N_TAXA = sum.count) 


 
###########
      
#             METRICS
      
###########
  
###
    # mark excluded
###


bugs.excluded <- markExcluded(bugs_ref.most_OTUs_sum_attr, TaxaLevels = c("Kingdom", "Phylum",
                "SubPhylum", "Class", "SubClass", "Order", "SubOrder", "SuperFamily", 
                "Family", "SubFamily", "Tribe", "GenusGroup", "Genus", "SubGenus", "SpeciesGroup",
                "SpeciesSubGroup", "SpeciesComplex", "Species"))






  
###
    # bug metrics
###  

# identify which metrics to keep out of many more in BioMonTools
mets.keep <- c("ni_total","li_total","ni_Chiro","ni_EPT","ni_Trich",
"nt_total","nt_Amph","nt_Bival","nt_Coleo","nt_COET","nt_CruMol","nt_Dipt","nt_ECT","nt_Ephem","nt_Ephemerellid",
"nt_ET","nt_EPT","nt_Gast","nt_Hepta","nt_Insect","nt_Isop","nt_Mega","nt_Mol","nt_Nemour","nt_NonIns","nt_Odon",
"nt_OET","nt_Oligo","nt_Perlid","nt_Pleco","nt_POET","nt_Ptero","nt_Rhya","nt_Tipulid","nt_Trich","nt_TrichNoHydro",
"nt_Tromb","nt_Tubif","pi_Amph","pi_AmphIsop","pi_Baet","pi_Bival","pi_Caen","pi_Cheu","pi_Coleo",
"pi_COET","pi_Corb","pi_CorixPhys","pi_CraCaeGam","pi_Cru","pi_CruMol","pi_Deca","pi_Dipt","pi_DiptNonIns",
"pi_ECT","pi_Ephem","pi_EphemNoCae","pi_EphemNoCaeBae","pi_EPT","pi_EPTNoBaeHydro","pi_EPTNoCheu","pi_EPTNoHydro",
"pi_ET","pi_Gast","pi_Hydro","pi_Hydro2EPT","pi_Hydro2Trich","pi_Insect","pi_Isop","pi_IsopGastHiru","pi_Juga",
"pi_JugaFlumi","pi_Mega","pi_Mol","pi_Nemata","pi_NonIns","pi_Odon","pi_OET","pi_Oligo","pi_Pleco","pi_POET",
"pi_Sphaer","pi_SphaerCorb","pi_Trich","pi_TrichNoHydro","pi_Tromb","pt_Amph","pt_Bival","pt_COET","pt_Coleo",
"pt_Deca","pt_Dipt","pt_ECT","pt_Ephem","pt_EPT","pt_ET","pt_Gast","pt_Insect","pt_Isop","pt_Mega","pt_NonIns",
"pt_Odon","pt_OET","pt_Oligo","pt_Pleco","pt_POET","pt_Trich","pt_TrichNoHydro","pt_Tromb","nt_Chiro","pi_Chiro",
"pt_Chiro","pi_Ortho","pi_Tanyt","pi_Tanyp","pi_COC2Chi","pi_ChCr2Chi","pi_Orth2Chi","pi_Tanyp2Chi","pi_ChiroAnne",
"pi_SimBtri","pi_Colesens","nt_ti_stenocold","nt_ti_cold","nt_ti_cool","nt_ti_warm",
"nt_ti_stenowarm","nt_ti_eury","nt_ti_cowa","nt_ti_stenocold_cold","nt_ti_stenocold_cold_cool","nt_ti_cowa_warm_stenowarm",
"nt_ti_warm_stenowarm","pi_ti_stenocold","pi_ti_cold","pi_ti_cool","pi_ti_warm","pi_ti_stenowarm","pi_ti_eury",
"pi_ti_cowa","pi_ti_stenocold_cold","pi_ti_stenocold_cold_cool","pi_ti_cowa_warm_stenowarm","pi_ti_warm_stenowarm",
"pt_ti_stenocold","pt_ti_cold","pt_ti_cool","pt_ti_warm","pt_ti_stenowarm","pt_ti_eury","pt_ti_cowa","pt_ti_stenocold_cold",
"pt_ti_stenocold_cold_cool","pt_ti_cowa_warm_stenowarm","pt_ti_warm_stenowarm","ri_ti_sccc_wsw","nt_tv_intol","nt_tv_intol4",
"nt_tv_toler","pi_tv_intol","pi_tv_intol4","pi_tv_toler","pi_tv_toler6","pt_tv_intol","pt_tv_intol4","pt_tv_toler",
"nt_tv_intol4_EPT","nt_tv_ntol","nt_tv_stol","pi_tv_ntol","pi_tv_stol","pt_tv_ntol","pt_tv_stol","pi_tv2_intol",
"nt_ffg_col","nt_ffg_filt","nt_ffg_pred","nt_ffg_scrap","nt_ffg_shred","nt_ffg_mah","nt_ffg_omn","nt_ffg_par",
"nt_ffg_pih","nt_ffg_xyl","nt_ffg_pred_scrap_shred","pi_ffg_col","pi_ffg_filt","pi_ffg_pred","pi_ffg_scrap",
"pi_ffg_shred","pi_ffg_mah","pi_ffg_omn","pi_ffg_par","pi_ffg_pih","pi_ffg_xyl","pi_ffg_col_filt","pt_ffg_col",
"pt_ffg_filt","pt_ffg_pred","pt_ffg_scrap","pt_ffg_shred","pt_ffg_mah","pt_ffg_omn","pt_ffg_par","pt_ffg_pih",
"pt_ffg_xyl","nt_habit_burrow","nt_habit_climb","nt_habit_climbcling","nt_habit_cling","nt_habit_sprawl","nt_habit_swim",
"pi_habit_burrow","pi_habit_climb","pi_habit_climbcling","pi_habit_cling","pi_habit_cling_PlecoNoCling","pi_habit_sprawl",
"pi_habit_swim","pt_habit_burrow","pt_habit_climb","pt_habit_climbcling","pt_habit_cling","pt_habit_sprawl","pt_habit_swim",
"nt_volt_multi","nt_volt_semi","nt_volt_uni","pi_volt_multi","pi_volt_semi","pi_volt_uni","pt_volt_multi","pt_volt_semi",
"pt_volt_uni","pi_dom01","pi_dom02","pi_dom03","pi_dom04","pi_dom05","x_Becks","x_Becks3","x_HBI","x_HBI2","x_Shan_e",
"x_Shan_2","x_Shan_10","x_D","x_D_G","x_D_Mg","x_Evenness","nt_habitat_brac","nt_habitat_depo","nt_habitat_gene",
"nt_habitat_head","nt_habitat_rheo","nt_habitat_rive","nt_habitat_spec","pi_habitat_brac","pi_habitat_depo",
"pi_habitat_gene","pi_habitat_head","pi_habitat_rheo","pi_habitat_rive","pi_habitat_spec","pi_habitat_unkn","pt_habitat_brac",
"pt_habitat_depo","pt_habitat_gene","pt_habitat_head","pt_habitat_rheo","pt_habitat_rive","pt_habitat_spec","nfam_Coleo",
"nfam_Ephem","nfam_Odon","nfam_Trich","ngen_Coleo","ngen_Ephem","ngen_Odon","ngen_Trich","ngen_Elmid","nt_oneind",
"pt_oneind","nt_dni","pi_dni","pt_dni","ni_Dipt")

        
          
bug.metrics <- metric.values(bugs.excluded, "bugs", fun.MetricNames = mets.keep) #, fun.cols2keep = "ReferenceSite"

 
# add ReferenceSite back in
ref.status <- bugs_ref.most %>%
  select(act_id, MLocID, ReferenceSite) %>%
  rename(SAMPLEID = act_id) %>%
  unique()

      

bug.metrics_ref.status <- bug.metrics %>%
  left_join(ref.status, by = 'SAMPLEID') %>%
  relocate(MLocID, .before = 2)%>%
  relocate(ReferenceSite, .before = 3)

  
                                                                                  #######
                                                                                    #########  METRICS VALIDATION 
                                                                                  #######
                                                                                    
                                                                                  # this is our first use of BioMonTools to calculate metrics
                                                                                  # take metrics and bugs.excluded into excel and explore to see the metrics are working correctly
                                                                                    
                                                                                 # write.xlsx(bugs.excluded, 'bugs analyses/MMI/_2024 model build/Verify BioMonTools metrics/bugs.excluded.xlsx')
                                                                                    
                                                                                 # write.xlsx(bug.metrics, 'bugs analyses/MMI/_2024 model build/Verify BioMonTools metrics/bug.metrics.xlsx')
                                                                                  
                                                                                    
  
  
  
##### Limit all samples to at least 200 count, same as used in O/E models


bug.metrics_ref.status <- bug.metrics_ref.status %>%
  filter(ni_total > 199)
  
  # 414 sites total: 158 most, 256 reference
  
  
  
  
  
  
  
    
  
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
                       inorgnwetdep_2008,k2o,kffact,mast_2008,mast_2009,mast_2013,mast_2014,
                       msst_2008,msst_2009,msst_2013,msst_2014,mwst_2008,mwst_2009,mwst_2013,mwst_2014,  
                       mgo,n,na2o,nh4_2008,no3_2008,om,p2o5,pctalkintruvol,pctalluvcoast,pctbl2001,
                       pctbl2004,pctcarbresid,pctcoastcrs,pctcolluvsed,pcteolcrs,pcteolfine,pctextruvol,
                       pctglaclakecrs,pctglaclakefine,pctglactilclay,pctglactilcrs,pctglactilloam,
                       pcthydric,pctice2001,pctice2004,pctice2006,pctice2008,pctice2011,
                       pctice2013,pctice2016,pctice2019,pctsallake,perm,precip08,precip09,
                       precip8110,rckdep,s,sand,sio2,tmax8110,tmean8110,tmin8110', 
                       aoi='watershed, other', region='16,17,18')

# convert multiple years to means
metrics_WS.OTHER_OR.CA.NV <- metrics_WS.OTHER_OR.CA.NV %>%
  mutate(MAST_mean08.14 = (MAST_2008+MAST_2009+MAST_2013+MAST_2014)/4) %>%
  mutate(MSST_mean08.14 = (MSST_2008+MSST_2009+MSST_2013+MSST_2014)/4) %>% 
  mutate(MWST_mean08.14 = (MWST_2008+MWST_2009+MWST_2013+MWST_2014)/4) %>%
  mutate(PCTICE_mean01.19 = (PCTICE2001WS+PCTICE2004WS+PCTICE2006WS+PCTICE2008WS+PCTICE2011WS+
                               PCTICE2013WS+PCTICE2016WS+PCTICE2019WS)/8) %>%
  select(-MAST_2008, -MAST_2009,-MAST_2013,-MAST_2014,-MSST_2008,-MSST_2009,
         -MSST_2013,-MSST_2014,-MWST_2008,-MWST_2009,-MWST_2013,-MWST_2014, 
         -PCTICE2001WS,-PCTICE2004WS,-PCTICE2006WS,-PCTICE2008WS,-PCTICE2011WS,
         -PCTICE2013WS,-PCTICE2016WS,-PCTICE2019WS)


# Catchment + Other (stream temp mets)

metrics_CAT.OTHER_OR.CA.NV <- sc_get_data(metric='al2o3,bfi,cao,clay,compstrgth,elev,fe2o3,hydrlcond,
                       inorgnwetdep_2008,k2o,kffact,mast_2008,mast_2009,mast_2013,mast_2014,
                       msst_2008,msst_2009,msst_2013,msst_2014,mwst_2008,mwst_2009,mwst_2013,mwst_2014,  
                       mgo,n,na2o,nh4_2008,no3_2008,om,p2o5,pctalkintruvol,pctalluvcoast,pctbl2001,
                       pctbl2004,pctcarbresid,pctcoastcrs,pctcolluvsed,pcteolcrs,pcteolfine,pctextruvol,
                       pctglaclakecrs,pctglaclakefine,pctglactilclay,pctglactilcrs,pctglactilloam,
                       pcthydric,pctice2001,pctice2004,pctice2006,pctice2008,pctice2011,
                       pctice2013,pctice2016,pctice2019,pctsallake,perm,precip08,precip09,
                       precip8110,rckdep,s,sand,sio2,tmax8110,tmean8110,tmin8110', 
                       aoi='catchment, other', region='16,17,18')


# convert multiple years to means
metrics_CAT.OTHER_OR.CA.NV <- metrics_CAT.OTHER_OR.CA.NV %>%
  mutate(MAST_mean08.14 = (MAST_2008+MAST_2009+MAST_2013+MAST_2014)/4) %>%
  mutate(MSST_mean08.14 = (MSST_2008+MSST_2009+MSST_2013+MSST_2014)/4) %>% 
  mutate(MWST_mean08.14 = (MWST_2008+MWST_2009+MWST_2013+MWST_2014)/4) %>%
  mutate(PCTICE_mean01.19 = (PCTICE2001CAT+PCTICE2004CAT+PCTICE2006CAT+PCTICE2008CAT+PCTICE2011CAT+
          PCTICE2013CAT+PCTICE2016CAT+PCTICE2019CAT)/8) %>%
  select(-MAST_2008, -MAST_2009,-MAST_2013,-MAST_2014,-MSST_2008,-MSST_2009,
         -MSST_2013,-MSST_2014,-MWST_2008,-MWST_2009,-MWST_2013,-MWST_2014, 
         -PCTICE2001CAT,-PCTICE2004CAT,-PCTICE2006CAT,-PCTICE2008CAT,-PCTICE2011CAT,
           -PCTICE2013CAT,-PCTICE2016CAT,-PCTICE2019CAT, -WSAREASQKM)

# Convert COMID to character to allow join with COMIDs in our excel files
metrics_WS.OTHER_OR.CA.NV$COMID <- as.character(metrics_WS.OTHER_OR.CA.NV$COMID)
metrics_CAT.OTHER_OR.CA.NV$COMID <- as.character(metrics_CAT.OTHER_OR.CA.NV$COMID)


##
#   save streamcat metrics for loading in other applications
##
save(metrics_WS.OTHER_OR.CA.NV, file = 'bugs analyses/RIVPACS_2022/_2024 model build/metrics_WS.OTHER_OR.CA.NV.Rdta' )
save(metrics_CAT.OTHER_OR.CA.NV, file = 'bugs analyses/RIVPACS_2022/_2024 model build/metrics_CAT.OTHER_OR.CA.NV.Rdta' )



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
             PCTBL2004 = PCTBL2004WS, INORGNWETDEP_2008 = INORGNWETDEP_2008WS,
             NO3_2008 = NO3_2008WS, NH4_2008 = NH4_2008WS, N = NWS,
             HYDRLCOND = HYDRLCONDWS, MGO = MGOWS, K2O = K2OWS, AL2O3 = AL2O3WS,
             NA2O = NA2OWS, SIO2 = SIO2WS, CAO = CAOWS, P2O5 = P2O5WS,             
             S = SWS, FE2O3 = FE2O3WS, PERM = PERMWS, RCKDEP = RCKDEPWS, OM = OMWS,
             PCTBL2001 = PCTBL2001WS, PRECIP08 = PRECIP08WS, PRECIP09 = PRECIP09WS )    

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
         PCTBL2004 = PCTBL2004CAT, INORGNWETDEP_2008 = INORGNWETDEP_2008CAT,
         NO3_2008 = NO3_2008CAT, NH4_2008 = NH4_2008CAT, N = NCAT,
         HYDRLCOND = HYDRLCONDCAT, MGO = MGOCAT, K2O = K2OCAT, AL2O3 = AL2O3CAT,
         NA2O = NA2OCAT, SIO2 = SIO2CAT, CAO = CAOCAT, P2O5 = P2O5CAT,             
         S = SCAT, FE2O3 = FE2O3CAT, PERM = PERMCAT, RCKDEP = RCKDEPCAT, OM = OMCAT,
         PCTBL2001 = PCTBL2001CAT, PRECIP08 = PRECIP08CAT, PRECIP09 = PRECIP09CAT )    

pred.mets_257 <- bind_rows(samps.257_WS_mets, samps.257_CAT_mets)


# predictors together for all 265 sites

summary(pred.mets_257)
      # most metrics with 4-5 missing values--this is good enough to replace with imputation
      # however, MAST (mean annual stream temp) has 37 missing values =---> need to drop this variable
      # variables with very little spread = drop
              # PCTGLACLAKEFINE, PCTGLACTILCLAY, PCTGLACTILLOAM, PCTCOASTCRS, PCTGLACLAKECRS, PCTEXTRUVOL, PCTEOLFINE,PCTEOLCRS, PCTSALLAKE, PCTHYDRIC,

pred.mets_257 <- pred.mets_265 %>% select(-MAST_mean08.14, -PCTGLACLAKEFINE, -PCTGLACTILCLAY, -PCTGLACTILLOAM, -PCTCOASTCRS, -PCTGLACLAKECRS, 
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
  select(-TMIN8110, -TMEAN8110, -PRECIP08, PRECIP09, -PCTBL2001, -NO3_2008, -NH4_2008, -AL2O3)


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

save(pred.mets_257_final, file='bugs analyses/RIVPACS_2022/_2024 model build/pred.mets_257_final.Rdata')









