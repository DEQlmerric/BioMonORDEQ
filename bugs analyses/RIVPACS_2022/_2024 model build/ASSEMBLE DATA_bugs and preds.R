#
#
#   Author: SLH 2.20.24
#
#   Purpose: assemble bug and predictor data for building new O/E models
#             once the two datasets are complete, bring them into th O/E model build R code to generate models


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
  
ref_265 <- read.xlsx('bugs analyses/RIVPACS_2022/_2024 model build/Reference sites_bug samples_total and OTU abundances.xlsx',
                          sheet = 'FINAL FINAL FINAL_265 ref samps')  

ref_265 <- ref_265 %>%
                    filter(Use_spatial=='Y')

# create a list of sampleIDs to use in querying bug data

sampleIDs.ref265 <- as.vector(ref_265$act_id)


# bring in saved bug data, downloaded from AWQMS in early February by LAM
      # AWQMS pulls haven't been working, this was a temporary work-around, suitable for model building but not permament
load('bugs analyses/RIVPACS_2022/_2024 model build/raw_bugs.Rdata') 


# filter raw_bugs to only the 265 ref sites

ref.bugs_265 <- raw_bugs %>%
            filter(act_id %in% sampleIDs.ref265) %>%
            filter(Char_Name=='Count') %>%
            select(act_id, MLocID, StationDes, COMID, EcoRegion2, EcoRegion3, Lat_DD, Long_DD, ELEV_Ft, 
                   SampleStart_Date, Sample_Method, DEQ_Taxon, Taxonomic_Name, Result_Numeric, StageID, UniqueTaxon)

# join to taxonomy and OTUs
taxonomy <- read.xlsx('bugs analyses/Taxonomy/ODEQ_Taxonomy_dec22.xlsx') 

taxonomy.otu <- taxonomy %>%
              select(DEQ_Taxon = DEQ_TAXON, Taxon, OTU_RIV_24)

taxonomy.otu$DEQ_Taxon <- as.character(taxonomy.otu$DEQ_Taxon)


ref.bugs.taxonomy <- ref.bugs_265 %>%
  left_join(taxonomy.otu, by='DEQ_Taxon')

# sum abundances across OTUs, drop 'DNI' taxa

ref.bugs_265_OTUs <- as.data.frame(ref.bugs.taxonomy %>%
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


b.rare.seed <- rarify.seed(na.omit(ref.bugs_265_OTUs), 'Sample', 'Count', 300) 



#####
#####
#       1.3 get total abundance of rarified samples for PREDATOR
######
######
tot.abund_notsub_265<-aggregate(ref.bugs_265_OTUs$Count, list(Sample=ref.bugs_265_OTUs$Sample, MLocID=ref.bugs_265_OTUs$MLocID), sum)
tot.abund_notsub_265 <- dplyr::rename(tot.abund_notsub_265, tot.abund_notsub_265 = x)


tot.abund.sub265<-aggregate(b.rare.seed$Count, list(Sample=b.rare.seed$Sample, MLocID=b.rare.seed$MLocID), sum)
tot.abund.sub265 <- dplyr::rename(tot.abund.sub265, tot.abund.sub265 = x)

#####
#####
#       1.4 Matrify: convert from long format (many rows per sample), to site X species matrix (one row per sample)
######
######

dm.rare <- tidyr::pivot_longer(b.rare.seed, Count, names_to = "variable", values_to = "value" )

# data.table::setDT(dm.rare)
# bugs.all <- data.table::dcast(dm.rare, Sample+MLocID+Eco2+Eco3 ~ OTU, fun.aggregate = sum) # Sample x OTU doesn't seem to be unique so you need the fun.aggregate.  
# head(bugs.all)

bugs.mat_265.ref <- dm.rare %>% pivot_wider(               # new tidy method, replacing dcast (data.table)
  names_from = OTU,
  id_cols = c(Sample),
  values_from = value,
  values_fill = 0,
  names_repair = "check_unique",
  names_sort = TRUE)

# export matrified bug data file to load directly in model building phase

save(bugs.mat_265.ref, file='bugs analyses/RIVPACS_2022/_2024 model build/bugs.mat_265.ref.Rdata')
  

  
  
  
  
  
###########

# StreamCat predictors

###########

region_params <- sc_get_params(param='areaOfInterest')
name_params <- sc_get_params(param='name')
name_params <- sort(name_params)  


# StreamCat natural metrics for model exploration

predictor.metrics <- c('al2o3,bfi,cao,clay,compstrgth,elev,fe2o3,hydrlcond,
                       inorgnwetdep_2008,k2o,kffact,mast_2008,mast_2009,mast_2013,mast_2014,
                       msst_2008,msst_2009,msst_2013,msst_2014,mwst_2008,mwst_2009,mwst_2013,mwst_2014,  
                       mgo,n,na2o,nh4_2008,no3_2008,om,p2o5,pctalkintruvol,pctalluvcoast,pctbl2001,
                       pctbl2004,pctcarbresid,pctcoastcrs,pctcolluvsed,pcteolcrs,pcteolfine,pctextruvol,
                       pctglaclakecrs,pctglaclakefine,pctglactilclay,pctglactilcrs,pctglactilloam,
                       pctgrs2001,pcthydric,pctice2001,pctice2004,pctice2006,pctice2008,pctice2011,
                       pctice2013,pctice2016,pctice2019,pctsallake,perm,precip08,precip09,
                       precip8110,rckdep,s,sand,sio2,tmax8110,tmean8110,tmin8110')  

    ###
      ### several of these (MAST, MSST, MWST, pctice, will need to be converted to a single average metric)
    ###



## Get data for the three Hydroregions covering: 17= OR+WA+ID, 16 = NV, 18 = CA

metrics <- sc_get_data(metric='al2o3,bfi,cao,clay,compstrgth,elev,fe2o3,hydrlcond,
                       inorgnwetdep_2008,k2o,kffact,mast_2008,mast_2009,mast_2013,mast_2014,
                       msst_2008,msst_2009,msst_2013,msst_2014,mwst_2008,mwst_2009,mwst_2013,mwst_2014,  
                       mgo,n,na2o,nh4_2008,no3_2008,om,p2o5,pctalkintruvol,pctalluvcoast,pctbl2001,
                       pctbl2004,pctcarbresid,pctcoastcrs,pctcolluvsed,pcteolcrs,pcteolfine,pctextruvol,
                       pctglaclakecrs,pctglaclakefine,pctglactilclay,pctglactilcrs,pctglactilloam,
                       pcthydric,pctice2001,pctice2004,pctice2006,pctice2008,pctice2011,
                       pctice2013,pctice2016,pctice2019,pctsallake,perm,precip08,precip09,
                       precip8110,rckdep,s,sand,sio2,tmax8110,tmean8110,tmin8110', 
                       aoi='watershed, other', region='16,17,18')



metrics <- metrics %>%
  mutate(MAST_mean08.14 = (MAST_2008+MAST_2009+MAST_2013+MAST_2014)/4) %>%
  mutate(MSST_mean08.14 = (MSST_2008+MSST_2009+MSST_2013+MSST_2014)/4) %>% 
  mutate(MWST_mean08.14 = (MWST_2008+MWST_2009+MWST_2013+MWST_2014)/4) %>%
  mutate(PCTICE_mean01.19 = (PCTICE2001WS+PCTICE2004WS+PCTICE2006WS+PCTICE2008WS+PCTICE2011WS+
          PCTICE2013WS+PCTICE2016WS+PCTICE2019WS)/8) %>%
  select(-MAST_2008, -MAST_2009,-MAST_2013,-MAST_2014,-MSST_2008,-MSST_2009,
         -MSST_2013,-MSST_2014,-MWST_2008,-MWST_2009,-MWST_2013,-MWST_2014, 
         -PCTICE2001WS,-PCTICE2004WS,-PCTICE2006WS,-PCTICE2008WS,-PCTICE2011WS,
           -PCTICE2013WS,-PCTICE2016WS,-PCTICE2019WS)


# get COMIDs from ref samples
comids <- ref.bugs_265 %>%
            select(act_id, COMID) %>%
            #group_by(act_id) %>%
            unique()


preds <- comids %>%
            left_join(metrics, by='COMID')
summary(preds)                                          



@@@@@@@@@@@@@@@ missing predictors due to missign COMIDs---how are we going to assign these to nearest CAT metrics values?
  then need to get CAT metrics into the same table for WS metrics





