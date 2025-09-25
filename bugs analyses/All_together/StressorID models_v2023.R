#
# Author: Shannon Hubler

# 12/15/2020

# Purpose: Current version = 2023 Weighted Averaging Models: BSTI adapted to R, MTTI created in R
      # 1) Query from AWQMS
      # 2) Prepare data for running Stressor ID models
      # 3) Align taxonomy to OTUs
      # 4) run models
      # 5) un-transform as needed

# 
# 
#

bug.stressors_run <- function(df_bugs){



library(tidyverse)
library(rioja)
library(randomForest)
#library(RODBC) 



####### 1.0 = MTTI

#####
#			bring in taxonomy lookup table 
#####


# install "BiomonTools" package

          #install if needed
         # if(!require(remotes)){install.packages("remotes")}  
         # install_github("leppott/BioMonTools", force=TRUE)

# Download first then read.
## URL BioMonTools
 
taxonomy_MTTI <- ORDEQBioassessment::taxonomy_leppo


# old csv copy locally: taxa <- read.csv('ORWA_TaxaTranslator_20230112_SLH.updated.csv') \

# 'DNI' = used to filter out and drop taxa not included in model,

# limit to necessary fields to void messy joins
taxa.otu <- taxonomy_MTTI %>%
  select(Taxon_orig, OTU_MTTI) #%>%




###

# Format  data

### 

                

bugs.MTTI <- bug_tax_data |> 
  mutate(Sample = act_id,
         Count = Result_Numeric) %>%
  filter(Char_Name == 'Count') %>% # need this to remove "density"
  filter(Result_Numeric > 0) # need to remove any records with 0 count
  
bugs.MTTI <- bugs.MTTI %>%
  select(Sample, Taxon, Count) 
 

bugs.MTTI <- bugs.MTTI %>%
  dplyr::rename(sample.id = Sample) %>%
  dplyr::rename(Taxon_orig = Taxon) #%>%
  #rename(Count = COUNTS)
  
  
# need relative abundances

abunds.MTTI <- bugs.MTTI %>% 			
  dplyr::group_by(sample.id) %>% 
  dplyr::summarize(tot.abund = sum(Count))

abunds.MTTI <- as.data.frame(abunds.MTTI)

bugs.MTTI <- bugs.MTTI %>%
  left_join(abunds.MTTI, by = 'sample.id')

bugs.RA <- bugs.MTTI %>%
  dplyr::group_by(sample.id, Taxon_orig) %>%
  dplyr::summarize(count = n(),
                   RA = (sum(Count)/first(tot.abund)))



#####
#####

#		join bugs and OTUs, filter out 'DNI' taxa, sum across OTUs within a sample

#####
#####

# join
bugs_otu <- bugs.RA %>%
  left_join(taxa.otu, by='Taxon_orig') %>% # join dataframes
  filter(OTU_MTTI != 'DNI')						# filter out DNI taxa





# sum RA's across all OTUs--should see a reduction in rows.  
# Also limits to the following: dataset (CAL/VAL/not), sample, OTU, (summed) RA

bugs_otu_sum.RA<-plyr::ddply(.data = bugs_otu, c('sample.id', 'OTU_MTTI'), 
                             plyr::summarize, RA=sum(RA))


#	Prepare data sets for modeling
#	need to crosstab the bug data (turn into a wide format) so that OTUs are columns
# then split into separate CAl and VAl datasets 

bugs.MTTI_cross <- bugs_otu_sum.RA %>% 
  pivot_wider(id_cols = c(sample.id), names_from = OTU_MTTI, values_from = RA,
              values_fn = sum) 

bugs.MTTI_cross[is.na(bugs.MTTI_cross)] <- 0 

bugs.MTTI_cross <-	column_to_rownames(bugs.MTTI_cross, 'sample.id') 

###

# MTTI calculations

###

#This will get saved to the ORDEQBioassessment package
# load MTTI model
load('bugs analyses/stressor/wa_MTTI.mar23.Rdata')


# make predictions for  data


wa_MTTI <- predict(wa_MTTI.mar23, newdata=bugs.MTTI_cross) #, sse=TRUE, nboot=100, match.data=TRUE, verbose=TRUE)

MTTI <- as.data.frame(wa_MTTI$fit)

MTTI <- MTTI %>%
  select(WA.cla.tol)

MTTI$Sample <- row.names(MTTI) 
rownames(MTTI) <- NULL 

MTTI <- MTTI %>%
  select(Sample, WA.cla.tol) %>%
  dplyr::rename(MTTI = WA.cla.tol) 
  #dplyr::rename(act_id = Sample)


# rescale score to min and max of calibration datasets
  # MTTI CAL = 3.6 - 30.8
  # BSTI CAL = 0 - 100

MTTI$MTTI <- ifelse(MTTI$MTTI < 3.6, 3.6, MTTI$MTTI)
MTTI$MTTI <- ifelse(MTTI$MTTI > 30.8, 30.8, MTTI$MTTI)

### 7/31/2023 - moved this to the end of the BSTI section 
#BSTI$BSTI <- ifelse(BSTI$BSTI < 0, 0, BSTI$BSTI)
#BSTI$BSTI <- ifelse(BSTI$BSTI > 100, 100, BSTI$BSTI)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Get streamcat data (for "Expected") ----------------------------------------------------------------------------------------------


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


print("Begin Streamcat datapull")

source('bugs analyses/All_together/get_streamcat.R')


# Get list of COMIDs and remove blanks
comidID <- unique(df_bugs$COMID)

comidID <- comidID[!is.na(comidID)]

# Get OE streamcat metrics at identifiyed comids

streamcat <- get_streamcat(comids = comidID, type = "Stress" )

#Get a list of unique activity ID and comids
actids <- df_bugs |> 
  select(act_id,  COMID, QC_Comm) |> 
  unique()


#Join steramcat data to the activity IDS
actid_streamcat <- actids |> 
  left_join(streamcat, by = join_by(COMID))


#Produce a list of errors- used in development
streamcat_errors <- actid_streamcat |> 
  filter(is.na(WSAREASQKM))


# Remove errors
# alternate comids get catchment
# exact comids get watershed
# calculate MWST_mean08.14

# ----------------> these are the final predictors for MTTI: MSST_mean08.14+clayws+tmax9120ws+bfiws+kffactws+Latitude+precip9120ws+elevws

streamcat_mloc_data <- actid_streamcat |> 
  filter(!is.na(WSAREASQKM)) |> 
  mutate(MSST_mean08.14 =  (MSST2008 + MSST2009 + MSST2013 + MSST2014)/4,
         CLAYWS = case_when(str_detect(QC_Comm, "Used closest COMID") ~ CLAYCAT,
                          TRUE ~ CLAYWS),
         TMAX9120WS = case_when(str_detect(QC_Comm, "Used closest COMID") ~ TMAX9120CAT,
                              TRUE ~ TMAX9120WS),
         BFIWS = case_when(str_detect(QC_Comm, "Used closest COMID") ~ BFICAT,
                         TRUE ~ BFIWS),
         KFFACTWS = case_when(str_detect(QC_Comm, "Used closest COMID") ~ KFFACTCAT,
                          TRUE ~ KFFACTWS),
         PRECIP9120WS = case_when(str_detect(QC_Comm, "Used closest COMID") ~ PRECIP9120CAT,
                            TRUE ~ PRECIP9120WS),
         ELEVWS = case_when(str_detect(QC_Comm, "Used closest COMID") ~ ELEVCAT,
                                TRUE ~ ELEVWS),
         
  ) |> 
  select(act_id, MSST_mean08.14, clayws = CLAYWS, tmax9120ws = TMAX9120WS, 
         bfiws = BFIWS, kffactws = KFFACTWS, precip9120ws = PRECIP9120WS, elevws = ELEVWS)



# need latitude as a predictor
LAT <- sample_info.un %>% select(act_id, Latitude = Lat_DD)



streamcat_mloc_data <- streamcat_mloc_data %>%
  left_join(LAT, by = 'act_id')



# drop samples with incomplete predictors

preds_mtti.rf <- streamcat_mloc_data[complete.cases(streamcat_mloc_data), ]

# Export a table of comids with missing streamcats
errors <- streamcat_mloc_data[!complete.cases(streamcat_mloc_data), ] |> 
  mutate(model_comment = "Missing streamcat features. No model run")

print("End Streamcat datapull")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Calculate Expected from saved RF model ----------------------------------------------------------------------------------------------


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



load('bugs analyses/stressor/reference expectations/rf.mtti.reduced.Rdata')

predict.mtti <- predict(rf.mtti.reduced, newdata=preds_mtti.rf, type="response",
                        norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE)
##
###

#  @@@ would be really nice to have this output contain act_ids to make a real join, rathe rthan just a paste

###
##


# combine back with act_id

MTTI.predicted <- preds_mtti.rf %>%
  select(Sample = act_id) %>%
  mutate(MTTI.pred = predict.mtti)


# join MTTI with MMTI.pred

MTTI_o.e <- MTTI %>% left_join(MTTI.predicted, by='Sample') %>%
  mutate(MTTI_o.e = MTTI/MTTI.pred)






####################################################################
# ####

#   2.0 = BSTI

# ####
######################################################################




#####
#			bring in taxonomy lookup table 
#####

                                    
                                    # install "BiomonTools" package
                                    
                                    #install if needed
                                    # if(!require(remotes)){install.packages("remotes")}  
                                    # install_github("leppott/BioMonTools", force=TRUE)
                                    
                                    # Download first then read.
                                    ## URL BioMonTools
              #                      url_bmt_base <- 'https://github.com/leppott/BioMonTools_SupportFiles/raw/main/data'
              #                      url_taxa_official_pick <- file.path(url_bmt_base, "taxa_official", "ORWA_TaxaTranslator_20230228.csv")
              #                      taxonomy_MTTI <- read.csv(url_taxa_official_pick)
                                    # old csv copy locally: taxa <- read.csv('ORWA_TaxaTranslator_20230112_SLH.updated.csv') \
                                    
                                    # 'DNI' = used to filter out and drop taxa not included in model,
                                    
                                    # limit to necessary fields to void messy joins
                #                    taxa.otu <- taxonomy_MTTI %>%
                #                      select(Taxon_orig, OTU_MTTI) #%>%
                                    
taxonomy_BSTI <- ORDEQBioassessment::taxonomy_leppo


# old csv copy locally: taxa <- read.csv('ORWA_TaxaTranslator_20230112_SLH.updated.csv') \

# 'DNI' = used to filter out and drop taxa not included in model,

# limit to necessary fields to void messy joins
taxa.otu.bsti <- taxonomy_BSTI %>%
  select(Taxon_orig, OTU_BSTI) #%>%



###

# Format  data

### 




bugs.BSTI <- bug_tax_data|> 
  mutate(Sample = act_id,
         Count = Result_Numeric)%>%
  filter(Char_Name == 'Count') %>% # need this to remove "density"
  filter(Result_Numeric > 0) # need to remove any records with 0 count

bugs.BSTI <- bugs.BSTI %>%
  select(Sample, Taxon, Count) 


bugs.BSTI <- bugs.BSTI %>%
  dplyr::rename(sample.id = Sample) %>%
  dplyr::rename(Taxon_orig = Taxon) #%>%
#rename(Count = COUNTS)

  
# need relative abundances

abunds.BSTI <- bugs.BSTI %>% 			
  dplyr::group_by(sample.id) %>% 
  dplyr::summarize(tot.abund = sum(Count))

abunds.BSTI <- as.data.frame(abunds.BSTI)

bugs.BSTI <- bugs.BSTI %>%
  left_join(abunds.BSTI, by = 'sample.id')

bugs.RA <- bugs.BSTI %>%
  dplyr::group_by(sample.id, Taxon_orig) %>%
  dplyr::summarize(RA = (sum(Count)/first(tot.abund)))



#####
#####

#		join bugs and OTUs, filter out 'DNI' taxa, sum across OTUs within a sample

#####
#####

# join
bugs_otu <- bugs.RA %>%
  left_join(taxa.otu.bsti, by='Taxon_orig') %>% # join dataframes
  filter(OTU_BSTI != 'DNI')						# filter out DNI taxa





# sum RA's across all OTUs--should see a reduction in rows.  
# Also limits to the following: dataset (CAL/VAL/not), sample, OTU, (summed) RA

bugs_otu_sum.RA<-plyr::ddply(.data = bugs_otu, c('sample.id', 'OTU_BSTI'), 
                             plyr::summarize, RA=sum(RA))


#	Prepare data sets for modeling
#	need to crosstab the bug data (turn into a wide format) so that OTUs are columns
# then split into separate CAl and VAl datasets 

bugs.BSTI_cross <- bugs_otu_sum.RA %>% 
  pivot_wider(id_cols = c(sample.id), names_from = OTU_BSTI, values_from = RA,
              values_fn = sum) 

bugs.BSTI_cross[is.na(bugs.BSTI_cross)] <- 0 

bugs.BSTI_cross <-	column_to_rownames(bugs.BSTI_cross, 'sample.id') 

###

# BSTI calculations

###


# load BSTI model
load('bugs analyses/stressor/wa_BSTI.v2_apr23.Rdata')

# make predictions for  data


wa_BSTI <- predict(wa_BSTI.v2_apr23, newdata=bugs.BSTI_cross) #, sse=TRUE, nboot=100, match.data=TRUE, verbose=TRUE)

BSTI <- as.data.frame(wa_BSTI$fit)

BSTI <- BSTI %>%
  select(WA.cla.tol)

BSTI$Sample <- row.names(BSTI) 
rownames(BSTI) <- NULL 

BSTI <- BSTI %>%
  select(Sample, WA.cla.tol) %>%
  dplyr::rename(BSTI = WA.cla.tol)


# return final products
tot.abund.MTTI <- abunds.MTTI %>%
  rename(Sample = sample.id)

tot.abund.BSTI <- abunds.BSTI %>%
  rename(Sample = sample.id)

BSTI$BSTI <- ifelse(BSTI$BSTI < 0, 0, BSTI$BSTI)
BSTI$BSTI <- ifelse(BSTI$BSTI > 100, 100, BSTI$BSTI)

stressorID <- MTTI_o.e %>%
  left_join(BSTI, by = 'Sample') %>%
  left_join(tot.abund.MTTI, by = 'Sample')%>%
  left_join(tot.abund.BSTI, by = 'Sample')

#stressorID <- list('MTTI' = MTTI,
#                   'BSTI' = BSTI, 
#                   'tot.abund.MTTI' = tot.abund.MTTI,
#                   'tot.abund.BSTI' = tot.abund.BSTI)


return(stressorID)


}