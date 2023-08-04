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

bug.stressors <- function(b_t_s = b_t_s){



library(tidyverse)
library(rioja)
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
url_bmt_base <- 'https://github.com/leppott/BioMonTools_SupportFiles/raw/main/data'
url_taxa_official_pick <- file.path(url_bmt_base, "taxa_official", "ORWA_TaxaTranslator_20230605.csv")
taxonomy_MTTI <- read.csv(url_taxa_official_pick)
# old csv copy locally: taxa <- read.csv('ORWA_TaxaTranslator_20230112_SLH.updated.csv') \

# 'DNI' = used to filter out and drop taxa not included in model,

# limit to necessary fields to void messy joins
taxa.otu <- taxonomy_MTTI %>%
  select(Taxon_orig, OTU_MTTI) #%>%




###

# Format  data

### 

                

bugs.MTTI <- b_t_s
  
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
  dplyr::summarize(RA = (Count/tot.abund))



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


# rescale score to min and max of calibration datasets
  # MTTI CAL = 3.6 - 30.8
  # BSTI CAL = 0 - 100

MTTI$MTTI <- ifelse(MTTI$MTTI < 3.6, 3.6, MTTI$MTTI)
MTTI$MTTI <- ifelse(MTTI$MTTI > 30.8, 30.8, MTTI$MTTI)

### 7/31/2023 - moved this to the end of the BSTI section 
#BSTI$BSTI <- ifelse(BSTI$BSTI < 0, 0, BSTI$BSTI)
#BSTI$BSTI <- ifelse(BSTI$BSTI > 100, 100, BSTI$BSTI)








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
                                    
 # interim until added to BiomonTools github                                   
taxa.otu <- read.csv('bugs analyses/stressor/OTU_bsti_v2.csv')				
                                    


###

# Format  data

### 




bugs.BSTI <- b_t_s

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
  dplyr::summarize(RA = (Count/tot.abund))



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

bugs_otu_sum.RA<-plyr::ddply(.data = bugs_otu, c('sample.id', 'OTU_BSTI.v2'), 
                             plyr::summarize, RA=sum(RA))


#	Prepare data sets for modeling
#	need to crosstab the bug data (turn into a wide format) so that OTUs are columns
# then split into separate CAl and VAl datasets 

bugs.BSTI_cross <- bugs_otu_sum.RA %>% 
  pivot_wider(id_cols = c(sample.id), names_from = OTU_BSTI.v2, values_from = RA,
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

stressorID <- list('MTTI' = MTTI,
                   'BSTI' = BSTI, 
                   'tot.abund.MTTI' = tot.abund.MTTI,
                   'tot.abund.BSTI' = tot.abund.BSTI)
return(stressorID)


}