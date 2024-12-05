library(tidyverse)
library(openxlsx)
library(StreamCatTools)
library(BioMonTools)
library(randomForest)


MMI_run <- function(df_bugs, df_sample, 
                    attribute_table_loc = 'https://raw.githubusercontent.com/leppott/BioMonTools_SupportFiles/main/data/taxa_official/ORWA_Attributes_20241121.csv')
{
  
# Testing ---------------------------------------------------------------------------------------------------------

 #df_bugs <- bug_tax_data_filtered
#  df_sample <-  sample_info
  
  

# Random subsample ------------------------------------------------------------------------------------------------

  source('bugs analyses/All_together/rand_subsample.R')
  
  df_rand <- random_subsample(df_bugs, OTU_col = OTU_MetricCalc)
  
  

  #Read in attribute table from BioMonTools_SupportFiles
  #this is the sheet that we were advised to use by Jen Stamp on 6/19/2024
  
  attribute_table <- read.csv(attribute_table_loc)

#Join subsetted data to the taxanomic attribute table from BioMonTools_SupportFiles
df_bugs_taxa <- df_rand |> 
  dplyr::left_join(select(sample_info, -MLocID),by = c('Sample' = 'act_id')) |> 
  dplyr::rename(TAXAID = OTU) |> 
  dplyr::left_join(attribute_table,
                   by = c('TAXAID' = 'Taxon'))

# We need slope and the corresponding INDEX_CLASS, 
# use the get_NHD_info function to get slope and SITE_TYPE
# rename and create a bunch of columns needed for metric calculation

source('bugs analyses/All_together/get_NHD_info.R')

bug_tax_nhd <- get_NHD_info(df_bugs_taxa) |> 
  dplyr::transmute(SampleID = Sample,
            Area_mi2 = NA_integer_,
            SurfaceArea = NA_integer_,
            TaxaID = TAXAID,
            N_Taxa = Count,
            slope,
            Index_Name ='MMI_metrics',
            INDEX_CLASS = str_to_title(SITE_TYPE),
            NonTarget,
            SITE_TYPE, 
            Kingdom,
            SubOrder,
            SubFamily,
            GenusGroup,
            SpeciesGroup,
            SpeciesSubGroup,
            SpeciesComplex,
            Phylum, 
            SubPhylum,
            Class, 
            SubClass,
            Order,
            SuperFamily,
            Family,
            Tribe,
            Genus,
            SubGenus, 
            Species,
            BCG_Attr  = BCG_attr,
            FFG,
            Habit,
            Life_Cycle,
            Thermal_Indicator = Thermal_indicator,
            TolVal,
            INFRAORDER = NA_character_,
            HABITAT = Habitat,
            ELEVATION_ATTR = NA_character_,
            GRADIENT_ATTR = NA_character_,
            WSAREA_ATTR = NA_character_,
            HABSTRUCT = NA_character_,
            UFC = NA_integer_,
            Density_ft2. = NA_integer_,
            DENSITY_M2 = NA_integer_,
            
  )



#use biomontools markExcluded function to generate the exclude column

bugs.excluded <- BioMonTools::markExcluded(bug_tax_nhd, TaxaLevels = c("Kingdom", "Phylum",
                                                                          "SubPhylum", "Class", "SubClass", "Order", "SubOrder", "SuperFamily", 
                                                                          "Family", "SubFamily", "Tribe", "GenusGroup", "Genus", "SubGenus", "SpeciesGroup",
                                                                          "SpeciesSubGroup", "SpeciesComplex", "Species"))





# Calculate metrics -----------------------------------------------------------------------------------------------


#These are the metrics to be used
mets.keep <- c('pt_tv_intol', 'nt_habitat_rheo', 'pt_ti_stenocold_cold_cool', 'pi_EPTNoHydro')


#Calculate metrics
# The boo.Shiny argument prevents the code from stopping and asking permission to calculate metrics with missing parameters.
# It will give a warning instead. 

# The function will return the metrics identified in mets.keep

metricsdf <- BioMonTools::metric.values(bugs.excluded, "bugs",fun.MetricNames = mets.keep, boo.Shiny	= TRUE)


# Predictors ------------------------------------------------------------------------------------------------------


## Streamcat -------------------------------------------------------------------------------------------------------


# Get list of COMIDs and remove blanks
comidID <- unique(df_sample$COMID)

comidID <- comidID[!is.na(comidID)]


source('bugs analyses/All_together/get_streamcat.R')
streamcat <- get_streamcat(comids = comidID, type = "MMI" )


actids <- df_sample |> 
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


streamcat_mloc_data <- actid_streamcat |> 
  filter(!is.na(WSAREASQKM)) |> 
  mutate(TMAX8110 = case_when(str_detect(QC_Comm, "Used closest COMID") ~ TMAX8110CAT,
                              TRUE ~ TMAX8110WS),
         CLAY = case_when(str_detect(QC_Comm, "Used closest COMID") ~ CLAYCAT,
                         TRUE ~ CLAYWS),
         OM = case_when(str_detect(QC_Comm, "Used closest COMID") ~ OMCAT,
                          TRUE ~ OMWS),
         KFFACT = case_when(str_detect(QC_Comm, "Used closest COMID") ~ KFFACTCAT,
                            TRUE ~ KFFACTWS),
         PRECIP8110 = case_when(str_detect(QC_Comm, "Used closest COMID") ~ PRECIP8110CAT,
                            TRUE ~ PRECIP8110WS),
         ELEV = case_when(str_detect(QC_Comm, "Used closest COMID") ~ ELEVCAT,
                                TRUE ~ ELEVWS),
         MSST_mean08.14 =  (MSST2008+MSST2009+MSST2013+MSST2014)/4,
         MWST_mean08.14 =  (MWST2008+MWST2009+MWST2013+MWST2014)/4,   # SLH 8.7.24 = this metric is new to the August run
         PERM = case_when(str_detect(QC_Comm, "Used closest COMID") ~ PERMCAT,
                          TRUE ~ PERMWS),
         P2O5 = case_when(str_detect(QC_Comm, "Used closest COMID") ~ P2O5CAT,
                          TRUE ~ P2O5WS),
         AREASQKM =  case_when(str_detect(QC_Comm, "Used closest COMID") ~ CATAREASQKM,
                               TRUE ~ WSAREASQKM),
  ) |> 
  select(act_id, TMAX8110, CLAY, OM, KFFACT, PRECIP8110, ELEV,MSST_mean08.14, MWST_mean08.14,PERM,P2O5, AREASQKM)


## slope -----------------------------------------------------------------------------------------------------------

#Get slope from nhd dataframe to add into predictors
slope_sample <- bug_tax_nhd |> 
  select(SampleID, slope) |> 
  rename(SLOPE = slope) |> 
  distinct()

#add slope to streamcat predictors
streamcat_mloc_data_slope <- streamcat_mloc_data |> 
  left_join(slope_sample, by = c('act_id' = 'SampleID'))


# Put everything together -----------------------------------------------------------------------------------------------


#Join metrics to streamcat predictors
Drfdat <-metricsdf |> 
  left_join(streamcat_mloc_data_slope, by = c('SAMPLEID' = 'act_id') ) |> 
  select(-INDEX_CLASS, -INDEX_NAME, -ni_total)


# RUN RANDOM.FORESTS MODELS - FOR EACH OF 4 METRICS ---------------------------------------------------------------


## Load models -----------------------------------------------------------------------------------------------------


load('bugs analyses/MMI/_2024 model build/rfmod_pt_tv_intol.Rdata' )
load('bugs analyses/MMI/_2024 model build/rfmod_nt_habitat_rheo.Rdata' )          
load('bugs analyses/MMI/_2024 model build/rfmod_pt_ti_stenocold_cold_cool.Rdata' )          
load('bugs analyses/MMI/_2024 model build/rfmod_pi_EPTNoHydro.Rdata' )                    


# which rf models to use
rfmodels <- c('rfmod_pt_tv_intol', 'rfmod_nt_habitat_rheo', 'rfmod_pt_ti_stenocold_cold_cool',
              'rfmod_pi_EPTNoHydro')



## test site predictions -------------------------------------------------------------------------------------------


Dpredictions=list()
for (i in 1:length(rfmodels)){
  tryCatch({Dpredictions[[paste0("E.",rfmodels[i])]]<- round(predict(eval(parse(text =paste0(rfmodels[i]))), Drfdat, type = "response"),digits=4)
  }, error =function (e){
    cat(paste0("/n/tERROR calculating: ",paste0(names(Drfdat)[i],"_pred"),"/n"))
    str(e,indent.str = "   "); cat("/n")
  })
}
predictionsdf=as.data.frame(do.call(cbind,Dpredictions))
#join predictions into master dataframe
Drfdat2=cbind(Drfdat,predictionsdf)



## CALCULATE RESIDUALS ---------------------------------------------------------------------------------------------



resid=list()
for (i in 2:5){
  tryCatch({resid[[paste0(colnames(Drfdat2)[i],"_resid")]]=Drfdat2[,i]- Drfdat2[,paste0("E.rfmod_",colnames(Drfdat2)[i])]
  
  }, error =function (e){
    cat(paste0("/n/tERROR calculating: ",paste0(Drfdat2[i],"_resid"),"/n"))
    str(e,indent.str = "   "); cat("/n")
  })
  
}



residualsdf=as.data.frame(do.call(cbind,resid))

rfdat_all_final4=cbind(Drfdat2,residualsdf)



# Select SAMPLEID + 4 metrics.residuals
candmetrics <- rfdat_all_final4 %>%
  select(SAMPLEID, pt_tv_intol_resid, nt_habitat_rheo_resid,
         pt_ti_stenocold_cold_cool_resid, pi_EPTNoHydro_resid)



## Rescale values --------------------------------------------------------------------------------------------------



# need to rescale based on percentiles of metrics
# in the final model, all 4 metrics have ref means > most disturbed means
# metric response = decrease with disturbance

# need to rescale based on percentiles of metrics
# in the final model, all 4 metrics have ref means > most disturbed means
# metric response = decrease with disturbance

                                                                                        # SLH 8.6.24 = these are the old (May 24) results
                                                                                        # here are the values to hard-code in:
                                                                                        # 
                                                                                        #                       REF_95th      MOST_5th
                                                                                        # pt_tv_intol           14.11435      -50.83933
                                                                                        # nt_habitat_rheo       9.185201      -23.714250
                                                                                        # pt_ti_steno...        14.89715      -39.30112
                                                                                        # pi_EPTNoHydro         24.32300      -54.54288

    

# SLH 8.6.24 = here are the values to hard-code in:
                                              # 
                                              #                       REF_95th      MOST_5th
                                              # pt_tv_intol           14.13465       -50.54222
                                              # nt_habitat_rheo       8.299599      -24.334100
                                              # pt_ti_steno...        15.15093      -38.35066
                                              # pi_EPTNoHydro         24.54683      -54.76710

# What we need: for each of the 4 metrics in "metrics", 
# first subtract the min from the metric, 
# second subtract min from the max, 
# then divide first by second
# RESULT = scaled between 0 -1


# SLH 8.6.24 = updated to new model results
metric_rs <- candmetrics |> 
  mutate(pt_tv_intol_resid = (pt_tv_intol_resid - -50.54222) /  (14.13465 -  -50.54222) ,
         nt_habitat_rheo_resid = (nt_habitat_rheo_resid - -24.334100) /  (8.299599 -  -24.334100) ,
         pt_ti_stenocold_cold_cool_resid = (pt_ti_stenocold_cold_cool_resid - -38.35066) /  (15.15093 -  -38.35066),
         pi_EPTNoHydro_resid = (pi_EPTNoHydro_resid - -54.76710) /  (24.54683 -  -54.76710)) %>%
  mutate(pt_tv_intol_resid = case_when(pt_tv_intol_resid <0 ~ 0,
                                       pt_tv_intol_resid >1 ~ 1,
                                       TRUE ~ pt_tv_intol_resid)) %>% 
  mutate(nt_habitat_rheo_resid = case_when(nt_habitat_rheo_resid <0 ~ 0,
                                           nt_habitat_rheo_resid >1 ~ 1,
                                           TRUE ~ nt_habitat_rheo_resid)) %>%  
  mutate(pt_ti_stenocold_cold_cool_resid = case_when(pt_ti_stenocold_cold_cool_resid <0 ~ 0,
                                                     pt_ti_stenocold_cold_cool_resid >1 ~ 1,
                                                     TRUE ~ pt_ti_stenocold_cold_cool_resid)) %>%  
  mutate( pi_EPTNoHydro_resid = case_when( pi_EPTNoHydro_resid <0 ~ 0,
                                           pi_EPTNoHydro_resid >1 ~ 1,
                                           TRUE ~  pi_EPTNoHydro_resid)) %>%  
  mutate(MMI = (pt_tv_intol_resid + nt_habitat_rheo_resid +
                       pt_ti_stenocold_cold_cool_resid+pi_EPTNoHydro_resid)  /4) |> 
  left_join(df_sample, by =c('SAMPLEID' = 'act_id')) |> 
  relocate(pt_tv_intol_resid,nt_habitat_rheo_resid,pt_ti_stenocold_cold_cool_resid,pi_EPTNoHydro_resid,MMI, .after = last_col())


#Throw MMI result and MMI metrics into a list for getting out of the function

MMI_list <- list(MMI_result = metric_rs,
                 MMI_metrics = metricsdf)

return(MMI_list)


}
