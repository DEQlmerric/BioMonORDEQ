
# This function will ru the O:E assessment for the biocriteria assessments. The actual model is run in teh function 
# model.predict.RanFor.4.2(). The rest of this function preps data to go into that function. The outputs are a list
# containing 2 items: 1) A dataframe for O:E scores and 2) a dataframe containing monitoring locations that did not get
# assessed due to some streamcat attributes missing in the datapull. 


OE_modelrun <- function(df_bugs){

  
  library(cluster); 
  library(Hmisc);
  library(randomForest);
  library(maps)
  library(dendextend)
  library(VSURF)
  library(tidyr)
  library(RColorBrewer)   
  library(tidyverse)
  library(openxlsx)
  
  #testing
  
  # df_bugs <- bug_tax_data_filtered

  
  
  

# Random subsample ------------------------------------------------------------------------------------------------

  
  source('bugs analyses/z_SLH_explore sample size/rand_subsample_varies.R')
  
  df_rand <- random_subsample.var(df_bugs, OTU_col = OTU_RIV_24)
  
  
  
  
  #####
  #####
  #       1.3 get total abundance of rarified samples for PREDATOR
  ######
  ######
  
  tot.abund_raw.bugs<-aggregate(df_rand$Count, list(Sample=df_rand$Sample, MLocID=df_rand$MLocID), sum)
  tot.abund_raw.bugs <- dplyr::rename(tot.abund_raw.bugs, tot.abund_raw.bugs = x)
  
  
  dm.rare <- tidyr::pivot_longer(df_rand, Count, names_to = "variable", values_to = "value" )
  
  
  #This is the bug data to be used
  bugs.mat_raw.bugs <- dm.rare %>% pivot_wider(               # new tidy method, replacing dcast (data.table)
    names_from = OTU,
    id_cols = c(Sample),
    values_from = value,
    values_fill = 0,
    names_repair = "check_unique",
    names_sort = TRUE)
  
  
  
  
  # Get streamcat data ----------------------------------------------------------------------------------------------
  
  

  
  print("Begin Streamcat datapull")
  
  source('bugs analyses/All_together/get_streamcat.R')
  
  
  # Get list of COMIDs and remove blanks
  comidID <- unique(df_bugs$COMID)
  
  comidID <- comidID[!is.na(comidID)]
  
  # Get OE streamcat metrics at identifiyed comids
  
  streamcat <- get_streamcat(comids = comidID, type = "OE" )
  
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
  
  
  streamcat_mloc_data <- actid_streamcat |> 
    filter(!is.na(WSAREASQKM)) |> 
    mutate(TMAX8110 = case_when(str_detect(QC_Comm, "Used closest COMID") ~ TMAX8110CAT,
                                TRUE ~ TMAX8110WS),
           BFI = case_when(str_detect(QC_Comm, "Used closest COMID") ~ BFICAT,
                           TRUE ~ BFIWS),
           ELEV = case_when(str_detect(QC_Comm, "Used closest COMID") ~ ELEVCAT,
                            TRUE ~ ELEVWS),
           MWST_mean08.14 =  (MWST2008+MWST2009+MWST2013+MWST2014)/4,
           CLAY = case_when(str_detect(QC_Comm, "Used closest COMID") ~ CLAYCAT,
                            TRUE ~ CLAYWS),
           PRECIP8110 = case_when(str_detect(QC_Comm, "Used closest COMID") ~ PRECIP8110CAT,
                                  TRUE ~ PRECIP8110WS),
    ) |> 
    select(act_id, TMAX8110, BFI, ELEV, MWST_mean08.14, CLAY, PRECIP8110)
  
  # drop samples with incomplete predictors
  
  preds_raw.bugs_mod <- streamcat_mloc_data[complete.cases(streamcat_mloc_data), ]
  
  # Export a table of comids with missing streamcats
  errors <- streamcat_mloc_data[!complete.cases(streamcat_mloc_data), ] |> 
    mutate(model_comment = "Missing streamcat features. No model run")
  
  print("End Streamcat datapull")
  
  #-------------------------------------------------------------------#
  #Step 5 - calculate OE scores for reference sites for chosen model;
  #-------------------------------------------------------------------#
  #######
  
  # match bug file and predictors file
  
  # subset the bug data to only the reference predictor data file for now 
  bugs.mat_raw.bugs_matching <- subset(bugs.mat_raw.bugs, Sample %in% preds_raw.bugs_mod$act_id) 
  
  preds_raw.bugs_mod <-subset(preds_raw.bugs_mod, act_id %in% bugs.mat_raw.bugs$Sample)
  
  
  bugs.mat_raw.bugs_matching<- bugs.mat_raw.bugs_matching %>% column_to_rownames(var='Sample')
  preds_raw.bugs_mod <- as.data.frame(preds_raw.bugs_mod)
  rownames(preds_raw.bugs_mod) <- preds_raw.bugs_mod[,1]  # tidy way wasn't working
  preds_raw.bugs_mod <- preds_raw.bugs_mod %>%  select(-act_id)
  
  
  
  bugs.mat_raw.bugs_matching= bugs.mat_raw.bugs_matching[order(rownames(bugs.mat_raw.bugs_matching)),];
  preds_raw.bugs_mod = preds_raw.bugs_mod[order(rownames(preds_raw.bugs_mod)),];
  
  
  # probably worth checking to make sure all these files have the same number of records...
  dim(bugs.mat_raw.bugs_matching)[1]
  dim(preds_raw.bugs_mod)
  summary(row.names(bugs.mat_raw.bugs_matching)==row.names(preds_raw.bugs_mod))
  
  
  
  
  
  # Model run -------------------------------------------------------------------------------------------------------
  
  
  print("Begin Model Run")
  # load the model to run
  load('bugs analyses/RIVPACS_2022/_2024 model build_REBUILD NEW OTUs/RIVPACS.2024__no.NBR_9grps_5preds.Rdata')
  #load('bugs analyses/RIVPACS_2022/_2024 model build/RIVPACS.2024__NoNBR_6out_8grps_6preds.Rdata')
  #LAM 11/25/2024 updated this to use the new 
  
  
  
  
  
  #Option 5.1 - Make predictions of E and O/E for calibration (reference) sites. Examine O/E statistics and plots;
  # To do this, run the model.predict.RanFor.4.2 function, using the calibration data as the 'new' data;
  # See Step 7 below, for more info on making predictions;
  # Also see internal documentation of model.predict.Ran.For.4.2;
  source("bugs analyses/RIVPACS_2022/_2024 model build/model.predict.RanFor.4.2.r");
  #source("bugs analyses/RIVPACS_2022/model.predict.RanFor.r");
  
  
  
  # run model with new dataset
  OE.AWQMS<-model.predict.RanFor.4.2(bugcal.pa,grps.final,preds.final, ranfor.mod=model.final,
                                     prednew=preds_raw.bugs_mod,
                                     bugnew=bugs.mat_raw.bugs_matching,
                                     Pc=0.5, Cal.OOB=FALSE); 
  
  
  
  test.RESULTS <- cbind(OE.AWQMS$OE.scores) |> 
    select(-Onull, -Enull, -OoverE.null, -BC.null)
  
  
  # get MLocID back into O/E results
  OE.raw_bugs <- test.RESULTS %>%
    rownames_to_column(var='act_id')
  
  
  bug_join <- df_bugs |> 
    select(org_id, Project1, AU_ID, MLocID, StationDes, MonLocType, EcoRegion3, EcoRegion4, 
           EcoRegion2, HUC8_Name, HUC12_Name, Lat_DD, Long_DD, Reachcode,Measure, ELEV_Ft, GNIS_Name, Conf_Score,
           COMID, QC_Comm,  ReferenceSite,  act_id, act_comments, Activity_Type, Result_Status,
           SampleStart_Date, SampleStart_Time, Sample_Media, Sample_Method, Assemblage) |> 
    distinct()
  
  
  
  test <-  rownames_to_column(preds_raw.bugs_mod, var='act_id')
  
  
  OE_Scores <- bug_join |> 
    left_join(  rownames_to_column(preds_raw.bugs_mod, var='act_id'),by = join_by(act_id)) |> 
    left_join(OE.raw_bugs, by = join_by(act_id))
  
  OE_Results <- list(OE_Scores = OE_Scores, 
                     missing_streamcat = errors)
  
return(OE_Results)

}

########



#This needs to be earlier in the process
# act.MLoc <- df_bugs %>%
#   select(act_id, MLocID,COMID, SampleStart_Date, Sample_Method, Char_Name) %>%
#   filter(SampleStart_Date > "1998-01-01") %>%
#   filter(Sample_Method %in% 'Benthic Kick - Riffle' | Sample_Method %in% 'Benthic Kick - Targeted Riffle'| Sample_Method %in% 'Benthic Kick - Transect') %>%
#   filter(Char_Name == 'Count') %>%
#   mutate(SampleStart_Date = lubridate::ymd(SampleStart_Date)) |> 
#   mutate( month = format(SampleStart_Date,"%m")) %>%
#   filter(month %in% '06' | month %in% '07' | month %in% '08' | month %in% '09' | month %in% '10') %>%
#   unique()


