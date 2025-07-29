#  _______   __                                                                                                         __     
# /       \ /  |                                                                                                       /  |    
# $$$$$$$  |$$/   ______   ______    _______  _______   ______    _______  _______  _____  ____    ______   _______   _$$ |_   
# $$ |__$$ |/  | /      \ /      \  /       |/       | /      \  /       |/       |/     \/    \  /      \ /       \ / $$   |  
# $$    $$< $$ |/$$$$$$  |$$$$$$  |/$$$$$$$//$$$$$$$/ /$$$$$$  |/$$$$$$$//$$$$$$$/ $$$$$$ $$$$  |/$$$$$$  |$$$$$$$  |$$$$$$/   
# $$$$$$$  |$$ |$$ |  $$ |/    $$ |$$      \$$      \ $$    $$ |$$      \$$      \ $$ | $$ | $$ |$$    $$ |$$ |  $$ |  $$ | __ 
# $$ |__$$ |$$ |$$ \__$$ /$$$$$$$ | $$$$$$  |$$$$$$  |$$$$$$$$/  $$$$$$  |$$$$$$  |$$ | $$ | $$ |$$$$$$$$/ $$ |  $$ |  $$ |/  |
# $$    $$/ $$ |$$    $$/$$    $$ |/     $$//     $$/ $$       |/     $$//     $$/ $$ | $$ | $$ |$$       |$$ |  $$ |  $$  $$/ 
# $$$$$$$/  $$/  $$$$$$/  $$$$$$$/ $$$$$$$/ $$$$$$$/   $$$$$$$/ $$$$$$$/ $$$$$$$/  $$/  $$/  $$/  $$$$$$$/ $$/   $$/    $$$$/ 
#
#
#Front end for running bioassessment score calculations
#
# . .... . . ..  . ... ..... . .. . . .   . ..  . .;.. .. . . . .     .  . . . . . .       . .  . ... 
# .... ...... ... .. .. . ..  . .... . ... .. ...:: . . . .  . . ... . . .  . . . . . .  .  .  . .  . 
# . .. . .  .. ... .. .. . ... . . .. . . . . ..;:...... .. . . . . . . . . . .   .    .  .  .    . .  
# .. ... .... . ... ... . . . . .  .. . . ... .+;..... .. . . . . . . . .      . . . . .. . . . . . . 
# ..... .. . .. . .. . ....... ....... . .  ..:+..... . .. . . . . .  .   . ...  .  . .  .       .  . 
# . . .. ..... ... ...... ......:++......... .+;........ .... . . . .. .. . .  . . . . . . . . . . ..
# .... ... . .. ..... ...:;:..++XXx:..... ...;x:.......... . . . . .. .  . . . .  . .  .  . .  .    . 
# .. ........ .... .........;x;+&$;::...:;XXx+.....:;:;+;++++;;;:::;;;;:. . . . . . . . . .   . . . . 
# ... ..............::::......+$X::::;$XXX+...:++x$$$+;.........:.:........  . . . . .  .   .     ...
# . ... . .........:+&&&&:...+$x:::+XX;::;;xXXXx;::::::::::::........... ..... . .. . .  ... .. .   . 
# .. ...........;XXX$&&&$$$:;$$+;;X&&$$$$X$XX+;;;;::::::.................... . .. .. .... . . . . . . 
# ........ ....x++$$&$&$$$$x&&$XX$&&$$$$$$X$$XX$$X+XX$Xx+X$$XxX$$$$$XXx+++;;.:..:::... . ... . .   ..  
# . .. . .....;++X&$$$$$$$$$$$$XX$$$XX$XX$$$XXX$$XxX$$$XXX$&$$&&&&&&&&&&&$&$$X$&&&$$$X+:. . . . . . . 
# ............:X+x$$$$&$$$$$&$$$X$&&$$$$$$$$$$$$$XxX$&$XX$$&$$&&&&&&&&&&&$&&$$$&&&&&&&&&&&X;::. . . . 
# .. ..........;$$X&&&&&$$$X&&&$$&&&&$$$$$$&$$&&&XXX$&$xx$&&$$&&&&&&&&&&&$&$$$$&&&&&$&&&&&$X;++++ . .
# .. .. .......:;;&&&&&$+;;+$$++X&&&$$$$$$XXxxXXx++xX+;++xX+++xXx++++;;;;;;::;x&&&&&&$$+:;:::... ....
# ...............:.::;;;;;:::+XX;;XXx++xX$XX++;;::::::::::::::::::::............::;+++xxXXX+:..... ..
# . .. ..........:..:.::....::XX$XX$&$XX&&$X$$$xX$&&X++;................................:.::::.. . ..
# ........ .....................:XXx;;;+x$$XX+:::;;::;:::x+:................................. ... ...
# . . ....... ...................:x+:::::;XX;:::...::::::;++:............ . .................   . . . 
# ........ ..... .................:x:::;xx::::::............:++:...... . .... .. . ... .. . . ... . . 
# . .... ........ . .............:x::++;:::::..................:;;;:..... . . .... . . .. . .   .  ..
# .. . ... . ...... . .... .. ...+;:+:::.......... . ... .. .........:::.. . . . . . .. . .. .. . . . 
# . .......... ... ..... .. ....:+:;:.............. . .. . .. .. .. .. . .... .   . .   .. .  . . . . 
# .. ... . . ... .. .  .........::........ .. . ........... . .... .. . .  . . .. .. ..  . . .    . .  
# ........ ...... .......  ......:...... .. .....  .. . . . .. .  . .. . . .. . .  . .  . .  .  .  .. 
# ... . ... .  ... . . ..... . .. .. . ... . .  ... . . .  . . .. .. .. . . . .. .    .. . . . . . ..
# . .... ...... ..... . .. .. . .. .. .. . .. ..  . .. . . . .. . . . . . . . . . ... . .   . . . . . 
#   




# Get data that will be shared by all components of bioassessment -------------------------------------------------
source('bugs analyses/All_together/Fetch_data.R')

bug_tax_data <- fetch_data(DEQ_taxonomy_table = 'bugs analyses/Taxonomy/ODEQ_Taxonomy_dec22.xlsx',
                           leppo_taxonomy_table_url = 'https://github.com/leppott/BioMonTools_SupportFiles/raw/refs/heads/main/data/taxa_official/ORWA/old/ORWA_TaxaTranslator_20240619.csv')


## Filter the original datapull ------------------------------------------------------------------------------------

bug_tax_data_filtered <- bug_tax_data |> 
  #filter(Result_Status != 'Rejected') |> ### 12/2 - LAM removed this for now - will apply later ? 
  filter(SampleStart_Date > "1997-01-01") %>% ### 12/2 - LAM modified this to 1997 -  SH used 97 and 98 data in the model builds 
  filter(Sample_Method %in% c('Benthic Kick - Riffle', 'Benthic Kick - Targeted Riffle', 'Benthic Kick - Transect','Benthic Kick - Mixed')) %>% # LAM added mixed for USU transect data inclusion
  filter(Char_Name == 'Count') %>%
  mutate(SampleStart_Date = lubridate::ymd(SampleStart_Date)) |> 
  mutate( month = format(SampleStart_Date,"%m")) %>%
  filter(month %in% '06' | month %in% '07' | month %in% '08' | month %in% '09' | month %in% '10') 
  

sample_info <- bug_tax_data_filtered |> 
  select(org_id, Project1, Project2, MLocID, StationDes, MonLocType, act_id, act_comments, Activity_Type,
         SampleStart_Date, SampleStart_Time,Sample_Media, Sample_Method,Result_Status, Assemblage, EcoRegion3, 
         EcoRegion4, EcoRegion2, HUC8_Name, HUC12_Name, Lat_DD, Long_DD, Reachcode, Measure, ELEV_Ft, 
         GNIS_Name, Conf_Score, QC_Comm,COMID, AU_ID,  ReferenceSite, Wade_Boat) |> 
  distinct()



### Check for missing comids ----------------------------------------------------------------------------------------

no_comid <- bug_tax_data_filtered |> 
  filter(is.na(COMID)) |> 
  select(MLocID, COMID) |> 
  distinct()

if(nrow(no_comid) > 0){
  
  warning("Missing COMIDs. Please address before continuing")
  
}


# Run O:E model ---------------------------------------------------------------------------------------------------

#pass nhd slope to output
source('bugs analyses/All_together/OE_run.R')

OE_results <- OE_modelrun(df_bugs = bug_tax_data_filtered)


OE_scores <- OE_results$OE_Scores

missing_streamcat <- OE_results$missing_streamcat




# MMI -------------------------------------------------------------------------------------------------------------

source('bugs analyses/All_together/MMI_run.R')

MMI_results <- MMI_run(df_bugs = bug_tax_data_filtered, df_sample= sample_info,
                       attribute_table_loc = 'https://github.com/leppott/BioMonTools_SupportFiles/raw/refs/heads/main/data/taxa_official/ORWA/old/ORWA_Attributes_20241121.csv')

MMI_scores <- MMI_results$MMI_result

MMI_metrics <- MMI_results$MMI_metrics

# BCG -------------------------------------------------------------------------------------------------------------


## Calculate Metrics ----------------------------------------------------------------------------------------------

source('bugs analyses/All_together/calculate_metrics.R')


BCG_metric_list <- calculate_metrics(bug_tax_data_filtered,
                                     attribute_table_loc = 'https://github.com/leppott/BioMonTools_SupportFiles/raw/refs/heads/main/data/taxa_official/ORWA/old/ORWA_Attributes_20241121.csv')

BCG_metrics <- BCG_metric_list$Metrics
BCG_Metric_taxa_attributes <- BCG_metric_list$metric_taxa_attribute

## Run BCG --------------------------------------------------------------------------------------------------------

source('bugs analyses/All_together/BCG_run.R')

BCG <- run_BCG(BCG_metrics)

BCG_results <- BCG$Levels.Flags
BCG_Metric.Membership <- BCG$Metric.Membership
BCG_Level.Membership <- BCG$Level.Membership


BCG_sample <- sample_info |> 
  left_join(BCG_results, by = c('act_id' = 'SampleID'))



# Run Stressor models ---------------------------------------------------------------------------------------------------


source('bugs analyses/All_together/StressorID models_v2023.R')

STRESS_results <- bug.stressors_run(df_bugs = bug_tax_data_filtered)

STRESS_results <- STRESS_results %>%
  dplyr::rename(act_id = Sample)

#___________________________________________________________________________


#### merge into one df ### 
OE <- OE_scores |> 
  #select(MLocID,org_id, AU_ID, act_id,EcoRegion3,EcoRegion4,ReferenceSite, OoverE) |> 
  mutate(BCG_region = case_when(EcoRegion3 %in% c(1,3,4) ~ "In region",
                                TRUE ~ "Out Region"))

BCG <- BCG_results |> 
  select(SampleID, Primary_BCG_Level, Continuous_BCG_Level) |> 
  rename(act_id = SampleID)


MMI <- MMI_scores |> 
  select(SAMPLEID, MMI) |> 
  rename(act_id = SAMPLEID)

MMI_met <- MMI_metrics |> 
  rename(act_id = SAMPLEID)

load("bugs analyses/Models_Validation/sample_info_model.Rdata") ### this comes from model_val script 

### this one keeps qualifiers 
joined_OE_BCG_MMI_STRESS_all <- left_join(OE, BCG, by = join_by(act_id)) |> 
  left_join(MMI_met) |> 
  left_join(MMI) |> 
  left_join(STRESS_results) |>
  #left_join(sample_info_model, by = 'act_id')
  mutate(qualifer = case_when(ni_total < 300 ~ 1,
                              act_id %in% c('22946-ORDEQ:19980812:R:SR-2',
                                             '22946-ORDEQ:19980812:R:SR-1',
                                              '22946-ORDEQ:19980812:R:SR-3',
                                               '21814-ORDEQ:20010912:R:SR',
                                               '21814-ORDEQ:19990922:R:SR',
                                               '21814-ORDEQ:20000920:R:SR',
                                               '35813-ORDEQ:20000901:R:SR') ~ 2,
                              EcoRegion3 == 80 ~ 3,
                              act_id %in% c('24044-ORDEQ:20000913:R:SR','35618-ORDEQ:20000702:R:SR',
                                            '32555-ORDEQ:20050801:R:SR') ~ 4,
                              MLocID %in% c('24454-ORDEQ','24416-ORDEQ','38549-ORDEQ',
                                               '38561-ORDEQ','30343-ORDEQ','35633-ORDEQ') ~ 5,
                              org_id %in% c('JCWC_AW(NOSTORETID)',
                                          'PDX_BES(NOSTORETID)',
                                          'UDESCHUTES_WC(NOSTORETID)',
                                          #'PBWC_WQX', keeping these for now
                                          'CITY_GRESHAM(NOSTORETID)',
                                          'CRBC_WQX') ~ 6,
                             #Wade_Boat == 'boatable' ~ 7, 
                             TRUE ~ 0),
       qualifer_text = case_when(qualifer == 1 ~ "less than 300 count",
                                 qualifer == 2 ~ "poor sample quality",
                                 qualifer == 3 ~ "Southeast Oregon",
                                 qualifer == 4 ~ "Lake Effect",
                                 qualifer == 5 ~ "Glacier",
                                 qualifer == 6 ~ "VolMon",
                                 #qualifer == 7 ~ "boatable",
                                 TRUE ~ NA))

### this one has all qualifiers removed 
joined_OE_BCG_MMI_STRESS_good <- joined_OE_BCG_MMI_all %>% 
  filter(qualifer == 0)%>% ## removes sus data (low counts, SE, glacial sites and poor samples)
  filter(Result_Status != 'Rejected') %>% # removes older and rejected data 
  filter(!is.na(ReferenceSite)) %>% # removes sites that have not gone through reference screen 
  mutate(MMI_rescale = MMI/0.7292573)

save(joined_OE_BCG_MMI, file = 'bioassess_11-25-24.Rdata')
save(joined_OE_BCG_MMI_all, file = 'bioassess_11-25-24_all.Rdata')
write.xlsx(joined_OE_BCG_MMI, file = paste0("biocriteria_scores", Sys.Date(), '.xlsx'))
