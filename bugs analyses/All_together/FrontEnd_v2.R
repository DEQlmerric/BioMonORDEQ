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
                           leppo_taxonomy_table_url = 'https://raw.githubusercontent.com/leppott/BioMonTools_SupportFiles/main/data/taxa_official/_archive/ORWA_TaxaTranslator_20240204.csv')


## Filter the original datapull ------------------------------------------------------------------------------------

bug_tax_data_filtered <- bug_tax_data |> 
  filter(Result_Status != 'Rejected') |> 
  filter(SampleStart_Date > "1999-01-01") %>%
  filter(Sample_Method %in% c('Benthic Kick - Riffle', 'Benthic Kick - Targeted Riffle', 'Benthic Kick - Transect','Benthic Kick - Mixed')) %>% # LAM added mixed for USU transect data inclusion
  filter(Char_Name == 'Count') %>%
  mutate(SampleStart_Date = lubridate::ymd(SampleStart_Date)) |> 
  mutate( month = format(SampleStart_Date,"%m")) %>%
  filter(month %in% '06' | month %in% '07' | month %in% '08' | month %in% '09' | month %in% '10') 
  

sample_info <- bug_tax_data_filtered |> 
  select(org_id, Project1, Project2, MLocID, StationDes, MonLocType, act_id, act_comments, Activity_Type, 
         SampleStart_Date, SampleStart_Time,Sample_Media, Sample_Method, Assemblage, EcoRegion3, 
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

MMI_results <- MMI_run(df_bugs = bug_tax_data_filtered, df_sample= sample_info)

MMI_scores <- MMI_results$MMI_result

MMI_metrics <- MMI_results$MMI_metrics

# BCG -------------------------------------------------------------------------------------------------------------


## Calculate Metrics ----------------------------------------------------------------------------------------------

source('bugs analyses/All_together/calculate_metrics.R')


BCG_metric_list <- calculate_metrics(bug_tax_data_filtered)

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
