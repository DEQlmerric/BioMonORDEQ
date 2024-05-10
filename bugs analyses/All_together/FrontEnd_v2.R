


# Get data that will be shared by all components of bioassessment -------------------------------------------------
source('bugs analyses/All_together/Fetch_data.R')

bug_tax_data <- fetch_data()

bug_tax_data_filtered <- bug_tax_data |> 
  filter(SampleStart_Date > "1998-01-01") %>%
  filter(Sample_Method %in% c('Benthic Kick - Riffle', 'Benthic Kick - Targeted Riffle', 'Benthic Kick - Transect')) %>%
  filter(Char_Name == 'Count') %>%
  mutate(SampleStart_Date = lubridate::ymd(SampleStart_Date)) |> 
  mutate( month = format(SampleStart_Date,"%m")) %>%
  filter(month %in% '06' | month %in% '07' | month %in% '08' | month %in% '09' | month %in% '10') 
  
  


# Randomize subsample ---------------------------------------------------------------------------------------------

source('bugs analyses/All_together/rand_subsample.R')

rand_subsample <- random_subsample(bug_tax_data_filtered)



# Run O:E model ---------------------------------------------------------------------------------------------------

source('bugs analyses/All_together/OE_run.R')

OE_results <- OE_modelrun(df_bugs = bug_tax_data_filtered,
                          df_rand = rand_subsample)


OE_scores <- OE_results$OE_Scores

missing_streamcat <- OE_results$missing_streamcat
