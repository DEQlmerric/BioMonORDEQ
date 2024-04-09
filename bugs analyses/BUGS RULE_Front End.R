# Authors: Shannon Hubler, Lesley Merrick

# Initial Code: 6.4.2021 - Revised and updated summer 2023

# Purpose: Provide a front end for users to source and run functions that will:
#     1) Query AWQMS to find which uploaded raw bug data samples DO NOT have corresponding summary data (using metrics as a filter)
#     2) Query out the "missing summary" raw data, from AWQMS
#     3) Calculate summary metrics
#     4) Calculate PREDATOR O/E scores and supporting outputs
#     5) Calculate Stressor ID models
#     6) generate a summary because Shannon is needy :) 
#     7) Generate AWQMS uploads for metric and index tables 

###########
###########
#            run data query from AWQMS
###########
###########

library(magrittr)

source('bugs analyses/data/Data_from_AWQMS.R') 

# TRUE = returns only samples with missing metrics
# FALSE = returns all data
only_no_metrics = FALSE

b_t_s <- data.raw.AWQMS(no_metrics = only_no_metrics, 
                        startdate='1949-09-15') #%>% 
                       # dplyr::filter(org_id != 'CAFW(NOSTORETID)')


###########
###########
#              run bug metrics function
###########
###########

#fix this
source('bugs analyses/metrics/bug metrics.R')
metrics <- bug.metrics(b_t_s)

###########
###########
#              run Stressor ID models function
###########
###########

#fix this
source('bugs analyses/stressor/StressorID models_v2023.R')
stressors <- bug.stressors(b_t_s)  

MTTI <- stressors$MTTI
BSTI <- stressors$BSTI
tot.abund.MTTI <- stressors$tot.abund.MTTI
tot.abund.BSTI <- stressors$tot.abund.BSTI 

###########
###########
#             run PREDATOR function
###########
###########

#fix this
source('bugs analyses/PREDATOR/PREDATOR models.R')
model_outputs <- bug.PREDATOR(b_t_s)

#unpack the model output list
oe.mwcf <- model_outputs[["oe.mwcf"]]
oe.wccp <- model_outputs[["oe.wccp"]]
oe.nbr <- model_outputs[["oe.nbr"]]
tot.abund.RIV <- model_outputs[["tot.abund.RIV"]]
tot.abund.STR <- model_outputs[["tot.abund.STR"]]

###########
###########
#             run SUMMARY TABLE function
###########
###########
source('bugs analyses/finalize_summary.table/summary.table.R')
oe.stress.mets.sta <- sum.bugs()

# export SUMMARY TABLE
#write.csv(oe.stress.mets.sta, paste0("//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/SUM_BUGS_",Sys.Date(), ".csv"), row.names=FALSE)

###########
###########
#             Generate AWQMS upload files for metrics and indexes
###########
###########
source('bugs analyses/ForAWQMS/fxn_AWQMSupload_Indexes.R')
Index_uploads <- AWQMS_Index()
#splits uploads by org 
for(i in 1:length(unique(Index_uploads$org_id))) {
  print(noquote(paste("Starting file", i, "of", length(unique(Index_uploads$org_id)) )))
  #get org abriviation to filter with
  org <-  unique(Index_uploads$org_id)[[i]]
  #filter dataframe to only one org
  org_template <-  Index_uploads %>%
    filter(org_id == org)
  #save the excel file
  write.csv(org_template, paste0('bugs analyses/ForAWQMS/Index_uploads_',Sys.Date(),org,".csv"), na = "",row.names=FALSE)
}

source('bugs analyses/ForAWQMS/fxn_AWQMSupload_Metrics.R')
Metric_uploads <- AWQMS_Metric()

#splits uploads by org 
for(i in 1:length(unique(Metric_uploads$org_id))) {
    print(noquote(paste("Starting file", i, "of", length(unique(Metric_uploads$org_id)) )))
  #get org abriviation to filter with
    org <-  unique(Metric_uploads$org_id)[[i]]
  #filter dataframe to only one org
  org_template <-  Metric_uploads %>%
    filter(org_id == org)
  #save the excel file
  write.csv(org_template, paste0('bugs analyses/ForAWQMS/Metric_uploads_',Sys.Date(),org,".csv"), na = "",row.names=FALSE)
}



