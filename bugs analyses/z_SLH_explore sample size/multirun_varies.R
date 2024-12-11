#this code will run functions multiple times and return stats on mean and varience of Bioassessment componadnts
#currently only works for OE, but can be easily modifed for the other componants. 



num_runs <- 20



# Copy frontend ---------------------------------------------------------------------------------------------------

#####################

# read in ref samples used in construction of both indexes

#####################

ref.samps_221 <- read.csv('bugs analyses/RIVPACS_2022/_2024 model build_REBUILD NEW OTUs/ref.samps_221.csv') 




###################

# bring in bug and pred data

####################


# Get data that will be shared by all components of bioassessment -------------------------------------------------
source('bugs analyses/All_together/Fetch_data.R')

bug_tax_data <- fetch_data(DEQ_taxonomy_table = 'bugs analyses/Taxonomy/ODEQ_Taxonomy_dec22.xlsx',
                           leppo_taxonomy_table_url = 'https://raw.githubusercontent.com/leppott/BioMonTools_SupportFiles/main/data/taxa_official/_archive/ORWA_TaxaTranslator_20240204.csv')


## Filter the original datapull ------------------------------------------------------------------------------------

bug_tax_data_filtered <- bug_tax_data |> 
  filter(SampleStart_Date > "1998-01-01") %>%
  filter(Sample_Method %in% c('Benthic Kick - Riffle', 'Benthic Kick - Targeted Riffle', 'Benthic Kick - Transect')) %>%
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



# limit to ref samples

samps.models <- bug_tax_data_filtered %>%
  filter(act_id %in% ref.samps_221$act_id) %>%
  filter(Result_Numeric >0)

tot_num_bugs <- samps.models |> 
  group_by(act_id) |> 
  summarise(tot_indiv = sum(Result_Numeric))

@@@@ this is raw total bugs ---> need total after OTUs?


samp.info_models <- sample_info %>%
  filter(act_id %in% ref.samps_221$act_id)

# drop items with all data, keep only ref CAL data
rm(bug_tax_data, bug_tax_data_filtered, sample_info)


######################

#####################

# establish which subsample level to use

#####################

#####################


which.sub.level = 300



# OE_multirun -----------------------------------------------------------------------------------------------------
#pass nhd slope to output
source('bugs analyses/z_SLH_explore sample size/OE_run_subsamp.varies.R')


oe_multi <- function(){
  
   
    
    
    OE_results <- OE_modelrun(df_bugs = samps.models)
    
    
    OE_scores <- OE_results$OE_Scores |> 
      dplyr::arrange(act_id)
    
    return(OE_scores)
}


result_list_OE <- purrr::map(1:num_runs, ~ oe_multi())


# View(result_list[[1]])
# View(result_list[[2]])

result_stats_OE_300 <- data.frame(sample = result_list_OE[[1]]$act_id,
                              #mean_indivduals = rowMeans(sapply(result_list_OE, function(df) df$num_indiv)),
                     #sd_ind = apply(sapply(result_list_OE, function(df) df$num_indiv), 1, sd),
                     #mean_num_taxa =  rowMeans(sapply(result_list_OE, function(df) df$num_taxa)),
                     #sd_taxa = apply(sapply(result_list_OE, function(df) df$num_taxa), 1, sd),
                     mean_OE = rowMeans(sapply(result_list_OE, function(df) df$OoverE)),
                     sd_OE = apply(sapply(result_list_OE, function(df) df$OoverE), 1, sd),
                     var_OE = apply(sapply(result_list_OE, function(df) df$OoverE), 1, var))

write.csv(result_stats_OE_300, 'bugs analyses/z_SLH_explore sample size/300ct_results_stats_OE.csv')


          # ggplot(result_stats_OE, aes(x=sd_OE)) + geom_histogram() +
          #  labs(title="Each Ref_CAL sample subsampled 20 times to 250 count")
          
          
          # ggsave('bugs analyses/z_SLH_explore sample size/OE_Multirun_250ct.png')

# MMI multirun ----------------------------------------------------------------------------------------------------

source('bugs analyses/z_SLH_explore sample size/MMI_run_subsamp.varies.R')


MMI_multi <- function(){
  
  
  
  MMI_results <- MMI_run(df_bugs = samps.models, df_sample= samp.info_models)
  
  MMI_scores <- MMI_results$MMI_result |> 
     dplyr::arrange(SAMPLEID) 
  
  

  
  return(MMI_scores)
}


result_list_MMI <- purrr::map(1:num_runs, ~ MMI_multi())


result_stats_MMI_300 <- data.frame(sample = result_list_MMI[[1]]$SAMPLEID,
                           # mean_indivduals = rowMeans(sapply(result_list_MMI, function(df) df$num_indiv)),
                           # sd_ind = apply(sapply(result_list_MMI, function(df) df$num_indiv), 1, sd),
                           # mean_num_taxa =  rowMeans(sapply(result_list_MMI, function(df) df$num_taxa)),
                           # sd_taxa = apply(sapply(result_list_MMI, function(df) df$num_taxa), 1, sd),
                           mean_MMI = rowMeans(sapply(result_list_MMI, function(df) df$MMI)),
                           sd_MMI = apply(sapply(result_list_MMI, function(df) df$MMI), 1, sd),
                           var_MMI = apply(sapply(result_list_MMI, function(df) df$MMI), 1, var))

write.csv(result_stats_MMI_300, 'bugs analyses/z_SLH_explore sample size/300ct_results_stats_MMI.csv')

ggplot(result_stats_OE_300, aes(x=sd_OE)) + 
  geom_histogram() +
  labs(title = paste("Histogram of 300 ct O:E standard deviation over",num_runs, "sampling runs" ))+
  ylab("Number of samples")+
  theme_minimal()
ggsave('bugs analyses/z_SLH_explore sample size/OE_Multirun_300ct.png')



ggplot(result_stats_MMI_300, aes(x=sd_MMI)) + 
  geom_histogram() +
  labs(title = paste("Histogram of MMI standard deviation over",num_runs, "subsampling runs" ))+
  ylab("Number of samples")+
  theme_minimal()
ggsave('bugs analyses/z_SLH_explore sample size/MMI_Multirun_300ct.png')













######################

#####################

# establish which subsample level to use

#####################

#####################


which.sub.level = 250



# OE_multirun -----------------------------------------------------------------------------------------------------
#pass nhd slope to output
source('bugs analyses/z_SLH_explore sample size/OE_run_subsamp.varies.R')


oe_multi <- function(){
  
   
    
    
    OE_results <- OE_modelrun(df_bugs = samps.models)
    
    
    OE_scores <- OE_results$OE_Scores |> 
      dplyr::arrange(act_id)
    
    return(OE_scores)
}


result_list_OE <- purrr::map(1:num_runs, ~ oe_multi())


# View(result_list[[1]])
# View(result_list[[2]])

result_stats_OE_250 <- data.frame(sample = result_list_OE[[1]]$act_id,
                              #mean_indivduals = rowMeans(sapply(result_list_OE, function(df) df$num_indiv)),
                     #sd_ind = apply(sapply(result_list_OE, function(df) df$num_indiv), 1, sd),
                     #mean_num_taxa =  rowMeans(sapply(result_list_OE, function(df) df$num_taxa)),
                     #sd_taxa = apply(sapply(result_list_OE, function(df) df$num_taxa), 1, sd),
                     mean_OE = rowMeans(sapply(result_list_OE, function(df) df$OoverE)),
                     sd_OE = apply(sapply(result_list_OE, function(df) df$OoverE), 1, sd),
                     var_OE = apply(sapply(result_list_OE, function(df) df$OoverE), 1, var))

write.csv(result_stats_OE_250, 'bugs analyses/z_SLH_explore sample size/250ct_results_stats_OE.csv')


          # ggplot(result_stats_OE, aes(x=sd_OE)) + geom_histogram() +
          #  labs(title="Each Ref_CAL sample subsampled 20 times to 250 count")
          
          
          # ggsave('bugs analyses/z_SLH_explore sample size/OE_Multirun_250ct.png')

# MMI multirun ----------------------------------------------------------------------------------------------------

source('bugs analyses/z_SLH_explore sample size/MMI_run_subsamp.varies.R')


MMI_multi <- function(){
  
  
  
  MMI_results <- MMI_run(df_bugs = samps.models, df_sample= samp.info_models)
  
  MMI_scores <- MMI_results$MMI_result |> 
     dplyr::arrange(SAMPLEID) 
  
  

  
  return(MMI_scores)
}


result_list_MMI <- purrr::map(1:num_runs, ~ MMI_multi())


result_stats_MMI_250 <- data.frame(sample = result_list_MMI[[1]]$SAMPLEID,
                           # mean_indivduals = rowMeans(sapply(result_list_MMI, function(df) df$num_indiv)),
                           # sd_ind = apply(sapply(result_list_MMI, function(df) df$num_indiv), 1, sd),
                           # mean_num_taxa =  rowMeans(sapply(result_list_MMI, function(df) df$num_taxa)),
                           # sd_taxa = apply(sapply(result_list_MMI, function(df) df$num_taxa), 1, sd),
                           mean_MMI = rowMeans(sapply(result_list_MMI, function(df) df$MMI)),
                           sd_MMI = apply(sapply(result_list_MMI, function(df) df$MMI), 1, sd),
                           var_MMI = apply(sapply(result_list_MMI, function(df) df$MMI), 1, var))

write.csv(result_stats_MMI_250, 'bugs analyses/z_SLH_explore sample size/250ct_results_stats_MMI.csv')

ggplot(result_stats_OE_250, aes(x=sd_OE)) + 
  geom_histogram() +
  labs(title = paste("Histogram of 250 ct O:E standard deviation over",num_runs, "sampling runs" ))+
  ylab("Number of samples")+
  theme_minimal()
ggsave('bugs analyses/z_SLH_explore sample size/OE_Multirun_250ct.png')



ggplot(result_stats_MMI_250, aes(x=sd_MMI)) + 
  geom_histogram() +
  labs(title = paste("Histogram of MMI standard deviation over",num_runs, "subsampling runs" ))+
  ylab("Number of samples")+
  theme_minimal()
ggsave('bugs analyses/z_SLH_explore sample size/MMI_Multirun_250ct.png')



######################

#####################

# establish which subsample level to use

#####################

#####################


which.sub.level = 200



# OE_multirun -----------------------------------------------------------------------------------------------------
#pass nhd slope to output
source('bugs analyses/z_SLH_explore sample size/OE_run_subsamp.varies.R')


oe_multi <- function(){
  
   
    
    
    OE_results <- OE_modelrun(df_bugs = samps.models)
    
    
    OE_scores <- OE_results$OE_Scores |> 
      dplyr::arrange(act_id)
    
    return(OE_scores)
}


result_list_OE <- purrr::map(1:num_runs, ~ oe_multi())


# View(result_list[[1]])
# View(result_list[[2]])

result_stats_OE_200 <- data.frame(sample = result_list_OE[[1]]$act_id,
                              #mean_indivduals = rowMeans(sapply(result_list_OE, function(df) df$num_indiv)),
                     #sd_ind = apply(sapply(result_list_OE, function(df) df$num_indiv), 1, sd),
                     #mean_num_taxa =  rowMeans(sapply(result_list_OE, function(df) df$num_taxa)),
                     #sd_taxa = apply(sapply(result_list_OE, function(df) df$num_taxa), 1, sd),
                     mean_OE = rowMeans(sapply(result_list_OE, function(df) df$OoverE)),
                     sd_OE = apply(sapply(result_list_OE, function(df) df$OoverE), 1, sd),
                     var_OE = apply(sapply(result_list_OE, function(df) df$OoverE), 1, var))

write.csv(result_stats_OE_200, 'bugs analyses/z_SLH_explore sample size/200ct_results_stats_OE.csv')


          # ggplot(result_stats_OE, aes(x=sd_OE)) + geom_histogram() +
          #  labs(title="Each Ref_CAL sample subsampled 20 times to 250 count")
          
          
          # ggsave('bugs analyses/z_SLH_explore sample size/OE_Multirun_250ct.png')

# MMI multirun ----------------------------------------------------------------------------------------------------

source('bugs analyses/z_SLH_explore sample size/MMI_run_subsamp.varies.R')


MMI_multi <- function(){
  
  
  
  MMI_results <- MMI_run(df_bugs = samps.models, df_sample= samp.info_models)
  
  MMI_scores <- MMI_results$MMI_result |> 
     dplyr::arrange(SAMPLEID) 
  
  

  
  return(MMI_scores)
}


result_list_MMI <- purrr::map(1:num_runs, ~ MMI_multi())


result_stats_MMI_200 <- data.frame(sample = result_list_MMI[[1]]$SAMPLEID,
                           # mean_indivduals = rowMeans(sapply(result_list_MMI, function(df) df$num_indiv)),
                           # sd_ind = apply(sapply(result_list_MMI, function(df) df$num_indiv), 1, sd),
                           # mean_num_taxa =  rowMeans(sapply(result_list_MMI, function(df) df$num_taxa)),
                           # sd_taxa = apply(sapply(result_list_MMI, function(df) df$num_taxa), 1, sd),
                           mean_MMI = rowMeans(sapply(result_list_MMI, function(df) df$MMI)),
                           sd_MMI = apply(sapply(result_list_MMI, function(df) df$MMI), 1, sd),
                           var_MMI = apply(sapply(result_list_MMI, function(df) df$MMI), 1, var))

write.csv(result_stats_MMI_200, 'bugs analyses/z_SLH_explore sample size/200ct_results_stats_MMI.csv')

ggplot(result_stats_OE_200, aes(x=sd_OE)) + 
  geom_histogram() +
  labs(title = paste("Histogram of 200 ct O:E standard deviation over",num_runs, "sampling runs" ))+
  ylab("Number of samples")+
  theme_minimal()
ggsave('bugs analyses/z_SLH_explore sample size/OE_Multirun_200ct.png')



ggplot(result_stats_MMI_200, aes(x=sd_MMI)) + 
  geom_histogram() +
  labs(title = paste("Histogram of 200 ct MMI standard deviation over",num_runs, "subsampling runs" ))+
  ylab("Number of samples")+
  theme_minimal()
ggsave('bugs analyses/z_SLH_explore sample size/MMI_Multirun_200ct.png')




######################

#####################

# establish which subsample level to use

#####################

#####################


which.sub.level = 150



# OE_multirun -----------------------------------------------------------------------------------------------------
#pass nhd slope to output
source('bugs analyses/z_SLH_explore sample size/OE_run_subsamp.varies.R')


oe_multi <- function(){
  
   
    
    
    OE_results <- OE_modelrun(df_bugs = samps.models)
    
    
    OE_scores <- OE_results$OE_Scores |> 
      dplyr::arrange(act_id)
    
    return(OE_scores)
}


result_list_OE <- purrr::map(1:num_runs, ~ oe_multi())


# View(result_list[[1]])
# View(result_list[[2]])

result_stats_OE_150 <- data.frame(sample = result_list_OE[[1]]$act_id,
                              #mean_indivduals = rowMeans(sapply(result_list_OE, function(df) df$num_indiv)),
                     #sd_ind = apply(sapply(result_list_OE, function(df) df$num_indiv), 1, sd),
                     #mean_num_taxa =  rowMeans(sapply(result_list_OE, function(df) df$num_taxa)),
                     #sd_taxa = apply(sapply(result_list_OE, function(df) df$num_taxa), 1, sd),
                     mean_OE = rowMeans(sapply(result_list_OE, function(df) df$OoverE)),
                     sd_OE = apply(sapply(result_list_OE, function(df) df$OoverE), 1, sd),
                     var_OE = apply(sapply(result_list_OE, function(df) df$OoverE), 1, var))

write.csv(result_stats_OE_150, 'bugs analyses/z_SLH_explore sample size/150ct_results_stats_OE.csv')


          # ggplot(result_stats_OE, aes(x=sd_OE)) + geom_histogram() +
          #  labs(title="Each Ref_CAL sample subsampled 20 times to 250 count")
          
          
          # ggsave('bugs analyses/z_SLH_explore sample size/OE_Multirun_250ct.png')

# MMI multirun ----------------------------------------------------------------------------------------------------

source('bugs analyses/z_SLH_explore sample size/MMI_run_subsamp.varies.R')


MMI_multi <- function(){
  
  
  
  MMI_results <- MMI_run(df_bugs = samps.models, df_sample= samp.info_models)
  
  MMI_scores <- MMI_results$MMI_result |> 
     dplyr::arrange(SAMPLEID) 
  
  

  
  return(MMI_scores)
}


result_list_MMI <- purrr::map(1:num_runs, ~ MMI_multi())


result_stats_MMI_150 <- data.frame(sample = result_list_MMI[[1]]$SAMPLEID,
                           # mean_indivduals = rowMeans(sapply(result_list_MMI, function(df) df$num_indiv)),
                           # sd_ind = apply(sapply(result_list_MMI, function(df) df$num_indiv), 1, sd),
                           # mean_num_taxa =  rowMeans(sapply(result_list_MMI, function(df) df$num_taxa)),
                           # sd_taxa = apply(sapply(result_list_MMI, function(df) df$num_taxa), 1, sd),
                           mean_MMI = rowMeans(sapply(result_list_MMI, function(df) df$MMI)),
                           sd_MMI = apply(sapply(result_list_MMI, function(df) df$MMI), 1, sd),
                           var_MMI = apply(sapply(result_list_MMI, function(df) df$MMI), 1, var))

write.csv(result_stats_MMI_150, 'bugs analyses/z_SLH_explore sample size/150ct_results_stats_MMI.csv')

ggplot(result_stats_OE_150, aes(x=sd_OE)) + 
  geom_histogram() +
  labs(title = paste("Histogram of 150 ct O:E standard deviation over",num_runs, "sampling runs" ))+
  ylab("Number of samples")+
  theme_minimal()
ggsave('bugs analyses/z_SLH_explore sample size/OE_Multirun_150ct.png')



ggplot(result_stats_MMI_150, aes(x=sd_MMI)) + 
  geom_histogram() +
  labs(title = paste("Histogram of 150 ct MMI standard deviation over",num_runs, "subsampling runs" ))+
  ylab("Number of samples")+
  theme_minimal()
ggsave('bugs analyses/z_SLH_explore sample size/MMI_Multirun_150ct.png')

























~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#read in good scores

library(openxlsx)
good_actid <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report 🎉 - IR_2026/Methodology/FW BioCriteria/biocriteria_scores2024-08-07_filtered.xlsx") |> 
  pull(act_id)


results_together <- result_stats_OE |> 
  select(sample, sd_OE) |> 
  left_join( select(result_stats_MMI, sample, sd_MMI)) |> 
  pivot_longer(cols = c(sd_OE, sd_MMI),
               names_to = 'type',
               values_to = 'st_dev') |> 
  left_join(tot_num_bugs, by = c('sample' = 'act_id') ) |> 
  filter(sample %in% good_actid)



ggplot(results_together, aes(x=st_dev, fill = type, color=type)) + 
  geom_histogram(alpha = 0.5,
                 aes(x=st_dev, fill = type, color=type),
                 position='identity') +
  labs(title = paste("Histogram of standard deviation over",num_runs, "subsampling runs" ),
       x = "Standard Deviation",
       legend = 'll')+
  ylab("Number of samples")+
  theme_minimal()+
  guides(color = FALSE,
         fill=guide_legend(title="Metric"))+
  scale_fill_discrete(labels=c('MMI', 'O:E'))+
  theme(text = element_text(size = 12))
  

ggplot(results_together, aes(x =tot_indiv,  y=st_dev,  color=type)) + 
  geom_point(alpha = 0.5)+
  labs(title = paste("Total individuals in sample and standard deviation over",num_runs, "subsampling runs" ),
       x = "Total individuals in sample",
       y = 'Standard Deviation')+
  theme_minimal()+
  guides(color = guide_legend(title="Metric"))+
  scale_color_discrete(labels=c('MMI', 'O:E'))+
  theme(text = element_text(size = 12))


