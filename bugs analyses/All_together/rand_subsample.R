
random_subsample <- function(df){
  
  #Testing
  df <- bug_tax_data
  
  
  
  raw.bugs_OTUs <- as.data.frame(df %>%
                                   group_by(Sample=act_id, MLocID, OTU=OTU_RIV_24) %>%
                                   summarise(Count=sum(Result_Numeric)) %>%
                                   filter(OTU != 'DNI'))
  
  source('bugs analyses/RIVPACS_2022/_2024 model build/rarify_w_seed.R')
  
  
  b.rare.seed <- rarify.seed(na.omit(raw.bugs_OTUs), 'Sample', 'Count', 300) 
  
  
  
}

