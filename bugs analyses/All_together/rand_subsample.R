# This function is a wrapper for the  rarify.seed() function created by John Van Sickle, USEPA. This function will 
# take an input dataframe of raw bug counts and output a random 300 count subsample. 


random_subsample <- function(df){
  

  
  raw.bugs_OTUs <- as.data.frame(df %>%
                                   group_by(Sample=act_id, MLocID, OTU=OTU_RIV_24) %>%
                                   summarise(Count=sum(Result_Numeric)) %>%
                                   filter(OTU != 'DNI'))
  
  source('bugs analyses/RIVPACS_2022/_2024 model build/rarify_w_seed.R')
  
  
  b.rare.seed <- rarify.seed(na.omit(raw.bugs_OTUs), 'Sample', 'Count', 300) 
  
  
  
}

