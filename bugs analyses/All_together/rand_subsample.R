# This function is a wrapper for the  rarify.seed() function created by John Van Sickle, USEPA. This function will 
# take an input dataframe of raw bug counts and output a random 300 count subsample. 


random_subsample <- function(df, OTU_col, subsiz = 300, seed){
  
  if(missing(seed)){
    stop("random_subsample() function needs a seed argument to continue. Use seed = 16412730 as default seed or NA for no seed.")
    
  }
  
  
df <- bug_tax_data_filtered
  
  raw.bugs_OTUs <- as.data.frame(df %>%
                                   group_by(Sample=act_id, MLocID, OTU={{OTU_col}}) %>%
                                   summarise(Count=sum(Result_Numeric)) %>%
                                   filter(OTU != 'DNI') |> 
                                   ungroup())
  
  #source('bugs analyses/RIVPACS_2022/_2024 model build/rarify_w_seed.R')
  
  
 # b.rare.seed <- rarify.seed(na.omit(raw.bugs_OTUs), 'Sample', 'Count', 300) 
  
  
  b.rare <-BioMonTools::rarify(na.omit(raw.bugs_OTUs), 'Sample', 'Count', subsiz, mySeed = seed) 
  
}

