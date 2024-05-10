library(tidyverse)
library(StreamCatTools)


fetch_data <- function(taxonomy_table = 'bugs analyses/Taxonomy/ODEQ_Taxonomy_dec22.xlsx'){
  
  

  
  ## Download raw bug data from AWQMS --------------------------------------------------------------------------------
  
  
  raw_bugs <- AWQMSdata::AWQMS_Raw_Macros() 
  
  
  ## Join Taxonomy table ---------------------------------------------------------------------------------------------
  
  
  taxonomy <- readxl::read_excel(taxonomy_table, col_types = "text")
  
  
  taxonomy.otu <- taxonomy %>%
    select(DEQ_Taxon = DEQ_TAXON, Taxon, OTU_RIV_24)
  
  taxonomy.otu$DEQ_Taxon <- as.character(taxonomy.otu$DEQ_Taxon)
  
  
  raw.bugs_taxonomy <- raw_bugs %>%
    left_join(taxonomy.otu, by='DEQ_Taxon')
  
  

  
  return(raw.bugs_taxonomy)
  
}

