library(tidyverse)
library(StreamCatTools)


fetch_data <- function(DEQ_taxonomy_table = 'bugs analyses/Taxonomy/ODEQ_Taxonomy_dec22.xlsx',
                       leppo_taxonomy_table_url = 'https://raw.githubusercontent.com/leppott/BioMonTools_SupportFiles/main/data/taxa_official/ORWA_TaxaTranslator_20240204.csv'){
  
  

  
  ## Download raw bug data from AWQMS --------------------------------------------------------------------------------
  
  
  raw_bugs <- AWQMSdata::AWQMS_Raw_Macros() 
  
  
  ## Join Taxonomy table ---------------------------------------------------------------------------------------------
  
  #AWQMS taxa = Taxonomic_Name
  #Join DEQ taxonomy table by DEQ_Taxon to get Taxon
  #Match Taxon to leppo's taxa table by Taxon = Taxon_orig
  #Keep   OTU_RIV_24 from DEA taxa table and OTU_MTTI, OTU_BST from leppo taxa table
  
  #read in DEQ taxa table
  taxonomy <- readxl::read_excel(DEQ_taxonomy_table, col_types = "text")
  
  
  taxonomy.otu <- taxonomy %>%
    select(DEQ_Taxon = DEQ_TAXON, Taxon, OTU_RIV_24)
  
  taxonomy.otu$DEQ_Taxon <- as.character(taxonomy.otu$DEQ_Taxon)
  
  
  #read in leppo taxa table
  taxonomy_leppo <- read.csv(leppo_taxonomy_table_url)
 
  
  
   taxonomy_leppo_select <- taxonomy_leppo |> 
    select(Taxon_orig,OTU_MTTI, OTU_BSTI) |> 
    rename(Taxon = Taxon_orig)
  
  

  
  raw.bugs_taxonomy <- raw_bugs %>%
    left_join(taxonomy.otu, by='DEQ_Taxon') |> 
    left_join(taxonomy_leppo_select, by = join_by(Taxon))
  


  
  return(raw.bugs_taxonomy)
  
}

