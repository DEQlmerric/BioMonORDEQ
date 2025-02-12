### Generate AWQMS upload from data template ###
### L. Merrick 5/7/2021 ###
### Revised to use contractor version received from Cole ecological in 2023

library(tidyverse)
library(readxl)
library(RODBC)


# Enter File Information 
# Be sure to delete unused rows on Sample and Results tabs of the template
data_path <- "//deqlab1/BioMon/Bugs/2023/2022_BioMon_ColeEco_DRAFT_Data_9-1-23_SLH edited.xlsx"
org = "OREGONDEQ"

# pull in hybrid taxon table from SQL BioMon database 
# this files lives and is updated in this BioMon repo 
hybrid_taxon <- read_excel("bugs analyses/Taxonomy/ODEQ_Taxonomy_dec22.xlsx") %>% 
  mutate(AWQMS_tax_uid = as.character(AWQMS_tax_uid))

# this is the downloaded AWQMS taxon table. I don't know the best way to get this integrated without downloaded the latest version each time. 
awqms_t <- read_excel("RawDataUpload2AWQMS/AWQMS_Taxon2oct23.xlsx", sheet = "Taxon") %>% 
         mutate(UID = as.character(UID))

#bring in data 
Samp <- read_excel(data_path, sheet = "Sample") %>%
  filter(!is.na('Monitoring Location ID')) %>% 
  select("Sample ID (Locked)","Project ID",Comments,"Sample Date", "Monitoring Location ID", #"Alternate Project ID"
         Taxonomist,"Subsample Amount", "Habitat Type","Field QA","Lab QA") %>%  
  rename(act_id= "Sample ID (Locked)",project = "Project ID",
         #AltProjectID = "Alternate Project ID",
         act_comments = Comments, Subsample_fraction_value = "Subsample Amount",
         Habitat_sampled = "Habitat Type",Field_QA = "Field QA",Lab_QA = "Lab QA",
         Date = "Sample Date", MLocID = "Monitoring Location ID",
         act_comments = Comments) %>%
  mutate(Methods_ok = "Yes",
         Fixed_Count = "fixed count 500",
         #Subsample_fraction_value = (subsample_squares/30), #Subsample_fraction_value moved to rename because there's no need to divide by total number of caton tray squares
         Area_sampled = 8) 

counts <- read_excel(data_path, sheet = "Results") %>% 
  select("Sample ID", "Taxonomic Serial Number (Locked)", # "Monitoring Location ID","Sample Date",
         "DEQ Taxon (Locked)","DEQ Taxon Code", 
         "Taxonomic Serial Number (Locked)","Stage ID","Count Value","Unique Taxon","Comments") %>%
  rename(act_id="Sample ID",TaxonSN_org = "Taxonomic Serial Number (Locked)",
         StageID = "Stage ID",Count = "Count Value",UniqueTaxon = "Unique Taxon",
         Taxon = "DEQ Taxon (Locked)", DEQ_TAXON = "DEQ Taxon Code",
         res_comments = "Comments") %>% 
  mutate(Taxon = case_when(Taxon == 'Eukiefferiella Pseudomontana group' ~ 'Eukiefferiella Pseudomontana Gr.',
                           Taxon == 'Tvetenia Bavarica group' ~ 'Tvetenia Bavarica Gr.',
                           Taxon == 'Eukiefferiella Claripennis group' ~ 'Eukiefferiella Claripennis Gr.',
                           Taxon == 'Eukiefferiella Brehmi group' ~ 'Eukiefferiella Brehmi Gr.',
                           Taxon == 'Heterotrissocladius Marcidus group' ~ 'Heterotrissocladius Marcidus Gr.',
                           Taxon == 'Microtendipes Pedellus group' ~ 'Microtendipes Pedellus gr.',
                           Taxon == 'Rhyacophila Betteni group' ~ 'Rhyacophila Betteni Gr.',
                           Taxon == 'Zapada Oregonensis group' ~ 'Zapada Oregonensis Gr.',
                           TRUE ~ Taxon))

### build table with sample info  
d_samp <- counts %>% 
  left_join(Samp, by = 'act_id') %>%
  left_join(hybrid_taxon,by = 'DEQ_TAXON') %>%
  left_join(awqms_t, by = c('AWQMS_tax_uid'= 'UID')) %>%
  mutate(colmeth = case_when(Habitat_sampled == 'R' ~ "Benthic Kick - Riffle",
                             Habitat_sampled == 'T' ~ "Benthic Kick - Transect",
                             Habitat_sampled == 'P' ~ "Benthic Kick - Pool",
                             Habitat_sampled == 'G' ~ "Benthic Kick - Glide",
                             TRUE ~ as.character('Benthic Kick - Mixed')),
         act_type = case_when(Field_QA %in% c('S','FP') & Lab_QA %in% c('S','CP') ~ 'SR', # translation in AWQMS
                              Field_QA == 'FD' & Lab_QA %in% c('S','CP') ~ 'QCFR',
                              Field_QA %in% c('S','FP','FD') & Lab_QA == 'CD' ~ 'QCLR',
                              TRUE ~ 'ERROR'), # should this default to sample routine for missing values? 
         media = 'Biological',
         assemblage = 'Benthic Macroinvertebrates',
         equip = 'D-Frame Net',
         status = if_else(Methods_ok == 'Yes','Final','Rejected'),
         value = 'Actual', #when would this be estimated? 
         method = Fixed_Count,
         context = 'OREGONDEQ',
         method_name = 'Benthic Macroinvertebrates',
         qual = if_else(Methods_ok == 'Yes','','ALT'),
         speciesID = if_else(UniqueTaxon == 'YES', 'UniqueTaxon','AmbiguousTaxon'),
         habit = "",
         Project1 = project,
         Comments = '')%>%
  select(act_id,act_type,Field_QA,Lab_QA,media,Date,Project1,MLocID,assemblage,act_comments,colmeth,equip,
         status,qual,value,Name,StageID,habit,FFG,Voltine,speciesID,method,context,DEQ_TAXON,
         Count,Area_sampled,Subsample_fraction_value,res_comments)  ## missing Taxonomy_lab, FFG,Volt from taxa table


### confirm act_ids #### - should match the number in the Samp table
d_actid_check <- d_samp %>% 
  select(act_id,act_type,Field_QA,Lab_QA) %>%
  distinct()

#create a table of counts 
d_count <- d_samp %>% 
  mutate(char = 'Count',
         unit = 'count',
         value = 'Actual', #when would this be estimated? 
         intent = 'Population Census') %>%
  select(act_id,act_type,media,Date,Project1,MLocID,assemblage,act_comments,colmeth,equip,char,Count,unit,
         status,qual,value,Name,StageID,habit,FFG,Voltine,speciesID,intent,res_comments,method,context,DEQ_TAXON) %>%  ## missing Taxonomy_lab, FFG,Volt from taxa table
  rename(result = Count)


#### density #####
d_density <- d_samp %>% 
  mutate(char = 'Density',
         result = (Count/(Area_sampled*Subsample_fraction_value)),
         unit = '#/ft2',
         value = 'Calculated', #when would this be estimated? 
         intent = 'Species Density') %>%
  select(act_id,act_type,media,Date,Project1,MLocID,assemblage,act_comments,colmeth,equip,char,result,unit,
         status,qual,value,Name,StageID,habit,FFG,Voltine,speciesID,intent,res_comments,method,context,DEQ_TAXON)  ## missing Taxonomy_lab, FFG,Volt from taxa table

# bring both together 
d <-rbind(d_count,d_density) 

# this is the AWQMS load file - use import configuration 1266

write.csv(d,"//deqlab1/BioMon/Bugs/2023/DEQ2022_RResults2_revised.csv",na = "",row.names = FALSE)

