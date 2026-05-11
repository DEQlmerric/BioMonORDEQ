############

# First run BiomonORDEQ/bugs analyses/All_together/FrontEnd_v2.R

############



# ~~~~~~~~~~~~~~~~~~~~~~~~~

#########

# Second, join bug data to attributes table

##########

# bring in the archived attributes from Travis' bioassessment package
library(ORDEQBioassessment)
library(tidyverse)

attributes <- ORDEQBioassessment::attribute_table


bug.attributes <- bug_tax_data %>%
  filter(Sample_Method=='Benthic Kick - Targeted Riffle' | Sample_Method=='enthic Kick - Transect') %>%
  filter(Char_Name=='Count') %>%
  left_join(attributes, by = 'Taxon') %>%
  select(MLocID, act_id, SampleStart_Date, Sample_Method, Taxon, Result_Numeric, BCG_attr)
