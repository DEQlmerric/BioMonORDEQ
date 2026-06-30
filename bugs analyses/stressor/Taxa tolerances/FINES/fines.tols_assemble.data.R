

# prepare fines and bugs data for use in tolerance designations

# origin date: 6/24/26, Shannon Hubler

# objectives: 
  # 1) bring in bug data from ORWA Thermal analysis (7977 samples)

@@@@@@ join to taxonomy lookup to get all proper STE names

@@@@@@ roll up across hierarcichy

@@@@@@ add in lat longs for mapping code


  # 2) bring in fines data used to create BSTI_v2 (ORWA; 1027 samples)

        # both datasets for 1 and 2 were copied over to this project

  # 3) join together, based on samples found in FINES dataset
  # 4) export file for use in tolerance code provided by Tetra Tech

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1) bug data: 

bugs.7977 <- read.csv('bugs analyses/stressor/Taxa tolerances/FINES/_Bio_All_7977.csv')
bugs.7977 <- bugs.7977 |>
  select(BugSampleID = UniqueID_v2, Taxon = TaxaID_v2, Count, RA, NonDistinct)

# 2) fines data:

fines.1027 <- read.csv('bugs analyses/stressor/Taxa tolerances/FINES/FN_all sites.csv')
fines.1027 <- fines.1027 |> 
  select(BugSampleID = UniqueID_v2, siteID, Date, PCT_FN, Latitude = lat, Longitude = long)


# 3) join bugs and fines, based on fines samples

bugs.fines <- fines.1027 |>
  left_join(bugs.7977, by = 'BugSampleID')


    # do a check to see how many unique samples there are
    
    length(unique(bugs.fines$BugSampleID)) # kaboomshkwa!

    
# 4) export a file for use in tol code

write.csv(bugs.fines, 'bugs analyses/stressor/Taxa tolerances/FINES/bugs.fines_1027.csv')

