library(tidyverse)

# bring in OTU csv file
otus <- read.csv('bugs analyses/stressor/OTU_bsti_v2.csv')



# bring in the BSTI tolerance data
load('bugs analyses/stressor/wa_BSTI.v2_apr23.Rdata')


bsti.tols <- as.data.frame(wa_BSTI.v2_apr23$coefficients)
bsti.tols <- rownames_to_column(bsti.tols, var = 'OTU_BSTI.v2')
bsti.tols <- bsti.tols %>% select(-Tolerances)


# thermal tolerances: use MWMT tols in attributes table; MTTI has fewer taxa at OTUs levels, MWMT has tols at multiple taxa levels

                                                                                    # ORWA BCG model
                                                                                    #attributes <- read.csv('ORWA_Attributes_20250528_updated 20260421 by SSullivan.csv')

                                                                                    # # bring in the MTTI tolerance data
                                                                                    # load('bugs analyses/stressor/wa_MTTI.11.23_tot.abund150.Rdata')
                                                                                    # 
                                                                                    # 
                                                                                    # mtti.tols <- as.data.frame(wa_MTTI.mar23$coefficients)
                                                                                    # mtti.tols <- rownames_to_column(mtti.tols, var = 'OTU_MTTI')
                                                                                    # mtti.tols <- mtti.tols %>% select(-Tolerances)

                                                                                    
# join the BSTI tolerance to the OTU table

otus_tols <- otus %>%
  left_join(bsti.tols, by = 'OTU_BSTI.v2')  %>%
  rename(BSTI_WA.optima = Optima)



otus_tols$BSTI_WA.optima <-  round(otus_tols$BSTI_WA.optima, digits = 1)

otus_tols2 <- otus_tols %>% select(Taxon = Taxon_orig, BSTI_WA.optima)


# bring in the attributes table from ORDEQBioassessment package (Travis'), or other attribute file from Leppo GitHub

  # ORDEQBioassessment package
  attributes <- ORDEQBioassessment::attribute_table

        # Maritime NW BCG model
        #attributes <- read.csv('MariNW_Attributes_20241121.csv')
        
        # ORWA BCG model
        #attributes <- read.csv('ORWA_Attributes_20250528_updated 20260421 by SSullivan.csv')
        


# add in the optima to attributes
attributes.2 <- attributes %>%
  left_join(otus_tols2, by = 'Taxon')





attributes.2 <- attributes.2 |>
  relocate(BSTI_WA.optima, .after = FSBI_2012)


# write csv for export to Leppo git/ORDEQBioassessment -- rename accordingly to project and date

write.csv(attributes.2, 'ORWA_Attributes_for CASTool metadata.csv')





