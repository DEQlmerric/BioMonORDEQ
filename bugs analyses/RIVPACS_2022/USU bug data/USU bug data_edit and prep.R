#
#
#
# Bring in bug data from USU (sent by Jennifer Courtwright on 12/20/23)

# Use this code to modify the data and make it work with DEQ's data
    # drop unnecessary sites
    # drop samples with non-compatible methods

library(openxlsx)
library(tidyverse)

# read in bug data
usu_sites <- read.xlsx("bugs analyses/RIVPACS_2022/USU bug data/Shannon_data_request2.xlsx", sheet = "sites")

usu_samps <- read.xlsx("bugs analyses/RIVPACS_2022/USU bug data/Shannon_data_request2.xlsx", sheet = "samples")

usu_bugs <- read.xlsx("bugs analyses/RIVPACS_2022/USU bug data/Shannon_data_request2.xlsx", sheet = "taxa")

##############################

# remove 3 'dfw_' sites/samples that are duplicated in AWQMS (all 3 were "most disturbed")

##############################

        # DFW_39720	Lobster Cr S Fk     sampleId =  151781
        # DFW_7057	Johnson Cr                      151785
        # DFW_8329	Sutherlin Cr                  151788

usu_sites2 <- usu_sites %>%
                  dplyr::filter(!MLocID %in% c('DFW_39720', 'DFW_7057', 'DFW_8329')) # results in 85 ref, 103 most disturbed


usu_samps2 <- usu_samps %>%
                  dplyr::filter(!siteName %in% c('DFW_39720', 'DFW_7057', 'DFW_8329')) 
# results in 394 samples....many duplicates from same station
# will need to determine which samples to keep later, based on 
                                                                # spatial proximity to other USU and DEQ sites
                                                                # total OTU abundance, post rarify --choose the samples that have closest to 300 ct


usu_bugs2 <- usu_bugs %>%
  dplyr::filter(!sampleId %in% c(151781, 151785, 151788))
# drops 142 rows from the three samples




########################################

#   drop samples with incompatible methods, etc.

#########################################



###### drop one sample collected in May (outside index period): 1130,	21842,	FLYNN Creek at RM 1.71 (Alsea),	REFERENCE
          # sampleId = 151167
          # unfortunately REF, but DEQ has samples from this creek

    # samples table
    usu_samps3 <- usu_samps2 %>% dplyr::filter(sampleId != 151167)
    # bugs table
    usu_bugs3  <- usu_bugs2  %>% dplyr::filter(sampleId != 151167)


######  drop 3 pool samples, 2 sites
          # siteId = 6758 Columbia River, sampleId = 156612
          #         15273 Camp Creek,     sample ID = 104090, 104092

    # sites table
    usu_sites4 <- usu_sites2 %>% dplyr::filter(NAMC_siteId != 6758) # Columbia River = non-wadeable, remove site entirely
    
    # samples table
    usu_samps4 <- usu_samps3 %>% dplyr::filter(!sampleId %in% c(156612, 104090, 104092))
    
    # bugs table
    usu_bugs4 <- usu_bugs3 %>% dplyr::filter(!sampleId %in% c(156612, 104090, 104092))


#####   drop 3 samples with low total area sampled
        # siteId 12717, 15273 (2x)   -----> both of these have another sample with correct effort, so don't delete from sites table
        # sampleId = 155922, 104091, 104093

   
    # samples
    usu_samps5 <- usu_samps4 %>% dplyr::filter(!sampleId %in% c(155922, 104091, 104093))
    
    # bugs
    usu_bugs5 <- usu_bugs4 %>% dplyr::filter(!sampleId %in% c(155922, 104091, 104093))
    
    
#####   drop 12 samples with "qualitative" samples
        #  these sites all have non-qualitative samples available, so don't drop from sites table (all mainstem Deschutes sites--above Billy Chinook)
        # sampleId = 126683, 127416, 127889, 126685, 127887, 126687, 127418, 127891, 126689, 127420, 127422, 127893
 
    # samples
    usu_samps6 <- usu_samps5 %>% dplyr::filter(!sampleId %in% c(126683, 127416, 127889, 126685, 127887, 126687, 127418, 127891, 126689, 127420, 127422, 127893))
    
    # bugs
    usu_bugs6 <- usu_bugs5 %>% dplyr::filter(!sampleId %in% c(126683, 127416, 127889, 126685, 127887, 126687, 127418, 127891, 126689, 127420, 127422, 127893))
    
        
       
    
######    drop non-wadeable sites and samples
            # decision on which sites to drop based on whether or not SLH would survey with wadeable protocols for NRSA
            # drop 32 sites ---> all MOST DISTURBED
            # drop 57 samples
    
    # sites 
    non.wadeable <- c('CROOKED RIVER','DESCHUTES RIVER','Deschutes River at Culver','Deschutes River at Lower Bridge','Deschutes River at Mile 125',
        'Deschutes River at Odin Falls','Deschutes River at Steelhead Falls','GRANDE RONDE RIVER','John Day','JOHN DAY','JOHN DAY RIVER',
        'PR-RV-10239:John Day River','PR-RV-10751:John Day River','PR-RV-11039:Crooked River','PR-RV-11087:John Day River',
        'Tualatin River at rivermile 1.5 near West Linn','Umatilla River')
        
    usu_sites7 <- usu_sites4 %>% dplyr::filter(!StationDes %in% non.wadeable)    # 155 sites remain
        

   # samples 
    
    usu_samps7 <- usu_samps6 %>% 
      dplyr::filter(!sampleId %in% c(168895,168896,168902,168903,168904,168905,126688,127421,127892,126684,127417,127886,127423,126682,127415,127888,
                    126686,127419,127890,171860,146650,150977,157308,171331,146651,150978,157309,171332,146652,150979,157310,171333,146653,150980,
                    157311,171334,146654,150981,157312,171335,146655,151013,157313,171336,167717,167715,167722,167704,211468,167716,167723,167705,
                    158406,158409,151742,150572,150571))
  
    # bugs 
    
    usu_bugs7 <- usu_bugs6 %>% 
      dplyr::filter(!sampleId %in% c(168895,168896,168902,168903,168904,168905,126688,127421,127892,126684,127417,127886,127423,126682,127415,127888,
                                     126686,127419,127890,171860,146650,150977,157308,171331,146651,150978,157309,171332,146652,150979,157310,171333,146653,150980,
                                     157311,171334,146654,150981,157312,171335,146655,151013,157313,171336,167717,167715,167722,167704,211468,167716,167723,167705,
                                     158406,158409,151742,150572,150571))
    
    

    
##############################################################
    
    
#     edit tables to only retain necessary information
    
##############################################################
    
    
usu_sites_limited <- usu_sites7 %>%
      dplyr::select(siteId = NAMC_siteId, MLocID, StationDes, Lat_DD, Long_DD, Eco3, COMID, Ref2020_FINAL, owner)

        
usu_samps_limited <- usu_samps7 %>%
  dplyr::select(sampleId, siteId, MLocID = siteName, StationDes = waterbodyName, Lat_DD = siteLatitude, Long_DD = siteLongitude, 
                sampleDate, Habitat = habitatName, area)

usu_bugs_limited <- usu_bugs7 %>%
  dplyr::select(sampleId, scientificName, phylum, class, order, family, subFamily, genus, species, lifeStage, count=splitCount)



       
##############################################################


#     link to DEQ taxonomy table

##############################################################

taxonomy <- read.xlsx("bugs analyses/Taxonomy/ODEQ_Taxonomy_dec22.xlsx")
 
