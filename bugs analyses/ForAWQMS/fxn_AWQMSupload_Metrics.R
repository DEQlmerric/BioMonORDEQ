# function to generate AWQMS metric upload from summary bug table 
# L. Merrick 7/31/2023

AWQMS_Metric <- function() { 
  
  metric_upload <- oe.stress.mets.sta %>% 
                   dplyr::select(-MLocID.y,
                                 -StationDes,
                                 -long,
                                 -lat,  
                                 -ELEV_m,
                                 -precip_mm,
                                 -temp_Cx10,
                                 -W_E,
                                 -Wade_Boat,
                                 -Eco3,
                                 -Eco2,
                                 -UN.rich, # do we need this?
                                 -pct_UN, # do we need this?
                                 -O,
                                 -E,
                                 -OoverE,
                                 -Onull,
                                 -Enull,
                                 -OoverE.null,
                                 -oe.cond,
                                 -BC,
                                 -BC.null,
                                 -outlier.05,
                                 -outlier.01,
                                 #-TS,
                                 -BSTI,
                                 -MTI,
                                 #-HUC6_Name,
                                 -HUC8_Name,
                                 #-HUC10_Name,
                                 -HUC12_Name) %>%
                  # rename to match AWQMS 
               dplyr::rename('Total Taxa Richness' = total.richness,
                            'Coleoptera Taxa Richness' = Coleoptera.rich,
                            'Diptera Taxa Richness' = Diptera.rich,
                            'Ephemeroptera Taxa Richness'= Ephemeroptera.rich,
                            'Plecoptera Taxa Richness'=Plecoptera.rich,
                            'Trichoptera Taxa Richness'=Trichoptera.rich,
                            'Baetidae Taxa Richness' = Baetidae.rich,
                            'Chironomidae Taxa Richness'= Chironomidae.rich,
                            'Hydropsychidae Taxa Richness'= Hydropsychidae.rich,
                            'EPT Taxa Richness'= EPT.rich,
                            '% Coleoptera Taxa'= pct_Coleoptera,
                            '% Diptera Taxa'= pct_Diptera,
                            '% Ephemeroptera Taxa'= pct_Ephemeroptera,
                            '% Plecoptera Taxa'= pct_Plecoptera,
                            '% Trichoptera Taxa'= pct_Trichoptera,
                            '% EPT Taxa'= pct_EPT,
                            'Multivoltine Taxa Richness' = multivoltine.rich,
                            'Semivoltine Taxa Richness'= semivoltine.rich,
                            'Univoltine Taxa Richness' = univoltine.rich,
                            '% Multivoltine Taxa' = pct_multivoltine,
                            '% Semivoltine Taxa' = pct_semivoltine,
                            '% Univoltine Taxa' = pct_univoltine,
                            'Collector Filterer Richness'= CF.rich,
                            'Collector Gatherer Richness' = CG.rich,
                            'Macrophyte Herbivore Richness' = MH.rich,
                            'Omnivore Richness' = OM.rich,
                            'Parasite Richness' = PA.rich,
                            'Piercer Herbivore Richness' = PH.rich,
                            'Predator Richness' = PR.rich,
                            'Scraper Richness'= SC.rich,
                            'Shredder Richness' = SH.rich,
                            '% Collector Filterer Taxa'= pct_CF,
                            '% Collector Gatherer Taxa'= pct_CG,
                            '% Macrophyte Herbivore Taxa'= pct_MH,
                            '% Omnivore Taxa' = pct_OM,
                            '% Parasite Taxa'= pct_PA,
                            '% Piercer Herbivore Taxa'= pct_PH,
                            '% Predator Taxa'= pct_PR,
                            '% Scraper Taxa'= pct_SC,
                            '% Shredder Taxa'= pct_SH,
                            '% Individuals in top 5 taxa'= pct_Dom.5,
                            '% Individuals in top 3 taxa' = pct_Dom.3,
                            '% Individuals in top 1 taxa'= pct_Dom.1,
                            'Shannon Diversity'= Shannon_diversity,
                            'Simpson Diversity'=Simpson_diversity,
                            'Non-Insect Taxa Richness'= Non.Insect_richness,
                            '% Non-Insect Taxa'= pct_Non.Insect,
                            'RIVPACS Sub-Sample Total Abundance'= tot.abund.RIV) %>%
                            #'Stressor Models Total Abundance'= total.abundance.STR #do we need this?
                            
                       # make long format
                  tidyr::gather('Total Taxa Richness','Coleoptera Taxa Richness','Diptera Taxa Richness','Ephemeroptera Taxa Richness',
                              'Plecoptera Taxa Richness','Trichoptera Taxa Richness','Baetidae Taxa Richness','Chironomidae Taxa Richness',
                              'Hydropsychidae Taxa Richness','EPT Taxa Richness','% Coleoptera Taxa','% Diptera Taxa','% Ephemeroptera Taxa',
                              '% Plecoptera Taxa','% Trichoptera Taxa','% EPT Taxa','Multivoltine Taxa Richness','Semivoltine Taxa Richness',
                              'Univoltine Taxa Richness','% Multivoltine Taxa','% Semivoltine Taxa','% Univoltine Taxa','Collector Filterer Richness',
                              'Collector Gatherer Richness','Macrophyte Herbivore Richness','Omnivore Richness','Parasite Richness','Piercer Herbivore Richness',
                              'Predator Richness','Scraper Richness','Shredder Richness','% Collector Filterer Taxa','% Collector Gatherer Taxa',
                              '% Macrophyte Herbivore Taxa','% Omnivore Taxa','% Parasite Taxa','% Piercer Herbivore Taxa','% Predator Taxa',
                              '% Scraper Taxa','% Shredder Taxa','% Individuals in top 5 taxa','% Individuals in top 3 taxa','% Individuals in top 1 taxa',
                              'Shannon Diversity','Simpson Diversity','Non-Insect Taxa Richness','% Non-Insect Taxa','Evenness','RIVPACS Sub-Sample Total Abundance',
                              key = "ID", value = "Score_long") %>% #'Stressor Models Total Abundance'
                       # add additional awqms required columns
                dplyr::mutate(Media = "Biological",
                              CollectionMethod = Habitat,
                              assemblage = "Benthic Macroinvertebrates",
                              equipment = "D-Frame Net") %>%
                       #Add context 
              dplyr::mutate(context = ifelse(ID == 'Total Taxa Richness',"USEPA",
                                               ifelse(ID == '% Ephemeroptera Taxa',"USEPA",       
                                                      ifelse(ID == '% EPT Taxa',"USEPA",
                                                             ifelse(ID == '% Individuals in top 3 taxa',"USEPA",
                                                                    ifelse(ID == '% Individuals in top 5 taxa',"USEPA",
                                                                           ifelse(ID == '% Non-Insect Taxa',"USEPA",
                                                                                  ifelse(ID == 'Ephemeroptera Taxa Richness',"USEPA",
                                                                                         ifelse(ID == 'EPT Taxa Richness',"USEPA",
                                                                                                ifelse(ID == 'Scraper Richness',"USEPA",
                                                                                                       ifelse(ID == 'Shannon Diversity',"USEPA",
                                                                                                              ifelse(ID == 'Shredder Richness', "USEPA",
                                                                                                                     ifelse(ID == 'Total Taxa Richness', "USEPA","OREGONDEQ"))))))))))))) %>%      
                       #round scores - AWQMS doesn't like long numbers 
    dplyr::mutate(score = round(Score_long,2),
                              DQL = ifelse(Use.303d == 'No',"E","A"),
                              com = reason.no.303) %>%
    dplyr::filter(!score == 'NA') %>%
    dplyr::select(Sample,Activity_Type,Media,Date,Project1,MLocID,ID,context,
                              score,DQL,com,CollectionMethod,assemblage,equipment)
     return(metric_upload)
  
  
  
   }

     # end of function
  
  