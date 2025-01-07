### script to apply new 2026 biocrtieria AM 

library(tidyverse)
library(openxlsx)

mloc_aves <-joined_OE_BCG_MMI_good %>% ### does not include "qualified" data
  group_by(MLocID) %>% 
  summarise(n= n(),
            MMI_site_avg=mean(MMI_rescale), 
            OE_site_avg=mean(OoverE),
            BCG_site_avg=mean(Continuous_BCG_Level))%>%
  mutate(BCG_level=round(BCG_site_avg),
         BCG_level=as.factor(BCG_level))%>%
  #filter(!is.na(BCG_level)&!is.na(MMI_site_avg)) %>% 
  left_join(mloc_info, by = "MLocID") %>% 
  filter(OE_site_avg >0)

AU_aves <- joined_OE_BCG_MMI_good %>% 
  group_by(AU_ID) %>% 
  summarise(n= n(),
            MMI_AU_avg=mean(MMI_rescale), 
            OE_AU_avg=mean(OoverE),
            BCG_AU_avg=mean(Continuous_BCG_Level))%>%
  mutate(BCG_level=round(BCG_AU_avg),
         BCG_level=as.factor(BCG_level))%>%
  #filter(!is.na(BCG_level)&!is.na(MMI_AU_avg)) #%>% 
  #left_join(mloc_info, by = "MLocID") %>% 
  filter(OE_AU_avg >0)

# non watershed AUs 
non_WS_Cat <- AU_aves %>% 
            filter(!str_detect(AU_ID, "^OR_WS")) %>%
            mutate(AU_Cat = case_when(MMI_AU_avg <= 0.81 & OE_AU_avg <= 0.79 ~ '5',
                              
                              MMI_AU_avg <= 0.90 & MMI_AU_avg > 0.81 & 
                                OE_AU_avg <= 0.91 & OE_AU_avg > 0.79 ~ '3C',
                              
                              MMI_AU_avg > 0.90 & OE_AU_avg > 0.91 ~ '2',
                              
                              MMI_AU_avg <= 0.81 &  OE_AU_avg > 0.91 ~ '3B', #OE good, mmi poor',
                              MMI_AU_avg >0.90 &  OE_AU_avg <= 0.79 ~ '3B', #OE poor, mmi good',
                              
                              OE_AU_avg <= 0.91 & OE_AU_avg > 0.79 & 
                                MMI_AU_avg <= 0.81 ~ '3B', #OE fair, mmi poor',
                              
                              OE_AU_avg <= 0.91 & OE_AU_avg > 0.79 & 
                                MMI_AU_avg > 0.90 ~ '3C', #OE fair, mmi good',
                              
                              MMI_AU_avg <= 0.90 & MMI_AU_avg> 0.81 &
                                OE_AU_avg > 0.91 ~ '3C', #OE good, mmi fair',
                              
                              MMI_AU_avg <= 0.90 & MMI_AU_avg > 0.81 & 
                                OE_AU_avg <= 0.79 ~ '3B', #OE poor, mmi fair',
                              
                              TRUE ~ 'ERROR'))

#### compare to 2024 IR 

non_WS_24 <-  read.xlsx('2024_biocriteria_status.xlsx', 
                        sheet = 'AU Decisions') %>% 
              select(AU_ID,AU_Name,HUC12,Char_Name,final_AU_cat,Rationale,stations,Year_listed,year_last_assessed)

non_WS_compare <- non_WS_24 %>% 
           filter(!str_detect(AU_ID, "^OR_WS")) %>%
           left_join(non_WS_Cat, by = 'AU_ID')

write.csv(non_WS_compare,"non_WS_compare.csv")

#watershed AUs 

WS_Cat <- mloc_aves %>% 
  filter(str_detect(AU_ID, "^OR_WS")) %>%
  mutate(Mloc_Cat = case_when(MMI_site_avg <= 0.81 & OE_site_avg <= 0.79 ~ '5',
                                              
                                              MMI_site_avg <= 0.90 & MMI_site_avg > 0.81 & 
                                                OE_site_avg <= 0.91 & OE_site_avg > 0.79 ~ '3C',
                                              
                                              MMI_site_avg > 0.90 & OE_site_avg > 0.91 ~ '2',
                                              
                                              MMI_site_avg <= 0.81 &  OE_site_avg > 0.91 ~ '3B', #OE good, mmi poor',
                                              MMI_site_avg >0.90 &  OE_site_avg <= 0.79 ~ '3B', #OE poor, mmi good',
                                              
                                              OE_site_avg <= 0.91 & OE_site_avg > 0.79 & 
                                                MMI_site_avg <= 0.81 ~ '3B', #OE fair, mmi poor',
                                              
                                              OE_site_avg <= 0.91 & OE_site_avg > 0.79 & 
                                                MMI_site_avg > 0.90 ~ '3C', #OE fair, mmi good',
                                              
                                              MMI_site_avg <= 0.90 & MMI_site_avg> 0.81 &
                                                OE_site_avg > 0.91 ~ '3C', #OE good, mmi fair',
                                              
                                              MMI_site_avg <= 0.90 & MMI_site_avg > 0.81 & 
                                                OE_site_avg <= 0.79 ~ '3B', #OE poor, mmi fair',
                                              
                                              TRUE ~ 'ERROR'))

WS_compare <- WS_Cat %>% 
  #filter(str_detect(AU_ID, "^OR_WS")) %>%
  left_join(non_WS_24, by = 'AU_ID') 

write.csv(WS_compare,"WS_compare.csv")
