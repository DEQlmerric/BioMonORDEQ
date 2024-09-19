### script to apply new 2026 biocrtieria AM 

library(tidyverse)
library(openxlsx)

mloc_aves <-joined_OE_BCG_MMI%>% 
  group_by(MLocID) %>% 
  summarise(MMI_site_avg=mean(MMI), 
            OE_site_avg=mean(OoverE),
            BCG_site_avg=mean(Continuous_BCG_Level))%>%
  mutate(BCG_level=round(BCG_site_avg),
         BCG_level=as.factor(BCG_level))%>%
  filter(!is.na(BCG_level)&!is.na(MMI_site_avg)) %>% 
  left_join(mloc_info, by = "MLocID") %>% 
  filter(OE_site_avg >0)

AU_aves <- joined_OE_BCG_MMI%>% 
  group_by(AU_ID) %>% 
  summarise(n= n(),
            MMI_AU_avg=mean(MMI), 
            OE_AU_avg=mean(OoverE),
            BCG_AU_avg=mean(Continuous_BCG_Level))%>%
  mutate(BCG_level=round(BCG_AU_avg),
         BCG_level=as.factor(BCG_level))%>%
  filter(!is.na(BCG_level)&!is.na(MMI_AU_avg)) #%>% 
  #left_join(mloc_info, by = "MLocID") %>% 
  #filter(OE_AU_avg >0)

# non watershed AUs 
non_WS_Cat <- AU_aves %>% 
              filter(!str_detect(AU_ID, "^OR_WS")) %>%
              mutate(AU_Cat = case_when(MMI_AU_avg <= 0.5 & OE_AU_avg <= 0.75 ~ '5',
                                           
                                           MMI_AU_avg <= 0.65 & MMI_AU_avg > 0.5 & 
                                           OE_AU_avg <= 0.75 & BCG_AU_avg > 4.5 ~ '5',
                                           
                                           OE_AU_avg <= 0.92 & OE_AU_avg > 0.75 & 
                                           MMI_AU_avg <= 0.5 & BCG_AU_avg > 4.5 ~ '5',
                                           
                                           MMI_AU_avg > 0.65 & OE_AU_avg > 0.92 ~ '2',
                                           
                                           MMI_AU_avg <= 0.65 & MMI_AU_avg > 0.5 &
                                           OE_AU_avg > 0.92 & BCG_AU_avg < 3.5 ~ '2',
                                           
                                           OE_AU_avg <= 0.92 & OE_AU_avg > 0.75 & 
                                           MMI_AU_avg > 0.65 & BCG_AU_avg < 3.5 ~ '2',
                                           
                                           MMI_AU_avg <= 0.65 & MMI_AU_avg > 0.5 & 
                                           OE_AU_avg <= 0.92 & OE_AU_avg > 0.75 ~ '3C',
                                           
                                           MMI_AU_avg <= 0.65 & MMI_AU_avg > 0.5 & 
                                           OE_AU_avg <= 0.75 & BCG_AU_avg <= 4.5 ~ '3C',
                                           
                                           OE_AU_avg <= 0.92 & OE_AU_avg > 0.75 & 
                                           MMI_AU_avg < 0.5 & BCG_AU_avg <= 4.5 ~ '3C',
                                           
                                           MMI_AU_avg <= 0.65 & MMI_AU_avg > 0.5 &
                                           OE_AU_avg > 0.92 & BCG_AU_avg >= 3.5 ~ '3C',
                                           
                                           OE_AU_avg <= 0.92 & OE_AU_avg > 0.75 & 
                                           MMI_AU_avg > 0.65 & BCG_AU_avg >= 3.5 ~ '3C', 
                                           
                                           MMI_AU_avg <= 0.5 & OE_AU_avg > 0.92 ~ '3B',
                                           MMI_AU_avg >0.65 & OE_AU_avg <= 0.75 ~ '3B',
                                           TRUE ~ 'ERROR')) 


#### compare to 2024 IR 

non_WS_24 <-  read.xlsx('2024_biocriteria_status.xlsx', 
                        sheet = 'AU Decisions') %>% 
              select(AU_ID,AU_Name,HUC12,Char_Name,final_AU_cat,Rationale,stations,Year_listed,year_last_assessed)

non_WS_compare <- non_WS_24 %>% 
           filter(!str_detect(AU_ID, "^OR_WS")) %>%
           left_join(non_WS_Cat, by = 'AU_ID') %>% 
           filter(!is.na(n))

write.csv(non_WS_compare,"non_WS_compare.csv")

#watershed AUs 

WS_Cat <- mloc_aves %>% 
  filter(str_detect(AU_ID, "^OR_WS")) %>%
  mutate(Mloc_Cat = case_when(MMI_site_avg <= 0.5 & OE_site_avg <= 0.75 ~ '5',
                            
                            MMI_site_avg <= 0.65 & MMI_site_avg > 0.5 & 
                              OE_site_avg <= 0.75 & BCG_site_avg > 4.5 ~ '5',
                            
                            OE_site_avg <= 0.92 & OE_site_avg > 0.75 & 
                              MMI_site_avg <= 0.5 & BCG_site_avg > 4.5 ~ '5',
                            
                            MMI_site_avg > 0.65 & OE_site_avg > 0.92 ~ '2',
                            
                            MMI_site_avg <= 0.65 & MMI_site_avg > 0.5 &
                              OE_site_avg > 0.92 & BCG_site_avg < 3.5 ~ '2',
                            
                            OE_site_avg <= 0.92 & OE_site_avg > 0.75 & 
                              MMI_site_avg > 0.65 & BCG_site_avg < 3.5 ~ '2',
                            
                            MMI_site_avg <= 0.65 & MMI_site_avg > 0.5 & 
                              OE_site_avg <= 0.92 & OE_site_avg > 0.75 ~ '3C',
                            
                            MMI_site_avg <= 0.65 & MMI_site_avg > 0.5 & 
                              OE_site_avg <= 0.75 & BCG_site_avg <= 4.5 ~ '3C',
                            
                            OE_site_avg <= 0.92 & OE_site_avg > 0.75 & 
                              MMI_site_avg < 0.5 & BCG_site_avg <= 4.5 ~ '3C',
                            
                            MMI_site_avg <= 0.65 & MMI_site_avg > 0.5 &
                              OE_site_avg > 0.92 & BCG_site_avg >= 3.5 ~ '3C',
                            
                            OE_site_avg <= 0.92 & OE_site_avg > 0.75 & 
                              MMI_site_avg > 0.65 & BCG_site_avg >= 3.5 ~ '3C', 
                            
                            MMI_site_avg <= 0.5 & OE_site_avg > 0.92 ~ '3B',
                            MMI_site_avg >0.65 & OE_site_avg <= 0.75 ~ '3B',
                            TRUE ~ 'ERROR')) 


write.csv(WS_Cat,"WS_Cat.csv")
