library(tidyverse)

###Exploring variance and Type I and II error rates 

##### site repeat variability #### 
mloc_info <- joined_OE_BCG_MMI %>% 
  select(org_id,AU_ID,MLocID,StationDes,COMID,
         GNIS_Name,ReferenceSite) %>% 
  distinct()


mloc_repeat <-joined_OE_BCG_MMI %>% 
  group_by(MLocID) %>% 
  summarise(n= n(),
            MMI_site_avg=mean(MMI),
            MMI_site_sd= sd(MMI),
            OE_site_avg=mean(OoverE),
            OE_site_sd= sd(OoverE),
            BCG_site_avg=mean(Continuous_BCG_Level),
            BCG_site_sd= sd(Continuous_BCG_Level)) %>% 
  filter(n>5) %>% 
  left_join(mloc_info, by = "MLocID") %>% 
  filter(!ReferenceSite == "MODERATELY DISTURBED") # I'm not sure about removing these 

mloc_repeat_sites <- mloc_repeat$MLocID

mloc_repeat_data <- joined_OE_BCG_MMI %>% 
                    mutate(repeat_site = case_when(MLocID %in% mloc_repeat_sites ~ 1,
                              TRUE ~ 0)) %>% 
                    filter(repeat_site == 1)

### looking at catergory changes over time. 
repeat_cats <- mloc_repeat_data %>% 
  #filter(!str_detect(AU_ID, "^OR_WS")) %>%
  mutate(AU_Cat = case_when(MMI <= 0.5 & OoverE <= 0.75 ~ '5',
                            MMI<= 0.65 & MMI > 0.5 & 
                              OoverE <= 0.75 & Continuous_BCG_Level > 4.5 ~ '5',
                            
                            OoverE <= 0.92 & OoverE > 0.75 & 
                              MMI <= 0.5 & Continuous_BCG_Level > 4.5 ~ '5',
                            
                            MMI > 0.65 & OoverE > 0.92 ~ '2',
                            
                            MMI <= 0.65 & MMI > 0.5 &
                              OoverE > 0.92 & Continuous_BCG_Level < 3.5 ~ '2',
                            
                            OoverE <= 0.92 & OoverE > 0.75 & 
                              MMI > 0.65 & Continuous_BCG_Level < 3.5 ~ '2',
                            
                            MMI <= 0.65 & MMI > 0.5 & 
                              OoverE <= 0.92 & OoverE > 0.75 ~ '3C',
                            
                            MMI <= 0.65 & MMI > 0.5 & 
                              OoverE <= 0.75 & Continuous_BCG_Level <= 4.5 ~ '3C',
                            
                            OoverE <= 0.92 & OoverE > 0.75 & 
                              MMI < 0.5 & Continuous_BCG_Level <= 4.5 ~ '3C',
                            
                            MMI <= 0.65 & MMI > 0.5 &
                              OoverE > 0.92 & Continuous_BCG_Level >= 3.5 ~ '3C',
                            
                            OoverE <= 0.92 & OoverE > 0.75 & 
                              MMI > 0.65 & Continuous_BCG_Level >= 3.5 ~ '3C', 
                            
                            MMI <= 0.5 & OoverE > 0.92 ~ '3B',
                            MMI >0.65 & OoverE <= 0.75 ~ '3B',
                            TRUE ~ 'ERROR'))


##### look into Type II error rates ##### 
### pull in ref screening metrics ### 
one_table_rule_all <- read.csv("Reference/one.table_rule.all.csv") %>% 
                      select(-StationDes,-Lat_DD,-Long_DD,-Eco3,-COMID,-owner)

most_aves <- mloc_repeat <-joined_OE_BCG_MMI %>% 
             group_by(MLocID) %>% 
             summarise(n= n(),
                      MMI_site_avg=mean(MMI),
                      OE_site_avg=mean(OoverE),
                      BCG_site_avg=mean(Continuous_BCG_Level)) %>% 
            left_join(mloc_info, by = "MLocID") %>% 
            filter(ReferenceSite == "MOST DISTURBED") %>% 
            left_join(one_table_rule_all, by = "MLocID")





