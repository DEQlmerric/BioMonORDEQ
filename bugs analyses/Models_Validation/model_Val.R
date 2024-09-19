library(tidyverse)
library(openxlsx)
### first run the FrontEnd_v2.R

### build a dataframe with sample information for O/E and MMI model development 


#### get samples used to build O/E model ####
o_E_ref_modelbuild <- read.csv('bugs analyses/RIVPACS_2022/RIVPACS.2024_FINAL_ref.build_OE.csv') %>% 
  select(X,O,E,OoverE) %>% 
  rename(act_id = X,"O_MB"="O", "E_MB"="E","OoverE_MB"= "OoverE")

ref_mb <- o_E_ref_modelbuild$act_id

### get samples used to build the MMI
mmi_modelbuild <- read.csv('bugs analyses/MMI/_2024 model build/final_MMI_4.metrics.csv') %>%
  select(SAMPLEID,MMI.2024) %>% 
  rename(act_id = SAMPLEID, MMI_MB = MMI.2024)

mmi_mb <- mmi_modelbuild$act_id


## Shannon reviewed samples and added additional qualifiers 
sampl_qual <- read.xlsx('bugs analyses/Models_Validation/biocriteria_scores2024-07-18.xlsx', 
                        sheet = 'Sheet 1') %>% 
              mutate(qualifer = case_when(Filter_Low.Count %in% 'low.count'~ 1,
                                          Filters_SLH %in% c('? Choked with aquatic moss',
                                                             'duplicate?','duplicated?',
                                                              '? Glacial') ~ 0,
                                          Filters_SLH %in% c('all 3 distinctly different from DEQ',
                                                             'poor sample - 1/2 rapids and boulders',
                                                              'reference outlier - poor taxonomy') ~ 2,
                                          Filters_SLH == 'SEOR' ~ 3,
                                          EcoRegion3 == 80 ~ 3,
                                          Filters_SLH %in% c('ref outlier - lake effect')~ 4, 
                                          Filters_SLH %in% c('Glacial','Glacial - DO NOT USE')~ 5,
                                          org_id %in% c('JCWC_AW(NOSTORETID)',
                                                        'PDX_BES(NOSTORETID)',
                                                        'UDESCHUTES_WC(NOSTORETID)',
                                                        #'PBWC_WQX', keeping these for now
                                                        'CITY_GRESHAM(NOSTORETID)',
                                                        'CRBC_WQX') ~ 6,
                                          #Wade_Boat == 'boatable' ~ 7, 
                                          TRUE ~ 0),
                     qualifer_text = case_when(qualifer == 1 ~ "low count",
                                               qualifer == 2 ~ "poor sample quality",
                                               qualifer == 3 ~ "Southeast Oregon",
                                               qualifer == 4 ~ "Lake Effect",
                                               qualifer == 5 ~ "Glacier",
                                               qualifer == 6 ~ "VolMon",
                                               #qualifer == 7 ~ "boatable",
                                               TRUE ~ NA)) %>% 
              select(Filters_SLH,'Filter_Low.Count',act_id,qualifer,qualifer_text)


#### assign samples used in model development - was able to use just the mmi act_ids ###
bugs_samp_used <- sample_info %>% 
  mutate(mb_used = case_when(act_id %in% mmi_mb ~ 1,
                              TRUE ~ 0),
         #most_used = case_when(act_id %in% sampleIDs.most.dist167 ~ 1,
                               #TRUE ~ 0),
         station_used = case_when(c(mb_used == 1)~1,
                                  TRUE ~ 0)) %>%
  filter(mb_used == 1)

samples_used <- as.vector(bugs_samp_used$act_id)
stations_used <- as.vector(bugs_samp_used$MLocID)

sample_info_model <- sample_info %>% 
  mutate(act_used = case_when(act_id %in% samples_used ~ 1,
                              TRUE ~ 0),
         station_used = case_when(MLocID %in% stations_used ~ 1,
                                  TRUE ~ 0),
         ref = case_when(ReferenceSite == "MOST DISTURBED" ~ 3,
                         ReferenceSite == "REFERENCE"~ 1,
                         TRUE ~ 2),
         model_status = case_when(EcoRegion3 == 80 ~ "Outlier-SE",
                                  Wade_Boat == 'boatable' ~ "Outlier-boatable",
                                  c(act_used ==1 & ref == 1)~ "Ref_Cal",
                                  c(act_used ==1 & ref == 3)~ "Most_Cal",
                                  c(station_used == 1 & act_used == 0 &ref == 1) ~ "Ref_Cal_dup",
                                  c(station_used == 1 & act_used == 0 & ref == 3) ~ "Most_Cal_dup",
                                  c(station_used == 0 & ref == 1) ~ "Ref_Val",
                                  c(station_used == 0 & ref == 3) ~ "Most_Val",
                                  TRUE ~ "Not_Ref")) %>%
  left_join(sampl_qual, by = 'act_id') %>% 
  mutate(qualifer = case_when(c(is.na(qualifer)& EcoRegion3 == 80) ~ 3,
                                c(is.na(qualifer)& !EcoRegion3 == 80) ~ 0,
                                 Wade_Boat == 'boatable' ~ 7,
                                TRUE ~ qualifer),#) %>%
         model_status_qual = paste(model_status,qualifer, sep = "-")) %>% 
  select(act_id,station_used,ref,model_status,qualifer,qualifer_text,model_status_qual)

save(sample_info_model, file = 'bugs analyses/Models_Validation/sample_info_model.Rdata')


###### did not use this approach as the sample size is much smaller #####
##assign these as Val_dups - this was done manually by select the newest sample, preference for 
### riffle over transect, primary sample over OC, 
# act_ref_val_dup <- c('21814-ORDEQ:20000920:R:SR','21814-ORDEQ:19990922:R:SR','21848-ORDEQ:19990825:R:SR',
#    '24416-ORDEQ:20000830:T:SR','31729-ORDEQ:20040831:R:QCLR','34658-ORDEQ:20070920:R:QCFR',
#    '34658-ORDEQ:20070920:R:SR','34658-ORDEQ:20080814:R:QCFR','34722-ORDEQ:20070809:R:QCLR',
#    '34722-ORDEQ:20070809:R:SR','34722-ORDEQ:20080813:R:SR','34722-ORDEQ:20090806:R:QCLR',
#    '35633-ORDEQ:20150624:R:SR','35633-ORDEQ:20160628:R:SR','35633-ORDEQ:20170803:R:SR',
#    '35633-ORDEQ:20190912:R:SR','35633-ORDEQ:20180913:R:SR','35633-ORDEQ:20210901:NA:R:SR',
#    '35633-ORDEQ:20000812:R:SR','35685-ORDEQ:19980808:R:SR-1','35685-ORDEQ:19980808:R:SR-2',
#    '35690-ORDEQ:19980707:R:SR-2','35690-ORDEQ:19980707:R:SR-3','35701-ORDEQ:19980729:R:SR-1',
#    '35701-ORDEQ:19980729:R:SR-3','35720-ORDEQ:20150910:R:SR','35720-ORDEQ:20170822:R:SR',
#    '35720-ORDEQ:20160908:R:SR','35720-ORDEQ:20190904:R:SR','35720-ORDEQ:20191010:R:SR',
#    '35720-ORDEQ:20180703:R:SR','35720-ORDEQ:20210630:NA:R:SR','35720-ORDEQ:19980813:R:SR-1',
#    '35720-ORDEQ:19980813:R:SR-2','35720-ORDEQ:19980813:R:SR-3','35732-ORDEQ:19980807:R:SR-2',
#    '35732-ORDEQ:19980807:R:SR-3','35735-ORDEQ:19980811:R:SR-1','35745-ORDEQ:19990629:R:SR',
#    '35812-ORDEQ:20000706:R:SR','35813-ORDEQ:20000901:R:SR','35825-ORDEQ:20150915:R:SR',
#    '35825-ORDEQ:20170830:R:SR','35825-ORDEQ:20160831:R:SR','35825-ORDEQ:20190924:R:SR',
#    '35825-ORDEQ:20180919:R:SR','35825-ORDEQ:20210908:NA:R:SR')
# # same process as reference
# act_most_val_dup <- c('21822-ORDEQ:20020925:R:SR','21822-ORDEQ:20030909:R:SR',
#                      '21822-ORDEQ:20040914:R:QCLR','21822-ORDEQ:20040726:R:SR',
#                      '21822-ORDEQ:20040914:R:SR','13242-ORDEQ:19980922:R:SR',
#                      '24438-ORDEQ:20000807:T:SR','25385-ORDEQ:20010802:R:SR',
#                      '10856-ORDEQ:20210720::T:SR','34443-ORDEQ:20070612:R:QCFR',
#                      '10856-ORDEQ:20210720::T:SR')
# 
# ### reassign the samples that are val dups 
# sample_info_model$model_status <- if_else(sample_info_model$act_id %in% act_ref_val_dup,"Ref_Val_dup",sample_info_model$model_status)
# sample_info_model$model_status <- if_else(sample_info_model$act_id %in% act_most_val_dup,"Ref_Most_dup",sample_info_model$model_status)                                            
# sample_info_model$model_status_qual <- paste(sample_info_model$model_status,sample_info_model$qualifer, sep = "-")
#### keep going here #####  

### pull in O/E scores from model build 

o_E_ref_modelbuild <- read.csv('bugs analyses/RIVPACS_2022/RIVPACS.2024_FINAL_ref.build_OE.csv') %>% 
  select(X,O,E,OoverE) %>% 
  rename(act_id = X,"O_MB"="O", "E_MB"="E","OoverE_MB"= "OoverE")

# this shows the missing ref site samples - one is from 1997 and one is reported as mixed habitat sampled 
O_E_cal_2 <- o_E_ref_modelbuild %>% 
  left_join(sample_info_model, by = "act_id") 

### generate averages for by mloc and model qual 
O_E_ave <- sample_info_model %>% 
  left_join(OE_scores, by = "act_id") %>% # this is from a model run using front end v2
  drop_na(OoverE) %>% 
  filter(OoverE > 0) %>% 
  filter(model_status_qual %in% c('Ref_Cal-0','Ref_Val-0','Not_Ref-0','Most_Cal-0','Most_Val-0')) %>% 
  mutate(model_status_qual = case_when(model_status_qual %in% "Ref_Cal-0" ~ "Reference Calibration",
                                       model_status_qual %in% "Ref_Val-0" ~"Reference Validation",
                                       model_status_qual %in% "Not_Ref-0"~"Moderately Disturbed",
                                       model_status_qual %in% c("Most_Cal-0","Most_Val-0") ~"Most Disturbed")) %>% 
  group_by(MLocID,model_status_qual) %>% # station aves 1st
  summarise(n= n(),
            OoverE_ave = mean(OoverE)) 

# get a table of O_E from model build and 
O_E_mb <- o_E_ref_modelbuild %>% 
  mutate(model_status_qual = "Reference Calibration") %>% 
  group_by(model_status_qual) %>% 
  summarise(O_E_mean_mb = mean(OoverE_MB),
            O_E_sd_mb = sd(OoverE_MB))

O_E_cal <- O_E_ave %>% 
  group_by(model_status_qual) %>% 
  summarise(O_E_mean = mean(OoverE_ave),
            O_E_sd= sd(OoverE_ave)) %>% 
  left_join(O_E_mb, by = "model_status_qual")

### generate plots ### 
O_E_ave$model_status_qual <- factor(O_E_ave$model_status_qual, levels = c("Most Disturbed",
                                                                          "Moderately Disturbed","Reference Calibration",
                                                                          "Reference Validation"))

OE_val <- ggplot(data=O_E_ave, mapping=aes(x=model_status_qual, y=OoverE_ave))+geom_boxplot()
OE_val

OE_val_ref <- O_E_ave %>% 
              filter(model_status_qual %in% c("Reference Calibration","Reference Validation"))

OE_val_hist <- ggplot(data=OE_val_ref, mapping=aes(x=OoverE_ave, color=model_status_qual, fill = model_status_qual)) +geom_histogram(alpha=0.5, position="identity")
OE_val_hist


### repeat process for MMI #####
mmi_modelbuild <- read.csv('bugs analyses/MMI/_2024 model build/final_MMI_4.metrics.csv') %>%
                  select(SAMPLEID,MMI.2024) %>% 
                  rename(act_id = SAMPLEID, MMI_MB = MMI.2024)
  
# this shows the missing ref site samples - one is from 1997 and one is reported as mixed habitat sampled 
mmi_cal_2 <- mmi_modelbuild %>% 
  left_join(sample_info_model, by = "act_id") %>% 
  drop_na(model_status) %>% 
  filter(model_status_qual %in% c('Ref_Cal-0','Most_Val-0')) %>% 
  mutate(model_status_qual = case_when(model_status_qual %in% "Ref_Cal-0" ~ "Reference Calibration",
                                       model_status_qual %in% "Most_Cal-0" ~"Most Disturbed Calibration")) %>% 
  mutate(MMI_rescale = MMI_MB/0.7292573) %>% # recale is MMI/mean of refenence site 
  group_by(model_status_qual) %>% 
  summarise(MMI_mean_mb = mean(MMI_MB),
            MMI_sd_mb = sd(MMI_MB),
            MMI_mean_mb_rescale = mean(MMI_rescale),
            MMI_sd_mb_rescale = sd(MMI_rescale))

# get a table of MMO from model build and 
mmi_cal <- sample_info_model %>% 
  #left_join(mmi_modelbuild, by = "act_id") %>% 
  select(act_id,model_status_qual,MMI_MB) %>%
  left_join(MMI_scores, by = c("act_id"= "SAMPLEID")) %>% # this is from a model run using front end v2
  drop_na(MMI)

mmi_stats <- sample_info_model %>% 
  filter()
  mutate(MMI_rescale = MMI/0.7292573) %>%
  group_by(model_status_qual) %>% 
  summarise(MMI_mean = mean(MMI),
            MMI_sd= sd(MMI),
            MMI_mean_rescale = mean(MMI_rescale),
            MMI_sd_rescale= sd(MMI_rescale))

# had to break this out cause MMI_MB had NAs for Most_Val - not sure why?? 
mmi_stats_mb <- mmi_cal_2 %>% 
  group_by(model_status_qual) %>% 
  summarise(MMI_mean_mb = mean(MMI_MB),
            MMI_sd_mb = sd(MMI_MB))

model_val_stats <- O_E_stats %>% 
                   left_join(mmi_stats, by = 'model_status_qual') %>% 
                   left_join(mmi_stats_mb, by = 'model_status_qual')

write.csv(model_val_stats, "model_val_stats.csv")

### generate plots ### 

mmi_val_plots <- mmi_cal %>% 
  filter(model_status_qual %in% c('Ref_Val-0','Ref_Cal-0',"Not_Ref-0",'Most_Cal-0','Most_Val-0'))

mmi_val <- ggplot(data=mmi_val_plots, mapping=aes(x=model_status_qual, y=MMI))+geom_boxplot()
mmi_val

mmi_val_hist <- ggplot(data=mmi_val_plots, mapping=aes(x=MMI, color=model_status_qual, fill = model_status_qual)) +geom_histogram(alpha=0.5, position="identity")
mmi_val_hist

mmi_status_plot <- mmi_cal %>% 
  filter(model_status_qual %in% c('Ref_Val-0','Ref_Cal-0',"Not_Ref-0",
                                  "Most_Cal-0","Most_Val-0")) 

mmi_by_status <- ggplot(data=mmi_status_plot, mapping=aes(x=model_status_qual, y=MMI))+geom_boxplot()
mmi_by_status

##### site variability #### 

Mloc_summary <- joined_OE_BCG_MMI %>% 
              group_by(MLocID) %>% 
              summarise(n = n(),
                        MMI_mean = mean(MMI),
                        MMI_sd= sd(MMI),
                        OE_mean = mean(OoverE),
                        OE_sd= sd(OoverE),
                        BCG_mean = mean(Continuous_BCG_Level),
                        BCG_sd= sd(Continuous_BCG_Level)) %>% 
              filter(n>5)
