### first run the FrontEnd_v2.R

### build a dataframe with sample information for O/E and MMI model development 


#### get samples used to build O/E model ####
### this is code from the RIVPACS model build 
ref_257 <- read.xlsx('bugs analyses/RIVPACS_2022/_2024 model build/Reference sites_bug samples_total and OTU abundances.xlsx',
                     sheet = 'FINAL FINAL FINAL_265 ref samps')  

ref_257 <- ref_257 %>%
  filter(Use_spatial=='Y') %>%
  rename(MLocID = Name, StaDes = Description)

# create a list of sampleIDs to use in querying bug data

sampleIDs.ref257 <- as.vector(ref_257$act_id)

# which samples to use? --SLH took the "MOST.DISTURBED_bug.samples_site.info_total.abundances.xlsx" file 
# and identified which sample to use when more than one sample existed for a MostDisturbed station

most.dist_USE <- read.csv('bugs analyses/MMI/_2024 model build/MOST.DISTURBED_bug.samples_site.info_total.abundances_USE2.csv')

most.dist_USE <- most.dist_USE %>%
  filter(Use_FINAL == 'Y')           # 'n' = 167 most disturbed samples to use for modeling

sampleIDs.most.dist167 <- as.vector(most.dist_USE$act_id) 


#### assign samples used in model development -  ### 
bugs_samp_used <- sample_info %>% 
  mutate(ref_used = case_when(act_id %in% sampleIDs.ref257 ~ 1,
                              TRUE ~ 0),
         most_used = case_when(act_id %in% sampleIDs.most.dist167 ~ 1,
                               TRUE ~ 0),
         station_used = case_when(c(ref_used == 1 | most_used == 1)~1,
                                  TRUE ~ 0)) %>%
  filter(ref_used == 1 | most_used == 1)

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
         model_status = case_when(EcoRegion3 == 80 ~ "Outlier",
                                  c(act_used ==1 & ref == 1)~ "Ref_Cal",
                                  c(act_used ==1 & ref == 3)~ "Most_Cal",
                                  c(station_used == 1 & act_used == 0 &ref == 1) ~ "Ref_Cal_dup",
                                  c(station_used == 1 & act_used == 0 & ref == 3) ~ "Most_Cal_dup",
                                  c(station_used == 0 & ref == 1) ~ "Ref_Val",
                                  c(station_used == 0 & ref == 3) ~ "Most_Val",
                                  TRUE ~ "Not_Ref")) %>%
  #filter(!EcoRegion3 == 80) %>% 
  select(org_id,Project1,MLocID,StationDes,act_id,SampleStart_Date,EcoRegion3,EcoRegion2,GNIS_Name,
         COMID,ReferenceSite,Wade_Boat,act_used,station_used,ref,model_status)

#### assign these as Val_dups - this was done manually by select the newest sample, preference for ####
### riffle over transect, primary sample over OC, 
act_ref_val_dup <- c('21814-ORDEQ:20000920:R:SR','21814-ORDEQ:19990922:R:SR','21848-ORDEQ:19990825:R:SR',
   '24416-ORDEQ:20000830:T:SR','31729-ORDEQ:20040831:R:QCLR','34658-ORDEQ:20070920:R:QCFR',
   '34658-ORDEQ:20070920:R:SR','34658-ORDEQ:20080814:R:QCFR','34722-ORDEQ:20070809:R:QCLR',
   '34722-ORDEQ:20070809:R:SR','34722-ORDEQ:20080813:R:SR','34722-ORDEQ:20090806:R:QCLR',
   '35633-ORDEQ:20150624:R:SR','35633-ORDEQ:20160628:R:SR','35633-ORDEQ:20170803:R:SR',
   '35633-ORDEQ:20190912:R:SR','35633-ORDEQ:20180913:R:SR','35633-ORDEQ:20210901:NA:R:SR',
   '35633-ORDEQ:20000812:R:SR','35685-ORDEQ:19980808:R:SR-1','35685-ORDEQ:19980808:R:SR-2',
   '35690-ORDEQ:19980707:R:SR-2','35690-ORDEQ:19980707:R:SR-3','35701-ORDEQ:19980729:R:SR-1',
   '35701-ORDEQ:19980729:R:SR-3','35720-ORDEQ:20150910:R:SR','35720-ORDEQ:20170822:R:SR',
   '35720-ORDEQ:20160908:R:SR','35720-ORDEQ:20190904:R:SR','35720-ORDEQ:20191010:R:SR',
   '35720-ORDEQ:20180703:R:SR','35720-ORDEQ:20210630:NA:R:SR','35720-ORDEQ:19980813:R:SR-1',
   '35720-ORDEQ:19980813:R:SR-2','35720-ORDEQ:19980813:R:SR-3','35732-ORDEQ:19980807:R:SR-2',
   '35732-ORDEQ:19980807:R:SR-3','35735-ORDEQ:19980811:R:SR-1','35745-ORDEQ:19990629:R:SR',
   '35812-ORDEQ:20000706:R:SR','35813-ORDEQ:20000901:R:SR','35825-ORDEQ:20150915:R:SR',
   '35825-ORDEQ:20170830:R:SR','35825-ORDEQ:20160831:R:SR','35825-ORDEQ:20190924:R:SR',
   '35825-ORDEQ:20180919:R:SR','35825-ORDEQ:20210908:NA:R:SR')
# same process as reference
act_most_val_dup <- c('21822-ORDEQ:20020925:R:SR','21822-ORDEQ:20030909:R:SR',
                     '21822-ORDEQ:20040914:R:QCLR','21822-ORDEQ:20040726:R:SR',
                     '21822-ORDEQ:20040914:R:SR','13242-ORDEQ:19980922:R:SR',
                     '24438-ORDEQ:20000807:T:SR','25385-ORDEQ:20010802:R:SR',
                     '10856-ORDEQ:20210720::T:SR','34443-ORDEQ:20070612:R:QCFR',
                     '10856-ORDEQ:20210720::T:SR')

### reassign the samples that are val dups 
sample_info_model$model_status <- if_else(sample_info_model$act_id %in% act_ref_val_dup,"Ref_Val_dup",sample_info_model$model_status)
sample_info_model$model_status <- if_else(sample_info_model$act_id %in% act_most_val_dup,"Ref_Most_dup",sample_info_model$model_status)                                            

#### build the table #### 
ref_cal <- sample_info_model %>% 
           filter(model_status == 'Ref_Cal')

# explore the table 


write.csv(sample_info_model, "sample_info_model.csv")


### pull in O/E scores from model build 

o_E_ref_modelbuild <- read.csv('bugs analyses/RIVPACS_2022/RIVPACS.2024_FINAL_ref.build_OE.csv') %>% 
  select(X,O,E,OoverE) %>% 
  rename(act_id = X,"O_MB"="O", "E_MB"="E","OoverE_MB"= "OoverE")

# this shows the missing ref site samples - one is from 1997 and one is reported as mixed habitat sampled 
O_E_cal_2 <- o_E_ref_modelbuild %>% 
  left_join(sample_info_model, by = "act_id") 

# get a table of O_E from model build and 
O_E_cal <- sample_info_model %>% 
  left_join(o_E_ref_modelbuild, by = "act_id") %>% 
  select(act_id,model_status,O_MB,E_MB,OoverE_MB) %>%
  left_join(OE_scores, by = "act_id") %>% # this is from a model run using front end v2
  drop_na(OoverE) %>% 
  filter(OoverE > 0)

# this excludes   
O_E_stats <- O_E_cal %>% 
             group_by(model_status) %>% 
             summarise(O_E_mean = mean(OoverE),
                       O_E_sd= sd(OoverE),
                       O_E_MB_mean = mean(OoverE_MB), 
                       O_E_MB_sd= sd(OoverE_MB))
  
### repeat process for MMI 
mmi_modelbuild <- read.csv('bugs analyses/MMI/_2024 model build/final_MMI_4.metrics.csv') %>%
                  select(SAMPLEID,MMI.2024) %>% 
                  rename(act_id = SAMPLEID, MMI_MB = MMI.2024)
  
# this shows the missing ref site samples - one is from 1997 and one is reported as mixed habitat sampled 
mmi_cal_2 <- mmi_modelbuild %>% 
  left_join(sample_info_model, by = "act_id") %>% 
  drop_na(model_status)

# get a table of O_E from model build and 
mmi_cal <- sample_info_model %>% 
  #left_join(mmi_modelbuild, by = "act_id") %>% 
 # select(act_id,model_status,MMI_MB) %>%
  left_join(MMI_scores, by = c("act_id"= "SAMPLEID")) %>% # this is from a model run using front end v2
  drop_na(MMI)

mmi_stats <- mmi_cal %>% 
  group_by(model_status) %>% 
  summarise(MMI_mean = mean(MMI),
            MMI_sd= sd(MMI))

# had to break this out cause MMI_MB had NAs for Most_Val - not sure why?? 
mmi_stats_mb <- mmi_cal_2 %>% 
  group_by(model_status) %>% 
  summarise(MMI_mean = mean(MMI_MB),
            MMI_sd= sd(MMI_MB))

model_val_stats <- O_E_stats %>% 
                   left_join(mmi_stats, by = 'model_status') %>% 
                   left_join(mmi_stats_mb, by = 'model_status')

write.csv(model_val_stats, "model_val_stats.csv")
