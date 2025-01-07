library (tidyverse)

# generate percentiles of reference sites used to build the models 


#### get samples used to build O/E model ####
o_E_ref_mb <- read.csv('bugs analyses/RIVPACS_2022/RIVPACS.2024_FINAL_ref.build_OE_REBUILD.new.OTUs_11.24.csv') %>% 
  select(Sample,O,E,OoverE) %>% 
  rename(act_id = Sample,"O_MB"="O", "E_MB"="E","OoverE_MB"= "OoverE")

o_E_ref_mb_percentile <- o_E_ref_mb %>%
                  reframe(enframe(quantile(OoverE_MB, c(0.05, 0.10, 0.15, 0.20,0.25,0.3,0.5,0.75,0.9,0.95)), "quantile", "OoverE_MB"))

### histogram for the paper  
OE_mb_hist <- ggplot(data=o_E_ref_mb, mapping=aes(x=OoverE_MB)) +
               geom_histogram(alpha=0.5, position="identity") +
               geom_vline(xintercept = 0.79, color ="darkviolet") +
               geom_vline(xintercept = 0.75, color ="darkviolet",linetype="dashed") +
               geom_vline(xintercept = 0.91, color = "darkgreen") +
               xlab("O/E Score") +
               theme_classic()
OE_mb_hist

#### box plots with O/E 
OE_box <- ggplot(data =  filter(joined_OE_BCG_MMI,!is.na(Primary_BCG_Level) ), aes(y = OoverE, x = as.factor(Primary_BCG_Level)))+
  geom_boxplot()+
  theme_bw()+
  geom_jitter(width = 0.2, alpha = .3)+
  labs(title = 'O:E + BCG',
       x = "Primary BCG")+
  theme_bw() +
  geom_hline(yintercept = 0.79, color = "darkviolet") +
  geom_hline(yintercept = 0.92, color = "darkgreen")+
  geom_hline(yintercept = 0.75, color ="darkviolet",linetype="dashed")
  
OE_box 

##### MMI ####### 
#### get samples used to build O/E model ####
MMI_ref_mb <- read.csv('bugs analyses/MMI/_2024 model build/final_MMI_4.metrics.csv') %>%
  select(SAMPLEID,ReferenceSite,MMI.2024) %>% 
  filter(ReferenceSite == 'REFERENCE') %>% 
  mutate(MMI_rescale = MMI.2024/0.7292573)  # rescale is MMI/mean of reference site 
  
MMI_ref_mb_percentile <- MMI_ref_mb %>% 
         reframe(enframe(quantile(MMI_rescale , c(0.05, 0.10, 0.15, 0.20,0.25,0.3,0.5,0.75,0.9,0.95)), "quantile", "MMI_rescale_MB"))

### histogram for the paper  
mmi_mb_hist <- ggplot(data=MMI_ref_mb, mapping=aes(x=MMI_rescale)) +
  geom_histogram(alpha=0.5, position="identity") +
  geom_vline(xintercept = 0.81, color ="darkviolet") +
  geom_vline(xintercept = 0.77, color ="darkviolet",linetype="dashed") +
  geom_vline(xintercept = 0.90, color = "darkgreen") +
  xlab("MMI Score") +
  theme_classic()
mmi_mb_hist

#### box plots 
joined_OE_BCG_MMI$MMI_rescale <- joined_OE_BCG_MMI$MMI/0.7292573

MMI_box <- ggplot(data =  filter(joined_OE_BCG_MMI,!is.na(Primary_BCG_Level) ), aes(y = MMI_rescale, x = as.factor(Primary_BCG_Level)))+
  geom_boxplot()+
  theme_bw()+
  geom_jitter(width = 0.2, alpha = .3)+
  labs(title = 'MMI + BCG',
       x = "Primary BCG")+
  theme_bw() +
  geom_hline(yintercept = 0.81, color = "darkviolet") +
  geom_hline(yintercept = 0.90, color = "darkgreen")+
  geom_hline(yintercept = 0.77, color ="darkviolet",linetype="dashed")

MMI_box 

### combined Box plot 
box_plot_data <- joined_OE_BCG_MMI %>% 
            select(MLocID,ReferenceSite,Primary_BCG_Level,OoverE,MMI_rescale) %>% 
            pivot_longer(cols = c("OoverE", "MMI_rescale"), names_to = "model", values_to = "score") %>%
            filter(!is.na(Primary_BCG_Level))

Combo_box <- ggplot(data =  box_plot_data, aes(y = score, x = as.factor(Primary_BCG_Level) ,fill = model))+
  geom_boxplot()+
  theme_bw()+
  #geom_jitter(width = 0.2, alpha = .3)+
  labs(title = 'Model Scores + BCG',
       x = "Primary BCG")+
  theme_bw() +
  geom_hline(yintercept = 0.80, color = "darkviolet") +
  geom_hline(yintercept = 0.91, color = "darkgreen")
  #geom_hline(yintercept = 0.77, color ="darkviolet",linetype="dashed")
Combo_box

Combo_box_flip <- ggplot(data =  box_plot_data, aes(y = as.factor(Primary_BCG_Level), x = score ,fill = model))+
  geom_boxplot()+
  #scale_y_reverse()+
  #scale_x_discrete(limits = rev(levels(box_plot_data$Primary_BCG_Level)))+
  theme_bw()+
  #geom_jitter(width = 0.2, alpha = .3)+
  labs(title = 'Model Scores + BCG',
       x = "Score",
       y= "")+
  geom_vline(xintercept = 0.80, color = "darkviolet") +
  geom_vline(xintercept = 0.91, color = "darkgreen")
#geom_hline(yintercept = 0.77, color ="darkviolet",linetype="dashed")
Combo_box_flip

### BC ### 

joined_OE_BCG_MMI_all$MMI_rescale <- joined_OE_BCG_MMI_all$MMI/0.7292573

BC <- joined_OE_BCG_MMI_all %>% 
      select(org_id,Project1,AU_ID,MLocID,ReferenceSite,SampleStart_Date,
         COMID,act_id,O,E,OoverE,BC,BCG_region,Primary_BCG_Level,Continuous_BCG_Level,
         ni_total,pt_tv_intol,nt_habitat_rheo,pt_ti_stenocold_cold_cool,
         pi_EPTNoHydro, MMI,MMI_rescale,model_status_qual,qualifer_text)
         

BC_ref_mb <- BC %>% 
             left_join(o_E_ref_mb,by = 'act_id') %>% 
             filter(!is.na(OoverE_MB)) %>% 
             filter(!is.na(OoverE)) #### 3 samples are missing MWST_mean08.14 - check on this??? 

BC_ref_mb_percentile <- BC_ref_mb %>% 
  reframe(enframe(quantile(BC, c(0.05, 0.10, 0.15, 0.20,0.25,0.3,0.5,0.75,0.9,0.95)), "quantile", "BC_MB"))

### histogram for the paper  
BC_mb_hist <- ggplot(data=BC_ref_mb, mapping=aes(x=BC)) +
  geom_histogram(alpha=0.5, position="identity") +
  geom_vline(xintercept = 0.27, color ="darkviolet") +
  geom_vline(xintercept = 0.29, color ="darkviolet",linetype="dashed") +
  geom_vline(xintercept = 0.24, color = "darkgreen") +
  xlab("BC Score") +
  theme_classic()
BC_mb_hist

BC_OE <- ggplot(data = joined_OE_BCG_MMI_good , aes(x = BC, y = OoverE))+
  geom_point() +
  stat_smooth(method=lm)
BC_OE

joined_OE_BCG_MMI_good$MMI_rescale <- joined_OE_BCG_MMI_good$MMI/0.7292573

BC_MMI <- ggplot(data = joined_OE_BCG_MMI_good , aes(x = BC, y = MMI_rescale))+
  geom_point() +
  stat_smooth(method=lm)
BC_MMI

#### explore the assessment matrix 

cats_station <- joined_OE_BCG_MMI_good %>% 
        select(org_id,Project1,AU_ID,MLocID,ReferenceSite,SampleStart_Date,
               COMID,O,E,OoverE,BC,BCG_region,Primary_BCG_Level,Continuous_BCG_Level,
               ni_total,pt_tv_intol,nt_habitat_rheo,pt_ti_stenocold_cold_cool,
               pi_EPTNoHydro, MMI,MMI_rescale,qualifer_text) %>% 
       mutate(sample_Cat = case_when(MMI_rescale <= 0.81 & OoverE <= 0.79 ~ '5',
                                     
                                     MMI_rescale <= 0.90 & MMI_rescale > 0.81 & 
                                       OoverE <= 0.91 & OoverE > 0.79 ~ '3C',
                                     
                                     MMI_rescale > 0.90 & OoverE > 0.91 ~ '2',
                                     
                                     MMI_rescale <= 0.81 &  OoverE > 0.91 ~ '3B', #OE good, mmi poor',
                                     MMI_rescale >0.90 &  OoverE <= 0.79 ~ '3B', #OE poor, mmi good',
                                     
                                     OoverE <= 0.91 & OoverE > 0.79 & 
                                       MMI_rescale <= 0.81 ~ '3B', #OE fair, mmi poor',
                                     
                                     OoverE <= 0.91 & OoverE > 0.79 & 
                                       MMI_rescale > 0.90 ~ '3C', #OE fair, mmi good',
                                     
                                     MMI_rescale <= 0.90 & MMI_rescale> 0.81 &
                                       OoverE > 0.91 ~ '3C', #OE good, mmi fair',
                                     
                                     MMI_rescale <= 0.90 & MMI_rescale > 0.81 & 
                                       OoverE <= 0.79 ~ '3B', #OE poor, mmi fair',
                                     
                                     TRUE ~ 'ERROR'))
write.csv(cats_station,'cats_station.csv')                                    

single_sample_station <- cats_station %>% 
                  group_by(MLocID) %>% 
                  summarise(n = n()) %>% 
                  filter(n ==1) %>% 
                  left_join(cats_station, by = 'MLocID')

write.csv(single_sample_station,'single_sample_station.csv')  

multi_sample_station <- cats_station %>% 
  group_by(MLocID) %>% 
  summarise(n = n()) %>% 
 # filter(n > 1) %>% 
  right_join(cats_station, by = 'MLocID') %>%
  filter(n > 1) 
write.csv(multi_sample_station,'multi_sample_station.csv')  

cat_counts <- multi_sample_station %>%
          arrange(SampleStart_Date) %>%
          group_by(MLocID,ReferenceSite) %>% 
          mutate(dates = paste0(SampleStart_Date, collapse = ";")) %>% 
          summarise(n = n(), 
                    dates = paste0(SampleStart_Date, collapse = ";"),
                    Cat2 = sum(sample_Cat == '2'),
                    Cat5 = sum(sample_Cat == '5'),
                    Cat3C = sum(sample_Cat == '3C'),
                    Cat3B = sum(sample_Cat == '3B'),
                    #OEfair_mmigood = sum(sample_Cat == 'OE fair, mmi good'),
                    #OEgood_mmifair = sum(sample_Cat == 'OE good, mmi fair'),
                    
                    #OEfair_mmipoor = sum(sample_Cat == 'OE fair, mmi poor'),                 
                    #OEpoor_mmifair = sum(sample_Cat == 'OE poor, mmi fair'),
                    
                    #OEgood_mmipoor = sum(sample_Cat == 'OE good, mmi poor'),
                    #OEpoor_mmigood = sum(sample_Cat == 'OE poor, mmi good'),
                    dates = paste0(SampleStart_Date, collapse = ";"))
write.csv(cat_counts,'multi_sample_station_cat_counts.csv')
                    
ref_cat_count <- counts %>% 
                 filter(ReferenceSite == 'REFERENCE') 

