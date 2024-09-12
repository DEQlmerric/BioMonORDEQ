library (tidyverse)

#### generate benchmarks based on average scores at a monitoring location 
mloc_info <- joined_OE_BCG_MMI %>% 
             select(org_id,Project1,AU_ID,MLocID,StationDes,COMID,
                    GNIS_Name,ReferenceSite) %>% 
             distinct()


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

#### get linear regression equation to find O_E values at BGC levels 3.5 and 4.5 - 
fit_OE_ave <- lm(formula = OE_site_avg ~ BCG_site_avg, data = mloc_aves)
summary(fit_OE_ave)

## determine the O_E value of BCG 4.5 using lm above 
attain_ave <- (-0.177638*3.5)  + 1.550113
impair_ave <- (-0.177638 *4.5) + 1.550113

# get linear regression equation to find MMI values at BGC levels 3.5 and 4.5 

fit_MMI_ave <- lm(formula = MMI_site_avg ~ BCG_site_avg, data = mloc_aves)
summary(fit_MMI_ave)

## determine the MMI value of BCG 4.5 & 3.5using lm above 
attain_mmi_ave <- (-0.14836 *3.5)  + 1.16599
impair_mmi_ave <- (-0.14836 *4.5)  + 1.16599

###### generate plots #### 
### add colors set colors to reference status - by alphabetical order ?? 
ref_colors <- c("yellow3","red3","royalblue3")

oe_bcg <- ggplot(data = mloc_aves, aes(x = BCG_site_avg, y = OE_site_avg))+
  geom_point(aes(color = ReferenceSite))+
  geom_vline(xintercept = 4.5, color = "darkviolet")+
  geom_vline(xintercept = 3.5, color = "darkgreen")+
  geom_smooth(#data = filter(joined_OE_BCG_MMI, ReferenceSite == 'REFERENCE' ),  
    method='lm', formula= y~x,color = "black")+
  scale_color_manual(values=ref_colors)+
  labs(title = 'O:E + BCG',
       y = "O/E Site Average",
       x = "BCG Level")+
  theme_bw()
oe_bcg

mmi_bcg <- ggplot(data = mloc_aves, aes(x = BCG_site_avg, y = MMI_site_avg))+
  geom_point(aes(color = ReferenceSite))+
  geom_vline(xintercept = 4.5, color = "darkviolet")+
  geom_vline(xintercept = 3.5, color = "darkgreen")+
  geom_smooth(#data = filter(joined_OE_BCG_MMI, ReferenceSite == 'REFERENCE' ),  
    method='lm', formula= y~x,color = "black")+
  scale_color_manual(values=ref_colors)+
  labs(title = 'MMI + BCG',
       y = "MMI Site Average",
       x = "BCG Level")+
  theme_bw()
mmi_bcg

##### get percentiles of mloc averages for all reference sites #### 

ref_mloc_aves <- mloc_aves %>% 
                 #left_join(mloc_info, by = "MLocID") %>% 
                  filter(ReferenceSite == "REFERENCE")

OE_ra_percentile <- ref_mloc_aves %>%
                    reframe(enframe(quantile(OE_site_avg, c(0.02,0.03,0.04,0.05, 0.10, 0.15, 0.20,0.25, 0.30,0.35)), "quantile", "OE_site_avg"))

MMI_ra_percentile <- ref_mloc_aves %>%
  reframe(enframe(quantile(MMI_site_avg, c(0.01,0.02,0.03,0.04,0.05, 0.10, 0.15, 0.20,0.25, 0.30,0.35)), "quantile", "MMI_site_avg"))

#### get percentiles from the model build to compare 

o_E_ref_mb_plot <- read.csv('bugs analyses/RIVPACS_2022/RIVPACS.2024_FINAL_ref.build_OE.csv') %>% 
  select(X,O,E,OoverE) %>% 
  rename(act_id = X,"O_MB"="O", "E_MB"="E","OoverE_MB"= "OoverE") %>% 
  mutate(model_status = "Ref_Cal") %>% 
  left_join(BCG, by = 'act_id')

o_E_ref_mb_percentile <- o_E_ref_mb_plot %>%
  group_by(model_status) %>% 
  reframe(enframe(quantile(OoverE_MB, c(0.02,0.03,0.05, 0.10, 0.15, 0.20,0.25, 0.30,0.35)), "quantile", "OoverE_MB"))

mmi_modelbuild <- read.csv('bugs analyses/MMI/_2024 model build/final_MMI_4.metrics.csv') %>%
  select(SAMPLEID,MMI.2024) %>% 
  rename(act_id = SAMPLEID, MMI_MB = MMI.2024) %>% 
  left_join(sample_info_model, by = "act_id") %>% 
  drop_na(model_status) %>% 
  left_join(BCG, by = 'act_id') 

mmi_mb_ref_percentile <- mmi_modelbuild %>%
  filter(model_status == 'Ref_Cal') %>%
  #group_by(model_status) %>% 
  reframe(enframe(quantile(MMI_MB, c(0.01,0.02,0.03,0.05, 0.10, 0.25,0.30,0.5,0.75,0.90)), "quantile", "MMI_MB"))

