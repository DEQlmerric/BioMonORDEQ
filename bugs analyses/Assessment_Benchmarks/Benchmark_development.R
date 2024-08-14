
### explore benchmark values 

### pull in O/E scores from model build 

o_E_ref_mb_plot <- read.csv('bugs analyses/RIVPACS_2022/RIVPACS.2024_FINAL_ref.build_OE.csv') %>% 
  select(X,O,E,OoverE) %>% 
  rename(act_id = X,"O_MB"="O", "E_MB"="E","OoverE_MB"= "OoverE") %>% 
  mutate(model_status = "Ref_Cal") %>% 
  left_join(BCG, by = 'act_id')

o_E_ref_mb_percentile <- o_E_ref_mb_plot %>%
                         group_by(model_status) %>% 
                         reframe(enframe(quantile(OoverE_MB, c(0.05, 0.10, 0.15, 0.20,0.25, 0.30,0.35)), "quantile", "OoverE_MB"))

OE_ref_mb_hist <- ggplot(data=o_E_ref_mb_plot, mapping=aes(x=OoverE_MB)) +geom_histogram(alpha=0.5, position="identity")
OE_ref_mb_hist

# not using this 
# R1 <- ggplot(data = o_E_ref_mb_plot, aes(x = Continuous_BCG_Level, y = OoverE_MB))+
#   geom_point(aes(color = model_status))+
#   #stat_smooth(method=lm)+
#   geom_hline(yintercept = 0.9, color = 'red')+
#   geom_hline(yintercept = 0.8, color = 'red')+
#   geom_vline(xintercept = 3.5, color = "forestgreen")+
#   geom_vline(xintercept = 4.5, color = "forestgreen")+
#   geom_smooth(#data = filter(joined_OE_BCG_MMI, ReferenceSite == 'REFERENCE' ),  
#     method='lm', formula= y~x)+
#   scale_color_hue(l=50)+
#   labs(title = 'O:E + BCG')+
#   theme_bw()

R1 
### repeat process for MMI 
mmi_modelbuild <- read.csv('bugs analyses/MMI/_2024 model build/final_MMI_4.metrics.csv') %>%
  select(SAMPLEID,MMI.2024) %>% 
  rename(act_id = SAMPLEID, MMI_MB = MMI.2024) %>% 
  left_join(sample_info_model, by = "act_id") %>% 
  drop_na(model_status) %>% 
  left_join(BCG, by = 'act_id') 
  

mmi_mb_percentile <- mmi_modelbuild %>%
  filter(model_status == 'Ref_Cal') %>%
  #group_by(model_status) %>% 
  reframe(enframe(quantile(MMI_MB, c(0.05, 0.10, 0.25,0.35,0.5,0.75,0.90)), "quantile", "MMI_MB"))

mmi_mb_ref <- mmi_modelbuild %>%
  filter(model_status == 'Ref_Cal')

mmi_ref_mb_hist <- ggplot(data=mmi_mb_ref, mapping=aes(x=MMI_MB, color=model_status, fill = model_status)) +geom_histogram(alpha=0.5, position="identity")
mmi_ref_mb_hist

mmi_modelbuild_ref <- mmi_modelbuild %>% 
                      filter(model_status == 'Ref_Cal')

R2 <- ggplot(data = mmi_modelbuild_ref, aes(x = Continuous_BCG_Level, y = MMI_MB))+
  geom_point(aes(color = model_status))+
  #stat_smooth(method=lm)+
  geom_hline(yintercept = 0.60, color = 'red')+
  geom_hline(yintercept = 0.70, color = 'red')+
  geom_vline(xintercept = 4.5, color = "forestgreen")+
  geom_vline(xintercept = 3.5, color = "forestgreen")+
  geom_smooth(#data = filter(joined_OE_BCG_MMI, ReferenceSite == 'REFERENCE' ), 
    method='lm', formula= y~x)+
  scale_color_hue(l=50)+
  labs(title = 'BCG + MMI')+
  theme_bw()
R2 

OE_MMI_MB_refcal <- o_E_ref_mb_plot %>% 
                    left_join(mmi_modelbuild, by = 'act_id')

R3 <- ggplot(data = OE_MMI_MB_refcal, aes(x = MMI_MB, y = OoverE_MB))+
  geom_point(aes(color = model_status.x))+
  #stat_smooth(method=lm)+
  #geom_hline(yintercept = 0.8, color = 'red')+
  #geom_vline(xintercept = quantile(ref$MMI, c(.10), na.rm = TRUE) , color = "forestgreen")+
  geom_smooth(#data = filter(joined_OE_BCG_MMI, ReferenceSite == 'REFERENCE' ), 
    method='lm')+
  scale_color_hue(l=50)+
  labs(title = 'O:E + MMI')+
  theme_bw()
R3

### Use the scores from Travis 

o_E_ref_plot <- joined_OE_BCG_MMI %>% 
  filter(model_status == 'Ref_Cal' & qualifer == 0)

o_E_ref_percentile <- o_E_ref_mb_plot %>%
  group_by(model_status) %>% 
  reframe(enframe(quantile(OoverE, c(0.05, 0.10, 0.15, 0.20,0.25, 0.30,0.35)), "quantile", "OoverE_MB"))

R1_T <- ggplot(data = o_E_ref_plot, aes(x = Continuous_BCG_Level, y = OoverE))+
  geom_point(aes(color = model_status))+
  #stat_smooth(method=lm)+
  geom_hline(yintercept = 0.9, color = 'red')+
  geom_vline(xintercept = 3, color = "forestgreen")+
  geom_smooth(#data = filter(joined_OE_BCG_MMI, ReferenceSite == 'REFERENCE' ),  
    method='lm', formula= y~x)+
  scale_color_hue(l=50)+
  labs(title = 'O:E + BCG')+
  theme_bw()



