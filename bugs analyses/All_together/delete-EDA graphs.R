
# Start here ------------------------------------------------------------------------------------------------------




## Put it all together ---------------------------------------------------------------------------------------------


OE <- OE_scores |> 
  #select(MLocID,org_id, AU_ID, act_id,EcoRegion3,EcoRegion4,ReferenceSite, OoverE) |> 
  mutate(BCG_region = case_when(EcoRegion3 %in% c(1,3,4) ~ "In region",
                               TRUE ~ "Out Region"))

BCG <- BCG_results |> 
  select(SampleID, Primary_BCG_Level, Continuous_BCG_Level) |> 
  rename(act_id = SampleID)


MMI <- MMI_scores |> 
  select(SAMPLEID, MMI) |> 
  rename(act_id = SAMPLEID)

MMI_met <- MMI_metrics |> 
   rename(act_id = SAMPLEID)

load("bugs analyses/Models_Validation/sample_info_model.Rdata") ### this comes from model_val script 

### this one keeps qualifiers 
joined_OE_BCG_MMI_withSE <- left_join(OE, BCG, by = join_by(act_id)) |> 
  left_join(MMI_met) |> 
  left_join(MMI) |> 
  left_join(sample_info_model, by = 'act_id') %>% 
  filter(qualifer %in% c(0,3)) %>% ## removes sus data (low counts, glacial sites and poor samples)
  filter(!is.na(ReferenceSite))

### this one has all qualifiers removed 
joined_OE_BCG_MMI <- left_join(OE, BCG, by = join_by(act_id)) |> 
  left_join(MMI_met) |> 
  left_join(MMI) |> 
  left_join(sample_info_model, by = 'act_id') %>% 
  filter(qualifer == 0)%>% ## removes sus data (low counts, SE, glacial sites and poor samples)
  filter(!is.na(ReferenceSite))


save(joined_OE_BCG_MMI, file = 'bioassess_8-6-24.Rdata')
save(joined_OE_BCG_MMI_withSE, file = 'bioassess_8-6-24_withSE.Rdata')
write.xlsx(joined_OE_BCG_MMI, file = paste0("biocriteria_scores", Sys.Date(), '.xlsx'))

## Graphs ----------------------------------------------------------------------------------------------------------

mmi_modelbuild <- read.csv('bugs analyses/MMI/_2024 model build/final_MMI_4.metrics.csv') %>%
  select(SAMPLEID,MMI.2024) %>% 
  rename(act_id = SAMPLEID, MMI_MB = MMI.2024)

ref <- joined_OE_BCG_MMI |> 
  left_join(mmi_modelbuild) |> 
  filter(ReferenceSite == 'REFERENCE',
         !is.na(MMI_MB)
         )




#quantile(OE_ref$OoverE, c(.10), na.rm = TRUE)

p1 <- ggplot(data = joined_OE_BCG_MMI, aes(x = Continuous_BCG_Level, y = OoverE))+
  geom_point(aes(color = ReferenceSite))+
  #stat_smooth(method=lm)+
  geom_hline(yintercept = 0.8, color = 'red')+
  geom_vline(xintercept = 4.5, color = "forestgreen")+
  geom_smooth(#data = filter(joined_OE_BCG_MMI, ReferenceSite == 'REFERENCE' ),  
              method='lm', formula= y~x)+
  scale_color_hue(l=50)+
  labs(title = 'O:E + BCG')+
  theme_bw()



p2 <- ggplot(data = joined_OE_BCG_MMI, aes(x = MMI, y = OoverE))+
  geom_point(aes(color = ReferenceSite))+
  #stat_smooth(method=lm)+
  geom_hline(yintercept = 0.8, color = 'red')+
  #geom_vline(xintercept = quantile(ref$MMI, c(.03), na.rm = TRUE) , color = "forestgreen")+
  geom_vline(xintercept = 0.51 , color = "purple")+
  geom_smooth(#data = filter(joined_OE_BCG_MMI, ReferenceSite == 'REFERENCE' ), 
    method='lm')+
  scale_color_hue(l=50)+
  labs(title = 'O:E + MMI')+
  theme_bw()


p3 <- ggplot(data = joined_OE_BCG_MMI, aes(x = Continuous_BCG_Level, y = MMI))+
  geom_point(aes(color = ReferenceSite))+
  #stat_smooth(method=lm)+
  #geom_hline(yintercept = quantile(ref$MMI, c(.03), na.rm = TRUE) , color = 'red')+
  geom_hline(yintercept = 0.51 , color = 'purple')+
  #geom_vline(xintercept = 0.53 , color = "forestgreen")
  geom_vline(xintercept = 4.5, color = "forestgreen")+
  geom_smooth(#data = filter(joined_OE_BCG_MMI, ReferenceSite == 'REFERENCE' ), 
    method='lm', formula= y~x)+
  scale_color_hue(l=50)+
  labs(title = 'BCG + MMI')+
  theme_bw()

###
p5 <- ggplot(data =  filter(joined_OE_BCG_MMI,!is.na(Primary_BCG_Level) ), aes(y = OoverE, x = as.factor(Primary_BCG_Level)))+
  geom_boxplot()+
  theme_bw()+
  geom_jitter(width = 0.2, alpha = .3)+
  labs(title = 'O:E + BCG',
       x = "Primary BCG")+
  theme_bw()



p4 <- ggplot(data =  filter(joined_OE_BCG_MMI,!is.na(Primary_BCG_Level) ), aes(y = MMI, x = as.factor(Primary_BCG_Level)))+
  geom_boxplot()+
  theme_bw()+
  geom_jitter(width = 0.2, alpha = .3)+
  labs(title = 'MMI + BCG',
       x = "Primary BCG")


#arrange
library(patchwork)

(p1 | p2 | p3) / (p5 | p4) + plot_layout(guides = "collect") +  plot_annotation(
  title = 'Biocriteria Assessment Comparisons',
  subtitle = 'MMI threshold line drawn at 10th percentile of reference sites',
  caption = paste0('Plot generated on ', Sys.Date())
)


library(openxlsx)


# 3D --------------------------------------------------------------------------------------------------------------


library(plotly) 
ggplotly(p1)



data3d <- joined_OE_BCG_MMI |> 
  filter(ReferenceSite == 'REFERENCE')

fig <- plot_ly(data3d, x = ~OoverE, y = ~Continuous_BCG_Level, z = ~MMI, color = ~ReferenceSite)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'OoverE'),
                                   yaxis = list(title = 'BCG'),
                                   zaxis = list(title = 'MMI')),
                      title = 'x=OE y = BCG z = MMI')

fig

htmlwidgets::saveWidget(as_widget(fig), "bugs analyses/All_together/bioassess_comp.html")








#Everything below here is old
# comparison ------------------------------------------------------------------------------------------------------


# 
# 
# 
# OE <- OE_scores |> 
#   select(MLocID,org_id, AU_ID, act_id,EcoRegion3,EcoRegion4,ReferenceSite, OoverE) |> 
#   mutate(ecoregion = case_when(EcoRegion3 %in% c(1,3,4) ~ "In region",
#                                TRUE ~ "Out Region"))
# 
# BCG <- BCG_results |> 
#   select(SampleID, Primary_BCG_Level, Continuous_BCG_Level) |> 
#   rename(act_id = SampleID)
# 
# 
# MMI <- MMI_scores |> 
#   select(SAMPLEID, MMI) |> 
#   rename(act_id = SAMPLEID)
# 
# joined_OE_BCG_MMI <- left_join(OE, BCG, by = join_by(act_id)) |> 
#   left_join(MMI)
# 
# #  joined_OE_BCG_MMI <- joined_OE_BCG_MMI |> 
# # #filter(!org_id %in% c('USU(NOSTORETID)', 'CAFW(NOSTORETID)')) |> 
# #   filter(ecoregion == "In region" )
# 
# 
# DEQ_data <- joined_OE_BCG_MMI |> 
#   filter(org_id == 'OREGONDEQ')
# 
# DEQ_data <- joined_OE_BCG_MMI |> 
#   filter(ReferenceSite != 'REFERENCE' )
# 
# 
# ggplot(data = DEQ_data, aes(x = Continuous_BCG_Level, y = OoverE))+
#   geom_point(aes(color = ReferenceSite))+
#   #stat_smooth(method=lm)+
#   geom_hline(yintercept = 0.8, color = 'red')+
#   geom_vline(xintercept = 3.5, color = "forestgreen")+
#   geom_smooth(data = filter(joined_OE_BCG_MMI, ReferenceSite != 'REFERENCE' ), 
#               method='lm', formula= y~x)+
#   scale_color_hue(l=50)
# 
# 
# ggplot(data = joined_OE_BCG_MMI, aes(x = Continuous_BCG_Level, y = OoverE))+
#   geom_point(aes(color = org_id))+
#   #stat_smooth(method=lm)+
#   geom_hline(yintercept = 0.8, color = 'red')+
#   geom_smooth(method='lm', formula= y~x)+
#   scale_color_hue(l=50)
# 
# 
# 
# 
# 
# 
# 
# joined_OE_BCG_MMI <- na.omit(joined_OE_BCG_MMI)
# ggplot(data = joined_OE_BCG_MMI, aes(y = OoverE, x = as.factor(Primary_BCG_Level)))+
#   geom_boxplot()+
#   theme_bw()+
# geom_jitter(width = 0.2, alpha = .3)
# 
# 
# 
# 
# # MMI comp --------------------------------------------------------------------------------------------------------
# 
# 
# ggplot(data = joined_OE_BCG_MMI, aes(x = MMI, y = OoverE))+
#   geom_point(aes(color = ReferenceSite))+
#   #stat_smooth(method=lm)+
#   geom_hline(yintercept = 0.8, color = 'red')+
#   #geom_vline(xintercept = 3.5, color = "forestgreen")+
#   geom_smooth(#data = filter(joined_OE_BCG_MMI, ReferenceSite != 'REFERENCE' ), 
#               method='lm', formula= y~x)+
#   scale_color_hue(l=50)
# 
# 
# 
# ggplot(data = joined_OE_BCG_MMI, aes(x = Continuous_BCG_Level, y = MMI))+
#   geom_point(aes(color = ReferenceSite))+
#   #stat_smooth(method=lm)+
#   #geom_hline(yintercept = 0.8, color = 'red')+
#   #geom_vline(xintercept = 3.5, color = "forestgreen")+
#   geom_smooth(#data = filter(joined_OE_BCG_MMI, ReferenceSite != 'REFERENCE' ), 
#     method='lm', formula= y~x)+
#   scale_color_hue(l=50)
# 
# 
# 
# 
# 
# ggplot(data =  na.omit(joined_OE_BCG_MMI), aes(y = MMI, x = as.factor(Primary_BCG_Level)))+
#   geom_boxplot()+
#   theme_bw()+
#   geom_jitter(width = 0.2, alpha = .3)
# 
# 
# # Missing NHD info ------------------------------------------------------------------------------------------------
# 
# no_NHD_acts <- BCG_metrics |> 
#   filter(is.na(SITE_TYPE)) |> 
#   select(SAMPLEID) |> 
#   pull()
# 
# 
# no_NHD <- sample_info |> 
#   filter(act_id %in% no_NHD_acts) |> 
#   filter(!is.na(COMID)) |> 
#   get_NHD_info()
# 
# 
# 
# 
# 
# 
# 
# # 3D --------------------------------------------------------------------------------------------------------------
# 
# 
# library(plotly) 
# 
# 
# 
# 
# 
# 
# fig <- plot_ly(joined_OE_BCG_MMI, x = ~OoverE, y = ~Continuous_BCG_Level, z = ~MMI, color = ~ReferenceSite)
# fig <- fig %>% add_markers()
# fig <- fig %>% layout(scene = list(xaxis = list(title = 'OoverE'),
#                                    yaxis = list(title = 'BCG'),
#                                    zaxis = list(title = 'MMI')),
#                       title = 'x=OE y = BCG z = MMI')
# 
# fig
# 
# htmlwidgets::saveWidget(as_widget(fig), "bioassess_comp.html")
