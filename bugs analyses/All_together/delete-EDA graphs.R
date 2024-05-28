


bug_tax_data_filtered <- bug_tax_data_filtered |> 
  filter(EcoRegion3 %in% c(1,3,4))



# comparison ------------------------------------------------------------------------------------------------------





OE <- OE_scores |> 
  select(MLocID,org_id, AU_ID, act_id,EcoRegion3,EcoRegion4,ReferenceSite, OoverE) |> 
  mutate(ecoregion = case_when(EcoRegion3 %in% c(1,3,4) ~ "In region",
                               TRUE ~ "Out Region"))

BCG <- BCG_results |> 
  select(SampleID, Primary_BCG_Level, Continuous_BCG_Level) |> 
  rename(act_id = SampleID)


joined_OE_BCG <- left_join(OE, BCG, by = join_by(act_id)) 

#  joined_OE_BCG <- joined_OE_BCG |> 
# #filter(!org_id %in% c('USU(NOSTORETID)', 'CAFW(NOSTORETID)')) |> 
#   filter(ecoregion == "In region" )


DEQ_data <- joined_OE_BCG |> 
  filter(org_id == 'OREGONDEQ')



ggplot(data = DEQ_data, aes(x = Continuous_BCG_Level, y = OoverE))+
  geom_point(aes(color = ReferenceSite))+
  #stat_smooth(method=lm)+
  geom_hline(yintercept = 0.8, color = 'red')+
  geom_smooth(method='lm', formula= y~x)+
  scale_color_hue(l=50)


ggplot(data = joined_OE_BCG, aes(x = Continuous_BCG_Level, y = OoverE))+
  geom_point(aes(color = org_id))+
  #stat_smooth(method=lm)+
  geom_hline(yintercept = 0.8, color = 'red')+
  geom_smooth(method='lm', formula= y~x)+
  scale_color_hue(l=50)




joined_OE_BCG <- na.omit(joined_OE_BCG)
ggplot(data = joined_OE_BCG, aes(y = OoverE, x = as.factor(Primary_BCG_Level)))+
  geom_boxplot()+
  theme_bw()+
geom_jitter(width = 0.2, alpha = .3)






# Missing NHD info ------------------------------------------------------------------------------------------------

no_NHD_acts <- BCG_metrics |> 
  filter(is.na(SITE_TYPE)) |> 
  select(SAMPLEID) |> 
  pull()


no_NHD <- sample_info |> 
  filter(act_id %in% no_NHD_acts) |> 
  filter(!is.na(COMID)) |> 
  get_NHD_info()

