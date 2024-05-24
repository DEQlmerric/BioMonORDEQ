


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

ggplot(data = joined_OE_BCG, aes(x = Continuous_BCG_Level, y = OoverE))+
  geom_point(aes(color = org_id))+
  #stat_smooth(method=lm)+
  geom_hline(yintercept = 0.8, color = 'red')+
  scale_color_hue(l=50)





joined_OE_BCG <- na.omit(joined_OE_BCG)
ggplot(data = joined_OE_BCG, aes(y = OoverE, x = as.factor(Primary_BCG_Level),color = ReferenceSite))+
  geom_boxplot()+
  theme_bw()+
geom_jitter(width = 0.2, alpha = .3)


ggplot(data = joined_OE_BCG, aes(as.factor(Primary_BCG_Level), OoverE, fill = EcoRegion4))+
  geom_boxplot()+
  geom_jitter(width = 0.2, alpha = .3)+
  theme_bw()





TEST <- df.metrics |> 
  filter(SAMPLEID == '103CDCHHR:20100914:T:QCFR')