# bring in saved bug data, downloaded from AWQMS in early February by LAM
# AWQMS pulls haven't been working, this was a temporary work-around, suitable for model building but not permament
load('bugs analyses/RIVPACS_2022/_2024 model build/raw_bugs.Rdata') 




# join to taxonomy and OTUs
taxonomy <- read.xlsx('bugs analyses/Taxonomy/ODEQ_Taxonomy_dec22.xlsx') 

taxonomy.otu <- taxonomy %>%
  select(DEQ_Taxon = DEQ_TAXON, Taxon, OTU_RIV_24)

taxonomy.otu$DEQ_Taxon <- as.character(taxonomy.otu$DEQ_Taxon)


bugs.taxonomy <- raw_bugs %>%
  left_join(taxonomy.otu, by='DEQ_Taxon') %>%
  filter(Sample_Method=='Benthic Kick - Transect' | Sample_Method=='Benthic Kick - Riffle' | Sample_Method=='Benthic Kick - Targeted Riffle')



# check outliers

horse.35752 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(act_id == '35752-ORDEQ:19990707:R:SR' )


horse.12092 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(act_id == '12092-ORDEQ:20070814:R:SR' )


chetco.21814 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(MLocID == '21814-ORDEQ' )



pollalie.30343 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(MLocID == '30343-ORDEQ' )

twinlakes.32555 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(MLocID == '32555-ORDEQ' )


eflostine.35813 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(MLocID == '35813-ORDEQ' )


freezeout_PIBO.0892 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(MLocID == 'PIBO:0892' )


miller.35618 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(MLocID == '35618-ORDEQ' )


mill.24044 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(MLocID == '24044-ORDEQ' )


sfsalmon.21821 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(MLocID == '21821-ORDEQ' )


tribsalmon.13200 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(MLocID == '13200-ORDEQ' )

cultus.12894 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(MLocID == '12894-ORDEQ' )




cottonwood_PIBO.0886 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(MLocID == 'PIBO:0886' )



dutchflat.134530 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(act_id == '134530' )



dutchflat.140016 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(act_id == '140016' )


lbadger.26929 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(act_id == '26929-ORDEQ:20020711:R:SR' )


526PNCAMF:20130625:T:SR


pine526PNCAMF <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(act_id == '526PNCAMF:20130625:T:SR' )


annie.121071 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(act_id == '121071' )



goldlake.31730 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(act_id == '31730-ORDEQ:20040901:R:SR' )


mccully.35883 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(MLocID == '35883-ORDEQ' )

24426-ORDEQ:20000718:R:SR


trib.straw_24426_2000 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(act_id == '24426-ORDEQ:20010719:R:SR' )


35717-ORDEQ


sfbreiten.35717 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(MLocID == '35717-ORDEQ' )



rock.PIBO0927 <- bugs.taxonomy %>% 
  select (act_id, MLocID, StationDes, Result_Numeric, Result_Unit,  Taxonomic_Name, OTU_RIV_24) %>%
  filter(Result_Unit =='count') %>%
  filter(act_id == '126606' )

















