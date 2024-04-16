#
#   Calculate bug metrics, using the BioMonTools package
#
#   Author: S. Hubler, 3.15.2024

# 1) read in bug data
# 2) associate with BCG attribute table
# 3) calculate metrics
library(tidyverse)

              #if(!require(remotes)){install.packages("remotes")}  #install if needed
              #install_github("leppott/BioMonTools", force=TRUE)

library(BioMonTools)
# bug data

@@@ temporary -- later, use AWQMS pull and Biomon front end

# bring in saved bug data, downloaded from AWQMS in early February by LAM
# AWQMS pulls haven't been working, this was a temporary work-around, suitable for model building but not permament
load('bugs analyses/MMI/_2024 model build/raw_bugs2.Rdata')


bugs <- raw_bugs %>%
  filter(Char_Name == 'Count') %>%
  select(act_id,Taxonomic_Name, Result_Numeric)




# DROP non-bug data, fix names, and DROP "ZERO" counts

bugs.clean <- bugs %>%   # vertebrates are showing up...why???
  filter(!(Taxonomic_Name %in% c("Ambystoma","Ambystoma gracile","Ambystomatidae","Cyprinidae"))) %>%
  filter(Result_Numeric > 0)
  

bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Acari***retired***use Arachnida'] <- 'Acari'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Ancylinae'] <- 'Ancylidae'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Arctotipula'] <- 'Tipula'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Athericidae'] <- 'Atherix'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Baetis Fuscatus group'] <- 'Baetis Fuscatus Group'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Bellardina'] <- 'Tipula'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Brachypoda (Aturidae)'] <- 'Brachypoda'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Caenis tardata'] <- 'Caenis'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Camptocladius'] <- 'Orthocladiinae'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Cascadoperla trictura'] <- 'Cascadoperla'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Caudatella orestes'] <- 'Caudatella'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Collembola (Collembola)'] <- 'Collembola'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Epeorus grandis/permagnus group'] <- 'Epeorus grandis/permagnus gr.'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Ephemera (Ephemeridae)'] <- 'Ephemera'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Ephemerella maculata'] <- 'Ephemerella'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Eucapnopsis'] <- 'Eucapnopsis brevicauda' # OTU = Capniidae
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Euphylidorea'] <- 'Limnophila' # OTU = Eloeophila

# "Exopalaemon modestus"  # not in taxonomy table...??? "Palaemon modestus" in taxonomy ---> OTU = DNI -- just remove?
                                              # 1 record from 2022, count = 2
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Exopalaemon modestus'] <- 'Palaemon' # OTU = Eloeophila
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Euphylidorea'] <- 'Limnophila' # OTU = Eloeophila
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Glyphopsyche'] <- 'Limnephilidae' 
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Gonidea'] <- 'Unionidae' # OTU = Eloeophila
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Helicopsychidae'] <- 'Helicopsyche' 
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Isoperla fulva'] <- 'Isoperla' 
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Isoperla marmorata'] <- 'Isoperla' 
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Isoperla mormona'] <- 'Isoperla' 
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Labiobaetis propinquus***retired***use Pseudocloeon propinquum'] <- 'Labiobaetis propinquus' 
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Limnophila (Animalia)'] <- 'Limnophila' 
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Limonia (Limoniini)'] <- 'Limonia' 
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Lumbricina***retired***use Crassiclitellata'] <- 'Lumbricina' 
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Menetus (Planorbidae)'] <- 'Menetus' 
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Meropelopia***retired***use Conchapelopia'] <- 'Meropelopia' 
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Nais (Animalia)'] <- 'Nais' 
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Neaviperla forcipata'] <- 'Neaviperla/Suwallia' 
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Nemata***retired***use Nematoda'] <- 'Nemata' 
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Neoleptophlebia sp.'] <- 'Neoleptophlebia' 

#  "Noteridae"   # Coleoptera family...no matching in BCG taxonomy --> Coleoptera? (1 record from 1997, count = 1)
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Noteridae'] <- 'Coleoptera' 


bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Oligochaeta (Oligochaeta)***retired***use Clitellata'] <- 'Oligochaeta'                    
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Ordobrevia nubifera***retired***use Ordobrevia nubifer'] <- 'Ordobrevia nubifera' 
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Ormosia (Eriopterini)'] <- 'Ormosia' 


# "Oroperla"   # Perlodidae genus (1 record from CA, count = 1)
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Oroperla'] <- 'Perlodidae' 

bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Orthocladius (Eudactylocladius)'] <- 'Orthocladius Eudactylocladius' 
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Orthocladius rivicola group'] <- 'Orthocladius' 
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Orthocladius rivulorum'] <- 'Orthocladius' 
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Ostrocerca'] <- 'Nemouridae'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Paratrichocladius'] <- 'Orthocladiinae'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Ptilostomis ocellifera'] <- 'Ptilostomis'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Rhithrogena hageni'] <- 'Rhithrogena'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Rhyacophila Iranda group'] <- 'Rhyacophila Iranda Group'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Rhyacophila Lieftincki group'] <- 'Rhyacophila lieftincki group'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Rhyacophila Nevadensis group'] <- 'Rhyacophila Nevadensis Group'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Rhyacophila oreta'] <- 'Rhyacophila Oreta Group'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Rhyacophila Oreta/Rotunda group'] <- 'Rhyacophila'

# "Saldidae"  # Hemiptera family (1 record from 1998, count = 1)
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Saldidae'] <- 'Hemiptera'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Sphaeriidae***retired***use Pisidiidae'] <- 'Sphaeriidae'

    # Sean: is it currently Sphaeriidae or Pisidiidae...this is all so confusing

bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Spongillidae'] <- 'Porifera'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Stictotarsus striatellus***retired***use Leconectes striatellus'] <- 'Hydroporinae'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'stygothrombium'] <- 'Stygothrombium'

    # why is this taxa showing up with this spelling? It is capitalized in the Taxonomy table. ~ 20 records across many different years

bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Stylurus'] <- 'Stylurus olivaceus'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Suwallini'] <- 'Suwalliini'

    # why is this taxa showing up with this spelling? Many records--all of them need spelling corrected.  Taxon is correct in Taxonomy table

bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Thyasidae***retired***use Hydryphantidae'] <- 'Thyasidae'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Tricladida'] <- 'Turbellaria'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Tricorythodes minutus***retired***use Tricorythodes explicatus'] <- 'Tricorythodes explicatus'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Tricyphona***retired***use Dicranota'] <- 'Tricyphona'
bugs.clean$Taxonomic_Name[bugs.clean$Taxonomic_Name == 'Zoniagrion'] <- 'Coenagrionidae'


 

      


# get taxonomy table from BioMonTools
url_bmt_base <- 'https://github.com/leppott/BioMonTools_SupportFiles/raw/main/data'
url_taxa_official_pick <- file.path(url_bmt_base, "taxa_official", "ORWA_TaxaTranslator_20240204.csv")
taxonomy <- read.csv(url_taxa_official_pick) #%>%
 # select(Taxon_orig, OTU_MetricCalc) # lots of duplicated columns in attributes table, keep only what is necessary


# link bugs and taxonomy - use OTU_MetricCalc
bugs.taxonomy <- bugs.clean %>%
  left_join(taxonomy, by = c('Taxonomic_Name' = 'Taxon_orig'))


# find unmatched taxa names and resolve
miss.taxa <- unique(bugs.taxonomy[which(is.na(bugs.taxonomy$OTU_MetricCalc)),c(2,4)])




## URL BioMonTools
                                              url_bmt_base <- 'https://github.com/leppott/BioMonTools_SupportFiles/raw/main/data'
                                              url_taxa_official_pick <- file.path(url_bmt_base, "taxa_official", "ORWA_Attributes_20240204.csv")
                                              attributes <- read.csv(url_taxa_official_pick)

attributes <- read.csv('bugs analyses/MMI/_2024 model build/ORWA_Attributes_20240204_DRAFT_FOR_SHANNON.csv') 

attributes <- attributes %>%  
    select(-Taxon_Group, -NonTarget, -TSN, -Kingdom, -Phylum, -SubPhylum, -Class, -SubClass, -Order, -SubOrder, -SuperFamily, -Family, -SubFamily, 
           -Tribe, -GenusGroup, -Genus, -SubGenus, -SpeciesGroup, -SpeciesSubGroup, -SpeciesComplex, -Species)


# link attributes to bugs, by OTU_MetricCalc
bugs.taxa.attributes <- bugs.taxonomy %>%
  left_join(attributes, by = c('OTU_MetricCalc' = 'Taxon_AttributeTable')) %>%
  rename(SAMPLEID = act_id, TAXAID = OTU_MetricCalc, N_TAXA = Result_Numeric)
  # use OTU_Metrics -- DO NOT use Taxonomic_Name



@@@@ do I need to do a sum function to roll all counts into a single record for each OTU? Exclude function doesnt seem to be working right



#######################

#     calculate metrics

#######################


###
    # mark excluded
###

@@@@ 423 records with ZERO counts...Why are these showing up? 424 records in 'bugs'

bugs.excluded <- markExcluded(bugs.taxa.attributes, TaxaLevels = c("Kingdom", "Phylum",
                "SubPhylum", "Class", "SubClass", "Order", "SubOrder", "SuperFamily", 
                "Family", "SubFamily", "Tribe", "GenusGroup", "Genus", "SubGenus", "SpeciesGroup",
                "SpeciesSubGroup", "SpeciesComplex", "Species"))





###
    # rarify
###



@@@ necessary ????????????


  
###
    # bug metrics
###  
  
bug.metrics <- metric.values(bugs.excluded, "bugs")


@@@@@ missing fields = INDEX_NAME, INDEX_CLASS



@@@@@@@@@@@@ 'taxa' metrics not working
  
  -- then link to attributes - dropping all but sample/otu/count


















