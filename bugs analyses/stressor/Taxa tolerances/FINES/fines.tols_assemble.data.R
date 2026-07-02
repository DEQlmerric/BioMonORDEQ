

# prepare fines and bugs data for use in tolerance designations

# origin date: 6/24/26, Shannon Hubler

# objectives: 
  # 1) bring in bug data from ORWA Thermal analysis (7977 samples)

  #     --join to translator and attributes get proper STE names and taxonomic hierarchy

  # 2) Sum counts at all taxonomic levels: roll up across hierarcichy




  # 3) bring in fines data used to create BSTI_v2 (ORWA; 1027 samples)

        # both datasets for 1 and 2 were copied over to this project

  # 4) join together, based on samples found in FINES dataset
  # 5) export file for use in tolerance code provided by Tetra Tech

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(openxlsx)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1) bug data: 

bugs.7977 <- read.csv('bugs analyses/stressor/Taxa tolerances/FINES/_Bio_All_7977.csv')
bugs.7977 <- bugs.7977 |>
  select(BugSampleID = UniqueID_v2, Taxon = TaxaID_v2, Count, RA, NonDistinct)


# bring in taxonomy, OTUs, attributes, etc.
attributes <- ORDEQBioassessment::attribute_table  # Taxon, taxonomic hierarchy, Taxon_Groups
attributes <- attributes %>% 
  select(Taxon, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily, Family,
         SubFamily, Tribe, GenusGroup, Genus, SubGenus, SpeciesGroup, SpeciesSubGroup, 
         SpeciesComplex, Species, Taxon_Group)


    # # fix INSECTA vs Insecta
    # attributes <- attributes |>
    #   mutate(Class = ifelse(Class == 'INSECTA', 'Insecta', Class))



translator <- ORDEQBioassessment::taxonomy_leppo    # Taxon, OTUs--standardized naming conventions

# join bugs with OTUs from translator

bugs.7977_OTUs <- bugs.7977 %>%
  left_join(translator, by=c('Taxon' = 'Taxon_orig')) %>%
  filter(NonTarget == 'FALSE') %>%
  select(-c(OTU_MTTI, OTU_BCG_MariNW, OTU_BSTI, NonTarget, Rationale))


# link to taxonomic hierarchy found in attributes
bugs.7977_otus.attr <- bugs.7977_OTUs %>%
  left_join(attributes, by = c('OTU_MetricCalc' = 'Taxon')) %>%
  select(BugSampleID, OTU_MetricCalc, everything()) %>%
  select(-Taxon) %>%
  rename(OTU_TolAnal = OTU_MetricCalc)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 2) roll up abundances across taxonomic levels, then combine all levels into same dataframe

                                                                                  # # Travis' code: Doesn't work as is...too many records at each level
                                                                                  # 
                                                                                  # bugs_aggregate.counts <- bugs.7977_otus.attr |>
                                                                                  #   select(-c(OTU_TolAnal, RA, NonDistinct, Taxon_Group))
                                                                                  #   
                                                                                  # taxa_sum <- function(df, summ_taxa, count_column = Count, new_sum_column){
                                                                                  #     df %>%
                                                                                  #       group_by(across(-c({{summ_taxa}}, {{count_column}}))) %>%
                                                                                  #       summarise({{new_sum_column}} := sum({{count_column}}, na.rm = TRUE), .groups = "drop")
                                                                                  #   }
                                                                                  # 
                                                                                  # 
                                                                                  # species2 <- taxa_sum(df=bugs_aggregate.counts, summ_taxa=Species, new_sum_column = Count)
                                                                                  





species.complex <- bugs.7977_otus.attr %>%
  filter(SpeciesComplex != "") |>
  group_by(BugSampleID, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily, Family, SubFamily, Tribe, 
           GenusGroup, Genus, SubGenus, SpeciesGroup, SpeciesSubGroup, SpeciesComplex, Taxon_Group) |>
  summarise(across(c(Count, RA), sum)) |>
  mutate(OTU_TolAnal = str_c(Genus, SpeciesComplex, sep=" ")) |>
  as.data.frame(species.complex) |>
  select(BugSampleID, OTU_TolAnal, Count, RA, Taxon_Group, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily, Family, SubFamily, Tribe)

species.subgroup <- bugs.7977_otus.attr %>%
  filter(SpeciesSubGroup != "") |>
  group_by(BugSampleID, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily, Family, SubFamily, Tribe, 
           GenusGroup, Genus, SubGenus, SpeciesGroup, SpeciesSubGroup, Taxon_Group) |>
  summarise(across(c(Count, RA), sum)) |>
  mutate(OTU_TolAnal = str_c(Genus, SpeciesSubGroup, sep=" ")) |>
  as.data.frame(species.subgroup) |>
  select(BugSampleID, OTU_TolAnal, Count, RA, Taxon_Group, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily, Family, SubFamily, Tribe)

species.group <- bugs.7977_otus.attr %>%
  filter(SpeciesGroup != "") |>
  group_by(BugSampleID, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily, Family, SubFamily, Tribe, 
           GenusGroup, Genus, SubGenus, SpeciesGroup, Taxon_Group) |>
  summarise(across(c(Count, RA), sum)) |>
  mutate(OTU_TolAnal = str_c(Genus, SpeciesGroup, sep=" ")) |>
  as.data.frame(species.group) |>
  select(BugSampleID, OTU_TolAnal, Count, RA, Taxon_Group, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily, Family, SubFamily, Tribe)


subgenus <- bugs.7977_otus.attr %>%
  filter(SubGenus != "") |>
  group_by(BugSampleID, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily, Family, SubFamily, Tribe, 
           GenusGroup, Genus, SubGenus, Taxon_Group) |>
  summarise(across(c(Count, RA), sum)) |>
  mutate(OTU_TolAnal = str_c(Genus, SubGenus, sep=" ")) |>
  as.data.frame(subgenus) |>
  select(BugSampleID, OTU_TolAnal, Count, RA, Taxon_Group, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily, Family, SubFamily, Tribe)

genus <- bugs.7977_otus.attr %>%
  filter(Genus != "") |>
  group_by(BugSampleID, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily, Family, SubFamily, Tribe, 
           GenusGroup, Genus, Taxon_Group) |>
  summarise(across(c(Count, RA), sum)) |>
  mutate(OTU_TolAnal = Genus) |>
  as.data.frame(genus) |>
  select(BugSampleID, OTU_TolAnal, Count, RA, Taxon_Group, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily, Family, SubFamily, Tribe)

genus.group <- bugs.7977_otus.attr %>%
  filter(GenusGroup != "") |>
  group_by(BugSampleID, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily, Family, SubFamily, Tribe, 
           GenusGroup, Taxon_Group) |>
  summarise(across(c(Count, RA), sum)) |>
  mutate(OTU_TolAnal = GenusGroup) |>
  as.data.frame(genus.group) |>
  select(BugSampleID, OTU_TolAnal, Count, RA, Taxon_Group, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily, Family, SubFamily, Tribe)

tribe <- bugs.7977_otus.attr %>%
  filter(Tribe != "") |>
  group_by(BugSampleID, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily, Family, SubFamily, Tribe, Taxon_Group) |>
  summarise(across(c(Count, RA), sum)) |>
  mutate(OTU_TolAnal = Tribe) |>
  as.data.frame(tribe) |>
  select(BugSampleID, OTU_TolAnal, Count, RA, Taxon_Group, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily, Family, SubFamily, Tribe)

subfamily <- bugs.7977_otus.attr %>%
  filter(SubFamily != "") |>
  group_by(BugSampleID, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily, Family, SubFamily, Taxon_Group) |>
  summarise(across(c(Count, RA), sum)) |>
  mutate(OTU_TolAnal = SubFamily) |>
  as.data.frame(subfamily) |>
  select(BugSampleID, OTU_TolAnal, Count, RA, Taxon_Group, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily, Family, SubFamily) |>
  mutate(Tribe = NA)

family <- bugs.7977_otus.attr %>%
  filter(Family != "") |>
  group_by(BugSampleID, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily, Family, Taxon_Group) |>
  summarise(across(c(Count, RA), sum)) |>
  mutate(OTU_TolAnal = Family) |>
  as.data.frame(family) |>
  select(BugSampleID, OTU_TolAnal, Count, RA, Taxon_Group, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily, Family) |>
  mutate(SubFamily = NA, Tribe = NA)


superfamily <- bugs.7977_otus.attr %>%
  filter(SuperFamily != "",  Class != 'Insecta') |>
  group_by(BugSampleID, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily, Taxon_Group) |>
  summarise(across(c(Count, RA), sum)) |>
  mutate(OTU_TolAnal = SuperFamily) |>
  as.data.frame(superfamily) |>
  select(BugSampleID, OTU_TolAnal, Count, RA, Taxon_Group, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily) |>
  mutate(Family = NA, SubFamily = NA, Tribe = NA)



suborder <- bugs.7977_otus.attr %>%
  filter(SubOrder != "",  Class != 'Insecta') |>
  group_by(BugSampleID, Phylum, SubPhylum, Class, SubClass, Order, SubOrder, Taxon_Group) |>
  summarise(across(c(Count, RA), sum)) |>
  mutate(OTU_TolAnal = SubOrder) |>
  as.data.frame(suborder) |>
  select(BugSampleID, OTU_TolAnal, Count, RA, Taxon_Group, Phylum, SubPhylum, Class, SubClass, Order, SubOrder) |>
  mutate(SuperFamily = NA, Family = NA, SubFamily = NA, Tribe = NA)

order <- bugs.7977_otus.attr %>%
  filter(Order != "",  Class != 'Insecta') |>
  group_by(BugSampleID, Phylum, SubPhylum, Class, SubClass, Order, Taxon_Group) |>
  summarise(across(c(Count, RA), sum)) |>
  mutate(OTU_TolAnal = Order) |>
  as.data.frame(order) |>
  select(BugSampleID, OTU_TolAnal, Count, RA, Taxon_Group, Phylum, SubPhylum, Class, SubClass, Order) |>
  mutate(SubOrder = NA, SuperFamily = NA, Family = NA, SubFamily = NA, Tribe = NA)


subclass <- bugs.7977_otus.attr %>%
  filter(SubClass != "" , Class != 'Insecta' , Class != 'Malacostraca') |>
  group_by(BugSampleID, Phylum, SubPhylum, Class, SubClass, Taxon_Group) |>
  summarise(across(c(Count, RA), sum)) |>
  mutate(OTU_TolAnal = SubClass) |>
  as.data.frame(subclass) |>
  select(BugSampleID, OTU_TolAnal, Count, RA, Taxon_Group, Phylum, SubPhylum, Class, SubClass) |>
  mutate(Order = NA, SubOrder = NA, SuperFamily = NA, Family = NA, SubFamily = NA, Tribe = NA)


class <- bugs.7977_otus.attr %>%
  filter(Class != "" , Class != 'Insecta' , Class != 'Malacostraca') |>
  group_by(BugSampleID, Phylum, SubPhylum, Class, Taxon_Group) |>
  summarise(across(c(Count, RA), sum)) |>
  mutate(OTU_TolAnal = Class) |>
  as.data.frame(class) |>
  select(BugSampleID, OTU_TolAnal, Count, RA, Taxon_Group, Phylum, SubPhylum, Class) |>
  mutate(SubClass = NA, Order = NA, SubOrder = NA, SuperFamily = NA, Family = NA, SubFamily = NA, Tribe = NA)


phylum <- bugs.7977_otus.attr %>%
  filter(Phylum != "" & (Phylum=='Nemata' | Phylum=='Nematomorpha' | Phylum=='Nemertea' | Phylum =='Platyhelminthes')) |>
  group_by(BugSampleID, Phylum, Taxon_Group) |>
  summarise(across(c(Count, RA), sum)) |>
  mutate(OTU_TolAnal = Phylum) |>
  as.data.frame(Class) |>
  select(BugSampleID, OTU_TolAnal, Count, RA, Taxon_Group, Phylum) |>
  mutate(SubPhylum = NA, Class = NA, SubClass = NA, Order = NA, SubOrder = NA, SuperFamily = NA, Family = NA, SubFamily = NA, Tribe = NA)

# combine all levels back together

bugs_rolled <- rbind(phylum, class, subclass, order, suborder, superfamily, family, subfamily, tribe, 
                     genus.group, genus, subgenus, species.group, species.subgroup, species.complex, species)



bugs_rolled <- bugs_rolled |>
  mutate(across(c(Phylum, SubPhylum, Class, SubClass, Order, SubOrder, SuperFamily, Family, SubFamily, Tribe), toupper))


# 2) fines data:

fines.1027 <- read.csv('bugs analyses/stressor/Taxa tolerances/FINES/FN_all sites.csv')
fines.1027 <- fines.1027 |> 
  select(BugSampleID = UniqueID_v2, siteID, Date, PCT_FN, Latitude = lat, Longitude = long)


# 3) join bugs and fines, based on fines samples

bugs.fines <- fines.1027 |>
  left_join(bugs_rolled, by = 'BugSampleID')


    # do a check to see how many unique samples there are
    
    length(unique(bugs.fines$BugSampleID)) # kaboomshkwa!

    
# 4) export a file for use in tol code

write.csv(bugs.fines, 'bugs analyses/stressor/Taxa tolerances/FINES/Input_data/ORWA/bugs.fines_1027_rolled.csv')

