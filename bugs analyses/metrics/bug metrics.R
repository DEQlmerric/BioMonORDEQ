#
# Author: Shannon Hubler, Bug Overlord
# Original Code = Bio Tools R upgrade_v3.4_noBCG.R

# Adapted to a stand-alone set of code, just for calculating bug metrics
# This code would typically follow running "Data_from_AWQMS.R"

# 12.14.2020
# Revised August 2023 with Lesley and Travis 

# optimally, this would be turned into a function, to roll through all samples at once

bug.metrics <- function(b_t_s = b_t_s){
  
  ## calculate total abundance from raw data
  #??????Why do we recalculate tot.abund when it is calculated in sample.info from the DATA_fom_AWQMS.R function????????
  
  tot.abund <- b_t_s %>% 
    dplyr::group_by(Sample) %>%
    dplyr::summarise(total.abundance = sum(Count))
  
  # create a dataframe of relative abundances
  rel.abund<-b_t_s[, c("Sample", "Taxon", "Count")] 
  rel.abund <- rel.abund %>% 
    dplyr::group_by(Sample, Taxon) %>%
    dplyr::summarise(Count = sum(Count)) %>% 
    dplyr::left_join(tot.abund, by = c('Sample')) %>% 
    dplyr::mutate(rel_abundance = Count/total.abundance) 
  
  # create a dataframe of relative abundances, but only for unique taxa -- used for Diversity/Evenness metrics
  b_t_s.unique <- b_t_s %>% 
    dplyr::filter(UniqueTaxon=='UniqueTaxon')
  
  
  tot.abund.unique <- b_t_s.unique %>% 
    dplyr::group_by(Sample) %>% 
    dplyr::summarise(tot.abund.unique = sum(Count))
  
  
  rel.abund.unique <- b_t_s.unique %>% 
    dplyr::select("Sample", "Taxon", "Count") %>% 
    dplyr::left_join(tot.abund.unique, by = 'Sample') %>% 
    dplyr::mutate(rel_abundance_unique = Count/tot.abund.unique) 
  
  ###
  ####
  ##  ## METALS TOLERANCE INDEX
  ####
  ###
  
  
  # need to drop taxa without tolerances, then re-adjust relative abundances
  # drop taxa missing MTI
  # calculate total abundance
  
  myvars <- c("Sample", "Taxon", "Count", "MTI")
  rel.abund.mti <- b_t_s[myvars] %>% 
    dplyr::filter(MTI != '666') %>% 
    dplyr::group_by(Sample) %>% 
    dplyr::mutate(tot.abund.mti = sum(Count)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(rel.abund = Count/tot.abund.mti,
                  tol_X_ra = MTI * rel.abund)
  
  #calculate MTI: sum_across.taxa(rel abund * tol)
  
  MTI <- rel.abund.mti %>% 
    dplyr::group_by(Sample) %>% 
    dplyr::summarise(MTI = sum(tol_X_ra))
  
  
  ###
  ####
  ##  ## Total Richness
  ####
  ###
  
  b_t_s <- b_t_s %>% 
    dplyr::mutate(Unique.num = dplyr::case_when(UniqueTaxon=='UniqueTaxon' ~ 1,
                                                TRUE ~ 0))
  
  total.richness <- b_t_s %>% 
    dplyr::group_by(Sample) %>% 
    dplyr::summarise(all_uniquetaxon_na = all(is.na(UniqueTaxon),na.rm=TRUE),
                     any_unique = any(UniqueTaxon == 'UniqueTaxon',na.rm=TRUE),
                     total.richness = sum(Unique.num,na.rm=TRUE)) %>% 
    dplyr::mutate(total.richness = ifelse(all_uniquetaxon_na == TRUE | any_unique == FALSE, NA_integer_,total.richness )) %>% 
    dplyr::select(-any_unique, -all_uniquetaxon_na)
  
  ###
  ####
  ##  ## Order_Family
  ####
  ###
  
  # richness of all Orders --separatetly
  order.rich<-plyr::count(b_t_s, vars=c('Sample','UniqueTaxon', 'Order'))
  order.rich <- order.rich %>% dplyr::filter(UniqueTaxon=='UniqueTaxon')
  
  order.rich<-reshape::cast(order.rich, Sample ~ Order)
  colnames(order.rich) <- paste(colnames(order.rich), "rich",  sep = ".") #add '.rich' to note the type of metric
  order.rich[is.na(order.rich)] <- 0
  colnames(order.rich)[colnames(order.rich)=="Sample.rich"] <- "Sample"
  
  order5.rich <- order.rich %>% 
    dplyr::select(Sample, Coleoptera.rich, Diptera.rich, Ephemeroptera.rich, Plecoptera.rich, Trichoptera.rich) # limit families to 3)
  
  
  # richness of all Families --separately  
  family.rich<-plyr::count(b_t_s, vars=c('Sample','UniqueTaxon', 'Family'))
  family.rich <- family.rich %>% 
    dplyr::filter(UniqueTaxon=='UniqueTaxon')
  
  family.rich<-reshape::cast(family.rich, Sample ~ Family)
  colnames(family.rich) <- paste(colnames(family.rich), "rich",  sep = ".") #add '.rich' to note the type of metric
  family.rich[is.na(family.rich)]<- 0
  colnames(family.rich)[colnames(family.rich)=="Sample.rich"] <- "Sample"    
  
  family3.rich <- family.rich %>% dplyr::select(Sample, Baetidae.rich, Chironomidae.rich, Hydropsychidae.rich)
  
  
  # percent Order
  # first calculate Order abundance
  a_temp <- b_t_s %>% 
    dplyr::group_by(Sample, Order) %>% 
    dplyr::summarise(Count = sum(Count))
  
  a <- tot.abund %>%
    dplyr::left_join(a_temp, by='Sample') 
  
  pct_Order <- a %>% 
    dplyr::mutate(pct_ = Count/total.abundance*100)
  
  pct_Order<-reshape::cast(pct_Order, Sample ~ Order)                    # matrify pct
  colnames(pct_Order) <- paste("pct", colnames(pct_Order),   sep = "_")  # add prefix to denote metric type
  colnames(pct_Order)[colnames(pct_Order)=="pct_Sample"] <- "Sample"
  pct_Order[is.na(pct_Order)]<- 0
  
  pct_order5 <- pct_Order %>% dplyr::select(Sample, pct_Coleoptera, pct_Diptera, pct_Ephemeroptera, pct_Plecoptera, pct_Trichoptera)
  
  ###
  ####
  ##  ## EPT 
  ####
  ###
  
  # richness
  
  EPT.richness<-plyr::count(b_t_s, vars=c('Sample','UniqueTaxon', 'Order'))
  EPT.richness <- EPT.richness %>% dplyr::filter(UniqueTaxon=='UniqueTaxon')
  EPT.richness <- EPT.richness %>% dplyr::filter(Order=='Ephemeroptera' | Order=='Plecoptera' |Order=='Trichoptera')
  
  EPT.richness<-aggregate(EPT.richness$freq,  list(Sample=EPT.richness$Sample), sum)  
  colnames(EPT.richness)[colnames(EPT.richness)=="x"] <- "EPT.rich"
  
  # % EPT
  
  pct_EPT <- pct_Order %>% 
    dplyr::group_by(Sample) %>% 
    dplyr::summarise( pct_EPT = sum(pct_Ephemeroptera, pct_Plecoptera, pct_Trichoptera))
  
  
  ##
  ###
  ####
  #     Voltinism = # of broods per year
  ####
  ###
  ##
  
  # richness
  
  voltine.rich<-plyr::count(b_t_s, vars=c('Sample','UniqueTaxon', 'Voltine'))%>% 
    dplyr::filter(UniqueTaxon=='UniqueTaxon')
  
  voltine.rich<-reshape::cast(voltine.rich, Sample ~ Voltine) 
  voltine.rich[is.na(voltine.rich)]<- 0
  voltine.rich <- voltine.rich %>% 
    dplyr::rename(multivoltine.rich = MV, semivoltine.rich = SV, univoltine.rich = UV) %>%
    dplyr::select(Sample, multivoltine.rich, semivoltine.rich, univoltine.rich)
  
  
  # pct_Voltinism
  a <- b_t_s %>% 
    dplyr::group_by(Sample, Voltine) %>% 
    dplyr::summarise(Count = sum(Count))
  
  a <- tot.abund %>% dplyr::left_join(a, by = 'Sample') %>% 
    dplyr::group_by(Sample, Voltine) 
  
  pct_Voltine <- a %>% 
    dplyr::summarise( pct = Count/total.abundance*100)
  
  
  pct_Voltine<-reshape::cast(pct_Voltine, Sample ~ Voltine) 
  pct_Voltine <- pct_Voltine %>% dplyr::select(Sample, MV, SV, UV)# remove blanks or unknowns
  
  pct_Voltine <- pct_Voltine %>% 
    dplyr::rename(pct_multivoltine = MV, pct_semivoltine = SV, pct_univoltine = UV)
  
  
  
  ##
  ###
  ####
  #     Functional Feeding Groups
  ####
  ###
  ##
  
  # richness
  ffg.rich<-plyr::count(b_t_s, vars=c('Sample','UniqueTaxon', 'FFG'))
  ffg.rich <- ffg.rich %>% dplyr::filter(UniqueTaxon=='UniqueTaxon')
  
  
  ffg.rich<-reshape::cast(ffg.rich, Sample ~ FFG) 
  colnames(ffg.rich) <- paste(colnames(ffg.rich), "rich",  sep = ".")
  colnames(ffg.rich)[colnames(ffg.rich)=="Sample.rich"] <- "Sample"
  
  # pct_FFG
  a <- b_t_s %>% 
    dplyr::group_by(Sample, FFG) %>% 
    dplyr::summarise(Count = sum(Count))
  
  a <- tot.abund %>% dplyr::left_join(a, by = 'Sample')
  
  pct_FFG <- a %>% 
    dplyr::group_by(Sample, FFG) %>% 
    dplyr::summarise(pct = Count/total.abundance*100)
  
  pct_FFG<-reshape::cast(pct_FFG, Sample ~ FFG) 
  colnames(pct_FFG) <- paste("pct", colnames(pct_FFG),   sep = "_")  # add prefix to denote metric type
  colnames(pct_FFG)[colnames(pct_FFG)=="pct_Sample"] <- "Sample"
  pct_FFG[is.na(pct_FFG)]<- 0
  
  ##
  ###
  ####
  #     Dominance
  ####
  ###
  ##
  
  # % Dominant - Top 5 taxa
  
  #z<-plyr::ddply(.data=b_t_s, .(Sample),  plyr::summarize, Count=tail(sort(Count),5))
  
  pct_Dom.5 <- b_t_s %>% 
    dplyr::group_by(Sample) %>% 
    dplyr::arrange(desc(Count)) %>% 
    dplyr::slice(1:5) %>% 
    dplyr::summarise(Count = sum(Count)) %>% 
    dplyr::left_join(tot.abund, by = 'Sample') %>% 
    dplyr::mutate(pct_Dom.5=Count/total.abundance*100)%>% 
    dplyr::select(Sample, pct_Dom.5)
  
  pct_Dom.3 <- b_t_s %>% 
    dplyr::group_by(Sample) %>% 
    dplyr::arrange(desc(Count)) %>% 
    dplyr::slice(1:3) %>% 
    dplyr::summarise(Count = sum(Count)) %>% 
    dplyr::left_join(tot.abund, by = 'Sample') %>% 
    dplyr::mutate(pct_Dom.3=Count/total.abundance*100)%>% 
    dplyr::select(Sample, pct_Dom.3)
  
  
  # % Dominant - Top 1 taxon
  
  pct_Dom.1 <- b_t_s %>% 
    dplyr::group_by(Sample) %>% 
    dplyr::arrange(desc(Count)) %>% 
    dplyr::slice(1) %>% 
    dplyr::summarise(Count = sum(Count)) %>% 
    dplyr::left_join(tot.abund, by = 'Sample') %>% 
    dplyr::mutate(pct_Dom.1=Count/total.abundance*100) %>% 
    dplyr::select(Sample, pct_Dom.1)
  
  # combine all dominance together
  
  pct_Dom <- pct_Dom.5 %>% 
    dplyr::left_join(pct_Dom.3,  by = "Sample") %>% 
    dplyr::left_join(pct_Dom.1,  by = "Sample") 
  
  ##
  ###
  ####
  #     Diversity/Evenness
  ####
  ###
  ##
  
  SH.div <- rel.abund.unique %>% 
    dplyr::filter(Count != 0) %>% 
    dplyr::group_by(Sample) %>% 
    dplyr::summarise(Shannon_diversity= -(sum(rel_abundance_unique*log(rel_abundance_unique))),
                     Simpson_diversity = 1-sum(rel_abundance_unique^2))
  
  
  # Evenness
  even <- SH.div %>% dplyr::left_join(total.richness, by = 'Sample') %>% 
    dplyr::group_by(Sample) %>% 
    dplyr::summarise(Evenness= Shannon_diversity/(log(total.richness)))
  
  
  ##
  ###
  ####
  #     Non-Insect
  ####
  ###
  ##
  
  
  # richness of all Non-Insects  
  noninsect.rich<-plyr::count(b_t_s, vars=c('Sample','UniqueTaxon', 'Class'))
  noninsect.rich <- noninsect.rich %>% 
    dplyr::filter(UniqueTaxon=='UniqueTaxon' & Class != 'Insecta') %>% 
    dplyr::group_by(Sample) %>% 
    dplyr::summarise(Non.Insect_richness=sum(freq))
  
  a <- b_t_s %>% 
    dplyr::filter(Class != 'Insecta') %>% 
    dplyr::group_by(Sample) %>% 
    dplyr::summarise(Count=sum(Count)) 
  
  #pct_Non.Insect<-plyr::ddply(.data=a, .(Sample), plyr::summarize, Count=sum(Count))
  pct_Non.Insect <- tot.abund %>% dplyr::left_join(a, by = 'Sample')# bring in total abundance
  #pct_Non.Insect<-plyr::ddply(.data = pct_Non.Insect, .(Sample), plyr::summarize, pct_Non.Insect=Count/total.abundance*100)
  pct_Non.Insect[is.na(pct_Non.Insect)] <- 0
  
  pct_Non.Insect <- pct_Non.Insect %>% 
    dplyr::group_by(Sample) %>% 
    dplyr::summarise(pct_Non.Insect = Count/total.abundance*100)
  pct_Non.Insect[is.na(pct_Non.Insect)] <- 0
  
  ####
  ### ##
  ####  ##
  ###     ####        Combine all metrics together        #### ###
  ####  ##
  ### ##
  ####
  
  list.of.data.frames = list(total.richness, order5.rich, family3.rich, 
                             pct_order5, EPT.richness, pct_EPT, 
                             voltine.rich, pct_Voltine,
                             ffg.rich, pct_FFG, pct_Dom, SH.div, 
                             even, noninsect.rich, pct_Non.Insect,
                             MTI)
  
  
  
  metrics<-base::merge(list.of.data.frames[1], list.of.data.frames[2], by='Sample', all.x = TRUE)
  
  for(i in 3:length(list.of.data.frames)){
    metrics <- base::merge(metrics, list.of.data.frames[i], by='Sample', all.x = TRUE)
  }
  
  
  
  return(metrics)
}

# end of function
  
 
  