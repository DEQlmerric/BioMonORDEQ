#
# Author: Shannon Hubler
# 12.15.2020
# Revised slight august 2023

# Adapted from Step 8 in Bio Tools R upgrade_v3.4_noBCG.

# Purpose: pull together all bug analyses into a single table
# metrics
# Stressor
# PREDATOR



#############################################################################
#############################################################################
#############################################################################
#############################################################################
##########################################################

# Step 1: Create SUMMARY BUGS table

sum.bugs <- function() {
  
  # 1.1 = combine OE from both models
  oe.all<-rbind(oe.wccp, oe.mwcf)
  oe.all<-cbind(Sample=rownames(oe.all), oe.all) # no 'Sample' column, causes problems later
  
  #need same columns as predictive models to append rows
  oe.nbr$O<-NA
  oe.nbr$E<-NA
  oe.nbr$OoverE<-NA
  oe.nbr$BC<-NA
  oe.nbr$BC.null<-NA
  oe.nbr$outlier.05<-NA
  oe.nbr$outlier.01<-NA
  
  oe.all<-rbind(oe.all, oe.nbr)  
  oe.all<-arrange(oe.all, Sample)  
  
  # 1.2 = combine abundances and station/sample info back in with O/E scores
  
  #metrics<-merge(metrics, tot.abund.RIV, by='Sample', all.x=TRUE, suffix=c('','.y')) 
  #metrics<-merge(metrics, tot.abund.MTTI, by='Sample', all.x=TRUE, suffix=c('','.y'))
  #metrics<-merge(metrics, tot.abund.BSTI, by='Sample', all.x=TRUE, suffix=c('','.y'))
  
  # combine O/E -> Stress -> mets
  oe.mets <- metrics %>%
    dplyr::left_join(oe.all, by=c('Sample'))
  
  
  oe.stress.mets <- oe.mets %>%
    dplyr::left_join(MTTI, by=c('Sample')) %>%
    dplyr::left_join(BSTI, by=c('Sample')) %>%
    dplyr::left_join(tot.abund.RIV, by=c('Sample')) %>%
    dplyr::left_join(tot.abund.MTTI, by=c('Sample')) %>%
    dplyr::left_join(tot.abund.BSTI, by=c('Sample'))
  

  
  ########################
  ########################
  ########################
  ########################
  
  # Data Quality Objectives for Integrated Report ####
  
  ########################
  ########################
  ########################
  ########################
  
  
  
  ## get STATION-level info associated back to Sample summary metrics
  
  
  sta <- b_t_s  %>%
    dplyr::select(org_id,Sample, MLocID, StationDes, Project1, Date, Habitat, Activity_Type, lat, long, Eco2, Eco3, ELEV_m, 
                  precip_mm, temp_Cx10, W_E, HUC8_Name, HUC12_Name, Wade_Boat, Result_Status)
  
  sta$Result_Status <- as.factor(sta$Result_Status) 
  sta <- unique(sta)  
  # merge data frames together
  oe.stress.mets.sta <- merge(oe.stress.mets, sta, by=c('Sample'), all.x=TRUE, suffix=c('','.y'))#
  
  
  # 9.1 = low count
  oe.stress.mets.sta$low.count<-as.factor(ifelse(oe.stress.mets.sta$tot.abund.RIV < 150, 'YES','NO'))
  
  
  #9.2 = field and lab methods
  oe.stress.mets.sta$methods.ok<-as.factor(ifelse(oe.stress.mets.sta$Result_Status == 'Final', 'Yes', "No"))
  
  # 9.3 = Outlier analysis
  oe.stress.mets.sta$OE.outlier<-as.factor(ifelse(oe.stress.mets.sta$outlier.01 == 1, 'Yes', 'No'))
  
  table(oe.stress.mets.sta$OE.outlier)
  table(oe.stress.mets.sta$Project, oe.stress.mets.sta$OE.outlier)  
  
  
  # 9.4 = Index period (date range)
  
  library(lubridate)
  
  oe.stress.mets.sta$month<-month(oe.stress.mets.sta$Date)
  oe.stress.mets.sta$month[oe.stress.mets.sta$month == 'NA'] <- -999
  table(oe.stress.mets.sta$month)
  
  oe.stress.mets.sta$index.period<-as.factor(ifelse(oe.stress.mets.sta$month == 6, 'Yes', 
                                                    ifelse(oe.stress.mets.sta$month == 7, 'Yes',
                                                           ifelse(oe.stress.mets.sta$month == 8, 'Yes',
                                                                  ifelse(oe.stress.mets.sta$month == 9, 'Yes',
                                                                         ifelse(oe.stress.mets.sta$month == 10, 'Yes', 'No'))))))
  

  
  
  table(oe.stress.mets.sta$index.period, oe.stress.mets.sta$month)
  summary(oe.stress.mets.sta$index.period)
  
  
  # 9.5 = FINAL Biocriteria/Integrated Report filter:
  oe.stress.mets.sta$Use.303d<- ifelse(oe.stress.mets.sta$low.count == 'NO' & oe.stress.mets.sta$Habitat == 'Benthic Kick - Targeted Riffle' &
                                         oe.stress.mets.sta$OE.outlier == 'No' & oe.stress.mets.sta$Wade_Boat == 'wadeable' & 
                                         oe.stress.mets.sta$index.period == 'Yes' & oe.stress.mets.sta$methods.ok == 'Yes', 
                                       'Yes', 'No')
  
  # 9.6 = create a column of why failed
  
  low.count<-as.factor(ifelse(oe.stress.mets.sta$tot.abund.RIV < 150, 'lowcount',''))
  
  methods<-as.factor(ifelse(oe.stress.mets.sta$methods.ok == 'Yes', '', 'methods'))
  
  
  outlier<-as.factor(ifelse(oe.stress.mets.sta$outlier.01 == 1, 'outlier', ''))
  
  
  habitat<-as.factor(ifelse(oe.stress.mets.sta$Habitat == 'Benthic Kick - Targeted Riffle', '', 'habitat'))
  
  wadeable<-ifelse(oe.stress.mets.sta$Wade_Boat == 'wadeable','', 'non.wadeable')
  
  index<-ifelse(oe.stress.mets.sta$index.period == 'Yes', '', 'index')
  
  oe.stress.mets.sta$reason.no.303<-as.factor(paste(low.count, methods, outlier, habitat, wadeable, index, sep=' '))
  
  summary(oe.stress.mets.sta$reason.no.303)
  
  
  # Relocate after a specific column
  oe.stress.mets.sta <- oe.stress.mets.sta %>% 
    relocate(c(MLocID, StationDes, Project1, Date, Habitat, Activity_Type, lat, long), .after = Sample)
  
  return(oe.stress.mets.sta)
  
  
}

  

#########################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
