# _____       __                               ____                  _                          _        
#|  __ \     / _|                             |  _ \                | |                        | |       
#| |__) |___| |_ ___ _ __ ___ _ __   ___ ___  | |_) | ___ _ __   ___| |__  _ __ ___   __ _ _ __| | _____ 
#|  _  // _ \  _/ _ \ '__/ _ \ '_ \ / __/ _ \ |  _ < / _ \ '_ \ / __| '_ \| '_ ` _ \ / _` | '__| |/ / __|
#| | \ \  __/ ||  __/ | |  __/ | | | (_|  __/ | |_) |  __/ | | | (__| | | | | | | | | (_| | |  |   <\__ \
#|_|  \_\___|_| \___|_|  \___|_| |_|\___\___| |____/ \___|_| |_|\___|_| |_|_| |_| |_|\__,_|_|  |_|\_\___/
                                                                                           
#   _     _  _______  _______  _______  ______      _______  __   __  _______  __   __  ___   _______  _______  ______    __   __ 
#  | | _ | ||   _   ||       ||       ||    _ |    |       ||  | |  ||       ||  |_|  ||   | |       ||       ||    _ |  |  | |  |
#  | || || ||  |_|  ||_     _||    ___||   | ||    |       ||  |_|  ||    ___||       ||   | |  _____||_     _||   | ||  |  |_|  |
#  |       ||       |  |   |  |   |___ |   |_||_   |       ||       ||   |___ |       ||   | | |_____   |   |  |   |_||_ |       |
#  |       ||       |  |   |  |    ___||    __  |  |      _||       ||    ___||       ||   | |_____  |  |   |  |    __  ||_     _|
#  |   _   ||   _   |  |   |  |   |___ |   |  | |  |     |_ |   _   ||   |___ | ||_|| ||   |  _____| |  |   |  |   |  | |  |   |  
#  |__| |__||__| |__|  |___|  |_______||___|  |_|  |_______||__| |__||_______||_|   |_||___| |_______|  |___|  |___|  |_|  |___|  

#OVERVIEW - This script imports, modifies, and summarizes data to support the development of water chemistry reference benchmarks.
#AUTHORS - A. Thompson & S. Berzins
#CREATED - 2022

#LOAD PACKAGES
library(AWQMSdata) # Visit 'https://github.com/TravisPritchardODEQ/AWQMSdata' for installation instructions
library(tidyverse)
library(writexl)
library(leaflet)
library(RColorBrewer)
library(lubridate)

#IMPORT STATIONS WITH A REFERENCE DESIGNATION FROM STATIONS DB (N = 2522 as of 8/8/25)
stations <- query_stations()
ref_stations <- stations %>% 
  filter(ReferenceSite %in% c("REFERENCE" , "MODERATELY DISTURBED", "MOST DISTURBED")) %>% 
  select(MLocID, COMID, ReferenceSite, OrgID)
rm(stations)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
# RETRIEVE AWQMS DATA AT REF STATIONS
# NOTE: DATA PULL CAN TAKE ~5-10 MINUTES; MUST BE CONNECTED TO VPN
#-----------------------------------------------------------------------------------------------------------------------------------------------------
chem.all_ref <- AWQMS_Data(MLocID = ref_stations$MLocID)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
#MERGE CHEM AND REF STATIONS TABLE TO BRING IN REFERENCE STATUS AND COMID
#-----------------------------------------------------------------------------------------------------------------------------------------------------
chem.all_ref <- inner_join(chem.all_ref, ref_stations, by = "MLocID")

#-----------------------------------------------------------------------------------------------------------------------------------------------------
#REMOVE UNWANTED DATA FROM FURTHER ANALYSIS
#-----------------------------------------------------------------------------------------------------------------------------------------------------
#VOIDED/REJECTED AND PRELIMINARY WATER CHEMISTRY DATA
chem.ref <- subset(chem.all_ref, chem.all_ref$Result_status != 'Rejected' & chem.all_ref$Result_status != 'Preliminary')

#DQL is E                                       #  Check w/ SH, AT
chem.ref <- chem.ref %>% filter (DQL != "E")

#CONTINUOUS DATA
chem.ref <- subset(chem.ref, chem.ref$SamplingMethod != "Continuous Summary")

#LOC TYPES OTHER THAN RIVERS AND STREAMS        #  Check w/ SH, AT
chem.ref <- chem.ref %>% filter(MonLocType == "River/Stream")

#SAMPLE MEDIA OTHER THAN WATER                  #   Check w/ SH, AT
chem.ref <- chem.ref %>%  filter(SampleMedia == "Water")

#LEGACY AMBIENT STATIONS
  #1: SUBSET AMBIENT PROJECT DATA
amb <- subset(chem.ref, chem.ref$Project1 == "Surface Water Ambient Monitoring")

  #2: MAKE LIST OF AMBIENT STATIONS
amb.stations <- amb %>% distinct(MLocID, .keep_all=TRUE)
amb.stations <- subset(amb.stations, select = MLocID)

  #3: FILTER OUT AMBIENT STATIONS
chem.ref <- anti_join(chem.ref, amb.stations, by = "MLocID")

rm(amb)
rm(amb.stations)

#TRIM DATA TO LOW FLOW INDEX PERIOD (JUNE 1-OCTOBER 15)
#1: DATE TO DATE FORMAT
chem.ref$SampleStartDate <- ymd(chem.ref$SampleStartDate)

#2: FILTER OUT RECORDS WHERE SAMPLING DATE IS OUTSIDE OF JUNE 1 - OCTOBER 15 DATE RANGE
chem.ref <- chem.ref %>%
  filter((month(SampleStartDate) > 6 | (month(SampleStartDate) == 6 & day(SampleStartDate) >= 1)) &
      (month(SampleStartDate) < 10 | (month(SampleStartDate) == 10 & day(SampleStartDate) <= 15)))

#UTAH STATE UNIVERSITY SITES 
  #They don't have water chemistry data
chem.ref <- subset(chem.ref, chem.ref$OrgID != 'USU(NOSTORETID)')

#-----------------------------------------------------------------------------------------------------------------------------------------------------
# MISC. DATA CLEAN-UP
#-----------------------------------------------------------------------------------------------------------------------------------------------------
# HARDCODE IN THE LEVEL 3 ECOREGIONS
chem.ref <- chem.ref %>% 
  mutate(L3Eco = EcoRegion3) %>%  
  mutate(L3Eco = case_when(
    L3Eco == "3" ~ "Willamette Valley",
    L3Eco == "9" ~ "Eastern Cascades Slopes and Foothills",
    L3Eco == "11" ~ "Blue Mountains",
    L3Eco == "78" ~ "Klamath Mountains",
    L3Eco == "1" ~ "Coast Range",
    L3Eco == "4" ~ "Cascades",
    L3Eco == "80" ~ "Northern Basin and Range",
    L3Eco == "10" ~ "Columbia Plateau",
    L3Eco == "12" ~ "Snake River Plain")) %>% 
  relocate(L3Eco, .before = EcoRegion3)

# POPULATE NEW NUMERIC RESULT COLUMN TO ACCOUNT FOR NON-DETECTS AND EXCEEDANCES
# Value for non-detect calculated as 1/2 MDL.
chem.ref <- chem.ref %>% 
  mutate(Result_Numeric_mod = ifelse(Result_Text == 'ND', 
                                     MDLValue * 0.5, # Non-detects based on result text column
                                     ifelse(Result_Operator == '<', Result_Numeric * 0.5, # Non-detects based on "<" prefix
                                            Result_Numeric))) %>%  # Otherwise provide value for standard result types 
  relocate(Result_Numeric_mod, .after = Result_Numeric)

# GET RID OF SOME UNUSED COLUMNS FOR READABILITY
chem.ref <- chem.ref %>% 
  select(!c(Project2, Project3, ResultCondName, Result_Depth:Result_Depth_Reference,Act_depth_Reference:stant_name))

# NARROW TABLE TO CHARS OF INTEREST ONLY (From StressorID Team: Temp, pH, DO, TP, TN, TSS, NH3)
chem.ref.wq <- chem.ref %>% 
  filter(Char_Name %in% c("Temperature, water", "pH", "Dissolved oxygen (DO)", "Dissolved oxygen saturation", "Total Phosphorus, mixed forms", 
    "Nitrogen", "Nitrate + Nitrite", "Total Kjeldahl nitrogen", "Total suspended solids", "Ammonia")) %>% 
  filter(!(Char_Name=='Total Phosphorus, mixed forms' & Char_Speciation=='as P')) # Excludes 1999 data with undefined method speciation 

# CALCULATE TOTAL NITROGEN. TN = TKN + Nitrate + Nitrite
# nits
tn.nits<-subset(chem.ref.wq, chem.ref.wq$`Char_Name`=="Nitrate + Nitrite")
# tkn
tn.tkn<-subset(chem.ref.wq, chem.ref.wq$`Char_Name`=="Total Kjeldahl nitrogen")
# merge
tn.nits.tkn<-rbind(tn.nits, tn.tkn)

# Only include samples that inlude both TKN and Nitrate + Nitrite. Only keep nits and tkn samples where
#  each appears once per sample time per site.
# Need a dateTime to get unique sample events


nit_count <- tn.nits.tkn %>% 
  group_by(MLocID, SampleStartDate) %>%   #CHANGE to dateTime field
  filter(any(Char_Name == "Nitrate + Nitrite") & any(Char_Name == "Total Kjeldahl nitrogen") 
         & sum(Char_Name == "Nitrate + Nitrite") == 1 & sum(Char_Name == "Total Kjeldahl nitrogen") == 1
  ) %>%
  ungroup() %>% 
  arrange(SampleStartDate, MLocID)
# Sum rows to calculate TN. Change the Char_Name column to TN (calculated)
# Remove nits and tkn from chem.ref.wq and replace with TN.
# SYB 9/9 working on it

#-----------------------------------------------------------------------------------------------------------------------------------------------------
#SUMMARIZE PARAMETERS AND DATES FOR EACH STATION
  #Know for each site which parameters were collected and when (min/max date)
  #Don't want to develop a parameter benchmark if bad geographic coverage
  #Look in bug metrics code in BiomonR for examples of Tidy ways.
  #Append result to data table

  #1: BY LOCATION AND PARAMETER
sum.loc <- chem.ref.wq %>%
  group_by(MLocID, StationDes, Char_Name,ReferenceSite) %>%
  summarise(n.Samples = n(),
            minDate = min(SampleStartDate),
            maxDate = max(SampleStartDate))

  #2: BY PARAMETER, REFERENCE STATUS, AND ECOREGION LEVEL 3
sum.eco <- chem.ref.wq %>%
  group_by(L3Eco, ReferenceSite, Char_Name) %>%
  summarise(n.Samples = n(),
            minDate = min(SampleStartDate),
            maxDate = max(SampleStartDate))

  #Export to Excel if wanting to explore more via data filters
#write.xlsx(sum.eco, file = "Benchmarks/Water Chemistry/summary_by_param_eco.xlsx")

#3: MAPS

# Single dot for each reference site with water chemistry data (site may have been sampled multiple times):
gry <- c("#969696", "#141414", "#fff" )
refpal <- colorFactor(gry, domain = chem.ref.wq$ReferenceSite)
labs <- c("Moderately disturbed", "Most disturbed", "Reference")

refmap <- leaflet(data = chem.ref.wq) %>%
  addTiles() %>%
  setView(lng = -120.5583, lat =44.0671, zoom =6.4) %>%
  addCircleMarkers(lng = ~Long_DD, lat = ~Lat_DD, fillColor = refpal(chem.ref.wq$ReferenceSite), stroke = TRUE,
                   color = "#000", weight = 0.5, fillOpacity = 2, radius = 3, 
                   popup = paste0("<strong>", "MLocID: ","</strong>", chem.ref.wq$MLocID, "<br>",
                                  "<strong>", "Station Description: ", "</strong>", chem.ref.wq$StationDes, "<br>",
                                  "<strong>", "Level 3 Ecoregion: ", "</strong>", chem.ref.wq$L3Eco, "<br>",
                                  "<strong>", "Reference Status: ", "</strong>", chem.ref.wq$ReferenceSite, "<br>")) %>% 
  addLegend(colors = gry, labels = labs, position = "bottomright",
            title = paste0("Sites with WQ & a reference designation<br>N = ",length(unique(chem.ref.wq$MLocID)), " unique MLocIDs"), opacity = 1)
refmap # Print map

#-----------------------------------------------------------------------------------------------------------------------------------------------------
#END OF PROOFED CODE; REST BELOW IS WORK IN PROGRESS ....
#-----------------------------------------------------------------------------------------------------------------------------------------------------

#SUMMARIZE DATA ~~~~~~~~~~~~~~~~~~~~??????????? use this section  ?????

  #1: SUBSET DATA BY REFERENCE CONDITION
ref <- subset(wqdata, wqdata$ReferenceSite == "REFERENCE")
mod <- subset(wqdata, wqdata$ReferenceSite == "MODERATELY DISTURBED")
most <- subset(wqdata, wqdata$ReferenceSite == "MOST DISTURBED")

  #2: LIST NUMBER OF PARAMETERS ASSOCIATED WITH EACH REFERENCE STATION
aggregate(data=ref, Char_Name ~ MLocID, function(x) length(unique(x)))

  #3: LIST NUMBER OF SAMPLING DATES ASSOCIATED WITH EACH REFERENCE STATION
aggregate(data=ref, SampleStartDate ~ MLocID, function(x) length(unique(x)))

  #4: LIST NUMBER OF PARAMETERS ASSOCIATED WITH ALL STATIONS
aggregate(data=wqdata, Char_Name ~ MLocID, function(x) length(unique(x)))





# testing boxplots of ref/mod/most by ecoregion for each parameter
tss <- subset(wqdata, wqdata$Char_Name == "Total suspended solids")

boxtss <- ggplot(tss, aes(x = ReferenceSite, y = Result_Numeric)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  facet_grid(~Eco3) +
  coord_cartesian(ylim = c(0, 25)) +
  labs(x = "Reference Condition", y = "Total Suspended Solids (mg/L)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(boxtss)
#avoid having one site drive results for an entire ecoregion (e.g., whychus in 9)
#sort by ref, mod, most


#TOTAL SUSPENDED SOLIDS
tss<-subset(wqdata, wqdata$Char_Name == 'Total suspended solids')
tss2<-ggplot(tss, aes(x = Ref2020_FINAL, y = Result_Numeric)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Reference Condition", y = "Total Suspended Solids (mg/L)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(tss2)





#testing function code from trend script
#RUN BOX PLOT FUNCTION
boxp <- function(data, x, y) {
  ggplot(data, aes({{x}}, {{y}})) +
    geom_boxplot(outlier.color = "red", outlier.shape = 8) + #red asterisk outliers
    labs(x = "Ecoregion", y = c(paste(data$Char_Name, "(",data$Result_Unit,")"))) + #automatic axis labeling
    #geom_hline(yintercept = data$MRLValue, linetype = "dashed", color = "red") + #MRL horizontal reference line
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
}



#GRAB WQ PARAMETERS BOX PLOTS (adjust y axis range as needed)
tss.ref <- subset(tss, tss$ReferenceSite == "REFERENCE")
tss.mod <- subset(tss, tss$ReferenceSite == "MODERATELY DISTURBED")
tss.most <- subset(tss, tss$ReferenceSite == "MOST DISTURBED")

boxp(tss.ref, EcoRegion3, Result_Numeric)
boxp(tss.mod, EcoRegion3, Result_Numeric)
boxp(tss.most, EcoRegion3, Result_Numeric)


boxp(turb, StationDes, Result_Numeric) + coord_cartesian(ylim = c(0, 25))
boxp(cond, StationDes, Result_Numeric_mod)
boxp(do.sat, StationDes, Result_Numeric_mod) + coord_cartesian(ylim = c(50, 120))
boxp(do.conc, StationDes, Result_Numeric_mod) + coord_cartesian(ylim = c(4, 12))
boxp(ph, StationDes, Result_Numeric_mod) + coord_cartesian(ylim = c(6.5, 8.5))
boxp(temp, StationDes, Result_Numeric_mod) + coord_cartesian(ylim = c(0, 25))
boxp(afdm, StationDes, Result_Numeric_mod) #messy MRLs
boxp(chla, StationDes, Result_Numeric_mod) + coord_cartesian(ylim = c(0, 0.01))

#LAB WQ PARAMETERS BOX PLOTS (add chart modifiers as needed)
boxp(tp, StationDes, Result_Numeric_mod)
boxp(sulf, StationDes, Result_Numeric_mod)
boxp(tn, StationDes, Result_Numeric_mod) #fix char_name


  #2: SUM NUMBER OF SITES ASSOCIATED WITH EACH PARAMETER FOR REFERENCE STATIONS ~~~~~~~~~~~~~~~~~~~~??????????? use this section  ?????
paramxsite <- ref %>% distinct(Char_Name, MLocID)
paramxsite <- dcast(paramxsite, Char_Name ~ MLocID, 
                  value.var = "Char_Name", fun.aggregate = length)
paramxsite$nsites <- rowSums(paramxsite[, 2:ncol(paramxsite)])

  #3: MAKE PLOT OF NUMBER OF SITES BY PARAMETER - ~~~~~~~~~~~~~~~~~~~~??????????? use this section  ?????
Params_by_site <- ggplot(paramxsite[paramxsite$nsites > 25,], aes(x = Char_Name, y = nsites)) + 
  geom_bar(stat="identity", position = position_dodge(), fill = "steelblue", color = "Black") +
  labs(x = "WQ Parameter", y = "Number of Samples (Reference)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #geom_text(position = position_dodge(width = 0.9, angle = 90, check_overlap = TRUE))
print(Params_by_site)

  #4: SAVE OUTPUT FOR FURTHER EXPLORATION
write_xlsx(paramxsite, path = "//deqlab1/ATHOMPS/Files/Biomon/R Chem Benchmarks/Params_By_Site.xlsx")

#-----------------------------------------------------------------------------------------------------------------------------------------------------

#BOX PLOTS OF PARAMETERS
  #CONDUCTIVITY
cond<-subset(wqdata, wqdata$Char_Name == 'Conductivity')
cond2<-ggplot(cond, aes(x = ReferenceSite, y = Result_Numeric)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Reference Condition", y = "Conductivity (uS/cm)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(cond2)

#transformation
  #CONDUCTIVITY - NO OUTLIERS
cond<-subset(wqdata, wqdata$Char_Name == 'Conductivity')
cond2<-ggplot(cond, aes(x = ReferenceSite, y = Result_Numeric)) +
  geom_boxplot(outlier.shape = NA) + 
  coord_cartesian(ylim =  c(0, 300)) +
  labs(x = "Reference Condition", y = "Conductivity (uS/cm)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(cond2)

#CONDUCTIVITY - NO OUTLIERS BY ECOREGION
cond<-subset(wqdata, wqdata$Char_Name == 'Conductivity')
cond2<-ggplot(cond, aes(x = EcoRegion3, y = Result_Numeric)) +
  geom_boxplot(outlier.shape = NA) + 
  coord_cartesian(ylim =  c(0, 1000)) +
  labs(x = "Ecoregion 3", y = "Conductivity (uS/cm)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(cond2)

  #DO mg/L
do_conc<-subset(wqdata, wqdata$Char_Name == 'Dissolved oxygen (DO)')
do_conc2<-ggplot(do_conc, aes(x = Ref2020_FINAL, y = Result_Numeric)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Reference Condition", y = "DO (mg/L)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(do_conc2)

  #DO %
do_perc<-subset(wqdata, wqdata$Char_Name == 'Dissolved oxygen saturation')
do_perc2<-ggplot(do_perc, aes(x = Ref2020_FINAL, y = Result_Numeric)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Reference Condition", y = "DO (%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(do_perc2)

  #pH
ph<-subset(wqdata, wqdata$Char_Name == 'pH')
ph2<-ggplot(ph, aes(x = Ref2020_FINAL, y = Result_Numeric)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Reference Condition", y = "pH (SU)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(ph2)

  #AMMONIA
ammonia<-subset(wqdata, wqdata$Char_Name == 'Ammonia')
ammonia2<-ggplot(ammonia, aes(x = Ref2020_FINAL, y = Result_Numeric)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Reference Condition", y = "Ammonia (mg/L)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(ammonia2)

  #TOTAL SUSPENDED SOLIDS
tss<-subset(wqdata, wqdata$Char_Name == 'Total suspended solids')
tss2<-ggplot(tss, aes(x = Ref2020_FINAL, y = Result_Numeric)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Reference Condition", y = "Total Suspended Solids (mg/L)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(tss2)

  #NITRATE + NITRITE
nits<-subset(wqdata, wqdata$Char_Name == 'Nitrate + Nitrite')
nits2<-ggplot(nits, aes(x = Ref2020_FINAL, y = Result_Numeric)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Reference Condition", y = "Nitrate + Nitrite (mg/L)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(nits2)

  #TOTAL SOLIDS
ts<-subset(wqdata, wqdata$Char_Name == 'Total solids')
ts2<-ggplot(ts, aes(x = Ref2020_FINAL, y = Result_Numeric)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Reference Condition", y = "Total solids (mg/L)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(ts2)

  #TOTAL KJELDAHL NITROGEN
tkn<-subset(wqdata, wqdata$Char_Name == 'Total Kjeldahl nitrogen')
tkn2<-ggplot(tkn, aes(x = Ref2020_FINAL, y = Result_Numeric)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Reference Condition", y = "Total Kjeldahl nitrogen (mg/L)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(tkn2)

  #ORTHOPHOSPHATE
ortho<-subset(wqdata, wqdata$Char_Name == 'Orthophosphate')
ortho2<-ggplot(ortho, aes(x = Ref2020_FINAL, y = Result_Numeric)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Reference Condition", y = "Orthophosphate (mg/L)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(ortho2)

  #TOTAL PHOSPHORUS
tp<-subset(wqdata, wqdata$Char_Name == 'Total Phosphorus, mixed forms')
tp2<-ggplot(tp, aes(x = Ref2020_FINAL, y = Result_Numeric)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Reference Condition", y = "Total phosphorus (mg/L)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(tp2)

  #ORGANIC CARBON
orgc<-subset(wqdata, wqdata$Char_Name == 'Organic carbon')
orgc2<-ggplot(orgc, aes(x = Ref2020_FINAL, y = Result_Numeric)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Reference Condition", y = "Organic carbon (mg/L)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(orgc2)

  #ALKALINITY
alk<-subset(wqdata, wqdata$Char_Name == 'Alkalinity, total')
alk2<-ggplot(alk, aes(x = Ref2020_FINAL, y = Result_Numeric)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Reference Condition", y = "Total alkalinity (mg/L)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(alk2)

  #BIOLOGICAL OXYGEN DEMAND
bod<-subset(wqdata, wqdata$Char_Name == 'Biochemical oxygen demand, non-standard conditions')
bod2<-ggplot(bod, aes(x = Ref2020_FINAL, y = Result_Numeric)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Reference Condition", y = "Biological oxygen demand (mg/L)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(bod2)

  #SULFATE
sulf<-subset(wqdata, wqdata$Char_Name == 'Sulfate')
sulf2<-ggplot(sulf, aes(x = Ref2020_FINAL, y = Result_Numeric)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Reference Condition", y = "Sulfate (mg/L)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(sulf2)

  #CHLORIDE
cl<-subset(wqdata, wqdata$Char_Name == 'Chloride')
cl2<-ggplot(cl, aes(x = Ref2020_FINAL, y = Result_Numeric)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Reference Condition", y = "Chloride (mg/L)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(cl2)

  #TURBIDITY <-need to merge w/ lab turb?
turb<-subset(wqdata, wqdata$Char_Name == 'Turbidity Field')
turb2<-ggplot(turb, aes(x = Ref2020_FINAL, y = Result_Numeric)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Reference Condition", y = "Turbidity (NTU)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(turb2)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
#IDENTIFY STATIONS FROM OUR REFERENCE SCREENS THAT DON'T HAVE WATER CHEMISTRY DATA IN AWQMS
#Reveals where additional data collections might be warranted
#-----------------------------------------------------------------------------------------------------------------------------------------------------
#1: CREATE TABLE CONTAINING SITES WITH NO CHEMISTRY DATA
nochem <- anti_join(ref_stations, chem.all_ref, by = "MLocID") # All rows from ref_stations with no match in chem.no_bio

#2: OPTIONAL - WRITE TO EXCEL FOR FURTHER EXAMINATION
#write_xlsx(nochem, path = "Benchmarks/Water Chemistry/SitesMissingChemData.xlsx")

#   #3: SUMMARIZE SITES BY OWNER, REF STATUS, AND ECOREGION  # needs work
# nochemsum <- nochem %>% 
#   group_by(OrgID, ReferenceSite, EcoRegion3) %>% 
#   summarise(n = n())
# 

#We are opting NOT to include chem data from nearby sites. (How close? up or downstream? land use changes? tribs? - hard to justify) SB/AT August 2025



####### NOT REALLY HELPFUL - DECIDE LATER IF KEEPING ##################
  #Plot individual stations
wt.30354<-subset(wt, wt$MLocID == '30354-ORDEQ')
wt.30354<-ggplot(wt.30354, aes(x = MLocID, y = Result_Numeric)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Site", y = "Water Temperature") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(wt.30354)


#   _   _                           _ 
#  | | | |                         | |
#  | |_| |__   ___    ___ _ __   __| |
#  | __| '_ \ / _ \  / _ \ '_ \ / _` |
#  | |_| | | |  __/ |  __/ | | | (_| |
#  \__|_| |_|\___|  \___|_| |_|\__,_|
                                   
                                   






#-----------------------------------------------------------------------------------------
#    ,-. ,-. ,-. .  , ,-. . . ,-. ,-. ,-| 
#    | | |   ,-| | /  |-' | | ,-| |   | | 
#    `-| '   `-^ `'   `-' `-| `-^ '   `-^ 
#     ,|                   /|             
#    `'                  `-'            

#-----------------------------------------------------------------------------------------------------------------------------------------------------
#REMOVE AMBIENT STATIONS FROM FURTHER ANALYSIS --- this happens later in process now.
#1: IMPORT CURRENT AMBIENT STATIONS LIST (UP-TO-DATE AS OF MAY 2022, N=161) ***TO DO: Import Ambient site list directly from AWQMS to ensure up-to-date
#amb <- read_excel("Benchmarks/Water Chemistry/Ambient_Stations_List.xlsx")
#write code to pull in stations for Ambient project <- AWQMS_Projects

#2: REMOVE AMBIENT STATIONS FROM ONE RULE ALL TABLE
one_rule_all <- one_rule_all[!(one_rule_all$MLocID %in% amb$MLocID),]
rm(amb)

#summarize by location and parameter --- OLD WAY
sum.loc <- chem.all %>%
  group_by(MLocID, Char_Name) %>%
  summarise(n.Samples = n(),
            minDate = min(SampleStartDate),
            maxDate = max(SampleStartDate))

#summarize by parameter and ecoregion
sum.eco <- chem.all %>%
  group_by(Char_Name, EcoRegion3) %>%
  summarise(n.Samples = n(),
            minDate = min(SampleStartDate),
            maxDate = max(SampleStartDate))


#write_xlsx(mlocid.chem, path = "//deqlab1/ATHOMPS/Files/Biomon/R Chem Benchmarks/MLocIDs_AWQMS.xlsx")


#testing summary pie charts - POTENTIALLY DELETE IF NOT HELPFUL
ggplot(tss, aes(x="", y = `n.Samples`, fill = `EcoRegion3`)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = n.Samples, label = EcoRegion3), color = "white", size=6) #+
#scale_fill_brewer(palette="Set1")


#CODE FOR DIAGNOSING UNIQUE MLOCIDS IN AWQMS DATA PULL
chemIDs <- distinct(data.frame(chem.all$MLocID))
write_xlsx(chemIDs, path = "//deqlab1/ATHOMPS/Files/Biomon/R Chem Benchmarks/ChemAll_MLocID.xlsx")

#another way of slicing out amb sites from oneruleall using dplyr anti join
anti_join(one_rule_all, amb, by = "MLocID")

#dplyr method for summarizing site numbers- DELETE?
ref %>%
  group_by(MLocID) %>% 
  summarise(length(unique(Char_Name)))

#alt system of identifying duplicates in one rule all. went with dplyr duplicates b/c was simpler
dups <- data.frame(table(one_rule_all$MLocID))
one_rule_all[one_rule_all$MLocID %in% dups$Var1[dups$Freq > 1],]

#??????????????????????????????
#TRY SH CODE INSTEAD OF ABOVE - adds 1301 records than what's in chem.all - likely sites that aren't in AWQMS
chem.oneruletest <- one_rule_all %>%
  left_join(chem.all, by = "MLocID")
#??????????????????????????????

###troubleshooting only
st10332 <- subset(chem.all, chem.all$MLocID == "10332-ORDEQ")
lady <- subset(chem.all, chem.all$MLocID == "35720-ORDEQ")
cond <- subset(lady, lady$Char_Name == 'Conductivity')

#Alternatively, enter the MLocIDs of the stations you wish to remove from the dataframe in R. -NO LONGER NEEDED B/C OF -USU SUFFIX
chem.all <-subset(chem.all, MLocID != "45" & MLocID != "1041") #add code as needed for additional stations (& MLocID != "#####")

#2: REMOVE AMBIENT STAITONS FOR WHICH WE HAVE NO BIOLOGICAL DATA
#a: First check that Ambient stations don't have bio data (Answer: none do).
amb <- subset(chem.onerule, chem.onerule$Project1 == 'Surface Water Ambient Monitoring') #<-- insufficient b/c amb sites hide under other projects, may need to import entire site list
sort(unique(amb$Char_Name))
rm(amb)
#b: Then run subset to isolate Ambient sites.
noamb <- subset(chem.onerule, chem.onerule$Project1 != 'Surface Water Ambient Monitoring')

###summary<-sort(unique(c((ref$MLocID), (ref$Char_Name))))

#paramxsite<-melt(ref, id.vars = 'MLocID', 'Char_Name')[(value = list(unique(value)))]

#paramxsite<-ref %>%
pivot_longer(cols = -c(Char_Name, MLocID)) %>%
  group_by(Char_Name) %>%
  summarise(value = list(unique(value))) %>%
  unnest(value)


#paramxsite<-cbind(paramxsite, rowSums(paramxsite[ , 2:174]))

####

paramxsite2<-ref %>%
  group_by(Char_Name) %>%
  summarise(MLocID) %>%
  select(unique.x=Char_Name)


paramxsite<-unique(ref[ , c("Char_Name", "MLocID")]) 
paramxsite = paramxsite %>%
  separate_rows(MLocID, sep=',') %>%
  mutate(i=1) %>%
  spread(MLocID, i, fill=0)



distinct(ref, Char_Name, MLocID, keep_all = TRUE)

paramxsite2<-dcast(ref, Char_Name + MLocID ~ MLocID, fun.aggregate = list, value.var="MLocID")

paramxsite<-ref %>% group_by(MLocID, Char_Name) %>% tidyr::nest(data = c(MLocID))

group_by(LAT, LONG) %>% 
  summarise(value = list(unique(value)))

paramxsite <- ref %>%
  group_by(MLocID, Char_Name) %>%
  tidyr::nest(MLocID)
###
paramxsite<-lapply(split(ref$MLocID, ref$Char_Name), unique)
lapply(paramxsite, function(x) write.table(data.frame(x), 'test.csv', append= T, sep=',' ))

###
paramxsite<-as.data.frame(do.call(rbind, ref))

paramxsite<-ldply (paramxsite, data.frame)

paramxsite<-pivot_longer(paramxsite, -c(Char_Name), values_to = "MLocID", names_to = "Char_Name")

paramxsite<-do.call(rbind, lapply(-c(paramxsite, MLocID), as.data.frame))

#paramxsite< data.frame(matrix(unlist(paramxsite), ncol = max(lengths(paramxsite)), byrow = TRUE))


write_xlsx(paramxsite, path = "//deqlab1/ATHOMPS/Files/Biomon/R Chem Benchmarks/Params_By_Site.xlsx")
#write.xlsx(paramxsite, "//deqlab1/ATHOMPS/Files/Biomon/R Chem Benchmarks/Params_By_Site.xlsx", sheetName = "Rad", col.names = TRUE, row.names = TRUE)


# SYB removed the following from above 8/8/25

#IMPORT STATIONS FROM BIOMON REFERENCE SCREEN
# one_rule_all<-read_csv("Reference/one.table_rule.all.csv", show_col_types = FALSE) # delete

#TEST AND REMOVE DUPLICATE ENTRIES IN ONE RULE ALL TABLE
#-----------------------------------------------------------------------------------------------------------------------------------------------------
length(unique(one_rule_all$MLocID))
#Value in console should match number of observations in one_rule_all.
#If matching, assume no duplicates and proceed to 'stations.all' code.
#If not matching, run code below to reveal which records appear more than once.
one_rule_all[duplicated(one_rule_all), ]
#Ideally, resolve duplicate entries in the One Rule All table and restart script.
#Alternatively, remove the duplicate records from dataframe in R.
one_rule_all <- one_rule_all[!duplicated(one_rule_all), ]


#-----------------------------------------------------------------------------------------------------------------------------------------------------
#IDENTIFY MLOCIDS IN ONE RULE ALL THAT HAVE SAME MLOCID IN AWQMS BUT CORRESPOND TO A DIFFERENT STATION
#-----------------------------------------------------------------------------------------------------------------------------------------------------
mlocid.awqms <- unique(chem.all[c("MLocID", "StationDes")])
mlocid.onerule <- unique(one_rule_all[c("MLocID", "StationDes")])
wrongstations <- inner_join(mlocid.awqms, mlocid.onerule, by = "MLocID")
wrongstations$ToFix <- ifelse(wrongstations$StationDes.x == wrongstations$StationDes.y, "Match", "Fix") 
view(wrongstations)
#Sort by ToFix column
#StationDes.x = AWQMS / StationDes.y = OneRule
#Resolve records that say "Fix" and redo steps above. Ignore mismatches due to extra spaces or misspellings.

rm(mlocid.awqms)
rm(mlocid.onerule)
rm(wrongstations)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
#INCORPORATE REFERENCE STATUS FROM STATIONS DATABASE
#best to use reference status from official stations DB vs one-rule-all table to avoid issues
#-----------------------------------------------------------------------------------------------------------------------------------------------------
#1: GENERATE STATIONS LIST FROM WQDATA
stations.wq <- wqdata[c("MLocID", "Ref2020_FINAL")]

#2: IMPORT REFERENCE STATUS FROM STATIONS DB
stdb <- query_stations(mlocs = stations.wq)

#3: SUBSET STATIONS DB INFO TO PREPARE FOR JOIN
stdb2 <- subset(stdb, select = c(MLocID, ReferenceSite))

#4: JOIN STATIONS DB REFERENCE STATUS WITH WQDATA
wqdata <- inner_join(wqdata, stdb2, by = "MLocID")

#5: COMPARE REF STATUS FROM STATIONS DB AND ONE-RULE TABLE
refcomp <- subset(wqdata, select = c(MLocID, ReferenceSite, Ref2020_FINAL))
refcomp$Ref2020_FINAL[refcomp$Ref2020_FINAL == "NO"] <- "MODERATELY DISTURBED" #rename to sync nomenclature
refcomp$ToFix <- ifelse(refcomp$ReferenceSite == refcomp$Ref2020_FINAL, "Match", "BAD") #make new column to run comparison
unique(refcomp$ToFix) #all should say "Match"

rm(stations.wq)
rm(stdb)
rm(stdb2)

#!!!!!!!!!!!!!!<code>

s10355 <- subset(wqdata, wqdata$MLocID == "10355-ORDEQ") #will at harris 9dates <- unique(sort(wqdata$SampleStartDate))
s12265 <- subset(wqdata, wqdata$MLocID == "12265-ORDEQ") 
dbcond <- subset(s12265, s12265$Char_Name == "Turbidity")
dbnick <- subset(s12265, s12265$Char_Name == "Nickel")

dates <- wqdata[c("MLocID", "SampleStartDate")]
datesuniq <- unique(sort(dates$SampleStartDate ~ "MLocID"))
library(openxlsx)
write.xlsx(datesuniq, file = "//deqlab1/ATHOMPS/Files/Biomon/R Chem Benchmarks/SamplingDates.xlsx")

#!!!!!!!!!!!!!!<code>
# #Snippet from Lesley. Filter out bug samples based on Bio_Intent column.
# chem.no_bio <- chem.all_ref %>% 
#   filter(!Bio_Intent %in% c("Population Census","Species Density")) # 1327 unique MLocIDs with no bug data.
# 
# chem.bio_only <- chem.all_ref %>% 
#   filter(Bio_Intent %in% c("Population Census", "Species Density"))

