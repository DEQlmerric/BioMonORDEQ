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
library(leaflet.providers)
library(RColorBrewer)
library(lubridate)

# IMPORT STATIONS WITH A REFERENCE DESIGNATION FROM STATIONS DB (N = 2522 as of 8/8/25, 2520 10/1/25)
stations <- query_stations()
ref_stations <- stations %>% 
  filter(ReferenceSite %in% c("REFERENCE" , "MODERATELY DISTURBED", "MOST DISTURBED")) %>% 
  select(MLocID, COMID, ReferenceSite, QC_Comm, OrgID)
rm(stations)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
# RETRIEVE AWQMS DATA AT REF STATIONS
# NOTE: DATA PULL CAN TAKE ~5-10 MINUTES; MUST BE CONNECTED TO VPN
#-----------------------------------------------------------------------------------------------------------------------------------------------------
chem.all_ref <- AWQMS_Data(MLocID = ref_stations$MLocID)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
# MERGE CHEM AND REF STATIONS TABLE TO BRING IN REFERENCE STATUS AND COMID
#-----------------------------------------------------------------------------------------------------------------------------------------------------
chem.all_ref <- inner_join(chem.all_ref, ref_stations, by = "MLocID")

#-----------------------------------------------------------------------------------------------------------------------------------------------------
# REMOVE UNWANTED DATA FROM FURTHER ANALYSIS
#-----------------------------------------------------------------------------------------------------------------------------------------------------
chem.ref <- chem.all_ref %>% 
  filter(Result_status != 'Rejected' & Result_status != 'Preliminary') %>%
  filter(DQL != 'E') %>% 
  filter(SamplingMethod != 'Continuous Summary' | is.na(SamplingMethod)) %>% # Note: Sampling Method starts to be left as NA (as opposed to Grab) for some (but not all) samples from four projects 
                                                                             # (Statewide Toxics, Statewide Biomon, TMDL, and Volmon) starting in 06/2012 through present day.
                                                                             # Remaining methods are 'Grab' and 'Unknown'.
  filter(MonLocType == 'River/Stream') %>% 
  filter(SampleMedia == 'Water') %>% 
  filter(SampleSubmedia == 'Surface Water' | is.na(SampleSubmedia))  %>% 
  filter(OrgID != 'USU(NOSTORETID)') %>% # Utah State Univ sites - they don't have water chem data.
  filter(COMID != '-99999' | !is.na(COMID)) # drop -99999 COMIDs (no representative watershed) and blank COMIDs.
  
# Note: There are two stations with no COMID (21844-ORDEQ, 34849-ORDEQ) as of 10/2025.
  
# LEGACY AMBIENT STATIONS
  #1: SUBSET AMBIENT PROJECT DATA
amb <- subset(chem.ref, chem.ref$Project1 == "Surface Water Ambient Monitoring")

  #2: MAKE LIST OF AMBIENT STATIONS
amb.stations <- amb %>% 
  distinct(MLocID, .keep_all=TRUE) %>% 
  select(MLocID)

  #3: REMOVE AMBIENT STATIONS
chem.ref <- anti_join(chem.ref, amb.stations, by = "MLocID")

rm(amb)
rm(amb.stations)

# TRIM DATA TO LOW FLOW INDEX PERIOD (JUNE 1-OCTOBER 15) and >=1997 ONLY
#1: DATE TO DATE FORMAT
chem.ref$SampleStartDate <- ymd(chem.ref$SampleStartDate)

#2: FILTER OUT RECORDS WHERE SAMPLING DATE IS OUTSIDE OF JUNE 1 - OCTOBER 15 DATE RANGE
chem.ref <- chem.ref %>%
  filter((month(SampleStartDate) > 6 | (month(SampleStartDate) == 6 & day(SampleStartDate) >= 1)) &
      (month(SampleStartDate) < 10 | (month(SampleStartDate) == 10 & day(SampleStartDate) <= 15)))

# REMOVE DATA PRIOR TO 1997
chem.ref <- chem.ref %>% 
  filter(year(SampleStartDate) >= '1997')

# REMOVE FOUR YEARS/SITES WITH CONTINUOUS TEMPERATURE DATA (in AWQMS as SAMPLING METHOD = GRAB). 
# chem.ref <- chem.ref %>% 
#   filter(!(MLocID == '12054-ORDEQ' & year(SampleStartDate) == '1994')) %>%  # McCoy Creek Lower #2 (Transect 1) in 1994 (Historical LASAR data)
#   filter(!(MLocID == '12372-ORDEQ' & year(SampleStartDate) == '1994')) %>%  # Lookout Creek (Transect 6) in 1994 (Historical LASAR data)
#   filter(!(MLocID == '12060-ORDEQ' & year(SampleStartDate) == '1994')) %>%  # Dark Canyon Creek Upper (Transect 1) 1994 (Historical LASAR data)
#   filter(!(MLocID == '12057-ORDEQ' & year(SampleStartDate) == '1995')) # Meadow Creek Lower (Transect 1) in 1995 (Historical LASAR data)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
# MISC. DATA CLEAN-UP
#-----------------------------------------------------------------------------------------------------------------------------------------------------
# HARDCODE IN THE LEVEL 3 ECOREGIONS AS TEXT
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

# ECOREGIONS AS FACTORS (in geographic order)
chem.ref$L3Eco <- factor(chem.ref$L3Eco, levels = c("Coast Range", "Willamette Valley", "Klamath Mountains", "Cascades",
    "Eastern Cascades Slopes and Foothills", "Columbia Plateau", "Blue Mountains", "Northern Basin and Range", "Snake River Plain"))

# ADD LEVEL 2 ECOREGIONS 
chem.ref <- chem.ref %>% 
  mutate(L2Eco = case_when(
    L3Eco %in%  c("Willamette Valley", "Coast Range") ~ "Marine West Coast Forest",
    L3Eco %in% c("Columbia Plateau", "Northern Basin and Range", "Snake River Plain") ~ "Cold Deserts",
    L3Eco %in% c("Eastern Cascades Slopes and Foothills", "Blue Mountains", "Klamath Mountains", "Cascades") ~ "Western Cordillera"
  )) %>% 
  relocate(L2Eco, .before = L3Eco)

# ECOREGIONS AS FACTORS (in geographic order)
chem.ref$L2Eco <- factor(chem.ref$L2Eco, levels = c("Marine West Coast Forest", "Western Cordillera", "Cold Deserts"))

# REF STATUS AS FACTORS
chem.ref$ReferenceSite <- factor(chem.ref$ReferenceSite, levels = c("REFERENCE", "MODERATELY DISTURBED", "MOST DISTURBED"))

# ADD A DATETIME FIELD
chem.ref <- chem.ref %>% 
  mutate(dateTime = str_sub(paste0(SampleStartDate, " ", SampleStartTime), end = -9)) %>%  # take 0s off the end of the time
  relocate(dateTime, .after = SampleStartDate)

# POPULATE NEW NUMERIC RESULT COLUMN TO ACCOUNT FOR NON-DETECTS AND EXCEEDANCES
# Value for non-detect calculated as 1/2 MDL.
chem.ref <- chem.ref %>% 
  mutate(Result_Numeric_mod = ifelse(Result_Text == 'ND', 
                                     MDLValue * 0.5, # Non-detects based on result text column
                                     ifelse(Result_Operator == '<', Result_Numeric * 0.5, # Non-detects based on "<" prefix
                                            Result_Numeric))) %>%  # Otherwise provide value for standard result types 
  relocate(Result_Numeric_mod, .after = Result_Numeric)
# Note 10/23: need to add code for exceedences. There are only 2 in the dataset (both Turbidity) so let's worry about this later.  -SB

# IF WATER TEMP WAS REPORTED IN DEG F (why??), CONVERT TO DEG C
chem.ref <- chem.ref %>% 
  mutate(Result_Numeric_mod = ifelse(Result_Unit == 'deg F', (Result_Numeric_mod - 32) * (5/9), Result_Numeric_mod)) %>% 
  mutate(Result_Unit = ifelse(Result_Unit == 'deg F', 'deg C', Result_Unit))       

# GET RID OF SOME UNUSED COLUMNS FOR READABILITY. (Could get rid of more.)
chem.ref <- chem.ref %>% 
  select(!c(Project2, Project3, act_id,Activity_Type, SampleStartTZ:chr_uid, Result_Text, URLType:URLUnit, Sample_Fraction:Result_UID, 
            Unit_UID:Analytical_Lab, WQX_submit_date:res_last_change_date))

# NARROW TABLE TO CHARS OF INTEREST ONLY (From StressorID Team: Temp, pH, DO, TP, TN, TSS, NH3) # AT, SH = do we want others included?
study_chars = c("Temperature, water", "pH", "Dissolved oxygen (DO)", "Dissolved oxygen saturation", "Total Phosphorus, mixed forms", 
                "Nitrogen", "Nitrate + Nitrite", "Total Kjeldahl nitrogen", "Total suspended solids", "Ammonia", "Turbidity Field", "Total solids", 
                "Conductivity", "Sulfate", "Chloride", "Specific conductance", "Orthophosphate", "Turbidity")

chem.ref.wq <- chem.ref %>% 
  filter(Char_Name %in% study_chars) %>% 
  filter(!(Char_Name=='Total Phosphorus, mixed forms' & Char_Speciation=='as P')) # Excludes 1999 data with undefined method speciation 
#--------------------------------------------------------------------------------------------------------------------
# CALCULATE TOTAL NITROGEN. TN = TKN + Nitrate + Nitrite
#-----------------------------------------------------------------------------------------------------------------------------------------------------
# nits
tn.nits<-subset(chem.ref.wq, chem.ref.wq$`Char_Name`=="Nitrate + Nitrite")
# tkn
tn.tkn<-subset(chem.ref.wq, chem.ref.wq$`Char_Name`=="Total Kjeldahl nitrogen")
# merge
tn.nits.tkn<-rbind(tn.nits, tn.tkn)

# ONLY INCLUDE SAMPLES THAT HAVE BOTH TKN AND NITRATE + NITRITE. Keep nits and tkn samples where
#  each appears once per sampling event per site.
tn1 <- tn.nits.tkn %>% 
  group_by(MLocID, dateTime) %>%   
  filter(any(Char_Name == "Nitrate + Nitrite") & any(Char_Name == "Total Kjeldahl nitrogen") 
         & sum(Char_Name == "Nitrate + Nitrite") == 1 & sum(Char_Name == "Total Kjeldahl nitrogen") == 1
  ) %>%
  ungroup() %>% 
  arrange(SampleStartTime, MLocID)

# Four sites have 2 TKN but 1 Nits as of 09/2025.  "37434-ORDEQ" "24566-ORDEQ" "37422-ORDEQ" "37424-ORDEQ"
# Remove one of those rows (if duplicate). Check to make sure there is one row TKN and one row Nits per sample.
# Check beforehand if both TKN results are the same, will need to decide which to keep otherwise.
tn2 <- tn.nits.tkn %>% 
  group_by(MLocID, dateTime) %>%   
  filter(any(Char_Name == "Nitrate + Nitrite") & any(Char_Name == "Total Kjeldahl nitrogen") 
         & sum(Char_Name == "Nitrate + Nitrite") == 1 & sum(Char_Name == "Total Kjeldahl nitrogen") == 2
  ) %>%
  ungroup() %>% 
  arrange(SampleStartDate, MLocID) %>% 
  distinct(Char_Name, SampleStartDate, .keep_all=TRUE)

# Sum rows to calculate TN. 
TN <- rbind(tn1, tn2) %>% 
  group_by(MLocID, dateTime) %>% 
  mutate(TN = sum(Result_Numeric_mod)) %>% 
  relocate(TN, .after= Result_Numeric_mod) %>% 
  distinct(MLocID, SampleStartDate, TN, .keep_all=TRUE) %>% #Removes one of the duplicate rows. By default removes TKN.
# Need to put the table back together so it looks like chem.ref.wq
  mutate(Result_Numeric_mod = TN) %>%  # Know that other columns (method, result_text, etc.) will only
                                       # reflect the methods for nitrate + nitrate (not TKN). If you need to look up
                                       # lab info for TKN you can use the act_id in chem.ref.
  mutate(Char_Name = "Nitrogen") %>%   # SH, AT = Should we add a column differentiating the calculated TN from the Char_Name = "Nitrogen" samples?
                                          # Right now they can't be differentiated (Char_Name = "Nitrogen" looks the same as this calculated TN).
                                          # But we could always go back to this point in the script if needed.
  select(-TN)

# Remove nits and tkn rows from chem.ref.wq and replace with TN.
chem.ref.wq <- rbind(TN, chem.ref.wq) %>% 
  filter(!Char_Name %in% c("Nitrate + Nitrite", "Total Kjeldahl nitrogen")) %>% 
  arrange(SampleStartDate)

# Clear out intermediaries
rm(list = c('TN', 'tn.nits', 'tn.nits.tkn', 'tn.tkn', 'tn1', 'tn2'))

#write_xlsx(chem.ref.wq, path = paste0("C://Users//sberzin//OneDrive - Oregon//Desktop//chem_ref_wq_", Sys.Date(), ".xlsx"))

#-----------------------------------------------------------------------------------------------------------------------------------------------------
# RANDOMLY SELECT ONE SITE FROM SITES THAT WERE SAMPLED MORE THAN ONCE, &
# FROM SITES THAT SHARE A STREAM SEGMENT.  
#-----------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(42) # If you run random functions (sample_n() in this case), this will give the same result on subsequent runs.

#1 Randomly select one sample for sites that were sampled more than once per char.
samp_mult_dates <- chem.ref.wq %>% 
  group_by(MLocID, Char_Name) %>% 
  filter(n()>1) %>% 
  sample_n(1) # randomly choose one sampling date  For most recent, try slice(which.max(SampleStartDate))

  # List sites that were sampled only one time per char.
samp_single_dates <- chem.ref.wq %>% 
  group_by(MLocID, Char_Name) %>% 
  filter(n()==1)

  # Join previous two tables together
samp_dates <- rbind(samp_single_dates,samp_mult_dates)

#2  ELIMINATE SITES ON THE SAME STREAM SEGMENT 
  # Randomly select one site for sites that share a Reachcode.
reach_mult <- samp_dates %>%
  group_by(Reachcode, Char_Name) %>%
  filter(n()>1) %>% 
  sample_n(1)

  # List sites that have only one sample per stream segment.
reach_single <- samp_dates %>%
  group_by(Reachcode, Char_Name) %>%
  filter(n()==1)

  # Join the previous two tables together.
cal.val_chem.ref.wq <- rbind(reach_mult, reach_single)

#3:  COMID CHECK
# There are some sites where there is more than one COMID per reachcode.  We should randomly select
# one of those sites to keep in the dataset (so we don't have different water chem results for the same set of predictors.)
# HOWEVER there are some misidentified COMIDs in the dataset, so these will need to be screened for each parameter. 

# Below are some manual edits, vetted FOR TSS ONLY.  Lackeys will need to revisit this for other parameters.
# Delete this once Dan updates Stations.
cal.val_chem.ref.wq <- cal.val_chem.ref.wq %>% 
  mutate(COMID = ifelse(MLocID == '12868-ORDEQ', -99999, COMID)) %>% # A-3 Channel.  Still need to ask Dan to update. Changed from 23764745.
  filter(COMID != '-99999') %>% # Drop this new one since we don't want -99999s.
  mutate(COMID = ifelse(MLocID == '33518-ORDEQ', 23815014, COMID)) %>%  # Miller Creek.  Still need to ask Dan to update.  Changed from 23815386.
  mutate(COMID = ifelse(MLocID== '16999-ORDEQ',24515990, COMID )) # Fox Creek at FSR.  Still need to ask Dan to update. Changed from 24516234.

# List sites that have more than one COMID.  Randomly sample 1 site to keep.
mult.comids <- cal.val_chem.ref.wq %>% 
  group_by(Char_Name, COMID) %>% 
  filter(n() > 1) %>% 
  sample_n(1) #%>% 
  #filter(Char_Name == 'Total suspended solids') # Look at this per each char and double-check COMIDs make sense.

# List all sites with just 1 COMID.
single.comids <- cal.val_chem.ref.wq %>% 
  group_by(Char_Name, COMID) %>% 
  filter(n() == 1)

# Join the previous two tables together.
cal.val_chem.ref.wq <- rbind(mult.comids, single.comids)

# TSS save
#write_xlsx(cal.val_chem.ref.wq[cal.val_chem.ref.wq$Char_Name == 'Total suspended solids',], path = paste0("C://Users//sberzin//OneDrive - Oregon//Desktop//tss", Sys.Date(), ".xlsx"))

# Clear out intermediaries
rm(list = c('samp_mult_dates', 'samp_single_dates', 'samp_dates', 'reach_mult', 'reach_single', 'mult.comids', 'single.comids'))
#-----------------------------------------------------------------------------------------------------------------------------------------------------
# CREATE CAL AND VAL DATASETS 
#-----------------------------------------------------------------------------------------------------------------------------------------------------
# FOR EACH CHAR
# Calculate sample quantiles for each char by L3 Ecoregion.
cal.val_chem.ref.wq <- cal.val_chem.ref.wq %>% 
  group_by(Char_Name, L3Eco) %>% 
  mutate(
    quantile_category = case_when(
      Result_Numeric_mod < quantile(Result_Numeric_mod, probs = 0.25) ~ "Q1",
      Result_Numeric_mod >= quantile(Result_Numeric_mod, probs = 0.25) & Result_Numeric_mod < quantile(Result_Numeric_mod, probs = 0.50) ~ "Q2",
      Result_Numeric_mod >= quantile(Result_Numeric_mod, probs = 0.50) & Result_Numeric_mod < quantile(Result_Numeric_mod, probs = 0.75) ~ "Q3",
      Result_Numeric_mod >= quantile(Result_Numeric_mod, probs = 0.75) ~ "Q4")) %>% 
  mutate(cal_val_group = paste0(quantile_category, "_", L3Eco, "_", Char_Name))

set.seed(42) # If you run random functions (sample_n() in this case), this will give the same result on subsequent runs.
# Randomly take half of samples and create CAL group.
# Note:  Group sizes are not exactly equal. slice_sample uses 'floor' to round, which means that When specifying the proportion of rows to include 
#   non-integer sizes are rounded down, so group a gets 0 rows.  In this case VAL has more rows than CAL.    

cal_chem.ref.wq <- cal.val_chem.ref.wq %>% 
  group_by(cal_val_group) %>% 
  slice_sample(prop=0.8, replace = FALSE) %>% 
  ungroup()

# Take remaining samples and create VAL group  
val_chem.ref.wq <- setdiff(cal.val_chem.ref.wq, cal_chem.ref.wq)

#Label groups 
val_chem.ref.wq <- val_chem.ref.wq %>% 
  mutate(cal_val = 'VAL')
cal_chem.ref.wq <- cal_chem.ref.wq %>% 
  mutate(cal_val = 'CAL')

# Combine the previous two tables.
cal.val_chem.ref.wq <- rbind(cal_chem.ref.wq, val_chem.ref.wq)

tss <- cal.val_chem.ref.wq %>%
  filter(Char_Name == 'Total suspended solids') %>% 
  select(c(MLocID, StationDes, Lat_DD, Long_DD, Result_Numeric_mod, L2Eco, L3Eco, EcoRegion4, HUC8, ReferenceSite, cal_val, COMID)) %>%
  mutate(TSS = Result_Numeric_mod) %>% 
  select(-Result_Numeric_mod)

write_xlsx(tss, path = paste0("C://Users//sberzin//OneDrive - Oregon//Desktop//TSS_", Sys.Date(), ".xlsx"))
#write_xlsx(tss, path = paste0("C://Users//athomps//OneDrive - Oregon//Desktop//TSS_", Sys.Date(), ".xlsx")) #temporary path so can include Ambient sites
#-----------------------------------------------------------------------------------------------------------------------------------------------------
# SUMMARY TABLES, FIGURES
#-----------------------------------------------------------------------------------------------------------------------------------------------------
#1: BY PARAMETER

sum.param <- cal.val_chem.ref.wq %>%
  group_by(Char_Name, cal_val) %>%
  summarise(n.Samples = n(),
            minDate = min(SampleStartDate),
            maxDate = max(SampleStartDate))

view(sum.param)

#2: BY PARAMETER, REFERENCE STATUS
sum.ref <- cal.val_chem.ref.wq %>%
  group_by(Char_Name, ReferenceSite, cal_val) %>%
  summarise(n.Samples = n(),
            minDate = min(SampleStartDate),
            maxDate = max(SampleStartDate))
view(sum.ref)

ggplot(sum.ref, aes(x = ReferenceSite, y = n.Samples, fill = cal_val)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + facet_wrap(~Char_Name)

  #3: BY PARAMETER, REFERENCE STATUS, AND ECOREGION LEVEL 3
sum.eco <- cal.val_chem.ref.wq %>%
  group_by(L3Eco, ReferenceSite, Char_Name, cal_val) %>%
  summarise(n.Samples = n(),
            minDate = min(SampleStartDate),
            maxDate = max(SampleStartDate))

view(sum.eco)

ggplot(sum.eco, aes(x = ReferenceSite, y = n.Samples, fill = cal_val)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + facet_wrap(~L3Eco)

  #4: BY YEAR AND PARAMETER
sum.year <- cal.val_chem.ref.wq %>% 
  group_by(Year = (year(SampleStartDate)), Char_Name) %>% 
  summarise(n.Samples = n())
view(sum.year)

ggplot(sum.year, aes(x = Year, y = n.Samples, fill = Char_Name)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlim(1995, 2025) 

#5: HOW MANY PARAMS PER SITE?
sum.chars <- cal.val_chem.ref.wq %>% 
  group_by(MLocID) %>% 
  summarise(n.chars = n_distinct(Char_Name))
view(sum.chars)

#6: SUMMARY STATS BY ECOREGION
sum.stats <- cal.val_chem.ref.wq %>% 
  group_by(Char_Name, L3Eco, cal_val) %>% 
  summarize(n.Samples = n(),
            mean = mean(Result_Numeric_mod),
            median = median(Result_Numeric_mod),
            min = min(Result_Numeric_mod),
            max = max(Result_Numeric_mod), 
            sd = sd(Result_Numeric_mod))

view(sum.stats)

#7: SUMMARY STATS BY ECOREGION AND REF STATUS
sum.stats.ref <- cal.val_chem.ref.wq %>% 
  group_by(L3Eco, ReferenceSite, Char_Name, cal_val) %>% 
  summarize(mean = mean(Result_Numeric_mod),
            median = median(Result_Numeric_mod),
            min = min(Result_Numeric_mod),
            max = max(Result_Numeric_mod), 
            sd = sd(Result_Numeric_mod),
            count = n())
view(sum.stats.ref)

# TSS only
ggplot(cal.val_chem.ref.wq[cal.val_chem.ref.wq$Char_Name == 'Total suspended solids',], aes(x = Result_Numeric_mod, fill = cal_val)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 10) + facet_wrap(~L3Eco) + scale_x_log10() +
  labs(x = "TSS mg/L (log scale x)", y = "Count")

# MAP

# Single dot for each reference site with water chemistry data (site may have been sampled multiple times):
labs <- c("Reference", "Moderately disturbed", "Most disturbed")
gry <- c("#1d9633", "#ff9100", "#e00707")
refpal <- colorFactor(gry, domain = cal.val_chem.ref.wq$ReferenceSite)

refmap <- leaflet(data = cal.val_chem.ref.wq) %>%
  addProviderTiles("OpenStreetMap", group = "Basic Map") %>% 
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain") %>%
  addLayersControl(
    baseGroups = c("Basic Map", "Terrain"),
    overlayGroups = c("REFERENCE", "MODERATELY DISTURBED", "MOST DISTURBED"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  setView(lng = -120.5583, lat =44.0671, zoom =6.4) %>%
  addCircleMarkers(lng = ~Long_DD, lat = ~Lat_DD, group = ~ReferenceSite, fillColor = refpal(cal.val_chem.ref.wq$ReferenceSite), stroke = TRUE,
                   color = "#000", weight = 0.5, fillOpacity = 2, radius = 3, 
                   popup = paste0("<strong>", "MLocID: ","</strong>", cal.val_chem.ref.wq$MLocID, "<br>",
                                  "<strong>", "Station Description: ", "</strong>", cal.val_chem.ref.wq$StationDes, "<br>",
                                  "<strong>", "Level 3 Ecoregion: ", "</strong>", cal.val_chem.ref.wq$L3Eco, "<br>",
                                  "<strong>", "Reference Status: ", "</strong>", cal.val_chem.ref.wq$ReferenceSite, "<br>")) %>% 
  addLegend(colors = gry, labels = labs,position = "bottomright",
            title = paste0("Sites with WQ & a reference designation<br>N = ",length(unique(cal.val_chem.ref.wq$MLocID)), " unique MLocIDs"), opacity = 1)
refmap # Print map

#-----------------------------------------------------------------------------------------------------------------------------------------------------
# PLOTS AND MAPS BY INDIVIDUAL CHARS:
# BOX PLOT FUNCTION
boxp <- function(data, x, y, fill) {
  ggplot(data, aes(x = {{x}},y = {{y}}, fill = {{fill}})) +
    geom_boxplot(outlier.size = 0.9) +
    facet_grid(~L3Eco, labeller = label_wrap_gen(width = 18)) +
    labs(x = "Reference Status", y = c(paste(data$Char_Name, "(",data$Result_Unit,")"))) + #automatic axis labeling
    #geom_hline(yintercept = data$MRLValue, linetype = "dashed", color = "red") + #MRL horizontal reference line
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
}

#MAP RESULTS FUNCTION
map_results <- function(data) {
  leaflet() %>%
  addProviderTiles("OpenStreetMap", group = "Basic Map") %>% 
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain") %>%
  setView(lng = -120.5583, lat =44.0671, zoom =6.4) %>%
  addCircleMarkers(data = data, group = ~ReferenceSite, lng = ~(jitter(data$Long_DD, factor = 0.5)), 
                     lat = ~jitter(data$Lat_DD, 0.5), fillColor = pal(data$Result_Numeric_mod), stroke = TRUE,
                   color = "#000", weight = 0.5, fillOpacity = 2, radius = 3, 
                   popup = paste0("<strong>", "MLocID: ","</strong>", data$MLocID, "<br>",
                                  "<strong>", "Station Description: ", "</strong>", data$StationDes, "<br>",
                                  "<strong>", "Project: ", "</strong>", data$Project1, "<br>",
                                  "<strong>", "Level 3 Ecoregion: ", "</strong>", data$L3Eco, "<br>",
                                  "<strong>", "Reference Status: ", "</strong>", data$ReferenceSite, "<br>",
                                  "<strong>", data$Char_Name, ": ", "</strong>", data$Result_Numeric_mod, " ", data$Result_Unit))  %>% 
    addLayersControl(
      baseGroups = c("Basic Map", "Terrain"),
      overlayGroups = c("REFERENCE", "MODERATELY DISTURBED", "MOST DISTURBED"),
      options = layersControlOptions(collapsed = FALSE)) %>% 
    addLegend(pal = pal,values = data$Result_Numeric_mod[data$Result_Numeric_mod <= (quantile(data$Result_Numeric_mod, 0.75) + 1.5 * 
      IQR(data$Result_Numeric_mod))], position = "bottomright" ,title = paste0(data$Char_Name[1], " (" , data$Result_Unit[1], ")"))
}

# Note for maps:  Grey circle = high outlier (beyond Q3 + 1.5IQR).
#-----------------------------------------------------------------------------------------------------------------------------------------------------
# AMMONIA (Plot = y axis log scaled.  Map = High outliers plot as grey circles.)
NH3 <- subset(cal.val_chem.ref.wq, cal.val_chem.ref.wq$Char_Name == "Ammonia")
boxp(NH3, ReferenceSite, Result_Numeric_mod, cal_val) +scale_y_log10() +labs(subtitle = '* log scale y-axis')

pal <- colorNumeric(palette = "Oranges",
  domain = c(0, (quantile(NH3$Result_Numeric_mod, 0.75)) + 1.5 * IQR(NH3$Result_Numeric_mod)))
map_results(NH3) 

# CHLORIDE (Plot = y axis log scaled.  Map = High outliers plot as grey circles.)
chlor <- subset(cal.val_chem.ref.wq, cal.val_chem.ref.wq$Char_Name == "Chloride")
boxp(chlor, ReferenceSite, Result_Numeric_mod, cal_val) +scale_y_log10() +labs(subtitle = '* log scale y-axis')

pal <- colorNumeric(palette = "PuBu",
  domain = c(0, (quantile(chlor$Result_Numeric_mod, 0.75)) + 1.5 * IQR(chlor$Result_Numeric_mod)))
map_results(chlor) 

# CONDUCTIVITY (Plot = y axis log scaled.  Map = High outliers plot as grey circles.)
cond <- subset(cal.val_chem.ref.wq, cal.val_chem.ref.wq$Char_Name == "Conductivity")
boxp(cond, ReferenceSite, Result_Numeric_mod, cal_val) +scale_y_log10() +labs(subtitle = '* log scale y-axis')

pal <- colorNumeric(palette = "Greens", 
  domain = c(0, (quantile(cond$Result_Numeric_mod, 0.75)) + 1.5 * IQR(cond$Result_Numeric_mod)))
map_results(cond) 

# DO mg
DO_mg <- subset(cal.val_chem.ref.wq, cal.val_chem.ref.wq$Char_Name == "Dissolved oxygen (DO)")
boxp(DO_mg, ReferenceSite, Result_Numeric_mod, cal_val) 

pal <- colorNumeric(palette = "Purples", domain = NULL) 
map_results(DO_mg) 

# DO percent
DO_sat <- subset(cal.val_chem.ref.wq, cal.val_chem.ref.wq$Char_Name == "Dissolved oxygen saturation")
boxp(DO_sat, ReferenceSite, Result_Numeric_mod, cal_val) 

pal <- colorNumeric(palette = "BuPu", domain =  NULL )
map_results(DO_sat) 

# SULFATE (Plot = y axis log scaled.  Map = High outliers plot as grey circles.)
SO4 <- subset(cal.val_chem.ref.wq, cal.val_chem.ref.wq$Char_Name == "Sulfate")
boxp(SO4, ReferenceSite, Result_Numeric_mod, cal_val) +scale_y_log10() +labs(subtitle = '* log scale y-axis')

pal <- colorNumeric(palette = "Oranges", 
  domain = c(0, (quantile(SO4$Result_Numeric_mod, 0.75)) + 1.5 * IQR(SO4$Result_Numeric_mod)))
map_results(SO4) 

# TOTAL NITROGEN (Plot = y axis log scaled.  Map = High outliers plot as grey circles.)
TN <- subset(cal.val_chem.ref.wq, cal.val_chem.ref.wq$Char_Name == "Nitrogen")
boxp(TN, ReferenceSite, Result_Numeric_mod, cal_val) +scale_y_log10() +labs(subtitle = '* log scale y-axis')

pal <- colorNumeric(palette = "Greens", domain = c(0, (quantile(TN$Result_Numeric_mod, 0.75)) + 1.5 * IQR(TN$Result_Numeric_mod)))
map_results(TN) 

# pH
pH <- subset(cal.val_chem.ref.wq, cal.val_chem.ref.wq$Char_Name == 'pH')
boxp(pH, ReferenceSite, Result_Numeric_mod, cal_val)

pal <- colorNumeric(palette = "RdYlBu", domain = NULL)
map_results(pH) 

# TOTAL PHOSPHORUS (Plot = y axis log scaled.  Map = High outliers plot as grey circles.)
TP <- subset(cal.val_chem.ref.wq, cal.val_chem.ref.wq$Char_Name == "Total Phosphorus, mixed forms")
boxp(TP, ReferenceSite, Result_Numeric_mod, cal_val) +scale_y_log10() +labs(subtitle = '* log scale y-axis')

pal <- colorNumeric(palette = "Reds", domain = c(0, (quantile(TP$Result_Numeric_mod, 0.75)) + 1.5 * IQR(TP$Result_Numeric_mod)))
map_results(TP) 

# TEMPERATURE
Temp <- subset(cal.val_chem.ref.wq, cal.val_chem.ref.wq$Char_Name == "Temperature, water")
boxp(Temp, ReferenceSite, Result_Numeric_mod, cal_val) 

pal <- colorNumeric(palette = "Blues", domain = NULL)
map_results(Temp) 

# TOTAL SOLIDS (Plot = y axis log scaled.  Map = High outliers plot as grey circles.)
ts <- subset(cal.val_chem.ref.wq, cal.val_chem.ref.wq$Char_Name == "Total solids")
boxp(ts, ReferenceSite, Result_Numeric_mod, cal_val) +scale_y_log10() +labs(subtitle = '* log scale y-axis')

pal <- colorNumeric(palette = "RdPu",
  domain = c(0, (quantile(ts$Result_Numeric_mod, 0.75)) + 1.5 * IQR(ts$Result_Numeric_mod)))
map_results(ts)

# TSS (Plot = y axis log scaled.  Map = High outliers plot as grey circles.)
tss <- subset(cal.val_chem.ref.wq, cal.val_chem.ref.wq$Char_Name == "Total suspended solids")
boxp(tss, ReferenceSite, Result_Numeric_mod, cal_val) +scale_y_log10() +labs(subtitle = '* log scale y-axis')

pal <- colorNumeric(palette = "PuRd",
  domain = c(0, (quantile(tss$Result_Numeric_mod, 0.75)) + 1.5 * IQR(tss$Result_Numeric_mod)))
map_results(tss)

# TURBIDITY (field) (Plot = y axis log scaled.  Map = High outliers plot as grey circles.)
turb <- subset(cal.val_chem.ref.wq, cal.val_chem.ref.wq$Char_Name == "Turbidity Field")
boxp(turb, ReferenceSite, Result_Numeric_mod, cal_val) +scale_y_log10() +labs(subtitle = '* log scale y-axis')

pal <- colorNumeric(palette = "YlOrBr", 
  domain = c(0, (quantile(turb$Result_Numeric_mod, 0.75)) + 1.5 * IQR(turb$Result_Numeric_mod)))
map_results(turb) 


#   _   _                           _ 
#  | | | |                         | |
#  | |_| |__   ___    ___ _ __   __| |
#  | __| '_ \ / _ \  / _ \ '_ \ / _` |
#  | |_| | | |  __/ |  __/ | | | (_| |
#  \__|_| |_|\___|  \___|_| |_|\__,_|
                                  

# #-----------------------------------------------------------------------------------------
# #    ,-. ,-. ,-. .  , ,-. . . ,-. ,-. ,-| 
# #    | | |   ,-| | /  |-' | | ,-| |   | | 
# #    `-| '   `-^ `'   `-' `-| `-^ '   `-^ 
# #     ,|                   /|             
# #    `'                  `-'            

# #!!!!!!!!!!!!!!<code>
# # #Snippet from Lesley. Filter out bug samples based on Bio_Intent column.
# # chem.no_bio <- chem.all_ref %>% 
# #   filter(!Bio_Intent %in% c("Population Census","Species Density")) # 1327 unique MLocIDs with no bug data.
# # 
# # chem.bio_only <- chem.all_ref %>% 
# #   filter(Bio_Intent %in% c("Population Census", "Species Density"))

# #-----------------------------------------------------------------------------------------------------------------------------------------------------
# #IDENTIFY STATIONS FROM OUR REFERENCE SCREENS THAT DON'T HAVE WATER CHEMISTRY DATA IN AWQMS
# #Reveals where additional data collections might be warranted
# #-----------------------------------------------------------------------------------------------------------------------------------------------------
# #1: CREATE TABLE CONTAINING SITES WITH NO CHEMISTRY DATA
# nochem <- anti_join(ref_stations, chem.all_ref, by = "MLocID") # All rows from ref_stations with no match in chem.no_bio
# 
# #2: OPTIONAL - WRITE TO EXCEL FOR FURTHER EXAMINATION
# #write_xlsx(nochem, path = "Benchmarks/Water Chemistry/SitesMissingChemData.xlsx")
# 
# #   #3: SUMMARIZE SITES BY OWNER, REF STATUS, AND ECOREGION  # needs work
# # nochemsum <- nochem %>% 
# #   group_by(OrgID, ReferenceSite, EcoRegion3) %>% 
# #   summarise(n = n())
# # 
# 
# #We are opting NOT to include chem data from nearby sites. (How close? up or downstream? land use changes? tribs? - hard to justify) SB/AT August 2025
# 
# 
#-----------------------------------------------------------------------------------------------------------------------------------------------------
# SUMMARY BOXPLOTS / MAPS
#-----------------------------------------------------------------------------------------------------------------------------------------------------
# Boxplot of each parameter by Ref Status and L3Ecoregion.
# Needs work, SYB to come back to this.
# chem.ref.wq %>% 
#   group_by(Char_Name, Result_Unit) %>% 
#   group_map(
#     .f = ~ ggplot(.x, aes(x = ReferenceSite, y = Result_Numeric_mod)) +
#       geom_boxplot() +
#       facet_grid(~L3Eco, scales = "free") +
#       labs(x = "Reference Status", y = paste0(.y$Char_Name, " (", .y$Result_Unit, ")")) +
#       theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
#   )


#1  ONLY SITES THAT HAVE BUG SAMPLES
# bugs_sites  <- chem.all_ref %>%
#   filter(Bio_Intent %in% c("Population Census", "Species Density")) %>%
#   select(MLocID)
# 
# bugs_only <- semi_join(chem.ref.wq, bugs_sites, by = 'MLocID')
# 
# chem.ref.wq <- bugs_only # Turn this back into chem.ref.wq so you can run everything below here.

# #-----------------------------------------------------------------------------------------------------------------------------------------------------
# # SUMMARY TABLES - for all samples, including dupe reachcode and n()>1
# #-----------------------------------------------------------------------------------------------------------------------------------------------------
# #SUMMARIZE PARAMETERS AND DATES FOR EACH STATION
# #Know for each site which parameters were collected and when (min/max date)
# #Don't want to develop a parameter benchmark if bad geographic coverage
# #Look in bug metrics code in BiomonR for examples of Tidy ways.
# 
# #1: BY PARAMETER
# sum.param <- chem.ref.wq %>%
#   group_by(Char_Name) %>%
#   summarise(n.Samples = n(),
#             minDate = min(SampleStartDate),
#             maxDate = max(SampleStartDate))
# 
# view(sum.param)
# 
# #2: BY SITE AND PARAMETER
# sum.site <- chem.ref.wq %>%
#   group_by(MLocID, StationDes, Char_Name, ReferenceSite) %>%
#   summarise(n.Samples = n(),
#             minDate = min(SampleStartDate),
#             maxDate = max(SampleStartDate))
# 
# #3: BY PARAMETER, REFERENCE STATUS
# sum.ref <- chem.ref.wq %>%
#   group_by(Char_Name, ReferenceSite) %>%
#   summarise(n.Samples = n(),
#             minDate = min(SampleStartDate),
#             maxDate = max(SampleStartDate))
# view(sum.ref)
# 
# ggplot(sum.ref, aes(fill = ReferenceSite, x = Char_Name, y = n.Samples)) +
#   geom_col(position = "dodge") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# 
# #4: BY PARAMETER, REFERENCE STATUS, AND ECOREGION LEVEL 3
# sum.eco <- chem.ref.wq %>%
#   group_by(L3Eco, ReferenceSite, Char_Name) %>%
#   summarise(n.Samples = n(),
#             minDate = min(SampleStartDate),
#             maxDate = max(SampleStartDate))
# 
# view(sum.eco)
# 
# #5: BY YEAR AND PARAMETER
# sum.year <- chem.ref.wq %>% 
#   group_by(Year = (year(SampleStartDate)), Char_Name) %>% 
#   summarise(n.Samples = n())
# view(sum.year)
# 
# ggplot(sum.year, aes(x = Year, y = n.Samples, fill = Char_Name)) +
#   geom_bar(position = "dodge", stat = "identity") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#   xlim(1990, 2025) # There are some before 1990, but only a handful and they make the plot unreadable.
# # Q: What happened to other sample types after 2010??
# 
# #6: HOW MANY TIMES WAS A SITE VISITED?
# sum.visits <- chem.ref.wq %>% 
#   group_by(MLocID) %>% 
#   summarise(num_visits = n_distinct(SampleStartDate))
# view(sum.visits)
# 
# #7: HOW MANY PARAMS PER SITE?
# sum.chars <- chem.ref.wq %>% 
#   group_by(MLocID) %>% 
#   summarise(n.chars = n_distinct(Char_Name))
# view(sum.chars)
# 
# #8: SUMMARY STATS BY ECOREGION
# sum.stats <- chem.ref.wq %>% 
#   group_by(L3Eco, Char_Name) %>% 
#   summarize(mean = mean(Result_Numeric_mod),
#             median = median(Result_Numeric_mod),
#             min = min(Result_Numeric_mod),
#             max = max(Result_Numeric_mod), 
#             sd = sd(Result_Numeric_mod),
#             count = n())
# view(sum.stats)
# 
# #9: SUMMARY STATS BY ECOREGION AND REF STATUS
# sum.stats.ref <- chem.ref.wq %>% 
#   group_by(L3Eco, ReferenceSite, Char_Name) %>% 
#   summarize(mean = mean(Result_Numeric_mod),
#             median = median(Result_Numeric_mod),
#             min = min(Result_Numeric_mod),
#             max = max(Result_Numeric_mod), 
#             sd = sd(Result_Numeric_mod),
#             count = n())
# view(sum.stats.ref)
# 
# # Export to Excel if wanting to explore more via data filters
# #write.xlsx(sum.eco, file = "Benchmarks/Water Chemistry/summary_by_param_eco.xlsx")
# 
# # MAP
# 
# # Single dot for each reference site with water chemistry data (site may have been sampled multiple times):
# labs <- c("Reference", "Moderately disturbed", "Most disturbed")
# gry <- c("#1d9633", "#ff9100", "#e00707")
# refpal <- colorFactor(gry, domain = chem.ref.wq$ReferenceSite)
# 
# refmap <- leaflet(data = chem.ref.wq) %>%
#   addProviderTiles("OpenStreetMap", group = "Basic Map") %>% 
#   addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain") %>%
#   addLayersControl(
#     baseGroups = c("Basic Map", "Terrain"),
#     options = layersControlOptions(collapsed = FALSE)) %>% 
#   setView(lng = -120.5583, lat =44.0671, zoom =6.4) %>%
#   addCircleMarkers(lng = ~Long_DD, lat = ~Lat_DD, fillColor = refpal(chem.ref.wq$ReferenceSite), stroke = TRUE,
#                    color = "#000", weight = 0.5, fillOpacity = 2, radius = 3, 
#                    popup = paste0("<strong>", "MLocID: ","</strong>", chem.ref.wq$MLocID, "<br>",
#                                   "<strong>", "Station Description: ", "</strong>", chem.ref.wq$StationDes, "<br>",
#                                   "<strong>", "Level 3 Ecoregion: ", "</strong>", chem.ref.wq$L3Eco, "<br>",
#                                   "<strong>", "Reference Status: ", "</strong>", chem.ref.wq$ReferenceSite, "<br>")) %>% 
#   addLegend(colors = gry, labels = labs,position = "bottomright",
#             title = paste0("Sites with WQ & a reference designation<br>N = ",length(unique(chem.ref.wq$MLocID)), " unique MLocIDs"), opacity = 1)
# refmap # Print map
# 
# #-----------------------------------------------------------------------------------------------------------------------------------------------------
# # PLOTS AND MAPS BY INDIVIDUAL CHARS:
# # BOX PLOT FUNCTION
# boxp <- function(data, x, y) {
#   ggplot(data, aes(x = {{x}},y = {{y}})) +
#     geom_boxplot(outlier.size = 0.9) +
#     facet_grid(~L3Eco, labeller = label_wrap_gen(width = 18)) +
#     labs(x = "Reference Status", y = c(paste(data$Char_Name, "(",data$Result_Unit,")"))) + #automatic axis labeling
#     #geom_hline(yintercept = data$MRLValue, linetype = "dashed", color = "red") + #MRL horizontal reference line
#     theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
# }
# 
# #MAP RESULTS FUNCTION
# map_results <- function(data) {
#   leaflet() %>%
#     addProviderTiles("OpenStreetMap", group = "Basic Map") %>% 
#     addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain") %>%
#     setView(lng = -120.5583, lat =44.0671, zoom =6.4) %>%
#     addCircleMarkers(data = data, group = ~ReferenceSite, lng = ~(jitter(data$Long_DD, factor = 0.5)), 
#                      lat = ~jitter(data$Lat_DD, 0.5), fillColor = pal(data$Result_Numeric_mod), stroke = TRUE,
#                      color = "#000", weight = 0.5, fillOpacity = 2, radius = 3, 
#                      popup = paste0("<strong>", "MLocID: ","</strong>", data$MLocID, "<br>",
#                                     "<strong>", "Station Description: ", "</strong>", data$StationDes, "<br>",
#                                     "<strong>", "Project: ", "</strong>", data$Project1, "<br>",
#                                     "<strong>", "Level 3 Ecoregion: ", "</strong>", data$L3Eco, "<br>",
#                                     "<strong>", "Reference Status: ", "</strong>", data$ReferenceSite, "<br>",
#                                     "<strong>", data$Char_Name, ": ", "</strong>", data$Result_Numeric_mod, " ", data$Result_Unit))  %>% 
#     addLayersControl(
#       baseGroups = c("Basic Map", "Terrain"),
#       overlayGroups = c("REFERENCE", "MODERATELY DISTURBED", "MOST DISTURBED"),
#       options = layersControlOptions(collapsed = FALSE)) %>% 
#     addLegend(pal = pal,values = data$Result_Numeric_mod[data$Result_Numeric_mod <= (quantile(data$Result_Numeric_mod, 0.75) + 1.5 * 
#                                                                                        IQR(data$Result_Numeric_mod))], position = "bottomright" ,title = paste0(data$Char_Name[1], " (" , data$Result_Unit[1], ")"))
# }
# 
# # Note for maps:  Grey circle = high outlier (beyond Q3 + 1.5IQR).
# #-----------------------------------------------------------------------------------------------------------------------------------------------------
# # AMMONIA (Plot = y axis log scaled.  Map = High outliers plot as grey circles.)
# NH3 <- subset(chem.ref.wq, chem.ref.wq$Char_Name == "Ammonia")
# boxp(NH3, ReferenceSite, Result_Numeric_mod) +scale_y_log10() +labs(subtitle = '* log scale y-axis')
# 
# pal <- colorNumeric(palette = "Oranges",
#                     domain = c(0, (quantile(NH3$Result_Numeric_mod, 0.75)) + 1.5 * IQR(NH3$Result_Numeric_mod)))
# map_results(NH3) 
# 
# # CHLORIDE (Plot = y axis log scaled.  Map = High outliers plot as grey circles.)
# chlor <- subset(chem.ref.wq, chem.ref.wq$Char_Name == "Chloride")
# boxp(chlor, ReferenceSite, Result_Numeric_mod) +scale_y_log10() +labs(subtitle = '* log scale y-axis')
# 
# pal <- colorNumeric(palette = "PuBu",
#                     domain = c(0, (quantile(chlor$Result_Numeric_mod, 0.75)) + 1.5 * IQR(chlor$Result_Numeric_mod)))
# map_results(chlor) 
# 
# # CONDUCTIVITY (Plot = y axis log scaled.  Map = High outliers plot as grey circles.)
# cond <- subset(chem.ref.wq, chem.ref.wq$Char_Name == "Conductivity")
# boxp(cond, ReferenceSite, Result_Numeric_mod) +scale_y_log10() +labs(subtitle = '* log scale y-axis')
# 
# pal <- colorNumeric(palette = "Greens", 
#                     domain = c(0, (quantile(cond$Result_Numeric_mod, 0.75)) + 1.5 * IQR(cond$Result_Numeric_mod)))
# map_results(cond) 
# 
# # DO mg
# DO_mg <- subset(chem.ref.wq, chem.ref.wq$Char_Name == "Dissolved oxygen (DO)")
# boxp(DO_mg, ReferenceSite, Result_Numeric_mod) 
# 
# pal <- colorNumeric(palette = "Purples", domain = NULL) 
# map_results(DO_mg) 
# 
# # DO percent
# DO_sat <- subset(chem.ref.wq, chem.ref.wq$Char_Name == "Dissolved oxygen saturation")
# boxp(DO_sat, ReferenceSite, Result_Numeric_mod) 
# 
# pal <- colorNumeric(palette = "BuPu", domain =  NULL )
# map_results(DO_sat) 
# 
# # SULFATE (Plot = y axis log scaled.  Map = High outliers plot as grey circles.)
# SO4 <- subset(chem.ref.wq, chem.ref.wq$Char_Name == "Sulfate")
# boxp(SO4, ReferenceSite, Result_Numeric_mod) +scale_y_log10() +labs(subtitle = '* log scale y-axis')
# 
# pal <- colorNumeric(palette = "Oranges", 
#                     domain = c(0, (quantile(SO4$Result_Numeric_mod, 0.75)) + 1.5 * IQR(SO4$Result_Numeric_mod)))
# map_results(SO4) 
# 
# # TOTAL NITROGEN (Plot = y axis log scaled.  Map = High outliers plot as grey circles.)
# TN <- subset(chem.ref.wq, chem.ref.wq$Char_Name == "Nitrogen")
# boxp(TN, ReferenceSite, Result_Numeric_mod) +scale_y_log10() +labs(subtitle = '* log scale y-axis')
# 
# pal <- colorNumeric(palette = "Greens", domain = c(0, (quantile(TN$Result_Numeric_mod, 0.75)) + 1.5 * IQR(TN$Result_Numeric_mod)))
# map_results(TN) 
# 
# # pH
# pH <- subset(chem.ref.wq, chem.ref.wq$Char_Name == 'pH')
# boxp(pH, ReferenceSite, Result_Numeric_mod)
# 
# pal <- colorNumeric(palette = "RdYlBu", domain = NULL)
# map_results(pH) 
# 
# # TOTAL PHOSPHORUS (Plot = y axis log scaled.  Map = High outliers plot as grey circles.)
# TP <- subset(chem.ref.wq, chem.ref.wq$Char_Name == "Total Phosphorus, mixed forms")
# boxp(TP, ReferenceSite, Result_Numeric_mod) +scale_y_log10() +labs(subtitle = '* log scale y-axis')
# 
# pal <- colorNumeric(palette = "Reds", domain = c(0, (quantile(TP$Result_Numeric_mod, 0.75)) + 1.5 * IQR(TP$Result_Numeric_mod)))
# map_results(TP) 
# 
# # TEMPERATURE
# Temp <- subset(chem.ref.wq, chem.ref.wq$Char_Name == "Temperature, water")
# boxp(Temp, ReferenceSite, Result_Numeric_mod) 
# 
# pal <- colorNumeric(palette = "Blues", domain = NULL)
# map_results(Temp) 
# 
# # TOTAL SOLIDS (Plot = y axis log scaled.  Map = High outliers plot as grey circles.)
# ts <- subset(chem.ref.wq, chem.ref.wq$Char_Name == "Total solids")
# boxp(ts, ReferenceSite, Result_Numeric_mod) +scale_y_log10() +labs(subtitle = '* log scale y-axis')
# 
# pal <- colorNumeric(palette = "RdPu",
#                     domain = c(0, (quantile(ts$Result_Numeric_mod, 0.75)) + 1.5 * IQR(ts$Result_Numeric_mod)))
# map_results(ts)
# 
# # TSS (Plot = y axis log scaled.  Map = High outliers plot as grey circles.)
# tss <- subset(chem.ref.wq, chem.ref.wq$Char_Name == "Total suspended solids")
# boxp(tss, ReferenceSite, Result_Numeric_mod) +scale_y_log10() +labs(subtitle = '* log scale y-axis')
# 
# pal <- colorNumeric(palette = "PuRd",
#                     domain = c(0, (quantile(tss$Result_Numeric_mod, 0.75)) + 1.5 * IQR(tss$Result_Numeric_mod)))
# map_results(tss)
# 
# # TURBIDITY (field) (Plot = y axis log scaled.  Map = High outliers plot as grey circles.)
# turb <- subset(chem.ref.wq, chem.ref.wq$Char_Name == "Turbidity Field")
# boxp(turb, ReferenceSite, Result_Numeric_mod) +scale_y_log10() +labs(subtitle = '* log scale y-axis')
# 
# pal <- colorNumeric(palette = "YlOrBr", 
#                     domain = c(0, (quantile(turb$Result_Numeric_mod, 0.75)) + 1.5 * IQR(turb$Result_Numeric_mod)))
# # map_results(turb) 



# Export to Excel if wanting to explore more via data filters
#write.xlsx(sum.eco, file = "Benchmarks/Water Chemistry/summary_by_param_eco.xlsx")
