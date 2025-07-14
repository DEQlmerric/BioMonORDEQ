<<<<<<< HEAD
  _______    ________  ______   ___ __ __   ______   ___   __      
/_______/\  /_______/\/_____/\ /__//_//_/\ /_____/\ /__/\ /__/\    
\::: _  \ \ \__.::._\/\:::_ \ \\::\| \| \ \\:::_ \ \\::\_\\  \ \   
\::(_)  \/_   \::\ \  \:\ \ \ \\:.      \ \\:\ \ \ \\:. `-\  \ \
\::  _  \ \  _\::\ \__\:\ \ \ \\:.\-/\  \ \\:\ \ \ \\:. _    \ \ 
\::(_)  \ \/__\::\__/\\:\_\ \ \\. \  \  \ \\:\_\ \ \\. \`-\  \ \
\_______\/\________\/ \_____\/ \__\/ \__\/ \_____\/ \__\/ \__\/
    ________     ________                                        ________                  _________
    ___  __ \_______  __/__________________________________      ___  __/________________________  /
    __  /_/ /  _ \_  /_ _  _ \_  ___/  _ \_  __ \  ___/  _ \     __  /  __  ___/  _ \_  __ \  __  / 
    _  _, _//  __/  __/ /  __/  /   /  __/  / / / /__ /  __/     _  /   _  /   /  __/  / / / /_/ /  
    /_/ |_| \___//_/    \___//_/    \___//_/ /_/\___/ \___/      /_/    /_/    \___//_/ /_/\__,_/   
    
           ,_   _, ___,_,       _,     ,_ ,    _, ,_  _, ___,___,  _, ,  ,              
           | \  |\  |  |\      /_  \_/ |_)|   / \ |_) |\  |   |   / \ |\ | 
          _|_/  |-\ |  |-\     \_  / \ |  |__ \_/ | \ |-\ |  _|_  \_/ | \| 
  #Created: AT, Dec 2019
  #Modified: AT, Dec 2022 - direct pull via AWQMS tool, graphing functions, condition classes
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

#LOAD PACKAGES
library(readxl)
library(ggplot2)
library(dplyr)
library(plyr)
library(stringr)
library(AWQMSdata)
#SH is radmobile dood like fo realz yo

# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
#  
#                                                                     PART I - IMPORT and FORMAT DATA
#                                                               (1) Chemistry, (2) Bugs, and (3) Habitat
#  
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
#>>>>>>>>>>>>>>>>>>>> ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      ___           #
     / __\    (1)   #                                                                                        
    / /             #                                                                                                    
   / /___           #
   \____/HEMISTRY   #
#>>>>>>>>>>>>>>>>>>>>
#CHEMISTRY: import/format====

#CREATE VECTOR OF OUR 12 REFERENCE TREND STATION IDS USED TO PULL DATA
stations <- c("26936-ORDEQ","35633-ORDEQ","35825-ORDEQ","35722-ORDEQ","32577-ORDEQ","23034-ORDEQ",
              "31495-ORDEQ","24419-ORDEQ","35720-ORDEQ","34636-ORDEQ","11850-ORDEQ","35794-ORDEQ")

#PULL TREND CHEMISTRY DATA FROM AWQMS (TYPICALLY TAKES 1-2 MINUTES)
chem <- AWQMS_Data(MLocID = stations)
rm(stations) 

#MAKE ABBREVIATED SITE NAMES
chem$`StationDes`[chem$`StationDes`=="Black Canyon Creek"] <- "Black Canyon"
chem$`StationDes`[chem$`StationDes`=="Booze Creek at River Mile 0.20"] <- "Booze"
chem$`StationDes`[chem$`StationDes`=="Bridge Creek at upper reference site"] <- "Bridge"
chem$`StationDes`[chem$`StationDes`=="ELK R NF AT RM 0.4"] <- "NF Elk"
chem$`StationDes`[chem$`StationDes`=="FISH LAKE CR AT RM 1.8"] <- "Fish Lake"
chem$`StationDes`[chem$`StationDes`=="Guano Creek, Hart Mountain"] <- "Guano"
chem$`StationDes`[chem$`StationDes`=="Kilchis R SF at RM 1.55"] <- "SF Kilchis"
chem$`StationDes`[chem$`StationDes`=="LADY CR AT RM 0.2"] <- "Lady"
chem$`StationDes`[chem$`StationDes`=="LITTLE MINAM R AT RM 1.3"] <- "Little Minam"
chem$`StationDes`[chem$`StationDes`=="Rock Creek at River Mile 1.5"] <- "Rock"
chem$`StationDes`[chem$`StationDes`=="West Fork Silver Creek, 25 minutes downstream from end of FSR 035 (Silver, Silver Lake)"] <- "WF Silver"
chem$`StationDes`[chem$`StationDes`=="Whychus CR NF AT MOUTH"] <- "NF Whychus"

#MAKE NEW COLUMN FOR YEAR
chem$Year<-substring(chem$`SampleStartDate`, 1, 4)

#ADD & POPULATE ECO3 COLUMN
chem$eco3<-ifelse(chem$`StationDes`=="NF Elk" | chem$`StationDes`=="Rock" | chem$`StationDes`=="SF Kilchis", "1",
                  ifelse(chem$`StationDes`=="Fish Lake" | chem$`StationDes`=="Lady", "4",
                         ifelse(chem$`StationDes`=="NF Whychus" | chem$`StationDes`=="WF Silver", "9",
                                ifelse(chem$`StationDes`=="Black Canyon" | chem$`StationDes`=="Little Minam", "11",
                                       ifelse(chem$`StationDes`=="Booze", "78",
                                              ifelse(chem$`StationDes`=="Bridge" | chem$`StationDes`=="Guano", "80", "ERROR"))))))
#CHECK FOR ECO3 ERRORS
unique(chem$eco3) #fix rows that say "ERROR"


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#CHECK FOR DUPLICATES -- how necessary is this? 
dups <- chem[duplicated(chem),] #should be 0 observations, if not follow up with AWQMS data stewards and rerun code
rm(dups)

#search through column positions
dups <- chem[duplicated(chem[c(3, 6, 8, 21, 23, 24, 30, 37)]),]

#setDT function
library(data.table)
dups <- unique(setDT(chem), by = c("SampleStartDate", "SampleStartTime", "MLocID", "act_id", "Result_Numeric"))

#dplyr filter
dups <- chem %>%
  filter(!duplicated(cbind(SampleStartDate, SampleStartTime, MLocID, SamplingMethod, Result_Numeric)))

#dplyr distinct
dups <- chem %>% 
  distinct(SampleStartDate, SampleStartTime, Result_Numeric, MLocID, SamplingMethod)#, .keep_all = TRUE)
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#REMOVE VOIDED & REJECTED RECORDS
chem <- subset(chem, chem$Result_Text != 'Void' & chem$Result_status != 'Rejected')

#POPULATE NEW NUMERIC RESULT COLUMN TO ACCOUNT FOR NON-DETECTS AND EXCEEDANCES
chem$'Result_Numeric_mod' <- ifelse(chem$Result_Text == 'ND', chem$'MDLValue' * 0.5, #non-detects based result text column
                                    ifelse(chem$Result_Operator == '<', chem$'Result_Numeric' * 0.5, #non-detects based on "<" prefix
                                    chem$Result_Numeric)) #otherwise provide value for standard result types
#>>>>>>>>>>>>>>
#exceed. placeholder
#chem$'exc' <- ifelse(grepl("^>", chem$Result_Text), "EXCEED", "...")
#>>>>>>>>>>>>>>


#READ REFERENCE BENCHMARKS FILE
 __ ___ _   _    
(_   | / \ |_) |  #To link reference benchmarks to Trend sites by Ecoregion 3, Eco3 number must be included in file below
__)  | \_/ |   o  #To determine Eco3 number, open ArcMap file "Trend_Ecoregions.mxd" in "Trend Sites/R/Ecoregion_GIS" folder

refbench <- read_excel('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/Reference_Benchmarks_modified.xlsx', sheet = "Benchmarks_reformatted2")
# ! # ! # ! # SH to verify the benchmarks file. LAM says to use other file w/ updated Willamette and new metrics # ! # ! # ! # 

#CHANGE NAME OF ECO3 AND PARAMETER COLUMNS IN REFBENCH
names(refbench)[names(refbench) == "CATEGORY"] <- "eco3"
names(refbench)[names(refbench) == "PARAMETER"] <- "Char_Name"

#CONVERT ECO3 COLUMN TO CHARACTER TO MATCH CHEM
refbench$eco3 <- as.character(refbench$eco3)

#MAKE CONDITION CLASS COLUMN IN CHEM FOR GOOD/FAIR/POOR BASED ON DEQ BENCHMARK VALUES
chem <- chem %>%
  left_join(refbench, by = c('eco3','Char_Name'))
chem <- chem %>%
  mutate(condclass = case_when(Result_Numeric_mod < GOOD_VAL ~ "Good",
                               Result_Numeric_mod > POOR_VAL ~ "Poor",
                               Result_Numeric_mod >= GOOD_VAL & Result_Numeric_mod <= POOR_VAL ~ "Fair",
                               TRUE ~ "NA"))

#SUBSET INDIVIDUAL WQ PARAMETERS FOR SUBSEQUENT EXPLORATIONS
# ~ ~ ~ GRAB WQ METER PARAMETERS
temp<-subset(chem, chem$`Char_Name`=='Temperature, water')
turb<-subset(chem, chem$`Char_Name`=='Turbidity')
cond<-subset(chem, chem$`Char_Name`=='Conductivity')
ph<-subset(chem, chem$`Char_Name`=='pH')
do.conc<-subset(chem, chem$`Char_Name`=='Dissolved oxygen (DO)')
do.sat<-subset(chem, chem$`Char_Name`=='Dissolved oxygen saturation')
afdm<-subset(chem, chem$`Char_Name` == 'Ash Free Dry Mass')
chla<-subset(chem, chem$'Char_Name' == 'Chlorophyll a')

# ~ ~ ~ LAB WQ PARAMETERS
tss<-subset(chem, chem$`Char_Name`=='Total suspended solids')
sulf<-subset(chem, chem$`Char_Name`=='Sulfate')
tp<-subset(chem, chem$`Char_Name`=='Total Phosphorus, mixed forms' & chem$`Char_Speciation`=='as P') #excludes 1999 data with undefined method speciation
  #TOTAL NITROGEN (requires extra steps to account for different methods)
tn.total<-subset(chem, chem$`Char_Name`=="Nitrogen") #subset results from new TN method
tn.nits.tkn<-subset(chem, chem$`Char_Name`=="Nitrate + Nitrite" | chem$`Char_Name`=="Total Kjeldahl nitrogen") #subset nits and TKN (pre-2018)


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#nits
tn.nits<-subset(chem, chem$`Char_Name`=="Nitrate + Nitrite")
#tkn
tn.tkn<-subset(chem, chem$`Char_Name`=="Total Kjeldahl nitrogen")
#merge
tn.nits.tkn<-merge(tn.nits, tn.tkn)
  
tn.nits.tkn %>% 
  group_by(SampleStartDate, StationDes, Year)
mutate(Result_Numeric_mod = sum(Result_Numeric_mod), 
       newresult = n_distinct(Result_Numeric_mod), .keep = 'all')



nitstkntest$Result_Numeric_mod2 <- aggregate(nitstkntest$Result_Numeric_mod, by = list(df$department, df$state), FUN=sum)

bind_rows(df1, df2) %>%
  group_by(ship_no) %>%
  summarise_all(funs(sum(., na.rm = TRUE)))

tn.nits.tkn<-aggregate(`Result_Numeric_mod`~ `SampleStartDate` + `StationDes` + Year + eco3, tn.nits.tkn, sum) #sum nits and TKN into single row when site & sample date match


#!!!!!!!!!!!!!!!!!!
#REWRITE SO IT TRANSFERS OVER METADATA FOR NITS AND TKN
test<-aggregate(`Result_Numeric_mod`~ ., tn.nits.tkn, sum)
#!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

tn<-rbind.fill(tn.nits.tkn, tn.total) #make new dataframe for total nitrogen results
rm(tn.total) #remove tn.total
rm(tn.nits.tkn) #remove tn.nits.tkn

#>>>>>>>>>>>>>>>>>>>> ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
     ___            #
    / __\     (2)   #
   /__\//           #
  / \/  \           #
  \_____/UGS        #
#>>>>>>>>>>>>>>>>>>>>
#BUGS: import/format====

#CREATE VECTOR OF OUR 12 REFERENCE TREND STATION IDS USED TO PULL DATA
stations <- c("26936-ORDEQ","35633-ORDEQ","35825-ORDEQ","35722-ORDEQ","32577-ORDEQ","23034-ORDEQ",
              "31495-ORDEQ","24419-ORDEQ","35720-ORDEQ","34636-ORDEQ","11850-ORDEQ","35794-ORDEQ")
     
#PULL BUG METRIC AND INDEX DATA FROM AWQMS
bugs.met <- AWQMS_Bio_Metrics(MLocID = stations)
bugs.ind <- AWQMS_Bio_Indexes(MLocID = stations)

#RENAME COLUMNS
names(bugs.met)[names(bugs.met) == "Metric_Name"] <- "Char_Name"
names(bugs.ind)[names(bugs.ind) == "Index_Name"] <- "Char_Name"
names(bugs.ind)[names(bugs.ind) == "Act_id"] <- "act_id"
names(bugs.ind)[names(bugs.ind) == "Comment"] <- "Comments"

#MERGE BUG METRIC AND INDEX DATA
bugs <- rbind.fill(bugs.met, bugs.ind)

#ADD NEW COLUMN FOR YEAR
bugs$Year <- substring(bugs$`Sample_Date`, 1, 4)

#MAKE ABBREVIATED SITE NAMES
bugs$StationDes[bugs$StationDes=="Black Canyon Cr (John Day)"] <- "Black Canyon"
bugs$StationDes[bugs$StationDes=="Black Canyon Creek"] <- "Black Canyon"
bugs$StationDes[bugs$StationDes=="Booze Cr_RM_0.20"] <- "Booze"
bugs$StationDes[bugs$StationDes=="Booze Creek RM 0.20"] <- "Booze"
bugs$StationDes[bugs$StationDes=="Booze Creek at River Mile 0.20"] <- "Booze"
bugs$StationDes[bugs$StationDes=="Bridge Creek at Upper Reference"] <- "Bridge"
bugs$StationDes[bugs$StationDes=="Bridge Creek at Upper reference site"] <- "Bridge"
bugs$StationDes[bugs$StationDes=="Bridge Creek at upper reference site"] <- "Bridge"
bugs$StationDes[bugs$StationDes=="ELK R NF AT RM 0.4"] <- "NF Elk"
bugs$StationDes[bugs$StationDes=="Elk River NF at RM 0.4"] <- "NF Elk"
bugs$StationDes[bugs$StationDes=="FISH LAKE CR AT RM 1.8"] <- "Fish Lake"
bugs$StationDes[bugs$StationDes=="Fish Lake Creek at RM 1.8"] <- "Fish Lake"
bugs$StationDes[bugs$StationDes=="Guano Creek (Hart Mountain)"] <- "Guano"
bugs$StationDes[bugs$StationDes=="Guano_Ck_Hart_Mtn"] <- "Guano"
bugs$StationDes[bugs$StationDes=="Guano Creek, Hart Mountain"] <- "Guano"
bugs$StationDes[bugs$StationDes=="Kilchis R SF at RM 1.55"] <- "SF Kilchis"
bugs$StationDes[bugs$StationDes=="Kilchis River SF at RM 1.55"] <- "SF Kilchis"
bugs$StationDes[bugs$StationDes=="LADY CR AT RM 0.2"] <- "Lady"
bugs$StationDes[bugs$StationDes=="Lady Creek at RM 0.2"] <- "Lady"
bugs$StationDes[bugs$StationDes=="Little Minam R at RM 1.3"] <- "Little Minam"
bugs$StationDes[bugs$StationDes=="LITTLE MINAM R AT RM 1.3"] <- "Little Minam"
bugs$StationDes[bugs$StationDes=="Rock Creek at RM 1.5 (Alsea)"] <- "Rock"
bugs$StationDes[bugs$StationDes=="Rock Creek at River Mile 1.5"] <- "Rock"
bugs$StationDes[bugs$StationDes=="WF Silver Creek 25 min D/S from end of USFS"] <- "WF Silver"
bugs$StationDes[bugs$StationDes=="WF Silver Creek 25 min D/S from end of USFS RD 035 (Silver, Silver Lake)"] <- "WF Silver"
bugs$StationDes[bugs$StationDes=="West Fork Silver Creek, 25 minutes downstream from end of FSR 035 (Silver, Silver Lake)"] <- "WF Silver"
bugs$StationDes[bugs$StationDes=="SQUAW CR NF AT MOUTH"] <- "NF Whychus"
bugs$StationDes[bugs$StationDes=="Whychus Creek NF at mouth"] <- "NF Whychus"
bugs$StationDes[bugs$StationDes=="Whychus CR NF AT MOUTH"] <- "NF Whychus"

#REMOVE 1992 DATA (UNREPRESENTATIVE METHODS)
bugs <- subset(bugs, bugs$Year!='1992')

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#REMOVE E LEVEL DATA AND POOL AND TRANSECT SAMPLE METHODS
bugstest <- subset(bugs, bugs$Sample_Method != "Benthic Kick - Targeted Pool" & bugs$Sample_Method != "Benthic Kick - Transect" & 
                     bugs$DQL != "E")



guanomix <- ifelse(bugs, bugs$Sample_Method == "Benthic Kick - Mixed habitat" | bugstest$MLocID == "31495-ORDEQ", "OK")


#long to wide
library(reshape2)
bugs_wide <- dcast(bugs, MLocID + Sample_Date ~ Char_Name, value.var = 'Score')



bugs_wide <- dcast(bugs, MLocID + Sample_Date ~ Char_Name, sum)

library(tidyr)
bugs <- spread(bugs, Char_Name, Score)


rm(stations)
rm(bugs.met)
rm(bugs.ind)
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#MAKE NEW COLUMN FOR O/E IRRESPECTIVE OF MODEL
OoverE_extract <- ifelse(grepl("COLD DESERTS",bugs$EcoRegion2), bugs$OoverE_null, bugs$OoverE)
bugs <- cbind(bugs, OoverE_extract)
rm(OoverE_extract)

#MAKE NEW COLUMNS FOR VOLTINISM RATIOS
voltratio.rich <- bugs$semivoltine_rich/bugs$multivoltine_rich
bugs <- cbind(bugs, voltratio.rich)
voltratio.pct <- bugs$pct_semivoltine/bugs$pct_multivoltine
bugs <- cbind(bugs, voltratio.pct)

#MAKE COLUMN NAMES UNIQUE TO AVOID GGPLOT PRINTING ERROR
colnames(bugs) <- make.unique(names(bugs))

#>>>>>>>>>>>>>>>>>>>> ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
                    #
     /\  /\     (3) #
    / /_/ /         #
   / __  /          #
   \/ |_/ABITAT     #
#>>>>>>>>>>>>>>>>>>>>
#HABITAT: import/format====

#  ON HOLD...
hab.input <- '//deqlab1/biomon/Projects/Biomon Redux/e-data/R/mets.cross_3.13.17.csv'
hab <- read.csv(hab.input)
rm(hab.input)

# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
#  
#                                                                         PART II - CALCULATE STATS
#                                                                 (1) Chemistry, (2) Bugs, and (3) Habitat
#  
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
#>>>>>>>>>>>>>>>>>>>> ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      ___           #
     / __\    (1)   #                                                                                               
    / /             #                                                                                                    
   / /___           #
   \____/HEMISTRY   #
#>>>>>>>>>>>>>>>>>>>>
#CHEMISTRY: stats====

#>>>>>>>>>>>>>>>>>>>>
#     BOX PLOTS     #
#>>>>>>>>>>>>>>>>>>>>

#RUN BOX PLOT FUNCTION
boxp <- function(data, x, y) {
  ggplot(data, aes({{x}}, {{y}})) +
    geom_boxplot(outlier.color = "red", outlier.shape = 8) + #red asterisk outliers
    labs(x = "Site", y = c(paste(data$Char_Name, "(",data$Result_Unit,")"))) + #automatic axis labeling
    geom_hline(yintercept = data$MRLValue, linetype = "dashed", color = "red") + #MRL horizontal reference line
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
}

#GRAB WQ PARAMETERS BOX PLOTS (adjust y axis range as needed)
boxp(turb, StationDes, Result_Numeric_mod) + coord_cartesian(ylim = c(0, 25))
boxp(cond, StationDes, Result_Numeric_mod)
boxp(do.sat, StationDes, Result_Numeric_mod) + coord_cartesian(ylim = c(50, 120))
boxp(do.conc, StationDes, Result_Numeric_mod) + coord_cartesian(ylim = c(4, 12))
boxp(ph, StationDes, Result_Numeric_mod) + coord_cartesian(ylim = c(6.5, 8.5))
boxp(temp, StationDes, Result_Numeric_mod) + coord_cartesian(ylim = c(0, 25))
boxp(afdm, StationDes, Result_Numeric_mod) #messy MRLs
boxp(chla, StationDes, Result_Numeric_mod) + coord_cartesian(ylim = c(0, 0.01))

#LAB WQ PARAMETERS BOX PLOTS (add chart modifiers as needed)
boxp(tss, StationDes, Result_Numeric_mod)
boxp(tp, StationDes, Result_Numeric_mod)
boxp(sulf, StationDes, Result_Numeric_mod)
boxp(tn, StationDes, Result_Numeric_mod) #fix char_name

#>>>>>>>>>>>>>>>>>>>>
#     BAR PLOTS     #
#>>>>>>>>>>>>>>>>>>>>

#RUN BAR PLOT FUNCTION
barp <- function(data, x, y) {
  ggplot(data, aes({{x}}, levels = Year, {{y}}, fill = factor(condclass))) +
    geom_bar(stat = "identity", position = "dodge2", colour = "black")  +
    labs(x = "Site", y = c(paste(data$Char_Name, "(",data$Result_Unit,")"))) + #automatic axis labeling
    geom_text(aes(label=Year), position=position_dodge2(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
    scale_fill_manual(breaks = c("Good", "Fair", "Poor", "NA"), values = c("green4", "orange", "red", "slategrey")) +
    geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) + #vertical separator lines
    geom_hline(yintercept = data$MRLValue, linetype = "dashed", color = "red") + #MRL horizontal reference line
    theme_classic() +
    theme(panel.background = element_rect(fill = "gray")) + 
    theme(legend.position = "bottom") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
}

#GRAB WQ PARAMETERS BAR PLOTS (adjust y axis range as needed)
barp(turb, StationDes, Result_Numeric_mod) +  coord_cartesian(ylim = c(0, 25)) 
barp(cond, StationDes, Result_Numeric_mod)
barp(do.sat, StationDes, Result_Numeric_mod) + coord_cartesian(ylim = c(50, 120))
barp(do.conc, StationDes, Result_Numeric_mod) + coord_cartesian(ylim = c(4, 12))
barp(ph, StationDes, Result_Numeric_mod) + coord_cartesian(ylim = c(6.5, 8.5)) #fix apparent duplicate records - lab vs field?
barp(temp, StationDes, Result_Numeric_mod) + coord_cartesian(ylim = c(0, 25))
barp(afdm, StationDes, Result_Numeric_mod) #messy MRLs

#LAB WQ PARAMETERS BOX PLOTS (add chart modifiers as needed)
barp(tss, StationDes, Result_Numeric_mod)
barp(tp, StationDes, Result_Numeric_mod)
barp(sulf, StationDes, Result_Numeric_mod)
barp(tn, StationDes, Result_Numeric_mod) #fix char_name

#PERIPHYTON VS NUTRIENTS
peri<-subset(chem, chem$'Char_Name' == 'Ash Free Dry Mass' | chem$'Char_Name' == 'Total Phosphorus, mixed forms' | 
               chem$'Char_Name' == 'Chlorophyll a' | chem$'Char_Name' == 'Nitrate + Nitrite')

library(reshape2)
peri_wide <- dcast(peri, MLocID + SampleStartDate ~ Char_Name, value.var = 'Result_Numeric')


peri_wide$log_afdm<-log(peri_wide$`Ash Free Dry Mass`)
peri_wide$log_chl<-log(peri_wide$`Chlorophyll a`)
peri_wide$log_nits<-log(peri_wide$`Nitrate + Nitrite`)
peri_wide$log_tp<-log(peri_wide$`Total Phosphorus, mixed forms`)

#afdm vs tp
afdm_tp<-ggplot(peri_wide, aes(x = log_tp, y = log_afdm)) + 
  geom_point() + 
  labs(x = "Log TP (mg/L)", y = "Log AFDM (mg/cm2)")
print(afdm_tp)

#afdm vs nits
afdm_nits<-ggplot(peri_wide, aes(x = log_nits, y = log_afdm)) + 
  geom_point() + 
  labs(x = "Log Nits (mg/L)", y = "Log AFDM (mg/cm2)")
print(afdm_nits)

#chl vs tp
chl_tp<-ggplot(peri_wide, aes(x = log_tp, y = log_chl)) + 
  geom_point() + 
  labs(x = "Log TP (mg/L)", y = "Log Chlorophyll a (ug/L)")
print(chl_tp)



write_xlsx(peri_wide, path = "//deqlab1/Biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/peri_wide.xlsx", col_names = TRUE)


# ! # ! # ! # MRL = 0.005 for TN but 0.2 for tkn - which ref line to use? # ! # ! # ! #
# ! # ! # ! # need note that method limits are different,  not actual result of declining N in 2018 # ! # ! # ! #

#>>>>>>>>>>>>>>>>>>>>~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
     ___            #
    / __\     (2)   #
   /__\//           #
  / \/  \           #
  \_____/UGS        #
#>>>>>>>>>>>>>>>>>>>>
#BUGS: stats====
     
#>>>>>>>>>>>>>>>>>>>>
#     BOX PLOTS     #
#>>>>>>>>>>>>>>>>>>>>

# ~ ~ ~ O/E
#ratio of expected to observed bug taxa irrespective of model (SE sites -> 'OoverE_null')
box_oe<-ggplot(bugs, aes(x = `StationDes`, y = `O/E Ratio`)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Site", y = "O/E") +
  geom_hline(yintercept = 1.0, color = "blue", linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(box_oe)

# ~ ~ ~ TS
#macroinvertebrate inferred 7-day average seasonal max temperature (?C)
box_ts<-ggplot(bugs, aes(x = `StationDes`, y = `Inferred Temperature`)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Site", y = "TS") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(box_ts)

# ~ ~ ~ BSTI
#Biological Sediemnt Tolerance Index - macroinvertebrate inferred % fines
box_bsti<-ggplot(bugs, aes(x = `StationDes`, y = `BSTI`)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Site", y = "BSTI") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(box_bsti)

# ~ ~ ~ BC
#Bray-Curtis Similarity Index (Van Sickle 2008)
box_bc<-ggplot(bugs, aes(x = `StationDes`, y = `Bray Curtis Similarity Index`)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Site", y = "BC") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(box_bc)

# ~ ~ ~ TOTAL RICHNESS
#number of unique taxa
box_total.richness<-ggplot(bugs, aes(x = `StationDes`, y = `Total Taxa Richness`)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Site", y = "Total Richness") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(box_total.richness)

# ~ ~ ~ PERCENT EPT
#% abundance of EPT
box_pct.ept<-ggplot(bugs, aes(x = `StationDes`, y = `% EPT Taxa`)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Site", y = "% EPT") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(box_pct.ept)

# ~ ~ ~ PERCENT NON INSECT
#% abundance of Non Insect taxa
box_pct.non.insect<-ggplot(bugs, aes(x = `StationDes`, y = `% Non-Insect Taxa`)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Site", y = "% Non Insect") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(box_pct.non.insect)

# ~ ~ ~ VOLTINISM
#% abundance of long-lived (semivoltine) taxa
box_semivolt<-ggplot(bugs, aes(x = `StationDes`, y = `% Semivoltine Taxa`)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Site", y = "% Semivoltine") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(box_semivolt)

#% abundance of short-lived (multivoltine) taxa
box_multivolt<-ggplot(bugs, aes(x = `StationDes`, y = `% Multivoltine Taxa`)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
labs(x = "Site", y = "% Multivoltine") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(box_multivolt)

#% abundance of univoltine taxa
box_univolt<-ggplot(bugs, aes(x = `StationDes`, y = `% Univoltine Taxa`)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Site", y = "% Univoltine") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(box_univolt)

#ratio of semivoltine:multivoltine richness
box_voltratio.rich<-ggplot(bugs, aes(x = `StationDes`, y = `Semivoltine Taxa Richness`)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Site", y = "Semivoltine : Multivoltine (Richness)") +
  geom_hline(yintercept = 1.0, color = "blue", linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(box_voltratio.rich)

#ratio of semivoltine:multivoltine percent
box_voltratio.pct<-ggplot(bugs, aes(x = StationDes, y = voltratio.pct)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Site", y = "Semivoltine : Multivoltine (%)") +
  geom_hline(yintercept = 1.0, color = "blue", linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(box_voltratio.pct)

# ~ ~ ~ DIVERSITY
#Simpson_diversity: 1 - sum(relative abundance^2)
box_div.simpson<-ggplot(bugs, aes(x = `StationDes`, y = `Simpson Diversity`)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Site", y = "Simpson Diversity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(box_div.simpson)

# ~ ~ ~ TOTAL ABUNDANCE RIV
#total abundance following subsampling for RIVPACS
box_total.abund.riv<-ggplot(bugs, aes(x = `StationDes`, y = `RIVPACS Sub-Sample Total Abundance`)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Site", y = "Total Abundance (RIVPACS)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(box_total.abund.riv)

# ~ ~ ~ TOTAL ABUNDANCE STR
#total abudnance used in Stressor Models (temp and fines)
box_total.abund.str<-ggplot(bugs, aes(x = `StationDes`, y = `Stressor Models Total Abundance`)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Site", y = "Total Abundance (Stressor Models)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(box_total.abund.str)


#>>>>>>>>>>>>>>>>>>>>
#     BAR PLOTS     #
#>>>>>>>>>>>>>>>>>>>>

#STACKED BAR PLOT FOR O/E CONDITION CLASSES
bugs2<-subset(bugs, bugs$`O/E Condition`!="NA")

bar_oestack<-ggplot(bugs2, aes(fill = `O/E Condition`, x = `StationDes`, y = 100)) +
  geom_bar(position = "fill", stat = "identity", colour = "black") + 
  labs(title = "O/E Condition Classes by Site, 1992-2018", x = "Site", y = "Proportion") +
  geom_text(aes(label=Year), position=position_fill(vjust = 0.5), size = 3) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c('#0066ff', '#009900', '#ff9900', '#e62e00')) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.25))
print(bar_oestack)

ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Bugs/OE_Condition_Classes.jpeg', device = "jpeg", width = 8, height = 10)

# ! # ! # ! diff colors for a and b data? # ! # ! # ! 

# ~ ~ ~ O/E
  #ratio of expected to observed bug taxa irrespective of model (SE sites -> 'OoverE_null')
bar_oe<-ggplot(bugs, aes(x = StationDes, level = Year, y = `O/E Ratio`, fill = factor(`O/E Condition`))) + #chronologically ordered & conditionally formatted by condition
  geom_bar(stat = "identity", position = "dodge", colour = "black") + #black bar outlines
  labs(x = "Site", y = "O/E") +
  geom_hline(yintercept = 1.0, color = "black", linetype = "dashed") + #reference line at O/E = 1
  geom_text(aes(label = Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) + #vertical separators between sites
  theme_classic() +
  theme(panel.background = element_rect(fill = "gray")) +
  scale_fill_manual(values = c("#0066ff", "#009900", "#ff9900", "#e62e00")) + #colors for condition classes
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(bar_oe)

  #facet wrap by year
bar_oe2<-ggplot(bugs, aes(x = StationDes, level = Year, y = `O/E Ratio`, fill = factor(`O/E Condition`))) + #chronologically ordered & conditionally formatted by condition
  geom_bar(stat = "identity", position = "dodge", colour = "black") + #black bar outlines
  labs(x = "Site", y = "O/E") +
  geom_hline(yintercept = 1.0, color = "black", linetype = "dashed") + #reference line at O/E = 1
  #geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) + #vertical separators between sites
  theme_classic() +
  theme(panel.background = element_rect(fill = "gray")) +
  scale_fill_manual(values = c("#0066ff", "#009900", "#ff9900", "#e62e00")) + #colors for condition classes
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  facet_wrap(~ Year) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(bar_oe2)
ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Bugs/O_over_E_FacetWrap_Year.jpeg', device = "jpeg", width = 12, height = 10)

  #facet wrap by site
bar_oe3<-ggplot(bugs, aes(x = Year, level = Year, y = `O/E Ratio`, fill = factor(`O/E Condition`))) + #chronologically ordered & conditionally formatted by condition
  geom_bar(stat = "identity", position = "dodge2", colour = "black") + #black bar outlines
  labs(x = "Year", y = "O/E") +
  geom_hline(yintercept = 1.0, color = "black", linetype = "dashed") + #reference line at O/E = 1
  #geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) + #vertical separators between sites
  theme_classic() +
  theme(panel.background = element_rect(fill = "gray")) +
  scale_fill_manual(values = c("#0066ff", "#009900", "#ff9900", "#e62e00")) + #colors for condition classes
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  facet_wrap(~ StationDes) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(bar_oe3)

# ~ ~ ~ TS
#macroinvertebrate inferred 7-day average seasonal max temperature (?C)
bar_ts<-ggplot(bugs, aes(x = StationDes, level = Year, y = TS, fill = factor(`O/E Condition`))) + #chronologically ordered & conditionally formatted by condition
  geom_bar(stat = "identity", position = "dodge", colour = "black") + #black bar outlines
  labs(x = "Site", y = "TS") +
  geom_text(aes(label = Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) + #vertical separators between sites
  theme_classic() +
  theme(panel.background = element_rect(fill = "gray")) +
  scale_fill_manual(values = c("#0066ff", "#009900", "#ff9900", "#e62e00")) + #colors for condition classes
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(bar_ts)

# ~ ~ ~ BSTI
#Biological Sediemnt Tolerance Index - macroinvertebrate inferred % fines
bar_bsti<-ggplot(bugs, aes(x = StationDes, level = Year, y = BSTI, fill = factor(`O/E Condition`))) + #chronologically ordered & conditionally formatted by condition
  geom_bar(stat = "identity", position = "dodge", colour = "black") + #black bar outlines
  labs(x = "Site", y = "BSTI") +
  geom_text(aes(label = Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) + #vertical separators between sites
  theme_classic() +
  theme(panel.background = element_rect(fill = "gray")) +
  scale_fill_manual(values = c("#0066ff", "#009900", "#ff9900", "#e62e00")) + #colors for condition classes
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(bar_bsti)

# ~ ~ ~ BC
#Bray-Curtis Similarity Index (Van Sickle 2008)
bar_bc<-ggplot(bugs, aes(x = StationDes, level = Year, y = BC, fill = factor(`O/E Condition`))) + #chronologically ordered & conditionally formatted by condition
  geom_bar(stat = "identity", position = "dodge", colour = "black") + #black bar outlines
  labs(x = "Site", y = "BC") +
  geom_text(aes(label = Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) + #vertical separators between sites
  theme_classic() +
  theme(panel.background = element_rect(fill = "gray")) +
  scale_fill_manual(values = c("#0066ff", "#009900", "#ff9900", "#e62e00")) + #colors for condition classes
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(bar_bc)

# ~ ~ ~ TOTAL RICHNESS
#number of unique taxa
bar_total.richness<-ggplot(bugs, aes(x = StationDes, level = Year, y = total_richness, fill = factor(`O/E Condition`))) + #chronologically ordered & conditionally formatted by condition
  geom_bar(stat = "identity", position = "dodge", colour = "black") + #black bar outlines
  labs(x = "Site", y = "Total Richness") +
  geom_text(aes(label = Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) + #vertical separators between sites
  theme_classic() +
  theme(panel.background = element_rect(fill = "gray")) +
  scale_fill_manual(values = c("#0066ff", "#009900", "#ff9900", "#e62e00")) + #colors for condition classes
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(bar_total.richness)

# ~ ~ ~ PERCENT EPT
#% abundance of EPT
bar_pct.ept<-ggplot(bugs, aes(x = StationDes, level = Year, y = pct_EPT, fill = factor(`O/E Condition`))) + #chronologically ordered & conditionally formatted by condition
  geom_bar(stat = "identity", position = "dodge", colour = "black") + #black bar outlines
  labs(x = "Site", y = "% EPT") +
  geom_text(aes(label = Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) + #vertical separators between sites
  theme_classic() +
  theme(panel.background = element_rect(fill = "gray")) +
  scale_fill_manual(values = c("#0066ff", "#009900", "#ff9900", "#e62e00")) + #colors for condition classes
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(bar_pct.ept)

# ~ ~ ~ PERCENT NON INSECT
#% abundance of Non Insect taxa
bar_pct.non.insect<-ggplot(bugs, aes(x = StationDes, level = Year, y = pct_NonInsect, fill = factor(`O/E Condition`))) + #chronologically ordered & conditionally formatted by condition
  geom_bar(stat = "identity", position = "dodge", colour = "black") + #black bar outlines
  labs(x = "Site", y = "% Non Insect") +
  geom_text(aes(label = Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) + #vertical separators between sites
  theme_classic() +
  theme(panel.background = element_rect(fill = "gray")) +
  scale_fill_manual(values = c("#0066ff", "#009900", "#ff9900", "#e62e00")) + #colors for condition classes
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(bar_pct.non.insect)

# ~ ~ ~ VOLTINISM


# ~ ~ ~ DIVERSITY
#Simpson_diversity: 1 - sum(relative abundance^2)
bar_div.simpson<-ggplot(bugs, aes(x = StationDes, level = Year, y = Simpson_diversity, fill = factor(`O/E Condition`))) + #chronologically ordered & conditionally formatted by condition
  geom_bar(stat = "identity", position = "dodge", colour = "black") + #black bar outlines
  labs(x = "Site", y = "Simpson Diversity") +
  geom_text(aes(label = Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) + #vertical separators between sites
  theme_classic() +
  theme(panel.background = element_rect(fill = "gray")) +
  scale_fill_manual(values = c("#0066ff", "#009900", "#ff9900", "#e62e00")) + #colors for condition classes
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(bar_div.simpson)

# ~ ~ ~ TOTAL ABUNDANCE RIV
#total abundance following subsampling for RIVPACS
bar_total.abund.riv<-ggplot(bugs, aes(x = StationDes, level = Year, y = tot_abund_RIV, fill = factor(`O/E Condition`))) + #chronologically ordered & conditionally formatted by condition
  geom_bar(stat = "identity", position = "dodge", colour = "black") + #black bar outlines
  labs(x = "Site", y = "Total Abundance (RIVPACS)") +
  geom_text(aes(label = Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) + #vertical separators between sites
  theme_classic() +
  theme(panel.background = element_rect(fill = "gray")) +
  scale_fill_manual(values = c("#0066ff", "#009900", "#ff9900", "#e62e00")) + #colors for condition classes
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(bar_total.abund.riv)

# ~ ~ ~ TOTAL ABUNDANCE STR
#total abundance used in Stressor Models (temp and fines)
bar_total.abund.str<-ggplot(bugs, aes(x = StationDes, level = Year, y = tot_abund_STR, fill = factor(`O/E Condition`))) + #chronologically ordered & conditionally formatted by condition
  geom_bar(stat = "identity", position = "dodge", colour = "black") + #black bar outlines
  labs(x = "Site", y = "Total Abundance (Stressor Models)") +
  geom_text(aes(label = Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) + #vertical separators between sites
  theme_classic() +
  theme(panel.background = element_rect(fill = "gray")) +
  scale_fill_manual(values = c("#0066ff", "#009900", "#ff9900", "#e62e00")) + #colors for condition classes
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(bar_total.abund.str)


#>>>>>>>>>>>>>>>>>>>>  
#     STACKED       #   #STACK BOX AND BAR PLOTS FOR EACH BUGS PARAMETER
#>>>>>>>>>>>>>>>>>>>>
# ~ ~ ~ O/E
plot_grid(bar_oe + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
          box_oe, ncol = 1)
ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Bugs/O_over_E.jpeg', device = "jpeg", width = 8, height = 10)

# ~ ~ ~ TS
plot_grid(bar_ts + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
          box_ts, ncol = 1)
ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Bugs/TS.jpeg', device = "jpeg", width = 8, height = 10)

# ~ ~ ~ BSTI
plot_grid(bar_bsti + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
          box_bsti, ncol = 1)
ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Bugs/BSTI.jpeg', device = "jpeg", width = 8, height = 10)

# ~ ~ ~ BC
plot_grid(bar_bc + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
          box_bc, ncol = 1)
ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Bugs/BC.jpeg', device = "jpeg", width = 8, height = 10)

# ~ ~ ~ TOTAL RICHNESS
plot_grid(bar_total.richness + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
          box_total.richness, ncol = 1)
ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Bugs/Total_Richness.jpeg', device = "jpeg", width = 8, height = 10)

# ~ ~ ~ PERCENT EPT
plot_grid(bar_pct.ept + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
          box_pct.ept, ncol = 1)
ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Bugs/Percent_EPT.jpeg', device = "jpeg", width = 8, height = 10)

# ~ ~ ~ PERCENT NON INSECT
plot_grid(bar_pct.non.insect + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
          box_pct.non.insect, ncol = 1)
ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Bugs/Percent_Non_Insect.jpeg', device = "jpeg", width = 8, height = 10)

# ~ ~ ~ VOLTINISM
#would be better if mutli and semi were stacked on one graph
plot_grid(box_multivolt + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
          box_semivolt, ncol = 1)
ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Bugs/Voltinism.jpeg', device = "jpeg", width = 8, height = 10)

# ~ ~ ~ DIVERSITY
plot_grid(bar_div.simpson + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
          box_div.simpson, ncol = 1)
ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Bugs/Simpson_Diversity.jpeg', device = "jpeg", width = 8, height = 10)

# ~ ~ ~ TOTAL ABUNDANCE RIV
plot_grid(bar_total.abund.riv + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
          box_total.abund.riv, ncol = 1)
ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Bugs/Total_Abundance_RIV.jpeg', device = "jpeg", width = 8, height = 10)

# ~ ~ ~ TOTAL ABUNDANCE STR
plot_grid(bar_total.abund.str + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
          box_total.abund.str, ncol = 1)
ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Bugs/Total_Abundance_STR.jpeg', device = "jpeg", width = 8, height = 10)


#>>>>>>>>>>>>>>>>>>>>
#   SCATTER PLOTS   #
#>>>>>>>>>>>>>>>>>>>>

# ~ ~ ~ O/E
#scatter
scat_oe<-ggplot(bugs, aes(x = Year, y = OoverE_extract)) +
  geom_point(aes(color = StationDes)) + 
  labs(x = "Year", y = "O/E") +
  geom_hline(yintercept = 1.0, color = "blue", linetype = "dashed") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(scat_oe)

#line
line_oe<-ggplot(bugs, aes(x = Year, y = OoverE_extract, group = StationDes)) +
  geom_line(aes(color = StationDes)) + 
  geom_point(aes(color = StationDes)) +
  labs(x = "Year", y = "O/E") +
  geom_hline(yintercept = 1.0, color = "blue", linetype = "dashed") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(line_oe)

#regression
reg_oe<-ggplot(bugs, aes(x = Year, y = OoverE_extract, group = StationDes, color = StationDes)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, aes(fill = StationDes)) +
  labs(x = "Year", y = "O/E") +
  geom_hline(yintercept = 1.0, color = "blue", linetype = "dashed") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(reg_oe)


# ~ ~ ~ TS
scat_ts<-ggplot(bugs, aes(x = Year, y = TS)) +
  geom_point(aes(color = StationDes)) + 
  labs(x = "Year", y = "TS") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(scat_ts)

# ~ ~ ~ BSTI
scat_bsti<-ggplot(bugs, aes(x = Year, y = BSTI)) +
  geom_point(aes(color = StationDes)) + 
  labs(x = "Year", y = "BSTI") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(scat_bsti)

# ~ ~ ~ BC
scat_bc<-ggplot(bugs, aes(x = Year, y = BC)) +
  geom_point(aes(color = StationDes)) + 
  labs(x = "Year", y = "BC") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(scat_bc)

# ~ ~ ~ TOTAL RICHNESS
scat_total.richness<-ggplot(bugs, aes(x = Year, y = total_richness)) +
  geom_point(aes(color = StationDes)) + 
  labs(x = "Year", y = "Total Richness") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(scat_total.richness)

# ~ ~ ~ PERCENT EPT
scat_pct.ept<-ggplot(bugs, aes(x = Year, y = pct_EPT)) +
  geom_point(aes(color = StationDes)) + 
  labs(x = "Year", y = "% EPT") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(scat_pct.ept)

# ~ ~ ~ PERCENT NON INSECT
scat_pct.non.insect<-ggplot(bugs, aes(x = Year, y = pct_NonInsect)) +
  geom_point(aes(color = StationDes)) + 
  labs(x = "Year", y = "% Non Insect") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(scat_pct.non.insect)

# ~ ~ ~ VOLTINISM


# ~ ~ ~ DIVERSITY
scat_div.simpson<-ggplot(bugs, aes(x = Year, y = Simpson_diversity)) +
  geom_point(aes(color = StationDes)) + 
  labs(x = "Year", y = "Simpson Diversity") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(scat_div.simpson)

#reg
reg_div.simpson<-ggplot(bugs, aes(x = Year, y = Simpson_diversity, group = StationDes, color = StationDes)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, aes(fill = StationDes)) +
  labs(x = "Year", y = "Simpson Diversity") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(reg_div.simpson)


# ~ ~ ~ TOTAL ABUNDANCE RIV
scat_total.abund.riv<-ggplot(bugs, aes(x = Year, y = tot_abund_RIV)) +
  geom_point(aes(color = StationDes)) + 
  labs(x = "Year", y = "Total Abundance (RIVPACS)") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(scat_total.abund.riv)

# ~ ~ ~ TOTAL ABUNDANCE STR
scat_total.abund.str<-ggplot(bugs, aes(x = Year, y = tot_abund_STR)) +
  geom_point(aes(color = StationDes)) + 
  labs(x = "Year", y = "Total Abundance (Stressor Models)") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
print(scat_total.abund.str)


#>>>>>>>>>>>>>>>>>>>> ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
                    #
     /\  /\     (3) #
    / /_/ /         #
   / __  /          #
   \/ |_/ABITAT     #
#>>>>>>>>>>>>>>>>>>>>
#HABITAT: stats====

#ON HOLD...

___       ___     ___       __  
 |  |__| |__     |__  |\ | |  \ 
 |  |  | |___    |___ | \| |__/ 
  

#QUESTIONS====
  
#  1.	Are the macroinvertebrate communities changing at these sites? Species? Tolerance levels? Abundance? Diversity?
#  2.	Are chemical parameters changing and in what direction?
#  3.	Are riparian and in-stream habitat conditions changing? Shade? Disturbance? Flow? In-stream wood? Substrate? Etc. 
#  4.	What does the inter-year variability look like for different parameters at each site? 
#  5.	How do the statewide reference sites compare to our larger reference site network? Can they be used to understand annual variability at other reference and study locations?
#  6.	How do the biological and chemical measurements at these sites compare to our water quality standards, benchmarks and narrative criteria?
#  7.	What are the physical attributes of the reference sites in this network? Ecoregion? Elevation? Land-use? Etc. 
#  8.	Do these sites tell us anything about climate change yet?
#  9.	Have any of these site been burned since we started monitoring?
  

  
  
#GRAVEYARD====  
#>>>>>>>>>>>>>>>>>>>> ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    .----------------.  .----------------.  .----------------.  .----------------.  .----------------.  .----------------.  .----------------.  .----------------.  .----------------. 
    | .--------------. || .--------------. || .--------------. || .--------------. || .--------------. || .--------------. || .--------------. || .--------------. || .--------------. |
    | |    ______    | || |  _______     | || |      __      | || | ____   ____  | || |  _________   | || |  ____  ____  | || |      __      | || |  _______     | || |  ________    | |
    | |  .' ___  |   | || | |_   __ \    | || |     /  \     | || ||_  _| |_  _| | || | |_   ___  |  | || | |_  _||_  _| | || |     /  \     | || | |_   __ \    | || | |_   ___ `.  | |
    | | / .'   \_|   | || |   | |__) |   | || |    / /\ \    | || |  \ \   / /   | || |   | |_  \_|  | || |   \ \  / /   | || |    / /\ \    | || |   | |__) |   | || |   | |   `. \ | |
    | | | |    ____  | || |   |  __ /    | || |   / ____ \   | || |   \ \ / /    | || |   |  _|  _   | || |    \ \/ /    | || |   / ____ \   | || |   |  __ /    | || |   | |    | | | |
    | | \ `.___]  _| | || |  _| |  \ \_  | || | _/ /    \ \_ | || |    \ ' /     | || |  _| |___/ |  | || |    _|  |_    | || | _/ /    \ \_ | || |  _| |  \ \_  | || |  _| |___.' / | |
    | |  `._____.'   | || | |____| |___| | || ||____|  |____|| || |     \_/      | || | |_________|  | || |   |______|   | || ||____|  |____|| || | |____| |___| | || | |________.'  | |
    | |              | || |              | || |              | || |              | || |              | || |              | || |              | || |              | || |              | |
    | '--------------' || '--------------' || '--------------' || '--------------' || '--------------' || '--------------' || '--------------' || '--------------' || '--------------' |
    '----------------'  '----------------'  '----------------'  '----------------'  '----------------'  '----------------'  '----------------'  '----------------'  '----------------' 
  
#SET WORKING DIRECTORY
setwd("//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R") #put required files on Git  

   __ ___ _   _    
  (_   | / \ |_) |  #In AWQMS, query all 12 Ref Trend sites (~1980-Present) and save report as Standard Export to folder below (rename to include date of data pull)
  __)  | \_/ |   o 

#chem<-read_excel('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/AWQMS_Trend_Data_12-01-2019.xlsx', sheet = "Results") #update file path as needed

#ND
#new format already accomplishes this - 
#chem$`Result Type`<-ifelse(!grepl("^>|^<", chem$`Result_Numeric`), "Standard", #add "Result Type" column based on how data are qualified
                     #      ifelse(grepl("^<", chem$`Result_Numeric`), "ND",
                     #             ifelse(grepl("^>", chem$`Result_Numeric`), "Exceedance", "Other")))
#chem$`Result_Numeric_mod`<-sub('>|<','', chem$`Result_Numeric`) #create new column with values stripped of  > & <)
#chem$`Result_Numeric_mod`<-sapply(chem$`Result Value_mod`, as.numeric, digits = 3, format = "f") #convert character values to numeric
#chem$`Result_Numeric_mod`<-ifelse(chem$`Result Type`=="ND", chem$`Result_Numeric_mod` * 0.99, #multiply ND values by 0.99
                                  ifelse(chem$`Result_Numeric`=="Exceedance", chem$`Result_Numeric_mod` * 1.01, #multiply Exceedance values by 1.01
                                         ifelse(chem$`Result Type`=="Standard", chem$`Result_Numeric_mod`, chem$`Result_Numeric_mod`))) #retain values for everything else
# ! # ! # ! # found a result value that says "ND" - treated as NA in analysis # ! # ! # ! # 
#Consider using 0.5 for ND (SH)

#side project
  #IR Summary
#  bugsIR<-subset(bugs, bugs$Project=='IR Revisits')
  
  #IR Revisits O/E scatter
#  bp_ir.oe<-ggplot(bugsIR, aes(x = STATION_KEY, y = OoverE)) +
    geom_point(aes(color=bugsIR$oe_cond)) +
    labs(x = "LASAR ID", y = "O/E") +
    geom_hline(yintercept = 1.0, color = "blue", linetype = "dashed") +
    scale_color_manual(values = c("green", "orange", "red")) +
    scale_y_continuous(breaks = seq(min(bugsIR$OoverE), max(bugsIR$OoverE), by = 0.05)) +
    geom_text(aes(label = OoverE), size = 3, hjust = 0.5, vjust = -1) +
    theme_dark() +
    theme(legend.position = "bottom") + 
    theme(legend.title = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(bp_ir.oe)
  ggsave("IR_Revisit_OE.jpeg", device = "jpeg")

  
  
##GARBAGE???? 

###if else
if(bugs$OoverE > 0) {
  ggplot(bugs, aes(x = StationDes, y = OoverE)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Site", y = "O/E") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5))
} else {ggplot(bugs, aes(x = StationDes, y = OoverE_null)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Site", y = "O/E") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5))
}

#testing ifelse statement format
ifelse(!is.na(bugs$OoverE), yes = "y", no = "n")

ifelse(grepl("COLD DESERTS",bugs$Eco2), 'se', 'rest')

if(bugs$Eco2=="COLD DESERTS"){
  print("se")
} else {
    print("rest")
}

#ifelse using O/E is not NA
ifelse(!is.na(bugs$OoverE), 
       yes = ggplot(bugs, aes(x = StationDes, y = OoverE, na.rm = TRUE)), 
       no = ggplot(bugs, aes(x = StationDes, y = OoverE_null, na.rm = TRUE))) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Site", y = "O/E") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5))

#ifelse using eco2 is Cold Deserts
ifelse(grepl("COLD DESERTS",bugs$Eco2),
      yes = ggplot(bugs, aes(x = StationDes, y = OoverE_null, na.rm = TRUE)),
      no = ggplot(bugs, aes(x = StationDes, y = OoverE, na.rm = TRUE))) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(x = "Site", y = "O/E") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5))


##
#NON DETECTS
chem$`Result Type`<-ifelse(chem$`Result Value` %in% c("^<", "^>", is.numeric()), "ND", "Exceedance", "Detect") #needs reworking

chem$`Result Type`<-ifelse(grepl("^<", chem$`Result Value`), "ND", "") #add "Result Type" column indicating if result is a ND
chem$`Result Type`<-ifelse(grepl("^>", chem$`Result Value`), "Exceedance", "")  #overwrites above entries

c("ND", "Exceedance")[grepl("^<|^>"), chem$`Result Value`]

qualifiers<-grepl("^>|^<", chem$`Result Value`)

chem$`Result Type`<-if(!grepl("^>|^<", chem$`Result Value`) {
  return("Normal")
} else {}
}
  
  if(grepl("^>", chem$`Result Value`) {
    chem$`Result Type`<-"Exceedance"
  } else {
    if(grepl("^<", chem$`Result Value`)
       chem$`Result Type`<-"ND"
  } else {
    chem$`Result Type`<-"Normal"
  }
  
  
  chem$`Result Value` %>% mutate(chem$`Result Type` = chem$`Result Value` %in% qualifiers)
  
  
  
  chem$`Result Type`<-str_detect(chem$`Result Type`, qualifiers)
  
  chem$`Result Type`<-sapply(qualifiers, grepl, chem$`Result Value`, ignore.case = TRUE)
  
  
  resultlabels<-c("ND", "Exceedance", "Regular")
  grepl(paste(chem$`Result Value`, collapse = "|"), qualifiers)
  
  grep("<|>", chem$`Result Value`), "ND", "Exceedance"
  
  ifelse(grepl("^< | ^>", chem$`Result Value`), "ND", "Exceed")
  
  #>>>>>>>>>>>>>>>>>>>>
  
  #more ND stuff from lab and grab results
  #REMOVE > AND < FROM RESULT VALUES AND CHANGE THEM TO SPECIFIC VALUES
  turb$`Result Value`[grepl('<1', turb$`Result Value`)] <- '0.99'
  turb$`Result Value`[grepl('>100', turb$`Result Value`)] <- '1001'
  tss$`Result Value`[grepl('<1', tss$`Result Value`)] <- '0.99'
  tp$`Result Value`[grepl('<0.01', tp$`Result Value`)] <- '0.009'
  
  #CONVERT RESULT VALUES FROM CHARACTER TO NUMERIC
  temp$`Result Value`<-as.numeric(temp$`Result Value`)
  turb$`Result Value`<-as.numeric(turb$`Result Value`)
  cond$`Result Value`<-as.numeric(cond$`Result Value`)
  ph$`Result Value`<-as.numeric(ph$`Result Value`)
  do.conc$`Result Value`<-as.numeric(do.conc$`Result Value`)
  do.sat$`Result Value`<-as.numeric(do.sat$`Result Value`)
  tss$`Result Value`<-as.numeric(tss$`Result Value`)
  sulf$`Result Value`<-as.numeric(sulf$`Result Value`)
  tp$`Result Value`<-as.numeric(tp$`Result Value`)
  
  
  #SUBSET GRAB WQ DATA
  grabWQ<-subset(chem, chem$`Characteristic Name`=='Dissolved oxygen saturation' | chem$`Characteristic Name`=='Dissolved oxygen (DO)' | 
                 chem$`Characteristic Name`=='pH' | chem$`Characteristic Name`=='Temperature, water' | chem$`Characteristic Name`=='Conductivity' | 
                 chem$`Characteristic Name`=='Turbidity')
  
  #OLD BUGS FILE PATHS
  !!!!!!!!!!!!!!
    #ENTER THE FILE PATH OF THE BUGS METRICS TABLE... (old file)
    bugs.input <- '//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/OE_Stress_Metrics_data quality.csv'
  
  !!!!!!!!!!!!!!
    new file name;SUM_BUGS_2021-09-22.csv
  bugs.input <- '//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/SUM_BUGS_2021-09-22.csv'
  
  #...AND BRING INTO R
  bugsOLD <- read.csv(bugs.input, stringsAsFactors = FALSE) #avoids site names becoming factors
  rm(bugs.input)
  #!!!!!!!!!!!!!!
  
  
#OLD BOX PLOT CODE PRE-FUNCTIONS
  #MAKE BOX PLOTS OF GRAB WQ PARAMETERS
  # ~ ~ ~ TURBIDITY
  box_turb<-ggplot(turb, aes(x = `StationDes`, y = `Result_Numeric_mod`)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
    labs(x = "Site", y = "Turbidity (NTU)") +
    coord_cartesian(ylim = c(0, 30)) + #change upper Y limit depending on data range
    #geom_segment(aes(x = 0.5, y = 1, xend = 1.5, yend = 1), color = "blue", linetype = "dashed") + #black canyon
    #geom_segment(aes(x = 0.5, y = 2, xend = 1.5, yend = 2), color = "red", linetype = "dashed") +
    #geom_segment(aes(x = 1.5, y = 1, xend = 2.5, yend = 1), color = "blue", linetype = "dashed") + #booze
    #geom_segment(aes(x = 1.5, y = 3, xend = 2.5, yend = 3), color = "red", linetype = "dashed") +
    #geom_segment(aes(x = 2.5, y = 4, xend = 3.5, yend = 4), color = "blue", linetype = "dashed") + #bridge
    #geom_segment(aes(x = 2.5, y = 13, xend = 3.5, yend = 13), color = "red", linetype = "dashed") +
    #geom_segment(aes(x = 3.5, y = 1, xend = 4.5, yend = 1), color = "blue", linetype = "dashed") + #fish lake
    #geom_segment(aes(x = 3.5, y = 2, xend = 4.5, yend = 2), color = "red", linetype = "dashed") +
    #geom_segment(aes(x = 4.5, y = 4, xend = 5.5, yend = 4), color = "blue", linetype = "dashed") + #guano
    #geom_segment(aes(x = 4.5, y = 13, xend = 5.5, yend = 13), color = "red", linetype = "dashed") +
    # DO THIS AUTOMATICALLY
  geom_hline(yintercept = turb$MRLValue, linetype = "dashed", color = "red") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(box_turb)
  
  # ~ ~ ~ CONDUCTIVITY
  box_cond<-ggplot(cond, aes(x = `StationDes`, y = `Result_Numeric_mod`)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
    labs(x = "Site", y = "Conductivity (uS/cm)") +
    geom_hline(yintercept = cond$MRLValue, linetype = "dashed", color = "red") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(box_cond)
  
  # ~ ~ ~ DISSOLVED OXYGEN SATURATION
  box_do.sat<-ggplot(do.sat, aes(x = `StationDes`, y = `Result_Numeric_mod`)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
    coord_cartesian(ylim = c(50, 120)) + #change y axis lower limit
    labs(x = "Site", y = "DO (%)") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(box_do.sat)
  
  # ~ ~ ~ DISSOLVED OXYGEN CONCENTRATION
  box_do.conc<-ggplot(do.conc, aes(x = `StationDes`, y = `Result_Numeric_mod`)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
    coord_cartesian(ylim = c(4, 12)) + #change y axis lower limit
    labs(x = "Site", y = "DO (mg/L)") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(box_do.conc)
  
  # ~ ~ ~ pH
  box_ph<-ggplot(ph, aes(x = `StationDes`, y = `Result_Numeric_mod`)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
    labs(x = "Site", y = "pH (SU)") +
    #geom_hline(yintercept = ph$MRLValue, linetype = "dashed", color = "red") + #two diff MRLs - which to use?
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(box_ph)
  
  # ~ ~ ~ TEMPERATURE
  box_temp<-ggplot(temp, aes(x = `StationDes`, y = `Result_Numeric_mod`)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
    labs(x = "Site", y = "Water Temperature (deg C)") +
    #geom_hline(yintercept = temp$MRLValue, linetype = "dashed", color = "red") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(box_temp)
  
  # ~ ~ ~ AFDM
  box_afdm<-ggplot(afdm, aes(x = `StationDes`, y = `Result_Numeric_mod`)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
    labs(x = "Site", y = "AFDM (mg/L)") +
    #geom_hline(yintercept = afdm$MRLValue, linetype = "dashed", color = "red") + #multiple MRLs - which to use?
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(box_afdm)
  
  
  ##MAKE BOX PLOTS OF LAB WQ PARAMETERS 
  # ~ ~ ~ TOTAL SUSPENDED SOLIDS
  box_tss<-ggplot(tss, aes(x = `StationDes`, y = `Result_Numeric_mod`)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
    labs(x = "Site", y = "TSS (mg/L)") +
    geom_hline(yintercept = tss$MRLValue, linetype = "dashed", color = "red") + #two diff MRLs - which to use?
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(box_tss)
  
  # ~ ~ ~ TOTAL PHOSPHORUS
  box_tp<-ggplot(tp, aes(x = `StationDes`, y = `Result_Numeric_mod`)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
    labs(x = "Site", y = "Total Phosphorus (mg/L)") +
    geom_hline(yintercept = tp$MRLValue, linetype = "dashed", color = "red") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(box_tp)
  
  # ~ ~ ~ SULFATE (available for Bridge and Rock only)
  box_sulf<-ggplot(sulf, aes(x = `StationDes`, y = `Result_Numeric_mod`)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
    labs(x = "Site", y = "Sulfate (mg/L)") +
    geom_hline(yintercept = sulf$MRLValue, linetype = "dashed", color = "red") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(box_sulf)
  
  # ~ ~ ~ TOTAL NITROGEN
  box_tn<-ggplot(tn, aes(x = `StationDes`, y = `Result_Numeric_mod`)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
    labs(x = "Site", y = "Total Nitrogen (mg/L)") +
    geom_hline(yintercept = tn$MRLValue, linetype = "dashed", color = "red") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(box_tn)
  
  #>>>>>>>>>>>>>>>>>>>> START NEEDS WORK  #>>>>>>>>>>>>>>>>>>>>
  #>>>>>>>>>>>>>>>>>>>> START NEEDS WORK  #>>>>>>>>>>>>>>>>>>>>
  #testing for loop to graph all params with less code
  box_chem<-function(mydata = chem, myexposure = "`Monitoring Location Name`", myoutcome = "`Result Value_mod`"){
    bp<-ggplot(chem, aes_(as.name(`Monitoring Location Name`), as.name(`Result Value_mod`)) + 
               geomboxplot()
               print(bp)
  }
  
  #ala volmon
  for(i in unique(chem$`Monitoring Location Name`)) {
    jpeg(file = paste("var_", i, ".jpg", sep=""))
    boxplot(chem$`Result Value_mod` ~ `Monitoring Location Name`)
    #xlab = "Site",
    #ylab = paste(chem$`Characteristic Name`, chem$`Result Unit`), sep = " "),
    dev.off()
  }
  
  #without saving functionality
  bpfunc<-function(chem, na.rm = TRUE) {
    nm<-unique(chem$`Monitoring Location Name`)
    for (i in seq_along(nm)) {
      boxplot<-ggplot(subset(chem, 
      ), aes(x = `Monitoring Location Name`, y = `Result Value_mod`)) +
        
        
        print(ggplot(chem, aes_string(x = nm[i]))) +
        geom_histogram() }
  }
  
  #with saving functionality
  bpfunc<-function(chem, na.rm = TRUE) {
    nm<-names(chem)
    for (i inName seq_along(nm)) {
      plots<-ggplot(chem, aes_string(x = nm[i])) +
        geom_histogram() 
      ggsave(plots, filename = paste("myplot",  nm[i], ".png", sep = ""))
    }
  }
  #>>>>>>>>>>>>>>>>>>>>  END NEEDS WORK  #>>>>>>>>>>>>>>>>>>>>
  #>>>>>>>>>>>>>>>>>>>>  END NEEDS WORK  #>>>>>>>>>>>>>>>>>>>>
  
  
  
  #>>>>>>>>>>>>>>>>>>>>~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  ___            #
  / __\     (2)   #
  /__\//           #
    / \/  \           #
  \_____/AR PLOTS   #

  #BAR PLOT FUNCTION WITH BASIC FILL (deleted in favor of condclass fill)
  barp <- function(data, x, y) {
    ggplot(data, aes({{x}}, levels = Year, {{y}}, fill = factor(Year))) +
      geom_bar(stat = "identity", position = position_dodge(), fill = "steelblue", colour = "Black") +
      labs(x = "Site", y = c(paste(data$Char_Name, "(",data$Result_Unit,")"))) + #automatic axis labeling
      geom_text(aes(label=Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
      scale_fill_brewer(palette = "Spectral") +
      geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) + #vertical separator lines
      geom_hline(yintercept = data$MRLValue, linetype = "dashed", color = "red") + #MRL horizontal reference line
      theme_classic() +
      theme(panel.background = element_rect(fill = "gray")) + 
      theme(legend.position = "") + #no legend
      theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  }
  
  # ~ ~ ~ TURBIDITY
  bar_turb<-ggplot(turb, aes(x = `StationDes`, levels = Year, y = `Result_Numeric_mod`, fill = factor(Year))) +
    geom_bar(stat = "identity", position = position_dodge(), fill = "steelblue", colour = "Black") +
    coord_cartesian(ylim = c(0, 25)) + #change upper Y limit depending on data range
    labs(x = "Site", y = "Turbidity (NTU)") +
    geom_text(aes(label=Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
    scale_fill_brewer(palette = "Spectral") +
    geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) +
    geom_hline(yintercept = turb$MRLValue, color = "red", linetype = "dashed") + #MRL reference
    theme_classic() +
    theme(panel.background = element_rect(fill = "gray")) + 
    theme(legend.position = "") + #no legend
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(bar_turb)
  
  # ~ ~ ~ CONDUCTIVITY
  bar_cond<-ggplot(cond, aes(x = `StationDes`, levels = Year, y = `Result_Numeric_mod`, fill = factor(Year))) +
    geom_bar(stat = "identity", position = position_dodge(), fill = "steelblue", colour = "Black") +
    labs(x = "Site", y = "Conductivity (uS/cm)") +
    geom_text(aes(label = Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
    #geom_text(data = mean.cond, x = `StationDes`, label = mean.cond$`Result_Numeric_mod`) +
    scale_fill_brewer(palette = "Spectral") +
    geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) +
    theme_classic() +
    theme(panel.background = element_rect(fill = "gray")) + 
    theme(legend.position = "") + #no legend
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(bar_cond)
  
  # ~ ~ ~ AFDM
  bar_afdm<-ggplot(afdm, aes(x = `StationDes`, levels = Year, y = `Result_Numeric_mod`, fill = factor(Year))) +
    geom_bar(stat = "identity", position = position_dodge(), fill = "steelblue", colour = "Black") +
    coord_cartesian(ylim = c(0, 2)) + #change upper Y limit depending on data range
    labs(x = "Site", y = "AFDM (mg/L)") +
    geom_text(aes(label=Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
    scale_fill_brewer(palette = "Spectral") +
    geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) +
    #geom_hline(yintercept = 1, color = "red", linetype = "dashed") + #MRL reference
    theme_classic() +
    theme(panel.background = element_rect(fill = "gray")) + 
    theme(legend.position = "") + #no legend
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(bar_afdm)
  
  # ~ ~ ~ DISSOLVED OXYGEN SATURATION
  bar_do.sat<-ggplot(do.sat, aes(x = `StationDes`, levels = Year, y = `Result_Numeric_mod`, fill = factor(Year))) +
    geom_bar(stat = "identity", position = position_dodge(), fill = "steelblue", colour = "Black") +
    coord_cartesian(ylim = c(50, 120)) + #change y axis lower limit
    labs(x = "Site", y = "DO (%)") +
    geom_text(aes(label=Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
    scale_fill_brewer(palette = "Spectral") +
    geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) +
    theme_classic() +
    theme(panel.background = element_rect(fill = "gray")) + 
    theme(legend.position = "") + #no legend
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(bar_do.sat)
  
  # ~ ~ ~ DISSOLVED OXYGEN CONCENTRATION
  bar_do.conc<-ggplot(do.conc, aes(x = `StationDes`, levels = Year, y = `Result_Numeric_mod`, fill = factor(Year))) +
    geom_bar(stat = "identity", position = position_dodge(), fill = "steelblue", colour = "Black") +
    coord_cartesian(ylim = c(4, 12)) + #change y axis lower limit
    labs(x = "Site", y = "DO (mg/L)") +
    geom_text(aes(label=Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
    scale_fill_brewer(palette = "Spectral") +
    geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) +
    theme_classic() +
    theme(panel.background = element_rect(fill = "gray")) + 
    theme(legend.position = "") + #no legend
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(bar_do.conc)
  
  # ~ ~ ~ pH
  bar_ph<-ggplot(ph, aes(x = `StationDes`, levels = Year, y = `Result_Numeric_mod`, fill = factor(Year))) +
    geom_bar(stat = "identity", position = position_dodge(), fill = "steelblue", colour = "Black") +
    coord_cartesian(ylim = c(6, 9)) + #change Y axis bounds
    labs(x = "Site", y = "pH (SU)") +
    geom_text(aes(label=Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
    scale_fill_brewer(palette = "Spectral") +
    geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) +
    theme_classic() +
    theme(panel.background = element_rect(fill = "gray")) + 
    theme(legend.position = "") + #no legend
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(bar_ph)
  
  # ~ ~ ~ TEMPERATURE
  bar_temp<-ggplot(temp, aes(x = `StationDes`, levels = Year, y = `Result_Numeric_mod`, fill = factor(Year))) +
    geom_bar(stat = "identity", position = position_dodge(), fill = "steelblue", colour = "Black") +
    labs(x = "Site", y = "Water Temperature (?C)") +
    geom_text(aes(label=Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
    scale_fill_brewer(palette = "Spectral") +
    geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) +
    theme_classic() +
    theme(panel.background = element_rect(fill = "gray")) + 
    theme(legend.position = "") + #no legend
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(bar_temp)
  
  #LAB WQ PARAMETERS 
  # ~ ~ ~ TOTAL SUSPENDED SOLIDS
  bar_tss<-ggplot(tss, aes(x = `StationDes`, levels = Year, y = `Result_Numeric_mod`, fill = factor(Year))) +
    geom_bar(stat = "identity", position = position_dodge(), fill = "steelblue", colour = "Black") +
    labs(x = "Site", y = "TSS (mg/L)") +
    geom_text(aes(label=Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
    scale_fill_brewer(palette = "Spectral") +
    geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) +
    geom_hline(yintercept = 1, color = "red", linetype = "dashed") + #MRL reference
    theme_classic() +
    theme(panel.background = element_rect(fill = "gray")) + 
    theme(legend.position = "") + #no legend
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(bar_tss)
  
  # ~ ~ ~ TOTAL PHOSPHORUS
  bar_tp<-ggplot(tp, aes(x = `StationDes`, levels = Year, y = `Result_Numeric_mod`, fill = factor(Year))) +
    geom_bar(stat = "identity", position = position_dodge(), fill = "steelblue", colour = "Black") +
    labs(x = "Site", y = "Total Phosphorus (mg/L)") +
    geom_text(aes(label=Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
    scale_fill_brewer(palette = "Spectral") +
    geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) +
    geom_hline(yintercept = 0.01, color = "red", linetype = "dashed") + #MRL reference
    theme_classic() +
    theme(panel.background = element_rect(fill = "gray")) + 
    theme(legend.position = "") + #no legend
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(bar_tp)
  
  # ~ ~ ~ SULFATE (Bridge and Rock only)
  bar_sulf<-ggplot(sulf, aes(x = `StationDes`, levels = Year, y = `Result_Numeric_mod`, fill = factor(Year))) +
    geom_bar(stat = "identity", position = position_dodge(), fill = "steelblue", colour = "Black") +
    labs(x = "Site", y = "Sulfate (mg/L)") +
    geom_text(aes(label=Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
    scale_fill_brewer(palette = "Spectral") +
    geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) +
    theme_classic() +
    theme(panel.background = element_rect(fill = "gray")) + 
    theme(legend.position = "") + #no legend
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(bar_sulf)
  
  # ~ ~ ~ TOTAL NITROGEN
  bar_tn<-ggplot(tn, aes(x = `StationDes`, levels = Year, y = `Result_Numeric_mod`, fill = factor(Year))) +
    geom_bar(stat = "identity", position = position_dodge(), fill = "steelblue", colour = "Black") +
    labs(x = "Site", y = "Total Nitrogen (mg/L)") +
    geom_text(aes(label=Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
    scale_fill_brewer(palette = "Spectral") +
    geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) +
    theme_classic() +
    theme(panel.background = element_rect(fill = "gray")) + 
    theme(legend.position = "") + #no legend
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(bar_tn)
  
  #testing calculating means, sd, min, max
  means <- aggregate(`Result_Numeric_mod` ~ `MLocID` + `Char_Name`, chem, mean)
  mean.cond <- subset(means, means$`Char_Name`=="Conductivity")
  
  
  #conductivity TEST - conditional formatting based on g, f, p
  cond$condition<-sample(c("Good", "Fair", "Poor"), 61, replace = TRUE, prob = c(0.5, 0.25, 0.25))
  
  cond$condition<-ifelse(refbench$CATEGORY=="1" & refbench$PARAMETER=="Conductivity", cond$`Result_Numeric_mod`<refbench$GOOD_VAL, "Good", "OTHER")
  
  for(i in cond) {
    param<-cond$`Char_Name`[i]
    eco<-cond$eco3[i]
    if param %in% refbench$PARAMETER[which(refbench$GOOD_DIR==">")]}
  
  bar_cond<-ggplot(cond, aes(x = `MLocID`, level = Year, y = `Result_Numeric_mod`, fill = factor(condition))) +
    geom_bar(stat = "identity", position = "dodge", colour = "black") +
    labs(x = "Site", y = "Conductivity (uS/cm)") +
    geom_text(aes(label=Year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
    scale_fill_brewer(palette = "Spectral") +
    geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) +
    theme_classic() +
    theme(panel.background = element_rect(fill = "gray")) + 
    scale_fill_manual(breaks = c("Good", "Fair", "Poor"), values = c("orange", "green4", "red")) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(bar_cond)
  
  
  #subset conductivity benchmarks
  refbenchcond<-subset(refbench, refbench$PARAMETER=="Conductivity")
  
  #create new data frame w/ desired chem columns
  condtest<-data.frame(cond$`Result Value_mod`, cond$eco3, cond$year, cond$`Monitoring Location Name`)
  
  #conditional for matching eco #
  ifelse(cond$eco3==refbenchcond$CATEGORY, "T", "F")
  
  #Travis method
  cond<-cond %>%
    mutate(cond, condclass = case_when(cond$`Result_Numeric_mod`<refbench$GOOD_VAL ~ "Good",
                                       cond$`Result_Numeric_mod`>refbench$POOR_VAL ~ "Poor",
                                       cond$`Result_Numeric_mod`>=refbench$GOOD_VAL & cond$`Result_Numeric_mod`<=refbench$POOR_VAL ~ "Fair",
                                       TRUE ~ "Error"))
  #FROM SH code
  ## Assign Reference status to 
  ref_screen <- bio_stations %>% 
    left_join(stations, by = c('StationDes'= 'StationDes', 'OrgID' = 'OrgID')) %>%
    select(OrgID,StationDes,StationDes,HUC8_Name,HUC12_Name,GNIS_Name,EcoRegion2,EcoRegion3,EcoRegion4,COMID) %>% 
    left_join(met_all, by = 'StationDes') %>% ### probably should add orgID to met file
    mutate(rd_Status = case_when(rdden_km_k < 1.347 ~ 1,
                                 rdden_km_k >3.87 ~ 2,
                                 TRUE ~ 0)) %>%
    mutate(xing_Status = case_when(xings_km2 < 0.241 ~ 1,
                                   xings_km2 >1.013 ~ 2,
                                   TRUE ~ 0)) %>%
    mutate(Ag_Status = case_when(P_AgLand < 0.047 ~ 1,
                                 P_AgLand >2.735 ~ 2,
                                 TRUE ~ 0)) %>%
    mutate(Urb21L_Status = case_when(P_Urban21L < 1.996 ~ 1,
                                     P_Urban21L >7.821 ~ 2,
                                     TRUE ~ 0)) %>%
    mutate(mines_status = case_when(mines_km2 < 0.209 ~ 1,
                                    mines_km2 >26.640 ~ 2,
                                    TRUE ~ 0)) %>% 
    mutate(gmines_status = case_when(grvl_mn_km < 0.00535 ~ 1,
                                     mines_km2 >0.0586 ~ 2,
                                     TRUE ~ 0)) %>%
    mutate(canal_status = case_when(P_canal < 1.7 ~ 1,
                                    mines_km2 >29.2 ~ 2,
                                    TRUE ~ 0))
  
  # create a column for ref "candidate" status, using AREMP 2016 methods
  ref_screen <- ref_screen %>%
    mutate(ref.status_2016 = case_when(
      rd_Status == 1 & xing_Status == 1 & Ag_Status == 1 & Urb21L_Status == 1 & mines_status == 1 & gmines_status == 1 ~ "Ref_CANDIDATE", # meets all 
      rd_Status == 2 & xing_Status == 2 & Ag_Status == 2 & Urb21L_Status == 2 & mines_status == 2 & gmines_status == 2 ~ "Trash", 
      TRUE ~ "Not"))
  
  ref_screen$ref.status_2016 <- as.factor(ref_screen$ref.status_2016)    
  
  
  ref_screen <- ref_screen %>%
    mutate(WorE = case_when(
      EcoRegion3 == '1' | EcoRegion3 == '3' | EcoRegion3 == '4' | EcoRegion3 == '78' ~ "W", 
      TRUE ~ "E"))   
  
  
  ref_screen$WorE <- as.factor(ref_screen$WorE)    
  
  with(ref_screen, table(ref.status_2016, WorE)) # summary table of Ref status by E/W
  with(ref_screen, table(ref.status_2016, EcoRegion3))
  
  #END SH CODE
  
  #need to merge tables so chem has good/poor values and dir?
  #bugs %>% group_by(StationDes, oe_cond) %>%
  #  tally %>%
  #  mutate(percent = n/sum(n)) %>%
  
  #>>>>>>>>>>>>>>>>>>>>  
  #     STACKED       #   #STACK BOX AND BAR PLOTS FOR EACH CHEM PARAMETER
  #>>>>>>>>>>>>>>>>>>>>
  
  # ~ ~ ~ TURBIDITY
  plot_grid(bar_turb + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
            box_turb, ncol = 1)
  ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Chem/Turbidity.jpeg', device = "jpeg", width = 8, height = 10)
  
  # ~ ~ ~ CONDUCTIVITY
  plot_grid(bar_cond + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
            box_cond, ncol = 1)
  ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Chem/Conductivity.jpeg', device = "jpeg", width = 8, height = 10)
  
  # ~ ~ ~ DISSOLVED OXYGEN SATURATION
  plot_grid(bar_do.sat + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
            box_do.sat, ncol = 1)
  ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Chem/DO_Saturation.jpeg', device = "jpeg", width = 8, height = 10)
  
  # ~ ~ ~ DISSOLVED OXYGEN CONCENTRATION
  plot_grid(bar_do.conc + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
            box_do.conc, ncol = 1)
  ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Chem/DO_Concentration.jpeg', device = "jpeg", width = 8, height = 10)
  
  # ~ ~ ~ pH
  plot_grid(bar_ph + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
            box_ph, ncol = 1)
  ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Chem/pH.jpeg', device = "jpeg", width = 8, height = 10)
  
  # ~ ~ ~ TEMPERATURE
  plot_grid(bar_temp + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
            box_temp, ncol = 1)
  ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Chem/Temperature.jpeg', device = "jpeg", width = 8, height = 10)
  
  # ~ ~ ~ TOTAL SUSPENDED SOLIDS
  plot_grid(bar_tss + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
            box_tss, ncol = 1)
  ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Chem/Total_Suspended_Solids.jpeg', device = "jpeg", width = 8, height = 10)
  
  # ~ ~ ~ TOTAL PHOSPHORUS
  plot_grid(bar_tp + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
            box_tp, ncol = 1)
  ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Chem/Total_Phosphorus.jpeg', device = "jpeg", width = 8, height = 10)
  
  # ~ ~ ~ SULFATE (Bridge and Rock only)
  plot_grid(bar_sulf + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
            box_sulf, ncol = 1)
  ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Chem/Sulfate.jpeg', device = "jpeg", width = 8, height = 10)
  
  # ~ ~ ~ TOTAL NITROGEN
  plot_grid(bar_tn + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
            box_tn, ncol = 1)
  ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Chem/Total_Nitrogen.jpeg', device = "jpeg", width = 8, height = 10)
  
  
  #testing desc stats
  describeBy(chem, list(chem$`Monitoring Location Name`, chem$`Characteristic Name`, chem$`Result Value_mod`, mat = TRUE))
  
  
  
  oetest<-mutate(bugs$oe_cond, prop = n/sum(n))
  
  bar_oestack2<-ggplot(bugs, aes(x = StationDes, fill = oe_cond)) +
    geom_bar(aes(position = "fill", stat = "identity", colour = "black")) + 
    labs(title = "O/E Condition Classes by Site, 1992-2018", x = "Site", y = "Percentage") +
    geom_text(aes(label = year), position = position_stack(vjust = 0.5), size = 2.5) +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank()) +
    scale_fill_manual(values = c('#0066ff', '#009900', '#ff9900', '#e62e00')) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.25))
  print(bar_oestack2)
  
  #getting closer but geom_text doesn't work still - may need to hand calc proportions
  bar_oestack3<-ggplot(bugs, aes(x = factor(StationDes), levels = year)) +
    geom_bar(aes(fill = factor(oe_cond)), position = "fill", colour = "black") +
    labs(x = "Site", y = "Percentage") + 
    scale_y_continuous(labels = scales::percent) +
    #geom_text(aes(label = year), position = position_stack(vjust = 0.5)) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank()) +
    scale_fill_manual(values = c('#0066ff', '#009900', '#ff9900', '#e62e00')) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.25))
  print(bar_oestack3)
  
  
  #o/e facet wrap to examine oe_cond through the years
  bar_oe3<-ggplot(bugs, aes(x = year, level = StationDes, y = `OoverE_extract`, fill = factor(oe_cond))) +
    geom_bar(stat = "identity", position = "dodge", colour = "black") +
    labs(x = "Year", y = "O/E") +
    geom_hline(yintercept = 1.0, color = "blue", linetype = "dashed") +
    #geom_text(aes(label = year), position=position_dodge(width = 0.9), hjust = -0.25, vjust = 0.5, angle = 90, size = 3, check_overlap = TRUE) +
    geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5), size = 0.5) +
    theme_classic() +
    theme(panel.background = element_rect(fill = "gray")) +
    scale_fill_manual(values = c("#0066ff", "#009900", "#ff9900", "#e62e00")) +
    facet_wrap(~ oe_cond) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5))
  print(bar_oe3)
  ggsave('//deqlab1/biomon/Projects/Biomon Redux/Trend Sites/R/R_Output/Bugs/O_over_E_FacetWrap_OE.jpeg', device = "jpeg", width = 12, height = 10)
  
=======
