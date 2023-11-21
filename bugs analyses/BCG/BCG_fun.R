#
# BCG code for use within Biomon_R scoring of metrics and indexes

# Author: Shannon Hubler, 3/6/2023
    # Based on scripts written by Erik Leppo, Tetra Tech
    # https://github.com/leppott/BCGcalc



# To install the current version use the code below to install from GitHub. The use of “force = TRUE” ensures the package is installed even if already present. If the package remotes is missing the code below will install it.

if(!require(remotes)){install.packages("remotes")}  #install if needed

install_github("leppott/BCGcalc", force=TRUE, build_vignettes=TRUE)



# bring package into workspace
library(BCGcalc)


library(BCGcalc)
library(readxl)
library(reshape2)
library(knitr)
library(BioMonTools)

# bring in taxonomy table from BiomonTools
url_bmt_base <- 'https://github.com/leppott/BioMonTools_SupportFiles/raw/main/data'
url_taxa_official_pick <- file.path(url_bmt_base, "taxa_official", "ORWA_TaxaTranslator_20230228.csv")
taxonomy <- read.csv(url_taxa_official_pick)

# bring in attributes table from BiomonTools
url_bmt_base <- 'https://github.com/leppott/BioMonTools_SupportFiles/raw/main/data'
url_taxa_official_pick <- file.path(url_bmt_base, "taxa_official", "ORWA_Attributes_20230228.csv")
attributes <- read.csv(url_taxa_official_pick)


#####
  #######
#             Format ODEQ bug data to work with BCG code
  #######
#####
bugs.bcg <- b_t_s %>%
  select(Sample, Taxon, Count, Result_Unit, UniqueTaxon) %>%
  dplyr::rename(SampleID = 'Sample', Taxon_orig = 'Taxon')

bugs.bcg <- bugs.bcg %>%
  left_join(taxonomy, by ='Taxon_orig')

bugs.bcg <- bugs.bcg %>%
  left_join(attributes, by=c('OTU_BCG_MariNW' = 'Taxon_AttributeTable'))
  
          # df.samps.bugs <- read_excel(system.file("./extdata/Data_BCG_PugLowWilVal.xlsx"
          #                                      , package="BCGcalc"))

> colnames(df.samps.bugs)
[1] "Index_Name"        "SampleID"          "INDEX_CLASS"       "Area_mi2"         
[5] "SurfaceArea"       "TaxaID"            "N_Taxa"            "Exclude"          
[9] "NonTarget"         "BCG_Attr"          "Phylum"            "SubPhylum"        
[13] "Class"             "SubClass"          "Order"             "SuperFamily"      
[17] "Family"            "SubFamily"         "Tribe"             "Genus"            
[21] "SubGenus"          "Species"           "Thermal_Indicator" "FFG"              
[25] "Habit"             "Life_Cycle"        "TolVal"            "SampID"           
[29] "Type_pSLOPE"       "Clinger"           "Year"              "Month"            
[33] "CollDate"          "CollMeth"          "RepNum"            "ExerciseID"       
[37] "StationID"         "NHD_pSLOPE"        "Density_m2"        "Density_ft2"      
[41] "Long_Lived"        "LongLived"         "NoteWorthy"        "FFG2"             
[45] "TolVal2"           "BCG_Attr2"         "Latitude"          "Longitude"        
[49] "COMID"             "AirBreather"    



  
myDF <- df.samps.bugs

# Columns to keep
myCols <- c("Area_mi2", "SurfaceArea", "Density_m2", "Density_ft2")

# Metrics to Keep
met2keep <- c("ni_total", "nt_total", "nt_BCG_att1i2", "pt_BCG_att1i23"
              , "pi_BCG_att1i23", "pt_BCG_att56", "pi_BCG_att56"
              , "nt_EPT_BCG_att1i23", "pi_NonInsJugaRiss_BCG_att456"
              , "pt_NonIns_BCG_att456", "pi_NonIns_BCG_att456", "nt_EPT")

# Run Function
df.metric.values.bugs <- metric.values(myDF, "bugs", fun.MetricNames=met2keep
                                       , fun.cols2keep=myCols)

# View Results
#View(df.metric.values.bugs)
kable(head(df.metric.values.bugs), caption="Selected metric results")




# BCG Level Assignment

df_rules <- readxl::read_excel(system.file("extdata/Rules.xlsx"
                                           , package="BCGcalc")
                               , sheet="Rules") 

# Calculate Metric Memberships
df_met_memb <- BCG.Metric.Membership(df.metric.values.bugs, df_rules)

# Calculate Level Memberships
df_lev_memb <- BCG.Level.Membership(df_met_memb, df_rules)

# Run Function
df_Levels <- BCG.Level.Assignment(df_lev_memb)

# QC Checks (flags)
#
# Import Checks
df_checks <- readxl::read_excel(system.file("extdata/MetricFlags.xlsx"
                                            , package="BCGcalc")
                                , sheet="Flags") 

# Run Function
df_flags <- BioMonTools::qc.checks(df.metric.values.bugs, df_checks)
# Change terminology; PASS/FAIL to NA/flag
df_flags[, "FLAG"][df_flags[, "FLAG"] == "FAIL"] <- "flag"
df_flags[, "FLAG"][df_flags[, "FLAG"] == "PASS"] <- NA

# long to wide format
df_flags_wide <- reshape2::dcast(df_flags
                                 , SAMPLEID ~ CHECKNAME
                                 , value.var="FLAG")
# Calc number of "flag"s by row.
df_flags_wide$NumFlags <- rowSums(df_flags_wide == "flag", na.rm = TRUE)
# Rearrange columns
NumCols <- ncol(df_flags_wide)
df_flags_wide <- df_flags_wide[, c(1, NumCols, 2:(NumCols - 1))]

# Merge Levels and Flags
df_lev_flags <- merge(df_Levels
                      , df_flags_wide
                      , by.x = "SampleID"
                      , by.y = "SAMPLEID"
                      , all.x = TRUE)

# Summarize Results
table(df_flags[, "CHECKNAME"], df_flags[, "FLAG"], useNA = "ifany")
table(df_lev_flags$BCG_Status)

# Show Results
# View(df_lev_flags)

# Save Results
write.csv(df_lev_flags, file.path(tempdir(), "Level_Flags.csv"))

# # Summary Report
# strFile.RMD <- system.file(paste0("rmd/Results_Summary.Rmd")
#                              , package = "BCGcalc")
# strFile.RMD.format <- "html_document"
# strFile.out <- "_bcgcalc_RESULTS.html"
# dir.export <- tempdir()
# rmarkdown::render(strFile.RMD
#                    , output_format = strFile.RMD.format
#                    , output_file = strFile.out
#                    , output_dir = dir.export
#                    , quiet = TRUE)

