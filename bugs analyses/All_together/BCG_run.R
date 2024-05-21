library(BCGcalc)
library(BioMonTools)
library(readxl)

# Join to master taxa table

df <- bug_tax_data_filtered


df_bugs_taxa <- df |> 
  dplyr::left_join(BioMonTools::TaxaMaster_Ben_BCG_PacNW,
                   by = c('Taxon' = 'TaxaID'))

source('bugs analyses/All_together/get_NHD_info.R')




# Get NHD info ----------------------------------------------------------------------------------------------------


bug_tax_nhd <- get_NHD_info(df_bugs_taxa)


# Get streamcat info ----------------------------------------------------------------------------------------------

# 
# comidID <- unique(bug_tax_nhd$COMID)
# 
# comidID <- comidID[!is.na(comidID)]

#streamcatdata <- get_streamcat(comidID, type = 'BCG') 


BCG_Bug_data <- bug_tax_nhd |> 
  transmute(SampleID = act_id,
            INDEX_CLASS = str_to_title(SITE_TYPE),
            Area_mi2 = NA_integer_,
            SurfaceArea = NA_integer_,
            TaxaID = Taxon, #is this correct???
            N_Taxa = Result_Numeric,
            Exclude = case_when(UniqueTaxon == 'UniqueTaxon' ~ FALSE,
                                UniqueTaxon == 'AmbiguousTaxon' ~ TRUE), #Is this correct??
            NonTarget,
            Index_Name = 'BCG_PugLowWilVal_500ct',
            SITE_TYPE, 
            Phylum, 
            SubPhylum,
            Class, 
            SubClass,
            Order,
            SuperFamily,
            Family,
            Tribe,
            Genus,
            SubGenus, 
            Species,
            BCG_Attr,
            FFG,
            Habit,
            Life_Cycle,
            Thermal_Indicator,
            TolVal,
            INFRAORDER = NA_character_,
            HABITAT = NA_character_,
            ELEVATION_ATTR = NA_character_,
            GRADIENT_ATTR = NA_character_,
            WSAREA_ATTR = NA_character_,
            HABSTRUCT = NA_character_,
            UFC = NA_integer_,
            Density_ft2. = NA_integer_,
            DENSITY_M2 = NA_integer_,
            
            )

# Add missing columns

# 1.A. Calculate Metrics
# Extra columns to keep in results
keep.cols <- c("Area_mi2"
               , "SurfaceArea"
               , "DENSITY_M2"
               , "Density_ft2."
               , "Site_Type")
# Run Function
df.metrics <- metric.values(BCG_Bug_data, "bugs", fun.cols2keep = keep.cols)

# QC
dim(df.metrics)
View(df.metrics)            
            
write.table(df.metrics
            , "Metric.Values.Test.tsv"
            , col.names=TRUE
            , row.names=FALSE
            , sep="\t")       



# 1.B. Metric Membership
# Import Rules
df.rules <- read_excel("C:/Users/tpritch/AppData/Local/R/win-library/4.2/BCGcalc/extdata/Rules.xlsx"
                       , sheet="Rules") #clean this up
# Run function
df.Metric.Membership <- BCG.Metric.Membership(df.metrics, df.rules)
# Show Results
View(df.Metric.Membership)
# Save Results
write.table(df.Metric.Membership, "Metric.Membership.Test.tsv"
            , row.names=FALSE, col.names=TRUE, sep="\t")

# 1.C. Level Assignment
# Run Function
df.Level.Membership <- BCG.Level.Membership(df.Metric.Membership, df.rules)

# 1.D. Level Membership
# Run Function
df.Levels <- BCG.Level.Assignment(df.Level.Membership)

# 1.E. Flags
# Import QC Checks
df.checks <- read_excel("C:/Users/tpritch/AppData/Local/R/win-library/4.2/BCGcalc/extdata/MetricFlags.xlsx"
                        , sheet="Flags") 
# Run Function


# Run Function
df.flags <- qc.checks(df.metrics, df.checks)
# Change terminology; PASS/FAIL to NA/flag
df.flags[,"FLAG"][df.flags[,"FLAG"]=="FAIL"] <- "flag"
df.flags[, "FLAG"][df.flags[,"FLAG"]=="PASS"] <- NA
# long to wide format
df.flags.wide <- reshape2::dcast(df.flags, SAMPLEID ~ CHECKNAME, value.var="FLAG")
# Calc number of "flag"s by row.
df.flags.wide$NumFlags <- rowSums(df.flags.wide=="flag", na.rm=TRUE)
# Rearrange columns
NumCols <- ncol(df.flags.wide)
df.flags.wide <- df.flags.wide[, c(1, NumCols, 2:(NumCols-1))]
# Merge Levels and Flags
df.Levels.Flags <- merge(df.Levels, df.flags.wide,by.x="SampleID", by.y="SAMPLEID", all.x=TRUE)