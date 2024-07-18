library(BCGcalc)
library(BioMonTools)
library(readxl)

# Join to master taxa table



calculate_metrics <- function(bug_data){
  
  df <- bug_data
  
  
  df_bugs_taxa <- df |> 
    dplyr::left_join(BioMonTools::TaxaMaster_Ben_BCG_PacNW,
                     by = c('OTU_MetricCalc' = 'TaxaID'))
  

  
  
  
  # Get NHD info ----------------------------------------------------------------------------------------------------
  
  source('bugs analyses/All_together/get_NHD_info.R')
  
  bug_tax_nhd <- get_NHD_info(df_bugs_taxa)
  
  
  
  # limit data to only whats needed ---------------------------------------------------------------------------------

  
  
  
  BCG_Bug_data <- bug_tax_nhd |> 
    transmute(SampleID = act_id,
              Area_mi2 = NA_integer_,
              SurfaceArea = NA_integer_,
              TaxaID = Taxon, #is this correct???
              N_Taxa = Result_Numeric,
              Index_Name = 'BCG_PugLowWilVal_500ct',
              INDEX_CLASS = str_to_title(SITE_TYPE),
              NonTarget,
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
  
  #limit data to only whats needed ---------------------------------------------------------------------------------
    bugs.excluded <- markExcluded(BCG_Bug_data, TaxaLevels = c("Kingdom", "Phylum",
                                                              "SubPhylum", "Class", "SubClass", "Order", "SubOrder", "SuperFamily", 
                                                              "Family", "SubFamily", "Tribe", "GenusGroup", "Genus", "SubGenus", "SpeciesGroup",
                                                              "SpeciesSubGroup", "SpeciesComplex", "Species"))
  
  
  # Extra columns to keep in results
  keep.cols <- c("Area_mi2"
                 , "SurfaceArea"
                 , "DENSITY_M2"
                 , "Density_ft2."
                 , "Site_Type")
  # Run Function
  df.metrics <- metric.values(bugs.excluded, "bugs", fun.cols2keep = keep.cols, boo.Shiny = TRUE)
  
  
}
