# Authors: Lesley Merrick, Shannon Hubler, Travis Pritchard

# 12.14.2020

# pull bug count data from AWQMS (using AWQMSdata package)
# pull Taxonomy table (from Biomon SQL dbase) - 8/3/23 lives in R repository 
# pull Stations table (from Station SQL dbase) - 8/3/23 station info comes from SQL data view
# Format data for analyses
#limit fields to only those necessary, 
#rename as needed to match existing models


# 5.4.2023- Travis Pritchard
# Code modified use AWQMSdata package instead of straight query from SQL
# function inputs (station, startdate, endate, AU_ID) can be used to limit datapull
# Leave as NULL, or do not use to pull entire views
# taxonomy_table input can be used to change taxonomy table used. Default for now is ODEQ_Taxonomy_dec22.xlsx
#   which is stored with Github repo
# Function does not hit stations database anymore, gets station data from AWQMS view

data.raw.AWQMS <- function(station = NULL,
                           startdate = NULL,
                           enddate = NULL,
                           AU_ID = NULL,
                           taxonomy_table = 'bugs analyses/Taxonomy/ODEQ_Taxonomy_dec22.xlsx',
                           no_metrics = FALSE)   {
  
  
  #function testing- run these if you are testing inside the function

  # station = NULL
  # startdate = NULL
  # enddate = NULL
  # AU_ID = NULL
  # taxonomy_table = 'bugs analyses/Taxonomy/ODEQ_Taxonomy_dec22.xlsx'
  # no_metrics = TRUE
  

  #Setup AWQMSdata parameters
  #the AWQMSdata package needs a start date to run. It defaults to '1949-09-15'
  #if no start datae given, use start date
  
  options(dplyr.summarise.inform = FALSE)
  startdate <- dplyr::if_else(is.null(startdate), '1949-09-15', startdate)
  
  

## Pulling raw data from AWQMS backend view via the AWQMSdatapackage
  print("Query data from AWQMS")
  
  bugs_raw <- AWQMSdata::AWQMS_Raw_Macros(station = station,
                                          startdate = startdate,
                                          enddate = enddate,
                                          AU_ID = AU_ID) 
 
# should represent unique samples # 
# act_id field joins raw bug data to index and metrics
  
  if(no_metrics){
    
    bugs_indexes <- AWQMSdata::AWQMS_Bio_Indexes(station = station,
                                                 startdate = startdate,
                                                 enddate = enddate,
                                                 AU_ID = AU_ID)
    bugs_metrics <- AWQMSdata::AWQMS_Bio_Metrics(station = station,
                                                 startdate = startdate,
                                                 enddate = enddate,
                                                 AU_ID = AU_ID)
    
    print("Data Query Complete")
    
    act_raw <- bugs_raw %>% 
      dplyr::select(org_id,act_id) %>%
      dplyr::distinct()
    
    act_met <- bugs_metrics %>% 
      dplyr::select(MLocID,act_id) %>% 
      dplyr::distinct()
    
    #### join by activity - to samples without metric and indexes in AWQMS ###
    bugs <- act_raw %>%
      dplyr::left_join(act_met,by= 'act_id') %>% 
      dplyr::filter(is.na(MLocID)) %>% # filters out bugs samples with no summary metrics
      dplyr::select(act_id) %>% #get table of just actvity IDs
      dplyr::left_join(bugs_raw, by = 'act_id') #join back to bugs_raw to only get bugs_raw without summary metrics
    
  } else {
    print("Data Query Complete")
    bugs <- bugs_raw
  }
  
  bugs.lim <- bugs %>%
    dplyr::select(
      org_id,
      act_id,
      MLocID,
      StationDes,
      Lat_DD,
      Long_DD,
      Predator_WorE,
      EcoRegion2,
      EcoRegion3,
      ELEV_Ft,
      precip_mm,
      temp_Cx10,
      #HUC6_Name,
      HUC8_Name,
      #HUC10_Name,
      HUC12_Name,
      Wade_Boat,
      COMID,
      Project1,
      SampleStart_Date,
      Sample_Method,
      Activity_Type,
      DEQ_Taxon,
      StageID,
      Result_Numeric,
      Result_Unit,
      UniqueTaxon,
      Result_Status) %>% 
    dplyr::rename(DEQ_TAXON = DEQ_Taxon)
  
  #change columns to factors so can extract levels
  factor_columns <- c("act_id",
                      "MLocID",
                      "StationDes",
                      "Project1",
                      "Sample_Method",
                      "Activity_Type",
                      "DEQ_TAXON",
                      "StageID",
                      "Result_Unit")
  
  bugs.lim <- bugs.lim %>%
    dplyr::mutate_at(factor_columns, as.factor)
  
  
  #split into count and density
  # count = ALL samples
  # density = incomplete
  bugs.count <- bugs.lim[bugs.lim$Result_Unit == 'count',]
  bugs.density <- bugs.lim[bugs.lim$Result_Unit == '#/ft2',]
  
  #########
  
  #     SAMPLE INFO Table
  
  #########
  
  # calculate and add total abundance
  sample.info <- bugs.count %>%
    dplyr::group_by(act_id, MLocID, StationDes, Project1, SampleStart_Date, Sample_Method, 
                    Activity_Type) %>%
    dplyr::summarize(tot.abund= sum(Result_Numeric))
  
  
  ################################
  
  # Bug count table
  
  ################################
  
  taxonomy <- readxl::read_excel(taxonomy_table)
  
  #limit columns to what is necessary
  taxonomy <- taxonomy %>%
    dplyr::select(DEQ_TAXON, Taxon, OTU_RIV_24, OTU_Stress05,  Phylum, Class, SubClass, Order, 
                  SubOrder, 'Super Family', Family, SubFamily, Tribe, Genus, SubGenus, SpeciesGroup, SpeciesComplex, 
                  Species, SubSpecies, MTI, Voltine, FFG)
  
  taxonomy$DEQ_TAXON <- as.factor(taxonomy$DEQ_TAXON)
  
  ###  
  
  # join bugs and taxonomy tables
  b_t <- bugs.count %>% 
    dplyr::left_join(taxonomy, by = c('DEQ_TAXON')) 
  
  
##############################
  
  
  ## add Stations info
  
##############################
  
  #need to add station key to AWQMS view
  b_t_mod <- b_t %>%
    dplyr::mutate(ELEV_m = ELEV_Ft*0.3048) %>% 
    dplyr::rename(#STATION_KEY = station_key, 
      lat = Lat_DD, 
      long = Long_DD, 
      W_E = Predator_WorE,
      Eco2 = EcoRegion2, Eco3 = EcoRegion3) %>% 
    dplyr::mutate(Eco2 = dplyr::case_when(Eco2 == "MARINE WEST COAST FOREST" ~ "MWCF",
                                          Eco2 == "WESTERN CORDILLERA" ~ "WC",
                                          TRUE ~ Eco2)) %>% 
    dplyr::rename(Count=Result_Numeric, Sample=act_id, Date=SampleStart_Date, Habitat=Sample_Method)
  
  
  # join stations table to bugs + taxonomy
  
  b_t_s <- b_t_mod 
  
  
  if(no_metrics){
    print("Only returning samples with no metrics or indexies calculated and stored in AWQMS")
  }
  
  return(b_t_s)  
  
}

# end of function
  