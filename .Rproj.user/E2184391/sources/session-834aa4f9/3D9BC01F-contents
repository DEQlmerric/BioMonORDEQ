###                                                                          ###  
### This script reads in a single set or batch of coordinates and determines ### 
### the appropriate COMID based on the NHDPlus Medium Resolution layer. This ###
### script is meant to help the BioMon crew more easily identify the COMID   ###
### of newly created sites. The batch_COMID_from_arcpy.R script is the same  ###
### but can be run from ArcGIS Pro. Any changes in one should be made to the ###
### other.                                                                   ###
###                                                                          ###

# Read in the libraries and datafiles needed
library(nhdplusTools)
library(sf)
library(tidyverse)

input_csv <- "//deqlab1/Assessment/AWQMS/Stations/COMID output/final_today_coords.csv"
output_csv <- "//deqlab1/BioMon/Projects/Biomon Redux/2026/COMID_update.csv"

# Read the coordinates from CSV
coords <-  ref_sta |>
  #read.csv(input_csv) |>
  select("orgid", "STATION_KEY", "StationDes", "Lat_DD", "Long_DD", "Datum") #|>
  
# Map Datum codes to EPSG (coordinate system numbers)
coords <- coords |>
  mutate(
    EPSG = case_when(
      Datum %in% c(1, "NAD27") ~ 4267,
      Datum %in% c(2, "NAD83") ~ 4269,                       
      Datum %in% c(16, "WGS84") ~ 4326,                      
      Datum %in% c(4, "Unknown") ~ 4326,
      TRUE        ~ 4326
    )
  )

# Group by EPSG, create a simple feature for each group, and transform to EPSG:4326 (WGS84)
points_sf <- coords |>
  group_split(EPSG) |>
  map(~ { # Loops through the different EPSGs in the dataset and converts them to 4326 (WGS84) which is what nhdplustools needs
    epsg_val <- unique(.x$EPSG)
    message(sprintf("Processing %d points in EPSG:%d", nrow(.x), epsg_val))
    
    .x |>
      st_as_sf(coords = c("Long_DD", "Lat_DD"), crs = epsg_val) |> # converts coordinate columns into a point (simple feature) so nhdplustools can interpret
      st_transform(4326) # transforms the epsg_val into 4326 (WGS84)
  }) |>
  (\(x) do.call(rbind, x))()

#Retrieve COMIDs from NHDplus and out put in a vector
comid_lookup <- points_sf |>
  distinct(STATION_KEY, .keep_all = TRUE)

comids <- comid_lookup |>
  mutate(COMID = map_dbl(seq_len(nrow(comid_lookup)), function(i) {
  pt_coords <- st_coordinates(comid_lookup[i, ])
  message(sprintf("Processing point %d: X=%f, Y=%f", i, pt_coords[1], pt_coords[2]))

  tryCatch( # The tryCatch function allows for the success/warning/error messages to be included
    {
      comid_value <- discover_nhdplus_id(comid_lookup[i, ]) # This is the important line for COMIDs
      if (is.na(comid_value) || length(comid_value) == 0) {
        message(sprintf("  ⚠ No COMID found for point %d (likely outside NHD coverage)", i))
        return(NA_real_)
      } else {
        message(sprintf("  ✅ COMID %s assigned to point %d", comid_value, i))
        return(as.numeric(comid_value))
      }
    },
    error = function(e) {
      message(sprintf("  ❌ Error for point %d: %s", i, e$message))
      return(NA_real_)
    }
  )
})) |>
  st_drop_geometry() |>
  select(STATION_KEY, COMID)

# Combine the input list of stations with the list of COMIDs
results <- coords |>
  left_join(comids, by = "STATION_KEY","orgid")

# Save to CSV
write.csv(results, output_csv, row.names = FALSE)
