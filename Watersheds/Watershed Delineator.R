#
#    __    __   ____  ______    ___  ____    _____ __ __    ___  ___   
#   |  |__|  | /    ||      |  /  _]|    \  / ___/|  |  |  /  _]|   \  
#   |  |  |  ||  o  ||      | /  [_ |  D  )(   \_ |  |  | /  [_ |    \ 
#   |  |  |  ||     ||_|  |_||    _]|    /  \__  ||  _  ||    _]|  D  |
#   |  `  '  ||  _  |  |  |  |   [_ |    \  /  \ ||  |  ||   [_ |     |
#    \      / |  |  |  |  |  |     ||  .  \ \    ||  |  ||     ||     |
#     \_/\_/  |__|__|  |__|  |_____||__|\_|  \___||__|__||_____||_____|
                                                                   
#    ___      ___  _      ____  ____     ___   ____  ______   ___   ____  
#   |   \    /  _]| |    |    ||    \   /  _] /    ||      | /   \ |    \ 
#   |    \  /  [_ | |     |  | |  _  | /  [_ |  o  ||      ||     ||  D  )
#   |  D  ||    _]| |___  |  | |  |  ||    _]|     ||_|  |_||  O  ||    / 
#   |     ||   [_ |     | |  | |  |  ||   [_ |  _  |  |  |  |     ||    \ 
#   |     ||     ||     | |  | |  |  ||     ||  |  |  |  |  |     ||  .  \
#   |_____||_____||_____||____||__|__||_____||__|__|  |__|   \___/ |__|\_|
                                           
#Made with love by Travis, adapted poorly by Adam

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1 - LOAD PACKAGES
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(nhdplusTools)
library(sf)
library(tidyverse)
library(mapview) #for viewing intermediate map products
library(openxlsx)
library(AWQMSdata)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 2 - IMPORT DATA
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Several different import options exist depending on the format, size, and variables contained in the original dataset.
# Use ONE of the following data import options below.
# If wanting to draw a watershed boundary for a single site, using https://streamstats.usgs.gov/ss/ will be easiest.



#??????????????????????????????????????????????????????????????????????????????????????????????????????????????????
# EXPERIMENTING WITH USER-DEFINED INPUT VALUES SIMILAR TO VOLMON SCRIPT

#Analyst Name
analyst <- "Travis McKewl"

#Set input directory where files come from
input_dir <- "//deqlab1/Biomon/FillInFilePath/FileName"

#Directory where files are saved to
output_dir <-"//deqlab1/Biomon/FillInFilePath/FileName"

####????????? COULD WE DEFINE INPUT FILE NAME FOR A VARIETY OF FORMATS HERE? USER WOULD RUN ONE DEPENDING ON DATA SOURCE.
#Data upload file - Choose 1 of the 3 options
#GIS Shapefile
file_input <- "FillInFileName.shp"

#XLSX
file_input <- "FillInFileName.xlsx"

#CSV
file_input <- "FillInFileName.csv"
#??????????????????????????????????????????????????????????????????????????????????????????????????????????????????


#-------------------------------------------
# OPTION A: GIS SHAPEFILE
#-------------------------------------------
willy<-st_read("C:/Users/athomps/OneDrive - Oregon/Desktop/New folder/willy.shp") #change name and file path as needed

#lookup crs and paste value below
st_crs(willy)

#put lat/longs into their own columns & generate comids based on lat/longs
willy<-willy %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>%
  mutate(comid2 = generatecomids(long, lat, refsystem = 4269))

#Create dataframe of the COMIDs in question ---- NO LONGER NEEDED
comid <- data.frame(comid = willy$ComID)


#-------------------------------------------
# OPTION B: EXCEL FILE
#-------------------------------------------
# if data is already in Excel and you don't have COMID, choose either Option B or C then run the subsequent lines of code.
# Will need, at minimum, lat/longs and a coordinate ref system (for when lat/longs were generated - official OR is 2992 (ft) and 2991 (m))
# Before proceeding, manually enter the coordinate reference system numbers in new "CRS" column in upload file (this is an existing field in stations db)
latlonginput <- read.xlsx("C:/Users/athomps/OneDrive - Oregon/Desktop/New folder/WillyTable.xlsx") #change name and file path as needed


#-------------------------------------------
# OPTION C: CSV FILE
#-------------------------------------------
latlonginput <- read.csv("C:/Users/athomps/OneDrive - Oregon/Desktop/New folder/LatLongImportTest.csv")


# WORK IN PROGRESS - FUNCTION APPROACH
generatecomids <- function(long, lat, refsystem) {
 points <- st_sfc(st_point(c(long, lat), crs = refsystem)) #draws on CRS entered in Excel file
 getcomid <- discover_nhdplus_id(points)
 return(getcomid)
}

latlongcomid <- latlonginput |>
  mutate(CRS = 4326) |> #another way of defining CRS in the absence of a pre-defined crs value in the input file
  mutate(comid = generatecomids(long = Long, lat = Lat, refsystem = CRS)) #change values after = sign to match the column names in upload file

mapview(points)


#-------------------------------------------
# OPTION D: SINGLE LAT/LONG
#-------------------------------------------
# CHANGE LAT/LONG AND CRS
start_point <- sf::st_sfc(sf::st_point(c(-122.8025, 43.8578)),
                          crs = 4326)


#-------------------------------------------
# OPTION E: STATIONS DB
#-------------------------------------------
stations <- query_stations()





# WORK IN PROGRESS - NEW DATAFRAME APPROACH FROM TEAMS CHAT
dataframe_comid <- latlonginput %>%
  rowwise() %>%
  mutate(points = list(sf::st_sfc(sf::st_point(c(Long,Lat)), crs = 4326))) |> 
  mutate(comid = discover_nhdplus_id(points)) |> 
  select(-points)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 3 - CREATE TEMP .GPKG FILE TO RECEIVE DATA FROM SUBSET_NHDPLUS
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
subset_file <- tempfile(fileext = ".gpkg")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 4 - RUN FUNCTION TO DELINEATE WATERSHEDS FOR EACH RECORD
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#--- Start of function ---#
delineate_comid_watersheds <- function(comid, ofid2, random, shp_file){

#Get flowline -- Travis: If this is needed later on in process also, we can figure that out
flowlines <- navigate_nldi(list(featureSource = "comid", 
                                featureID = comid), 
                           mode = "upstreamTributaries", 
                           distance_km = 1000)

#Get nhdplus files
subset <- subset_nhdplus(comids = as.integer(flowlines$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, 
                         overwrite = TRUE)

#Get catchment files
catchment <- sf::read_sf(subset_file, "CatchmentSP")

#Dissolve files to single catchment
catchment <- sf::st_union(catchment)

#Convert to sf object and add an attribute so we can associate the catchment with the comid used to generate
# if we need any other attributes, this is where to put it.
###  Change the mutate function here to include whatever you want to pass to the shp file
catchment2  <- st_sf(catchment)|> 
  mutate(orig_comid = comid,
         ofid= ofid2,
         rand = random) |> #delete "|>" if removing line below
  st_transform(crs=2992)  #reproject to NAD83 / Oregon GIC Lambert (ft). Change depending on what projection you need in the next step. Or remove

#Write to shp file. Create new file if it doesn't exists, append if it does
st_write(catchment2, shp_file, append = TRUE)

}
#--- End of function ---#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 5 - QC R-ASSIGNED COMIDS IN GIS
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Follow steps in section 2.3 from Reference Selection SOP to make sure that the COMIDs generated in R are accurate.
# Add code to overwrite wrong COMIDs with the right ones. Something like:
input$comid[input$comid=="123456"] <- "456789"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 6 - WRITE WATERSHED DELINEATIONS TO GIS SHAPEFILE
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
start.time <- Sys.time()

#process for mapping over dataframe with multiple arguments

#### Change the arguments here to what you want to pass to the shp file. The numbers after the dots refer to the column position

#don't forget to change 'test' to the dataframe from above and to set the shp file location and name. 

#version 1
purrr::map(comids, ~ delineate_comid_watersheds(.,shp_file = "C:/Users/athomps/OneDrive - Oregon/Desktop/New folder/delintestall.shp"))

#version 2
purrr::pmap(test, ~delineate_comid_watersheds(comid = {..1}, random = {..2}, ofid2 = {..3}, shp_file = "C:/Users/athomps/OneDrive - Oregon/Desktop/New folder/delintestall.shp"))


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#calculate watershed areas
#area is calculate in sq meters here. You can convert if needed
catchment2 <- st_sf(catchment)|> 
  mutate(orig_comid = comid) 

catchment2 <- catchment2|> 
  mutate(area = st_area(catchment2))








######################################################################################

########################################### ARCHIVED CODE ############################

######################################################################################

# METHOD 2 - Load COMIDs via dataframe
### Replace this with some dataframe of comids and other information you want to pass to the shp file
test <- data.frame(
  ORIG_FID = c(10765L,10766L,11037L,11171L,11520L,
               11528L,11575L,11576L,11689L,11690L,11691L,11695L,11779L,
               11780L,11782L,11789L,12090L,12091L,16652L,16653L,16699L,
               16701L,16722L,16723L,16724L,16725L,16741L,16742L,16743L,
               16963L,16964L,16966L,16968L,16972L,16976L,16977L,16980L,
               16981L,16986L,16988L,16989L,16990L,16991L,16993L,16994L,
               16995L,16997L,16998L,18021L,18022L,18023L,18130L,
               18131L,18132L,18133L,18134L,18135L,18136L,18137L,18180L,
               18335L,18336L,18342L,18347L,18358L,18374L,18386L,18394L,
               18395L,18420L,18421L,18425L,18440L,18443L,18522L,18550L,
               11080L,11081L,11139L,11821L,11822L,11823L,11825L,11826L,
               11828L,11829L,11830L,11831L,11832L,11833L,11864L,11865L,
               11866L,11868L,11869L,11870L,11871L,11882L,12025L,12037L,
               12038L,12039L,12041L,12042L,12043L,12044L,12046L,12084L,
               12089L,12339L,12519L,12520L,12534L,12604L,12606L,
               13473L,13477L,16821L,11793L,11817L,11818L,11819L,11876L,
               11888L,11900L,11907L,11914L,11925L,11926L,11962L,11991L,
               11999L,12000L,12008L,12009L,12010L,12013L,13562L,13563L,
               13573L,13578L,13579L,13580L,13585L,13595L,13615L,13616L,
               13617L,13646L,13647L,13669L,13695L,13696L,13698L,13702L,
               13708L,13717L,13718L,13722L,13723L,13727L,13729L,13760L,
               13800L,13829L,13605L,13606L,13607L,13672L,13679L,13680L,
               13681L,13682L,13685L,13687L,13688L,13869L,13879L,
               13880L,13887L,13888L,13889L,13890L,13892L,13894L,13895L,
               13896L,13903L,13905L,13906L,13907L,13909L,13914L,13918L,
               13920L,13921L,13922L,13926L,13939L,13942L,13943L,13944L,
               13948L,13952L,13953L,13960L,13961L,13967L,13968L,13975L,
               13976L,13978L,13979L,13980L,13981L,13984L,13985L,13986L,
               13987L,13989L,13990L,13998L,13999L,14001L,14004L,14562L,
               14567L,14568L,14569L,14570L,14571L,14573L),
  ComID = c(23751816L,23751818L,23752608L,23752970L,
            23753004L,25018663L,25018661L,23752604L,23759972L,
            23759974L,23759982L,23760004L,23759854L,23759618L,23759604L,
            23759904L,23765565L,23765567L,23889780L,23889782L,23889910L,
            23889916L,23889968L,23889970L,23889972L,23889974L,23890016L,
            23890018L,23890020L,23890508L,23890510L,23890514L,
            23890518L,23890526L,23890534L,23890536L,23890548L,23890550L,
            23890566L,23890572L,23890574L,23890578L,23890580L,23890584L,
            23890588L,23890590L,23890594L,23890596L,24527238L,
            24527240L,24527242L,24527490L,24527492L,24527494L,24527496L,
            24527498L,24527500L,24527502L,24527504L,24527606L,24527972L,
            24527974L,24527988L,24527998L,24528020L,24528050L,24528074L,
            24528090L,24528092L,24528146L,24528148L,24528156L,
            24528190L,24528200L,24528396L,24529004L,23752720L,23752722L,
            23752892L,23763003L,23763005L,23763177L,23763189L,23763191L,
            23763205L,23763515L,23763579L,23763581L,23763611L,23763613L,
            23763713L,23763715L,23763717L,23763801L,23763807L,
            23763849L,23763851L,23763933L,23764741L,23764963L,23764969L,
            23764971L,23765117L,23765127L,23765143L,23765151L,23765161L,
            23765385L,23765555L,23773501L,23774151L,23774153L,
            23774193L,23774473L,23774479L,23786975L,23787017L,23890204L,
            23762695L,23762823L,23762825L,23762827L,23763895L,23763955L,
            23764005L,23764055L,23764079L,23764097L,23764101L,23764195L,
            23764375L,23764451L,23764423L,23764527L,23764539L,
            23764557L,23764573L,23791119L,23791121L,23791461L,23792093L,
            23792121L,23792109L,23792153L,23792211L,23796623L,23796625L,
            23796667L,23796831L,23796897L,23796999L,23797175L,23797187L,
            23797239L,23797215L,23797249L,23797265L,23797269L,
            23797277L,23797281L,23797303L,23797309L,23797619L,23801240L,
            23801316L,23796553L,23796555L,23796557L,23797029L,23797057L,
            23797059L,23797061L,23797063L,23797099L,23797123L,
            23797133L,23804992L,23805066L,23805068L,23805084L,23805086L,
            23805088L,23805122L,23805132L,23805160L,23805162L,23805164L,
            23805180L,23805196L,23805198L,23805200L,23805210L,23805300L,
            23805310L,23805332L,23805334L,23805336L,23805350L,
            23805384L,23805380L,23805386L,23805412L,23805392L,23805456L,
            23805404L,23805430L,23805434L,23805448L,23805446L,23805508L,
            23805514L,23805520L,23805462L,23805548L,23805560L,23805600L,
            23805628L,23805614L,23805630L,23805674L,23805658L,
            23805760L,23805778L,23805844L,23805120L,23815296L,23815334L,
            23815336L,23815340L,23815342L,23815352L,23815356L)
) |> 
  select(ComID, ORIG_FID)

test <- head(test) |> 
  mutate(random = runif(1:6))



##################################### NEW LATEST GREATEST ###################################################################
#define excel import table containing points of interest
import <- read.xlsx("C:/Users/athomps/OneDrive - Oregon/Desktop/New folder/WillyTable.xlsx")

#make dataframe of comids
dataframe_comid <- import %>%
  rowwise() %>%
  mutate(point = list(sf::st_sfc(sf::st_point(c(lon, lat)),crs = 4326 ))) |> 
  mutate(comid = discover_nhdplus_id(point)) |> 
  select(-point)

dataframe_comid <- import$ComID

#function start ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
delin <- function(comid, ofid2, random, shp_file){

#designate output shapefile
shp_file <- "C:/Users/athomps/OneDrive - Oregon/Desktop/New folder/willytest.shp"

#enter starting comid
start_comid <- import$ComID
#start_comid <- "23759604"

flowlines <- navigate_nldi(list(featureSource = "comid",
                                featureID = start_comid),
                           mode = "upstreamTributaries",
                           distance_km = 1000)

subset_file <- tempfile(fileext = ".gpkg")

subset <- subset_nhdplus(comids = as.integer(flowlines$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download",
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

catchment <- sf::read_sf(subset_file, "CatchmentSP")

catchment <- sf::st_union(catchment)

flownet <- subset$NHDFlowline_Network

#calculate watershed area in square meters
#catchment$area_sqm <- st_area(catchment)

catchment2 <- st_sf(catchment)|> 
  mutate(orig_comid = comid,
         ofid = ofid2,
         rand = random) |>
  st_transform(crs = 2992)  #reproject to NAD83 / Oregon GIC Lambert (ft). Change depending on what projection you need in the next step.

#append watershed to shapefile specified above
st_write(catchment2, shp_file, append = TRUE)
}
#end of function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#optional - view output
mapview(flowlines) + mapview(catchment) + mapview(flownet)

#write to shapefile
purrr::pmap(as.list(dataframe_comid), ~delin(comid = {..8}, random = {..1}, ofid2 = {..3} ,shp_file = "C:/Users/athomps/OneDrive - Oregon/Desktop/New folder/delineate_test.shp" ))
