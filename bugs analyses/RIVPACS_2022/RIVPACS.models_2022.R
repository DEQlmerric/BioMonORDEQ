#
# Author: Shannon Hubler

# 12/14/2020

# Purpose: create stand alone script for running 2005 version RIVPACS models (PREDATOR)
# 1) Data formating for input files
# 2) Run PREDATOR models: MWCF, WCCP
# 3) Run NBR null model
#metrics function

# 12/1/23

# modified old PREDATOR model code to run newer models based on Random Forests

# 1) Data formating for input files
# 2) Run new RF model: 1 statewide model, excluding NBR ecoregion




bug.RIVPACS <- function(b_t_s = b_t_s){
  
  
  library(devtools)
  library(dplyr)
  library(lubridate)
  library(tidyr)
  #library(data.table)
  
#############################################################################
#############################################################################
#############################################################################
#############################################################################
##########################################################



# Step 1: Make RIVPACS_2024 input files



#########################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################


#####
#####
#       1.1  Subset for PREDATOR
######
######

# first, get counts from Taxon/Taxon_Code ----> OTUs (no ambuiguous taxa)




b.oe<-aggregate(b_t_s$Count,  list(Sample=b_t_s$Sample, OTU=b_t_s$OTU_RIV_24, MLocID=b_t_s$MLocID,  
                                   Eco2=b_t_s$Eco2, Eco3=b_t_s$Eco3), sum )# For each OTU in a Sample, sum the Counts


#b.oe<-rename(b.oe, c("Count"="x")) ---> doesn't work....? why? I didn't change it before now and worked fine?
b.oe <- dplyr::rename(b.oe, Count = x)


b.oe<-arrange(b.oe, Sample)               # sort the d.f by Sample
b.oe <- b.oe %>% filter(OTU != 'DNI') # remove 'DNI' from d.f (they are ambiguous taxa not used in models)
b.oe<- dplyr::mutate(b.oe, OTU = ifelse(OTU == "", NA_character_, OTU)) #remove empty character strings and replace with NA values



#############          
#############
#############
#############

# IF new data is subsampled to 300 count 'AND' in MAtrix format, execute the following  (IGNORE subsampling below)
# bug.test<-read.table("MWCF_bugs_mat.txt",row.names="sample",header=T,sep="\t");
# dim(bug.test)

#############          
#############
#############
#############

#####
#####
#       1.2 rarify to 300 count
######
######

# rarify to 300 count per sample --> this standardizes 'effort' across all samples 
# since O/E is basically 'reference taxa richness', it is highly related to total count


# load the 'reshape2' package and source in the 'rarify' script for subsampling to 300

source('bugs analyses/PREDATOR/rarify_w_seed.R')


# To subsample and turn into a site*taxa matrix, execute the following:
# rarify does not affect samples with OTU counts less than 300

b.rare.seed <- rarify.seed(na.omit(b.oe), 'Sample', 'Count', 300) 
#b.rare <- rarify(na.omit(b.oe), 'Sample', 'Count', 300) #original rarify code from JVS, without a seed set.  Use to compare rarify.seed results




#####
#####
#       1.3 get total abundance of rarified samples for PREDATOR
######
######


tot.abund.RIV<-aggregate(b.rare.seed$Count, list(Sample=b.rare.seed$Sample, MLocID=b.rare.seed$MLocID), sum)
tot.abund.RIV <- dplyr::rename(tot.abund.RIV, tot.abund.RIV = x)

#####
#####
#       1.4 Matrify: convert from long format (many rows per sample), to site X species matrix (one row per sample)
######
######

dm.rare <- tidyr::pivot_longer(b.rare.seed, Count, names_to = "variable", values_to = "value" )


bugs.all <- dm.rare %>% pivot_wider(               # new tidy method, replacing dcast (data.table)
  names_from = OTU,
  id_cols = c(Sample, MLocID, Eco2, Eco3),
  values_from = value,
  values_fill = 0,
  names_repair = "check_unique",
  names_sort = TRUE
)
#####
#####
#       1.5 subset the data for the single RIVPACS 2024 model
######
######

bugs_RIV24 <- bugs.all %>% filter(Eco3 != '80')


#####
#####
#       1.6  Create data.frame for NBR Null Model 
######
######

# samples from Eco3 = '80' 
bugs.NBR <- bugs.all %>% filter(Eco3== 80 )

dim(bugs.NBR)
table(bugs.NBR$Eco2, bugs.NBR$Eco3) # All samples must be from NBR (Eco3: 80)-- no Eco3 should be from '10'




#####
#####
#     1.7 create predictor files to match the bugs.model files
#####
#####

#need to bring in StreamCat data
kitty <- read.csv('StreamCat_all_OR.csv')

# bring in NHD slopes
slope <- read.csv('NHD_slope_all.csv')
slope.lim <- slope %>%  select(COMID, SLOPE)

# join kitty and slope together

kitty.slope <- kitty %>%
                  left_join(slope.lim, by='COMID')


# limit to predictors used in RIVPACS 2024 Model

kitty_riv.preds <- kitty.slope %>% select(COMID, Tmean8110Ws, Precip8110Ws, WsAreaSqKm, ElevCat, SLOPE, Fe2O3Ws)


# get single records of Samples (SVN) and Stations (MLocID)
b.samps.sta<-unique(b_t_s[,c('Sample', 'MLocID', 'Date', 'Habitat', 'Activity_Type','COMID', 'long', 'lat',
                             'temp_Cx10', 'precip_mm','Eco2', 'Eco3', 'ELEV_m', 'W_E')])   

# get daynum
b.samps.sta$daynum<-yday(b.samps.sta$Date)      

# get kitty variables into b.samps.sta
b.samps.sta_kitty <- b.samps.sta %>%
  left_join(kitty_riv.preds, by = 'COMID')




#subset predictors to match model regions
preds_RIV24 <- b.samps.sta_kitty %>% 
  select(Sample, Tmean8110Ws, Precip8110Ws, WsAreaSqKm, Eco3, ElevCat, SLOPE, Fe2O3Ws ) %>%
  subset(Eco3 != '80')





# #check for duplicate Samples in each file
which(duplicated(bugs_RIV24$Sample))
which(duplicated(preds_RIV24$Sample))


bugs_RIV24[duplicated(bugs_RIV24), ]
preds_RIV24[duplicated(preds_RIV24), ]




# total # of observations in predictors may not match bugs.....merge tables so only matched remain

dim(bugs_RIV24); dim(preds_RIV24)


b.p <- bugs_RIV24 %>%
  left_join(preds_RIV24, by=c('Sample'), suffix = c("", ".annoying_duplicate_column")) %>%
  select(-ends_with(".annoying_duplicate_column"))





#carve out bug and predictor data into FINAL input data.frames

 
bugs_RIV24_no.nbr <-b.p %>%
  select(-c(Eco2, Eco3, Tmean8110Ws,	Precip8110Ws,	WsAreaSqKm,	ElevCat, SLOPE, Fe2O3Ws))

preds_RIV24_no.nbr <- b.p %>%
  select(Sample, MLocID, Tmean8110Ws, Precip8110Ws, WsAreaSqKm, Eco3, ElevCat, SLOPE, Fe2O3Ws)



# all above data sets are as tibbles, need to convert to data.frame to retain row names            
bugs_RIV24_no.nbr <- as.data.frame(bugs_RIV24_no.nbr)
preds_RIV24_no.nbr <- as.data.frame(preds_RIV24_no.nbr)



#verify dimensions are same for each input file

dim(bugs_RIV24_no.nbr); dim(preds_RIV24_no.nbr) 



# need this to get Sample as row.names for 'model.predict.v4.1'.  If don't then O/E not assigned to Sample IDs.

row.names(bugs_RIV24_no.nbr)<-bugs_RIV24_no.nbr$Sample  
row.names(preds_RIV24_no.nbr)<-preds_RIV24_no.nbr$Sample


##  Align bug and predictor data, by site/sample;
bugs_RIV24_no.nbr <- bugs_RIV24_no.nbr[order(bugs_RIV24_no.nbr$Sample),]
preds_RIV24_no.nbr <- preds_RIV24_no.nbr[order(preds_RIV24_no.nbr$Sample),]


#############################################################################
#############################################################################
#############################################################################
#############################################################################
##########################################################



# Step 2: Run PREDATOR = Making predictions for new (all) data.



#########################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################



############################

# From JVS original code:
# The 2 files (bugs and predictors) should have similar formats as the original taxa and predictor data sets used to build the model (see step 1 above);
# Notes on format --
#   A) The sample ID column in both files should be read into R as a row name (see Step 1 examples).
#   B) Predictor data set -- Must include columns with the same names, units, etc.,
#        as the model's predictor variables. All other columns will be ignored;
#        Column order does not matter;
#        Predictions and calculations of O/E will be made only for those samples that have;
#        complete data for all model predictors.;
#   C)  Sample-by-taxa matrix.  Sample ID's (row names) must match those of predictor data.
#        Model predictions will include only those taxa (columns) in this file that are also
#        in the calibration data (bugcal.pa);
#        To see a list of the calibration taxa, do the following:
#                                                         names(bugcal.pa)[colSums(bugcal.pa)>0];
##########;

#source in prediction script and load models
source("bugs analyses/RIVPACS_2022/model.predict.RanFor.4.2.r") # bring in model predict function (JVS script)

# load and run each model separately


#################

# 2.1 ALL OR (except NBR) model

################

# bring in saved 2024 model
load('bugs analyses/RIVPACS_2022/RIVPACS.2024__5grp_7.preds_noNBR_FINAL.Rdata');    


#Drop all samples/sites from bug and predictor data that do not not have complete data for the model predictors;
preds_RIV24_no.nbr <- as.data.frame(preds_RIV24_no.nbr)
bugs_RIV24_no.nbr <- as.data.frame(bugs_RIV24_no.nbr)


preds_RIV24_no.nbr<-preds_RIV24_no.nbr[complete.cases(preds_RIV24_no.nbr[,preds.final]),]
bugs_RIV24_no.nbr<-bugs_RIV24_no.nbr[row.names(preds_RIV24_no.nbr),]
dim(preds_RIV24_no.nbr) 
dim(bugs_RIV24_no.nbr)   



#make predictions for test data;
OE.assess.test<-model.predict.RanFor.4.2(bugcal.pa,grps.final,preds.final, 
                          ranfor.mod= model.final,
                          prednew=    preds_RIV24_no.nbr,
                          bugnew=     bugs_RIV24_no.nbr,
                          Pc=0.5,Cal.OOB=FALSE); 


oe.RIV24_noNBR<-OE.assess.test$OE.scores; #create a d.f out of OE.scores



head(OE.assess.test$OE.scores)# look at O/E scores, for all samples;

head(OE.assess.test$Group.Occurrence.Probs) # Look at the predicted group membership probabilities;
head(OE.assess.test$Capture.Probs)  # Look at the predicted probabilities for each taxon at each site;

# Assign PREDATOR condition classes == MWCF benchmarks
# oe.mwcf$OoverE<-round(oe.mwcf$OoverE, 2)
# oe.mwcf$oe.cond<-(ifelse(oe.mwcf$OoverE <= 0.85, 'Most disturbed', 
#                         ifelse(oe.mwcf$OoverE > 0.85 & oe.mwcf$OoverE < 0.92, 'Moderately disturbed', 
#                                ifelse(oe.mwcf$OoverE >= 0.92 & oe.mwcf$OoverE < 1.25, 'Least disturbed', 
#                                       ifelse(oe.mwcf$OoverE >= 1.25, 'Enriched', -999)))))



# calculate min - max for each condition class
#ddply(oe.mwcf, .(oe.cond), plyr::summarize, min = min(OoverE), max = max(OoverE))

#This works better- TP
# oe.RIV24_noNBR %>%
#  dplyr::group_by(oe.cond) %>%
#  dplyr::summarise(min = min(OoverE),
#                   max = max(OoverE))

# verify that results are consistent with PREDATOR documentation benchmarks: <=0.85, 0.86 - 0.91, 0.92 - 1.24, > 1.24
# verify no '-999' values

#.GlobalEnv$oe.mwcf <- oe.mwcf

# assess all samples: 

source('bugs analyses/RIVPACS_2022/assess.all.samples.v4.1_2.r')              

assess.all_RIV24<- assess.all.samples.1.0(result.prd=OE.assess.test, bugnew=bugs_RIV24_no.nbr, Pc=0.5)


#' # bring in BCG attributes for each taxon, merge with assess df
#' 
#' @@@@
#' @@@@@@@@@ Need to pull in from GitHub: https://github.com/leppott/BCGcalc.git
#' @@@@
#' 
#'   if(!require(remotes)){install.packages("remotes")}  #install if needed
#' install_github("leppott/BCGcalc", force=TRUE, build_vignettes=TRUE)  
#'   
#'   
#'   
#'     
#' library(BCGcalc)
#' bcg.taxa<-TaxaMaster_Ben_BCG_PacNW
#' 
#' bcg.taxa<-bcg.taxa %>%
#'   select(-c(Phylum, SubPhylum, Class, SubClass, Order, Family, Tribe, Genus, SubGenus, Species))
#' 
#' assess.all_MWCF <- merge(assess.all_MWCF, bcg.taxa, by.x=c('Taxon'), by.y=c('TaxaID'), all.x=TRUE )            
#' 
#' assess.all_MWCF <- arrange(assess.all_MWCF, Sample)
#' 
#' 
#' assess.all_MWCF<-assess.all_MWCF[,c("Sample", "MLocID", "Eco2", "Eco3", "Taxon", "observed", "Predicted", "Big.diff", "In.OtoE", "Class",            
#'                                     "BCG_Attr", "NonTarget", "Thermal_Indicator", "Long_Lived", "FFG", "Habit", "Life_Cycle")]
#' 
#' assess.all_MWCF<- arrange(assess.all_MWCF, Sample, desc(Class), BCG_Attr)
#' 
#' 

## Write  tables of O/E outputs
write.csv(OE.assess.test$OE.scores, paste0("//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/RIV24_OEscores_",Sys.Date(), ".csv"), row.names=TRUE)
write.csv(OE.assess.test$Group.Occurrence.Probs, paste0("//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/RIV24_grp.probs_", Sys.Date(), ".csv"), row.names=TRUE)
write.csv(OE.assess.test$Capture.Probs, paste0("//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/RIV24_capt.probs_", Sys.Date(), ".csv"), row.names=TRUE)

write.csv(assess.all_RIV24, paste0('//deqlab1/biomon/R Stats/Bio Tools_Upgrade with R/assess.all_RIV24', Sys.Date(), '.csv'), row.names=TRUE)





#############################################################################
#############################################################################
#############################################################################
#############################################################################
##########################################################



# Step 3: Assess NBR samples with Null model



#########################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################


# create a null model taxa list
# null.taxa.NBR<-list("Baetis", "Chironominae", "Optioservus", "Orthocladiinae", "Rhyacophila", "Trombidiformes", "Diphetor_hageni",
#                     "Epeorus", "Zaitzevia", "Brachycentrus")
# 
# #bugs.NBR.melt<- data.table::melt(bugs.NBR, id.vars=c('Sample', 'MLocID','Eco2', 'Eco3'), variable.name = "Taxon", value.name = "Count")
# 
# bugs.NBR.melt <- bugs.NBR %>% pivot_longer(
#   cols = !c(Sample, MLocID, Eco2, Eco3),
#   names_to = "Taxon",
#   values_to = "Count"
# )
# 
# 
# #data.table::setnames(bugs.NBR.melt, old=c('variable', 'value'), new=c('Taxon', 'Count'))
# bugs.NBR.melt<-arrange(bugs.NBR.melt, Sample)
# bugs.NBR.melt.null.taxa<-bugs.NBR.melt[bugs.NBR.melt$Taxon %in% null.taxa.NBR & bugs.NBR.melt$Count > 0 , ] # cut down to only include null taxa > 0
# #bugs.NBR.melt.null.taxa$Taxon<-droplevels(bugs.NBR.melt.null.taxa$Taxon)
# table(bugs.NBR.melt.null.taxa$Taxon) # ensure only null.taxa.NBR appear
# 
# 
# #Replacing below with more modern dplyr syntax
# #oe.nbr<-plyr::ddply(bugs.NBR.melt.null.taxa, .variables = 'Sample', summarize, Onull=length(Taxon), Enull=7.56, OoverE.null=length(Taxon)/7.56)  
# 
# 
# oe.nbr <- bugs.NBR.melt.null.taxa %>%
#   dplyr::group_by(Sample) %>%
#   dplyr::summarise(Onull = length(Taxon),
#             Enull = 7.56,
#             OoverE.null = Onull/Enull)
# 
# 
# oe.nbr$OoverE.null<-round(oe.nbr$OoverE.null, 2)
# oe.nbr$oe.cond<-(ifelse(oe.nbr$OoverE.null <= 0.50, 'Most disturbed', 
#                         ifelse(oe.nbr$OoverE.null > 0.50 & oe.nbr$OoverE.null < 0.75, 'Moderately disturbed', 
#                                ifelse(oe.nbr$OoverE.null >= 0.75 & oe.nbr$OoverE.null < 1.31, 'Least disturbed', 
#                                       ifelse(oe.nbr$OoverE.null >= 1.31, 'Enriched', -999)))))
# 

model_outputs <- list(oe.RIV24_noNBR = as.data.frame(oe.RIV24_noNBR),
                      #oe.nbr = as.data.frame(oe.nbr),
                      tot.abund.RIV = as.data.frame(tot.abund.RIV))

return(model_outputs)

}
