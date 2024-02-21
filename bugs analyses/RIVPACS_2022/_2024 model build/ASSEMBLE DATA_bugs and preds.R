#
#
#   Author: SLH 2.20.24
#
#   Purpose: assemble bug and predictor data for building new O/E models
#             once the two datasets are complete, bring them into th O/E model build R code to generate models


library(tidyverse)
library(openxlsx)
library(StreamCatTools)
              #library(remotes)
              #install_github("USEPA/StreamCatTools", build_vignettes=FALSE)


#######

#     BUG DATA

#######

#     1) bring in bug sample list
#     2) query bug samples from AWQMS
#     3) assign to OTUs
#     4) subsample to 300 count
 #     ? crosstab ?
#      ? change to '1/0' ?
  
ref.bugs_265 <- read.xlsx('bugs analyses/RIVPACS_2022/_2024 model build/Reference sites_bug samples_total and OTU abundances.xlsx',
                          sheet = 'FINAL FINAL FINAL_265 ref samps')  

ref.bugs_265 <- ref.bugs_265 %>%
                    filter(Use_spatial=='Y')

# create a list of sampleIDs to use in querying bug data

sampleIDs.ref265 <- as.vector(ref.bugs_265$act_id)


-------> need to get COMIDs associated with these samples, so can associate with StreamCat
  
  
###########

# StreamCat predictors

###########




region_params <- sc_get_params(param='areaOfInterest')
name_params <- sc_get_params(param='name')
name_params <- sort(name_params)  


# StreamCat natural metrics for model exploration

predictor.metrics <- c('al2o3,bfi,cao,clay,compstrgth,elev,fe2o3,hydrlcond,
                       inorgnwetdep_2008,k2o,kffact,mast_2008,mast_2009,mast_2013,mast_2014,
                       msst_2008,msst_2009,msst_2013,msst_2014,mwst_2008,mwst_2009,mwst_2013,mwst_2014,  
                       mgo,n,na2o,nh4_2008,no3_2008,om,p2o5,pctalkintruvol,pctalluvcoast,pctbl2001,
                       pctbl2004,pctcarbresid,pctcoastcrs,pctcolluvsed,pcteolcrs,pcteolfine,pctextruvol,
                       pctglaclakecrs,pctglaclakefine,pctglactilclay,pctglactilcrs,pctglactilloam,
                       pctgrs2001,pcthydric,pctice2001,pctice2004,pctice2006,pctice2008,pctice2011,
                       pctice2013,pctice2016,pctice2019,pctsallake,perm,precip08,precip09,
                       precip8110,rckdep,s,sand,sio2,tmax8110,tmean8110,tmin8110')  

    ###
      ### several of these (MAST, MSST, MWST, pctice, will need to be converted to a single average metric)
    ###



## Get data for the three Hydroregions covering: 17= OR+WA+ID, 16 = NV, 18 = CA

metrics <- sc_get_data(metric=predictor.metrics, aoi='watershed', region='16,17,18')


                                          