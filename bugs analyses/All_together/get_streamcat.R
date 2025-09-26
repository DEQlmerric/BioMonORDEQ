# This function is a wrapper function for StreamCatTools::sc_get_data(). The intention is for each bioassessment type
# different metrics are pulled from the StreamCatTools get data function. This function only exisists to maje it easier
# to pull data in similar ways between the different types.




get_streamcat <- function(comids, type = c("OE", "MMI", 'BCG','Stress', 'TEST')){
  
  type <- match.arg(type)
  
  # The streamcat API has a limit to how long the call is. Need to split the 
  # comids into shorter groups, and then run the groups separately and then
  # recombine
  
  # comids_split is a list with each element being a 
  
  comid_narm <- na.omit(comids)
  comids_split <- split(comid_narm, ceiling(seq_along(comids)/750))
  
  
  if(type == "OE"){
    


    streamcat <- purrr::map_dfr(comids_split, ~StreamCatTools::sc_get_data(.,
                                                                           metric = 'TMAX8110,BFI,ELEV,clay,precip8110,mwst2008,mwst2009,mwst2013,mwst2014', 
                                                                           showAreaSqKm = TRUE,
                                                                           aoi='catchment,watershed,other'))
    
  }
  
  if(type == "BCG"){
    
    

    #Need to add MMI metrics here also
    streamcat <- purrr::map_dfr(comids_split, ~StreamCatTools::sc_get_data(.,
                                                                           metric = 'elev,Precip8110,ICI,IWI',
                                                                           showAreaSqKm = TRUE,
                                                                           aoi='catchment,watershed,other'))
    
  }
  
  if(type == 'MMI'){
    
    streamcat <-purrr::map_dfr(comids_split, ~StreamCatTools::sc_get_data(comid = .,
                                                                          metric = 'TMAX8110,CLAY,OM,KFFACT,PRECIP8110,ELEV,msst2008,msst2009,msst2013,msst2014,mwst2008,mwst2009,mwst2013,mwst2014,PERM,P2O5',
                                                                          showAreaSqKm = TRUE,
                                                                           aoi='catchment,watershed,other'))
    
  }
  
# --------------------------------------->  
  if(type == "Stress"){ #---------------> probably need to call MTTI and BSTI separately
# --------------------------------------->    
  
    streamcat <- purrr::map_dfr(comids_split, ~StreamCatTools::sc_get_data(.,
                                                                           metric = 'msst2008,msst2009,msst2013,msst2014,tmax9120,bfi,kffact,clay,elev,precip9120', 
                                                                           showAreaSqKm = TRUE,
                                                                           aoi='catchment,watershed,other'))
          # need 'Latitude'==doesn't come from StreamCat.  Keep from STATIONS
  }
    
  
  names(streamcat) <- base::toupper(names(streamcat))

  return(streamcat)
  
  
}