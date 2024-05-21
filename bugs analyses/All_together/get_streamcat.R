# This function is a wrapper function for StreamCatTools::sc_get_data(). The intention is for each bioassessment type
# different metrics are pulled from the StreamCatTools get data function. This function only exisists to maje it easier
# to pull data in similar ways between the different types.




get_streamcat <- function(comids, type = c("OE", "MMI", 'BCG')){
  
  
  type <- match.arg(type)
  
  if(type == "OE"){
    
    comid_narm <- na.omit(comids)
    #Need to add MMI metrics here also
    streamcat <-StreamCatTools::sc_get_data(comid = comids,
                             metric = 'TMAX8110,BFI,ELEV,clay,precip8110,mwst2008,mwst2009,mwst2013,mwst2014', 
                             aoi='catchment,watershed,other')
    
  }
  
  if(type == "BCG"){
    
    

    #Need to add MMI metrics here also
    streamcat <-StreamCatTools::sc_get_data(comid = comids,
                                            metric = 'elev,Precip8110,ICI,IWI')
    
  }
  


  return(streamcat)
  
  
}