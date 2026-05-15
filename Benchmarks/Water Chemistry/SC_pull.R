# This function pulls all available variables from StreamCat based on a given list of comids. 
# A. Thompson 2025

# NOTE: StreamCat doesn't pull data for 'other' areas of interest when aoi='watershed, catchment, other'. 
# Instead, use aoi='watershed' and run separate line for aoi='other' since they don't pull unless on their own.
# ALSO: Do not separate metric lists onto a new line by hitting the enter button, as any metric after the break will be excluded from the pull.

# FUNCTION - START +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
get_streamcat <- function(comids, type = c("Agriculture","Climate","Dams","Flow","IWI","Land Cover","Lithology","Mining_Toxics","Roads",
                                           "Urban","Wildfire","Others")){
  type <- match.arg(type)
  
  if(type == "Agriculture"){
    
    streamcat <- purrr::map_dfr(comids, ~StreamCatTools::sc_get_data(.,
                                                                     metric = 'canaldens,pctagdrainage,pctagslphigh2001,pctagslpmid2001,pestic1997,sw_flux,waterinput,wdrw_LD,pctcrop2001,pcthay2001,cbnf,fert,manure,nani,nsurp,agkffact',
                                                                     showAreaSqKm = FALSE,
                                                                     aoi='watershed')) 
  }
  
  if(type == "Climate"){
    
    streamcat <- purrr::map_dfr(comids, ~StreamCatTools::sc_get_data(.,
                                                                     metric = 'precip2008,precip8110,precip9120,tmax8110,tmax9120,tmean2008,tmean8110,tmean9120,tmin8110,tmin9120,inorgnwetdep2008,nh42008,no32008,sn2008,precip_minus_evt,wetindex', 
                                                                     showAreaSqKm = FALSE,
                                                                     aoi='watershed'))
  }
  
  if(type == "Dams"){
    
    streamcat <- purrr::map_dfr(comids, ~StreamCatTools::sc_get_data(.,
                                                                     metric = 'damdens,damnidstor,damnrmstor,NABD_Dens,NABD_NIDStor,NABD_NrmStor', 
                                                                     showAreaSqKm = FALSE,
                                                                     aoi='watershed'))
  }
  
  if(type == "Flow"){
    
    streamcat <- purrr::map_dfr(comids, ~StreamCatTools::sc_get_data(.,
                                                                     metric = 'bfi,elev,runoff', 
                                                                     showAreaSqKm = FALSE,
                                                                     aoi='watershed'))
  }
  
  if(type == "IWI"){
    
    streamcat <- purrr::map_dfr(comids, ~StreamCatTools::sc_get_data(.,
                                                                     metric = 'chem,conn,habt,hyd,sed,temp', 
                                                                     showAreaSqKm = FALSE,
                                                                     aoi='watershed'))
  }
  
  if(type == "Land Cover"){
    
    streamcat <- purrr::map_dfr(comids, ~StreamCatTools::sc_get_data(.,
                                                                     metric = 'pctbl2001,pctconif2001,pctdecid2001,pctgrs2001,pcthbwet2001,pctice2001,pctmxfst2001,pctow2001,pctshrb2001,pcturbhi2001,pcturblo2001,pcturbmd2001,pcturbop2001,pctwdwet2001,pctfrstloss2002', 
                                                                     showAreaSqKm = FALSE,
                                                                     aoi='watershed'))
  }
  
  if(type == "Lithology"){ 
    
    streamcat <- purrr::map_dfr(comids, ~StreamCatTools::sc_get_data(.,
                                                                     metric = 'al2o3,cao,compstrgth,fe2o3,hydrlcond,k2o,mgo,na2o,p2o5,pctalkintruvol,pctalluvcoast,pctcarbresid,pctcoastcrs,pctcolluvsed,pcteolcrs,pcteolfine,pctextruvol,pctglaclakecrs,pctglaclakefine,pctglactilclay,pctglactilcrs,pctglactilloam,pcthydric,pctnoncarbresid,pctsallake,pctsilicic,pctwater,rockn,s,sio2,clay,kffact,om,perm,Rckdep,rckdep,sand,wtdep,n',
                                                                     showAreaSqKm = FALSE,
                                                                     aoi='watershed'))
  }
  
  if(type == "Mining_Toxics"){
    
    streamcat <- purrr::map_dfr(comids, ~StreamCatTools::sc_get_data(.,
                                                                     metric = 'coalminedens,minedens,superfunddens,tridens', 
                                                                     showAreaSqKm = FALSE,
                                                                     aoi='watershed'))
  }
  
  if(type == "Roads"){ 
    
    streamcat <- purrr::map_dfr(comids, ~StreamCatTools::sc_get_data(.,
                                                                     metric = 'rdcrs,rdcrsslpwtd,rddens', 
                                                                     showAreaSqKm = FALSE,
                                                                     aoi='watershed'))
  }
  
  if(type == "Urban"){ 
    
    streamcat <- purrr::map_dfr(comids, ~StreamCatTools::sc_get_data(.,
                                                                     metric = 'huden2010,npdesdens,popden2010,septic,wwtpalldens,wwtpmajordens,wwtpminordens,pctimp2001,pctimpslphigh2001,pctimpslpmid2001,pctnonagintrodmanagveg', 
                                                                     showAreaSqKm = FALSE,
                                                                     aoi='watershed'))
  }
  
  if(type == "Wildfire"){ 
    
    streamcat <- purrr::map_dfr(comids, ~StreamCatTools::sc_get_data(.,
                                                                     metric = 'pctfire2002,pctburnarea2002,pcthighsev2002,pctincvegresp2002,pctlowsev2002,pctmodsev2002,pctnonprocmask2002,pctundsev2002', 
                                                                     showAreaSqKm = FALSE,
                                                                     aoi='watershed'))
  }
  
  if(type == "Others"){
    
    streamcat <- purrr::map_dfr(comids, ~StreamCatTools::sc_get_data(.,
                                                                     metric = 'bankfulldepth,bankfullwidth,ici,iwi,mast2008,msst2008,mwst2008,thalwegdepth,wettedwidth,prg_bmmi0809', 
                                                                     showAreaSqKm = FALSE,
                                                                     aoi='other')) 
  }
  
  names(streamcat) <- base::toupper(names(streamcat))
  return(streamcat)
}
# FUNCTION - END  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++