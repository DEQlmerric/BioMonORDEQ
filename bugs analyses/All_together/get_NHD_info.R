# NHDplus
## download VAA




get_NHD_info <- function(df, comid_col = COMID){

print("Get NHD info")
nhdplusTools::nhdplusTools_data_dir(file.path("data")) # set dir
nhdplusTools::download_vaa(path = nhdplusTools::get_vaa_path()
                           , force = FALSE
                           , updated_network = FALSE)
#nhdplusTools::get_vaa_names() # VAA table names
vaa_names2get <- c("slope"
                   , "slopelenkm"
                   , "gnis_name"
                   , "streamorde"
                   , "ftype"
                   , "fcode"
                   , "lengthkm"
                   , "totdasqkm"
                   , "areasqkm"
)

nhdplus_vaa <- nhdplusTools::get_vaa(vaa_names2get)
## merge with sites_sc
df_results <- df |> 
  dplyr::left_join(nhdplus_vaa, by = c('COMID' = 'comid')) |> 
  dplyr::mutate(NHD_pSLOPE = 100 * slope) |> 
  dplyr::mutate(SITE_TYPE = dplyr::case_when(NHD_pSLOPE >= 1 ~ 'hi',
                                             NHD_pSLOPE <1 ~ 'lo'))

df_results_fail <- df_results |> 
  filter(is.na(SITE_TYPE))

if(nrow(df_results_fail) > 0){
  warning("Some monitoring locations failed NHD join and slope determination")
  
}


return(df_results)

}




