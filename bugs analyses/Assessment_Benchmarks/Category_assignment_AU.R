### script to apply new 2026 biocrtieria AM 

library(tidyverse)
library(openxlsx)

#### generate benchmarks based on average scores at a monitoring location 
mloc_info <- joined_OE_BCG_MMI_good %>% 
  select(org_id,Project1,AU_ID,MLocID,StationDes,COMID,
         GNIS_Name,ReferenceSite) %>% 
  distinct()



bioassess_assessment_fun <- function(df_data = joined_OE_BCG_MMI_good, AU_type){
  
  
  if(AU_type == "other"){  
    group1 <- c('AU_ID')
    inverse <- TRUE
    
    
  } else if (AU_type == "WS"){
    group1 <- c('AU_ID', 'MLocID', 'GNIS_Name')
    inverse <- FALSE
  }
  
  averages <- joined_OE_BCG_MMI_good %>% 
    filter(str_detect(AU_ID, "WS", negate = inverse)) |> 
    group_by_at(group1) %>% 
    summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
              n= n(),
              MMI_AU_avg=mean(MMI_rescale, na.rm = TRUE), 
              OE_AU_avg=mean(OoverE, na.rm = TRUE),
              BCG_AU_avg=mean(Continuous_BCG_Level, na.rm = TRUE))%>%
    mutate(BCG_level=round(BCG_AU_avg),
           BCG_level=as.factor(BCG_level))%>%
    #filter(!is.na(BCG_level)&!is.na(MMI_AU_avg)) #%>% 
    #left_join(mloc_info, by = "MLocID") %>% 
    filter(OE_AU_avg >0)
  
  
  non_WS_Cat <- averages %>% 
    #filter(!str_detect(AU_ID, "^OR_WS")) %>%
    mutate(AU_Cat = case_when(MMI_AU_avg <= 0.81 & OE_AU_avg <= 0.79 ~ '5',
                              
                              MMI_AU_avg <= 0.90 & MMI_AU_avg > 0.81 & 
                                OE_AU_avg <= 0.91 & OE_AU_avg > 0.79 ~ '3C',
                              
                              MMI_AU_avg > 0.90 & OE_AU_avg > 0.91 ~ '2',
                              
                              MMI_AU_avg <= 0.81 &  OE_AU_avg > 0.91 ~ '3B', #OE good, mmi poor',
                              MMI_AU_avg >0.90 &  OE_AU_avg <= 0.79 ~ '3B', #OE poor, mmi good',
                              
                              OE_AU_avg <= 0.91 & OE_AU_avg > 0.79 & 
                                MMI_AU_avg <= 0.81 ~ '3B', #OE fair, mmi poor',
                              
                              OE_AU_avg <= 0.91 & OE_AU_avg > 0.79 & 
                                MMI_AU_avg > 0.90 ~ '3C', #OE fair, mmi good',
                              
                              MMI_AU_avg <= 0.90 & MMI_AU_avg> 0.81 &
                                OE_AU_avg > 0.91 ~ '3C', #OE good, mmi fair',
                              
                              MMI_AU_avg <= 0.90 & MMI_AU_avg > 0.81 & 
                                OE_AU_avg <= 0.79 ~ '3B', #OE poor, mmi fair',
                              
                              TRUE ~ 'ERROR')) |> 
    mutate(AU_Cat = factor(AU_Cat, levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5", '4A', '4B', '4C'), ordered=TRUE),
           Rationale =case_when(is.nan(BCG_AU_avg) ~ paste0("OE average = ", round(OE_AU_avg, 2), ". MMI average = ", round(MMI_AU_avg, 2), ". BCG cannot be calculated" ),
                                TRUE ~  paste0("OE average = ", round(OE_AU_avg, 2), ". MMI average = ", round(MMI_AU_avg, 2), ". BCG average = ", round(BCG_AU_avg, 1), "."  )
                                )
           )    |> 
    mutate(Pollu_ID = '156',
           wqstd_code = '5')
  
  
  
  
}

# This is the non watershed assessment grouped by AU
biocriteria_AU <-  bioassess_assessment_fun(df_data = joined_OE_BCG_MMI_good, AU_type = "other")

#This is the watershed assessment by mloc ID
biocriteria_WS <- bioassess_assessment_fun(df_data = joined_OE_BCG_MMI_good, AU_type = "WS")


## Rollup mloc to GNIS assessment
WS_GNIS_rollup <- biocriteria_WS %>%
  mutate(Rationale =  paste0(MLocID, ": ", Rationale)) |> 
  ungroup() %>%
  group_by(AU_ID, GNIS_Name, Pollu_ID, wqstd_code) %>%
  summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
            IR_category_GNIS = max(AU_Cat),
            Rationale_GNIS = str_c(Rationale,collapse =  " ~ " ),
            #Delist_eligability = max(Delist_eligability)
            )
  # mutate(Delist_eligability = case_when(Delist_eligability == 1 & IR_category_GNIS_24 == '2'~ 1,
  #                                       TRUE ~ 0)) |> 
  #mutate(IR_category_GNIS = factor(IR_category_GNIS, levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5", '4A', '4B', '4C'), ordered=TRUE)) 


### Rollup GNIS assessment to AU
WS_AU_rollup <- WS_GNIS_rollup |> 
  mutate(Rationale_GNIS =  paste0(GNIS_Name, ": ", Rationale_GNIS)) |> 
  group_by(AU_ID,Pollu_ID, wqstd_code) |> 
  summarise(stations = stringr::str_c(unique(stations), collapse = "; ") ,
             AU_Cat = max(IR_category_GNIS),
            Rationale = str_c(Rationale_GNIS,collapse =  " ~ " ))




#Throw is all together, for now

biocriteria_assessment <- bind_rows(select(biocriteria_AU, -n, -MMI_AU_avg, -OE_AU_avg, -BCG_AU_avg, -BCG_level), 
                                    WS_AU_rollup)

#### compare to 2024 IR 
# 
# non_WS_24 <-  read.xlsx('2024_biocriteria_status.xlsx', 
#                         sheet = 'AU Decisions') %>% 
#               select(AU_ID,AU_Name,HUC12,Char_Name,final_AU_cat,Rationale,stations,Year_listed,year_last_assessed)
# 
# non_WS_compare <- non_WS_24 %>% 
#            filter(!str_detect(AU_ID, "^OR_WS")) %>%
#            left_join(non_WS_Cat, by = 'AU_ID')
# 
# write.csv(non_WS_compare,"non_WS_compare.csv")

