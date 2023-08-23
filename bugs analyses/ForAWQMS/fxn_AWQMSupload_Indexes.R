# function to generate AWQMS upload from summary bug table 
# L. Merrick 7/31/2023
AWQMS_Index <- function() { 
 
   Index_upload <-
    oe.stress.mets.sta %>% 
    dplyr::select(org_id,Date,Sample,MLocID.y,Project1,OoverE,BC,MTI,BSTI,MTTI, #missing TS
                  oe.cond,Use.303d,Eco2,low.count,
                  index.period,reason.no.303,methods.ok) %>% 
    dplyr::mutate(taxa_loss = (1-OoverE)*100,
                  BC = round(BC,2),
                  MTI = round(MTI,2)) %>%
    dplyr::rename('O/E Ratio' = OoverE,
                  'Bray Curtis Similarity Index' = BC,
                  'BSTI' = BSTI,
                  'MTTI'= MTTI,
                  #'Inferred Temperature' = TS,
                  'MTI' = MTI,
                  'O/E Condition' = oe.cond,
                  '% Taxa Loss'= taxa_loss) %>%
    gather('O/E Ratio','% Taxa Loss','Bray Curtis Similarity Index','MTI','MTTI', #'Inferred Temperature'
           'BSTI','O/E Condition',key = "ID", value = "Score") %>%
    # Shorten condition names to fit in AWQMS... sorry Shannon 
    dplyr::mutate(Score = ifelse(Score == 'Least disturbed', "Good",
                                 ifelse(Score == 'Moderately disturbed', "Fair",
                                        ifelse(Score == 'Most disturbed',"Poor",paste0(Score))))) %>%
    dplyr::mutate(DQL = case_when(Use.303d == 'Yes' & !Eco2 == 'COLD DESERTS' ~"A",
                                  Use.303d == 'Yes' & Eco2 == 'COLD DESERTS' ~ "B",
                                  Use.303d == 'No' ~ "E",
                                  low.count == 'NO'& methods.ok == 'Yes'& 
                                    index.period =='Yes'& Eco2 == 'COLD DESERTS'~ "B",
                                  TRUE ~ "ERROR"),
                  com = reason.no.303, 
                  Qualifier = if_else(DQL== 'E','ALT',"")) %>%
    #generate index ID 
    dplyr::mutate(id_short = ifelse(ID == 'O/E Ratio', "OE",
                                    ifelse(ID == '% Taxa Loss',"TL",
                                           ifelse(ID == 'Bray Curtis Similarity Index',"BC",
                                                  #ifelse(ID == 'Inferred Temperature',"TS",
                                                         ifelse(ID == 'BSTI', "BSTI",
                                                                ifelse(ID == 'MTI', "MTI",
                                                                       ifelse(ID == 'MTTI', "MTTI",
                                                                       ifelse(ID == 'O/E Condition', "COND",""))))))))%>%
    dplyr::mutate(activity = paste(Sample,id_short, sep =":")) %>%
    dplyr::filter(!Score == 'NA') %>%
    dplyr::select(org_id,activity,ID,Score,Date,MLocID.y,Project1,com,Qualifier,DQL)
  
  return(Index_upload)
   
   
   
}

# end of function