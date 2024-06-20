library(randomForest)
library(VSURF)
library(psych)
library(dplyr)
library(tibble)

#-------------------------------------------------------------------------------------------------------#
# Step 1- Read in predictors
#-------------------------------------------------------------------------------------------------------#
#predictorsdf=read.csv("predictors.csv")



      load('bugs analyses/MMI/_2024 model build/mmi.pred.mets_414_final.Rdata')
      predictorsdf_NAs <- mmi.pred.mets_414_final %>%
        select(-met.type, -REGIONID, -PRECIP09) # drop precip09--repetitive with other precip 
      rm(mmi.pred.mets_414_final)

# NAs not permitted, so need to impute
     
      predictorsdf_NAs$SAMPLEID <- as.factor(predictorsdf_NAs$SAMPLEID) 
      predictorsdf_NAs$MLocID <- as.factor(predictorsdf_NAs$MLocID) 
      predictorsdf_NAs$ReferenceSite <- as.factor(predictorsdf_NAs$ReferenceSite)
      predictorsdf_NAs$COMID <- as.factor(predictorsdf_NAs$COMID)
      
      predictorsdf <- na.roughfix(predictorsdf_NAs)
      
     


      

#-------------------------------------------------------------------------------------------------------#
# Step 2- Read in macroinvertebrate metrics 
#-------------------------------------------------------------------------------------------------------#
 #metricsdf=read.csv("macroinvert_metrics.csv")
 
      load('bugs analyses/MMI/_2024 model build/bug.metrics_ref.status.Rdata')
      metricsdf <- bug.metrics_ref.status
      rm(bug.metrics_ref.status)

                                     
                                     
#-------------------------------------------------------------------------------------------------------#
# Step 3- combine predictors and metrics into two dataframes, one with only reference sites and one with all sites
      
      
#-------------------------------------------------------------------------------------------------------#
#rfdat_all=dplyr::left_join(metricsdf,predictorsdf,by="sampleId")
#rfdat=subset(rfdat_all,reference=='Y')

        # limit ref samples to exact same set as used in O/E 2024 model build
      
        #  Probably don't need this.  SLH tried to modify all of thes einput files inthe "ASSEMBLE" code, 
        #    but StreamCat wasnt working.  Unfortunately, I had saved a copy of the edited bug.metrics_ref.status.Rdata,
        #    so that limits the ref sites already 
        #    Just note_ total num sites  should be 379 sites: 221 ref, 158 most disturbed
        # load('bugs analyses/RIVPACS_2022/_2024 model build/ref.samples_RIV24_221.Rdata')
      
        rfdat_all <- metricsdf %>% 
        select(-MLocID, -ReferenceSite, -INDEX_CLASS, -INDEX_NAME, -ri_ti_sccc_wsw, -nt_Tubif, -nt_habit_climb,
               -pi_habit_climb, -pt_habit_climb, -pi_Juga, -pi_JugaFlumi, -nt_dni, -pi_dni, -pt_dni) %>% 
              # drop non-numeric and all zero metrics
        left_join(predictorsdf, by='SAMPLEID') %>%
        select(-MLocID, -COMID) %>%
        column_to_rownames('SAMPLEID') %>%
        relocate(ReferenceSite, .before = 1)

      rfdat <- rfdat_all %>%
        filter(ReferenceSite =='REFERENCE') %>%
        select(-ReferenceSite)

#----------------------------------------------------------------------------------------------------#
#Step 4- create random forest models to predict natural variation in all metrics
#----------------------------------------------------------------------------------------------------#
# run Vsurf in a loop to select predictors using data frame with only reference sites
#metrics=names(rfdat[2:152])
#formulas=list()
#variance_explained=list()

      metrics=names(rfdat[1:277])
      formulas=list()
      variance_explained=list()





# for (i in 1:length(metrics)){
#   assign(paste0("rfdat",i),rfdat[,c(i,287:316)])
#   species.vsurf = VSURF(rfdat[,287:316], rfdat[,i])
#   names = as.data.frame(names(rfdat[,c(287:316,i)]))
#   selected.pred=names[species.vsurf$varselect.pred,]
#   assign(paste0("rfmod_",names(rfdat)[i]),
#          randomForest(as.formula(paste0(names(rfdat)[i],"~",paste(selected.pred,collapse="+"))), data=eval(parse(text =paste0("rfdat",i))), ntree=2000, importance=TRUE, norm.votes=TRUE, keep.forest=TRUE)) 
#   print(paste0(names(rfdat)[i],"~",paste(selected.pred,collapse="+")))
#   print(eval(parse(text =paste0("rfmod_",names(rfdat)[i]))))
#   
#   formulas[[names(rfdat)[i]]]<-print(paste0(names(rfdat)[i],"~",paste(selected.pred,collapse="+")))
#   variance_explained[[names(rfdat)[i]]]<-eval(parse(text =paste0("rfmod_",names(rfdat)[i],"$rsq[2000]")))
# }
#       
      
      for (i in 1:length(metrics)){
        assign(paste0("rfdat",i),rfdat[,c(i,278:305)])
        species.vsurf = VSURF(rfdat[,278:305], rfdat[,i])
        names = as.data.frame(names(rfdat[,c(278:305,i)]))
        selected.pred=names[species.vsurf$varselect.pred,]
        assign(paste0("rfmod_",names(rfdat)[i]),
               randomForest(as.formula(paste0(names(rfdat)[i],"~",paste(selected.pred,collapse="+"))), data=eval(parse(text =paste0("rfdat",i))), ntree=2000, importance=TRUE, norm.votes=TRUE, keep.forest=TRUE)) 
        print(paste0(names(rfdat)[i],"~",paste(selected.pred,collapse="+")))
        print(eval(parse(text =paste0("rfmod_",names(rfdat)[i]))))
        
        formulas[[names(rfdat)[i]]]<-print(paste0(names(rfdat)[i],"~",paste(selected.pred,collapse="+")))
        variance_explained[[names(rfdat)[i]]]<-eval(parse(text =paste0("rfmod_",names(rfdat)[i],"$rsq[2000]")))
      }
      
      
      
      
      
      
      
      
# nestedlist=list(unlist(formulas),unlist(variance_explained))
# 
# randomforest_results=as.data.frame(do.call(cbind,nestedlist))
# write.csv(randomforest_results,"randomforest_results.csv")

    nestedlist=list(unlist(formulas),unlist(variance_explained))
    
    randomforest_results=as.data.frame(do.call(cbind,nestedlist)) %>%
      rownames_to_column('metric')
    
    randomforest_results$V2 <- as.numeric(randomforest_results$V2)    
    
    
    
    write.csv(randomforest_results,"bugs analyses/MMI/_2024 model build/randomforest_results.csv")

    
    # create a list of metrics where rf models explain less than 10%
    
    mods.poor <- randomforest_results[(randomforest_results$V2 < 0.10),]
    mods.good <- randomforest_results[(randomforest_results$V2 >= 0.10),]
    
    metrics_poor.mods <-mods.poor$metric[!is.na(mods.poor$metric )] 
    metrics_good.mods <-mods.good$metric[!is.na(mods.good$metric )] 

    metrics_good.mods<- paste(metrics_good.mods, 'resid', sep = '_')
    
    # drop these metrics, throws off rfdat_all_final3
    metrics_good.mods <- metrics_good.mods[!metrics_good.mods %in%  c("nfam_Coleo_resid","nfam_Ephem_resid","nfam_Trich_resid", "nfam_Odon_resid")]
    
    # the following metrics were dropped from rf mods:
    # "nt_Isop" "pi_Caen" "pi_Corb" "pi_Isop" "pt_Isop"
    
    
#----------------------------------------------------------------------------------------------------#
#Step 5- Get predicted values out of model objects for reference sites and predict values for degraded sites
#----------------------------------------------------------------------------------------------------#
##reference site predictions
#select random forest models of interest from workspace
#rfmodels=objects()[243:249]

                              
      # better to call by name? select the top performing models, based on 'randomforest_results'

      
     rfmodels <- c('rfmod_ngen_Coleo','rfmod_ngen_Elmid','rfmod_ngen_Ephem','rfmod_ngen_Odon',
     'rfmod_ngen_Trich','rfmod_ni_Chiro','rfmod_ni_Dipt','rfmod_ni_EPT',
     'rfmod_ni_total','rfmod_ni_Trich','rfmod_nt_Amph','rfmod_nt_Bival',
     'rfmod_nt_Chiro','rfmod_nt_COET','rfmod_nt_Coleo','rfmod_nt_CruMol',
     'rfmod_nt_Dipt','rfmod_nt_ECT','rfmod_nt_Ephem','rfmod_nt_Ephemerellid',
     'rfmod_nt_EPT','rfmod_nt_ET','rfmod_nt_ffg_col','rfmod_nt_ffg_filt',
     'rfmod_nt_ffg_mah','rfmod_nt_ffg_omn','rfmod_nt_ffg_par','rfmod_nt_ffg_pih',
     'rfmod_nt_ffg_pred','rfmod_nt_ffg_pred_scrap_shred','rfmod_nt_ffg_scrap',               
     'rfmod_nt_ffg_shred','rfmod_nt_ffg_xyl','rfmod_nt_Gast','rfmod_nt_habit_burrow',            
     'rfmod_nt_habit_climbcling','rfmod_nt_habit_cling','rfmod_nt_habit_sprawl',
     'rfmod_nt_habit_swim','rfmod_nt_habitat_brac','rfmod_nt_habitat_depo',            
     'rfmod_nt_habitat_gene','rfmod_nt_habitat_head','rfmod_nt_habitat_rheo',
     'rfmod_nt_habitat_rive','rfmod_nt_habitat_spec','rfmod_nt_Hepta','rfmod_nt_Insect',
     'rfmod_nt_Mega','rfmod_nt_Mol','rfmod_nt_Nemour',
     'rfmod_nt_NonIns','rfmod_nt_Odon','rfmod_nt_OET','rfmod_nt_Oligo',
     'rfmod_nt_oneind','rfmod_nt_Perlid','rfmod_nt_Pleco','rfmod_nt_POET',
     'rfmod_nt_Ptero','rfmod_nt_Rhya','rfmod_nt_ti_cold','rfmod_nt_ti_cool',
     'rfmod_nt_ti_cowa','rfmod_nt_ti_cowa_warm_stenowarm','rfmod_nt_ti_eury',                 
     'rfmod_nt_ti_stenocold','rfmod_nt_ti_stenocold_cold','rfmod_nt_ti_stenocold_cold_cool',
     'rfmod_nt_ti_stenowarm','rfmod_nt_ti_warm','rfmod_nt_ti_warm_stenowarm',       
     'rfmod_nt_Tipulid','rfmod_nt_total','rfmod_nt_Trich','rfmod_nt_TrichNoHydro',            
     'rfmod_nt_Tromb','rfmod_nt_tv_intol','rfmod_nt_tv_intol4','rfmod_nt_tv_intol4_EPT',           
     'rfmod_nt_tv_ntol','rfmod_nt_tv_stol','rfmod_nt_tv_toler','rfmod_nt_volt_multi',              
     'rfmod_nt_volt_semi','rfmod_nt_volt_uni','rfmod_pi_Amph','rfmod_pi_AmphIsop',                
     'rfmod_pi_Baet','rfmod_pi_Bival','rfmod_pi_ChCr2Chi',                
     'rfmod_pi_Cheu','rfmod_pi_Chiro','rfmod_pi_ChiroAnne','rfmod_pi_COC2Chi',                 
     'rfmod_pi_COET','rfmod_pi_Coleo','rfmod_pi_Colesens',                    
     'rfmod_pi_CorixPhys','rfmod_pi_CraCaeGam','rfmod_pi_Cru','rfmod_pi_CruMol',                  
     'rfmod_pi_Deca','rfmod_pi_Dipt','rfmod_pi_DiptNonIns','rfmod_pi_dom01',                   
     'rfmod_pi_dom02','rfmod_pi_dom03','rfmod_pi_dom04','rfmod_pi_dom05',                   
     'rfmod_pi_ECT','rfmod_pi_Ephem','rfmod_pi_EphemNoCae','rfmod_pi_EphemNoCaeBae',           
     'rfmod_pi_EPT','rfmod_pi_EPTNoBaeHydro','rfmod_pi_EPTNoCheu','rfmod_pi_EPTNoHydro',              
     'rfmod_pi_ET','rfmod_pi_ffg_col','rfmod_pi_ffg_col_filt','rfmod_pi_ffg_filt',               
     'rfmod_pi_ffg_mah','rfmod_pi_ffg_omn','rfmod_pi_ffg_par','rfmod_pi_ffg_pih',                 
     'rfmod_pi_ffg_pred','rfmod_pi_ffg_scrap','rfmod_pi_ffg_shred','rfmod_pi_ffg_xyl',                 
     'rfmod_pi_Gast','rfmod_pi_habit_burrow','rfmod_pi_habit_climbcling','rfmod_pi_habit_cling',             
     'rfmod_pi_habit_cling_PlecoNoCling','rfmod_pi_habit_sprawl','rfmod_pi_habit_swim',
     'rfmod_pi_habitat_brac','rfmod_pi_habitat_depo','rfmod_pi_habitat_gene',            
     'rfmod_pi_habitat_head','rfmod_pi_habitat_rheo','rfmod_pi_habitat_rive',
     'rfmod_pi_habitat_spec','rfmod_pi_habitat_unkn','rfmod_pi_Hydro',                  
     'rfmod_pi_Hydro2EPT','rfmod_pi_Hydro2Trich','rfmod_pi_Insect',                   
     'rfmod_pi_IsopGastHiru','rfmod_pi_Mega','rfmod_pi_Mol','rfmod_pi_Nemata',                 
     'rfmod_pi_NonIns','rfmod_pi_Odon','rfmod_pi_OET','rfmod_pi_Oligo',                  
     'rfmod_pi_Orth2Chi','rfmod_pi_Ortho','rfmod_pi_Pleco','rfmod_pi_POET',                   
     'rfmod_pi_SimBtri','rfmod_pi_Sphaer','rfmod_pi_SphaerCorb','rfmod_pi_Tanyp',                  
     'rfmod_pi_Tanyp2Chi','rfmod_pi_Tanyt','rfmod_pi_ti_cold','rfmod_pi_ti_cool',                
     'rfmod_pi_ti_cowa','rfmod_pi_ti_cowa_warm_stenowarm','rfmod_pi_ti_eury',
     'rfmod_pi_ti_stenocold','rfmod_pi_ti_stenocold_cold','rfmod_pi_ti_stenocold_cold_cool', 
     'rfmod_pi_ti_stenowarm','rfmod_pi_ti_warm','rfmod_pi_ti_warm_stenowarm',
     'rfmod_pi_Trich','rfmod_pi_TrichNoHydro','rfmod_pi_Tromb','rfmod_pi_tv_intol',
     'rfmod_pi_tv_intol4','rfmod_pi_tv_ntol','rfmod_pi_tv_stol','rfmod_pi_tv_toler',
     'rfmod_pi_tv_toler6','rfmod_pi_tv2_intol','rfmod_pi_volt_multi',             
     'rfmod_pi_volt_semi','rfmod_pi_volt_uni','rfmod_pt_Amph',                   
     'rfmod_pt_Bival','rfmod_pt_Chiro','rfmod_pt_COET','rfmod_pt_Coleo',                  
     'rfmod_pt_Deca','rfmod_pt_Dipt','rfmod_pt_ECT','rfmod_pt_Ephem',                  
     'rfmod_pt_EPT','rfmod_pt_ET','rfmod_pt_ffg_col','rfmod_pt_ffg_filt',               
     'rfmod_pt_ffg_mah','rfmod_pt_ffg_omn','rfmod_pt_ffg_par','rfmod_pt_ffg_pih',                
     'rfmod_pt_ffg_pred','rfmod_pt_ffg_scrap','rfmod_pt_ffg_shred','rfmod_pt_ffg_xyl',                
     'rfmod_pt_Gast','rfmod_pt_habit_burrow','rfmod_pt_habit_climbcling','rfmod_pt_habit_cling',            
     'rfmod_pt_habit_sprawl','rfmod_pt_habit_swim','rfmod_pt_habitat_brac','rfmod_pt_habitat_depo',           
     'rfmod_pt_habitat_gene','rfmod_pt_habitat_head','rfmod_pt_habitat_rheo','rfmod_pt_habitat_rive',           
     'rfmod_pt_habitat_spec','rfmod_pt_Insect','rfmod_pt_Mega',                   
     'rfmod_pt_NonIns','rfmod_pt_Odon','rfmod_pt_OET','rfmod_pt_Oligo',                  
     'rfmod_pt_oneind','rfmod_pt_Pleco','rfmod_pt_POET','rfmod_pt_ti_cold',                
     'rfmod_pt_ti_cool','rfmod_pt_ti_cowa','rfmod_pt_ti_cowa_warm_stenowarm','rfmod_pt_ti_eury',                
     'rfmod_pt_ti_stenocold','rfmod_pt_ti_stenocold_cold','rfmod_pt_ti_stenocold_cold_cool',
     'rfmod_pt_ti_stenowarm','rfmod_pt_ti_warm','rfmod_pt_ti_warm_stenowarm',      
     'rfmod_pt_Trich','rfmod_pt_TrichNoHydro','rfmod_pt_Tromb','rfmod_pt_tv_intol',               
     'rfmod_pt_tv_intol4','rfmod_pt_tv_ntol','rfmod_pt_tv_stol','rfmod_pt_tv_toler',               
     'rfmod_pt_volt_multi','rfmod_pt_volt_semi','rfmod_pt_volt_uni',                     
     'rfmod_x_Becks','rfmod_x_Becks3','rfmod_x_D',                       
     'rfmod_x_D_G','rfmod_x_D_Mg','rfmod_x_Evenness','rfmod_x_HBI',                     
     'rfmod_x_HBI2','rfmod_x_Shan_10','rfmod_x_Shan_2','rfmod_x_Shan_e')
     
     # dropped metrics? 'rfmod_nt_Isop','rfmod_pi_Caen','rfmod_pi_Corb','rfmod_pi_Isop','rfmod_pt_Isop',
     
     
     # @@@@@ - below (step 6) calculating residuals and calling by indexing (not by name)....just do it for all metrics at once?
      
      
                               
                                  

df=list()
for (i in 1:length(rfmodels)){
  df[[paste0("E.",rfmodels[i])]]=eval(parse(text =paste0(rfmodels[i])))$predicted
}
Rpredicteddf=as.data.frame(do.call(cbind,df))
#join predictions into master dataframe
rfdat2=cbind(rfdat,Rpredicteddf)
  # rfdat2: metrics, predictors, Expected

##degraded site predictions
#Drfdat=subset(rfdat_all, reference=="N")

    Drfdat=subset(rfdat_all, ReferenceSite=="MOST DISTURBED")
    Drfdat <- Drfdat %>% select(-ReferenceSite)

Dpredictions=list()
for (i in 1:length(rfmodels)){
  tryCatch({Dpredictions[[paste0("E.",rfmodels[i])]]<- round(predict(eval(parse(text =paste0(rfmodels[i]))), Drfdat, type = "response"),digits=4)
  }, error =function (e){
    cat(paste0("/n/tERROR calculating: ",paste0(names(rfdat)[i],"_pred"),"/n"))
    str(e,indent.str = "   "); cat("/n")
  })
}
predictionsdf=as.data.frame(do.call(cbind,Dpredictions))
#join predictions into master dataframe
Drfdat2=cbind(Drfdat,predictionsdf)

#join reference and degraded sites back together

rfdat_all_final=rbind(rfdat2,Drfdat2)           
write.csv(rfdat_all_final,"bugs analyses/MMI/_2024 model build/rfdat_all_final.csv")


#----------------------------------------------------------------------------------------------------#
# Step 6 Calculate residuals
#----------------------------------------------------------------------------------------------------#


# resid=list()
# for (i in 2:190){
#   tryCatch({resid[[paste0(colnames(rfdat_all_final)[i],"_resid")]]=rfdat_all_final[,i]- rfdat_all_final[,paste0("E.rfmod_",colnames(rfdat_all_final)[i])]
#   
#   }, error =function (e){
#     cat(paste0("/n/tERROR calculating: ",paste0(rfdat_all_final[i],"_resid"),"/n"))
#     str(e,indent.str = "   "); cat("/n")
#   })
#   
# }

                resid=list()
                for (i in 1:277){
                  tryCatch({resid[[paste0(colnames(rfdat_all_final)[i],"_resid")]]=rfdat_all_final[,i]- rfdat_all_final[,paste0("E.rfmod_",colnames(rfdat_all_final)[i])]
                  
                  }, error =function (e){
                    cat(paste0("/n/tERROR calculating: ",paste0(rfdat_all_final[i],"_resid"),"/n"))
                    str(e,indent.str = "   "); cat("/n")
                  })
                  
                }

                
                
residualsdf=as.data.frame(do.call(cbind,resid))

rfdat_all_final4=cbind(rfdat_all_final,residualsdf)

rfdat_all_final4 <- rfdat_all_final4 %>%
  rownames_to_column("SAMPLEID")





      #create a new dframe of only resids and orig.<10, use this in both 7 & 8
      
      rfdat_all_final3 <- rfdat_all_final4 %>%
            select(SAMPLEID, all_of(c(metrics_poor.mods, metrics_good.mods))) 
      
      # Get ReferenceSite in the dframe
      site.type <- metricsdf %>%
          select(SAMPLEID, ReferenceSite)
      
      rfdat_all_final3 <- rfdat_all_final3 %>%
          left_join(site.type, by = 'SAMPLEID') %>%
          relocate(ReferenceSite, .before = 2)

write.csv(rfdat_all_final4, 'bugs analyses/MMI/_2024 model build/rfdat_all_final4.csv')      
write.csv(rfdat_all_final3, 'bugs analyses/MMI/_2024 model build/rfdat_all_final3.csv')      
     
      
#----------------------------------------------------------------------------------------------------#
#Step 7- deterimine how well residuals discrimiate between reference and degraded sites with t-tests
#----------------------------------------------------------------------------------------------------#


tvalues=list()


  
          

# for (i in 436:624){     
#   tryCatch({t=t.test(rf_dat_all_final4[,i]~rf_dat_all_final4$reference)
#   tvalues[[paste0(colnames(rf_dat_all_final4)[i])]]=unlist(t$statistic[[1]])
#   }, error =function (e){
#     cat(paste0("/n/tERROR calculating: ",paste0(names(rf_dat_all_final4)[i],"_tvalue"),"/n"))
#     str(e,indent.str = "   "); cat("/n")
#   })
# }
# tvalues=as.data.frame(do.call(rbind,tvalues))
# write.csv(tvalues,"tvalues.csv")


          
          
          # need to index the residuals for good models, but the original metrics 
          # for poor models -- this was done in 'rfdat_all_final3'

          
          for (i in 3:266){     
            tryCatch({t=t.test(rfdat_all_final3[,i]~rfdat_all_final3$ReferenceSite)
            tvalues[[paste0(colnames(rfdat_all_final3)[i])]]=unlist(t$statistic[[1]])
            }, error =function (e){
              cat(paste0("/n/tERROR calculating: ",paste0(names(rfdat_all_final3)[i],"_tvalue"),"/n"))
              str(e,indent.str = "   "); cat("/n")
            })
          }
          tvalues=as.data.frame(do.call(rbind,tvalues)) 
          
          tvalues <- tvalues %>% rownames_to_column('metric')
          
          
          
          write.csv(tvalues,"bugs analyses/MMI/_2024 model build/tvalues.csv")

          # 264 metrics













#----------------------------------------------------------------------------------------------------#
#Step 8- Use PCA to select 5 metric residuals that are least correlated and have highest t values
#----------------------------------------------------------------------------------------------------#
#select metric residuals for metrics with randomforest mode R2>0.10 and if not use original metric values instead


          #  only want residuals and originals.<10 for ref only (see end of step 6 df)           
          
        
              
              
          
# master=rfdat_all_final3
# reference=subset(master, reference=="Y")
# select=principal(reference[c(-1,-2)],nfactors=5,covar=FALSE, rotate='varimax',scores=TRUE)
# select$loadings
        

          master=rfdat_all_final3   
          reference=subset(master, ReferenceSite=="REFERENCE")
          select=principal(reference[c(-1,-2)],nfactors=5,covar=FALSE, rotate='varimax',scores=TRUE)
          select$loadings

          
          # make a df out of pca loads, then combine with tvalues to select metrics
          pca.loads <- as.data.frame(select$loadings[1:264, 1:5]) %>%
            rownames_to_column('metric')

          
          
          pca.tval <- pca.loads %>%
            left_join(tvalues, by = 'metric')


          write.csv(pca.tval, 'bugs analyses/MMI/_2024 model build/pca.tval.csv')




#----------------------------------------------------------------------------------------------------#
#Step 9- rescale selected X (4?, 5?) metrics to be on same scale, then average them for a final MMI value
#----------------------------------------------------------------------------------------------------#
# candmetrics=master[,1:7]   
# row.names(metrics)=metrics$sampleId
# ref_metrics=subset(candmetrics, reference=="Y")
# mostdeg_metrics=subset(candmetrics, reference=="N")
# metrics_rs=matrix(nrow=dim(metrics)[1],ncol=0)
# for(n in 3:dim(metrics)[2]){
#   metric=metrics[,n]
#   ref_metric=ref_metrics[,n]
#   mostdeg_metric=mostdeg_metrics[,n]
#   if(mean(ref_metric)>mean(mostdeg_metric)){
#     min=quantile(mostdeg_metric,0.05)
#     max=quantile(ref_metric,0.95)
#     metric_rs=(metric-min)/(max-min)}
#   if(mean(ref_metric)<mean(mostdeg_metric)){
#     min=quantile(ref_metric,0.05)
#     max=quantile(mostdeg_metric,0.95)
#     metric_rs=1-((metric-min)/(max-min))}
#   metric_rs[metric_rs>1]=1
#   metric_rs[metric_rs<0]=0
#   metrics_rs=cbind(metrics_rs,metric_rs)}
# colnames(metrics_rs)=colnames(metrics[3:8])
# row.names(metrics_rs)=rownames(metrics)
# 
# metrics_rs=as.data.frame(metrics_rs)
# #then average across rescaled metrics
# write.csv(metrics_rs,'final_MMI.csv')

          



          
          
          
          # first choice metrics
          candmetrics <- master %>%
            select(SAMPLEID, ReferenceSite, pt_habitat_rheo_resid, nt_habit_cling_resid,
                    pt_ti_stenocold_cold_cool_resid, pi_tv_intol_resid, pi_Pleco_resid)


                     
          
          metrics=candmetrics
          row.names(metrics)=metrics$SAMPLEID
          ref_metrics=subset(candmetrics, ReferenceSite=="REFERENCE")
          mostdeg_metrics=subset(candmetrics, ReferenceSite=="MOST DISTURBED")
          metrics_rs=matrix(nrow=dim(metrics)[1],ncol=0)
          for(n in 3:dim(metrics)[2]){
            metric=metrics[,n]
            ref_metric=ref_metrics[,n]
            mostdeg_metric=mostdeg_metrics[,n]
            if(mean(ref_metric)>mean(mostdeg_metric)){
              min=quantile(mostdeg_metric,0.05)
              max=quantile(ref_metric,0.95)
              metric_rs=(metric-min)/(max-min)}
            if(mean(ref_metric)<mean(mostdeg_metric)){
              min=quantile(ref_metric,0.05)
              max=quantile(mostdeg_metric,0.95)
              metric_rs=1-((metric-min)/(max-min))}
            metric_rs[metric_rs>1]=1
            metric_rs[metric_rs<0]=0
            metrics_rs=cbind(metrics_rs,metric_rs)}
          colnames(metrics_rs)=colnames(metrics[3:7]) # 7
          row.names(metrics_rs)=rownames(metrics)
          
          metrics_rs=as.data.frame(metrics_rs)
          
          metrics_rs <- metrics_rs %>%
            rownames_to_column('SAMPLEID') %>%
            left_join(site.type, by='SAMPLEID')%>%
            relocate(ReferenceSite, .before = 2)   %>%
            mutate(MMI.2024 = (pt_habitat_rheo_resid + nt_habit_cling_resid +
                    pt_ti_stenocold_cold_cool_resid + pi_tv_intol_resid + pi_Pleco_resid)/5)
          
          
          #then average across rescaled metrics
          write.csv(metrics_rs,'bugs analyses/MMI/_2024 model build/final_MMI_5.metrics.csv')
          
          
          
          boxplot(metrics_rs$MMI.2024 ~ metrics_rs$ReferenceSite, main='1st choice metrics - 5', ylim=c(0,1))
          
          t.test(metrics_rs$MMI.2024 ~metrics_rs$ReferenceSite)
              # t = -12.616
              # X most = 0.438
              # X ref = 0.680
          
          
          
          

          # second choice metrics
          candmetrics <- master %>%
            select(SAMPLEID, ReferenceSite, pt_tv_intol_resid, nt_habitat_rheo_resid,
                    pt_ti_stenocold_cold_cool_resid, pi_EPTNoHydro_resid)

          
          metrics=candmetrics
          row.names(metrics)=metrics$SAMPLEID
          ref_metrics=subset(candmetrics, ReferenceSite=="REFERENCE")
          mostdeg_metrics=subset(candmetrics, ReferenceSite=="MOST DISTURBED")
          metrics_rs=matrix(nrow=dim(metrics)[1],ncol=0)
          for(n in 3:dim(metrics)[2]){
            metric=metrics[,n]
            ref_metric=ref_metrics[,n]
            mostdeg_metric=mostdeg_metrics[,n]
            if(mean(ref_metric)>mean(mostdeg_metric)){
              min=quantile(mostdeg_metric,0.05)
              max=quantile(ref_metric,0.95)
              metric_rs=(metric-min)/(max-min)}
            if(mean(ref_metric)<mean(mostdeg_metric)){
              min=quantile(ref_metric,0.05)
              max=quantile(mostdeg_metric,0.95)
              metric_rs=1-((metric-min)/(max-min))}
            metric_rs[metric_rs>1]=1
            metric_rs[metric_rs<0]=0
            metrics_rs=cbind(metrics_rs,metric_rs)}
          colnames(metrics_rs)=colnames(metrics[3:6]) # 7
          row.names(metrics_rs)=rownames(metrics)
          
          metrics_rs=as.data.frame(metrics_rs)
          
          metrics_rs <- metrics_rs %>%
            rownames_to_column('SAMPLEID') %>%
            left_join(site.type, by='SAMPLEID')%>%
            relocate(ReferenceSite, .before = 2)   %>%
            mutate(MMI.2024 = (pt_tv_intol_resid + nt_habitat_rheo_resid +
                    pt_ti_stenocold_cold_cool_resid + pi_EPTNoHydro_resid)/4)
          
          
          #then average across rescaled metrics
          write.csv(metrics_rs,'bugs analyses/MMI/_2024 model build/final_MMI_4.metrics.csv')
          
          
          
          boxplot(metrics_rs$MMI.2024 ~ metrics_rs$ReferenceSite, main='4 metric model', ylim=c(0,1))
          
          t.test(metrics_rs$MMI.2024 ~metrics_rs$ReferenceSite)
                # t = -13.205
                # X most = 0.454
                # X ref = 0.727
          

          
          
          # FINAL FINAL FINAL = use 4 metric model
          
          
           
##############################################################################
          
#         SAVE RANDOM FORESTS MODELS FOR FINAL METRICS
          
#############################################################################
          

# 4 metric model
save(rfmod_pt_tv_intol, file = 'bugs analyses/MMI/_2024 model build/rfmod_pt_tv_intol.Rdata' )
save(rfmod_nt_habitat_rheo, file = 'bugs analyses/MMI/_2024 model build/rfmod_nt_habitat_rheo.Rdata' )          
save(rfmod_pt_ti_stenocold_cold_cool, file = 'bugs analyses/MMI/_2024 model build/rfmod_pt_ti_stenocold_cold_cool.Rdata' )          
save(rfmod_pi_EPTNoHydro, file = 'bugs analyses/MMI/_2024 model build/rfmod_pi_EPTNoHydro.Rdata' )                    
          



#################################################################################

#       MMI PRECISION AND RESPONSIVENESS

#################################################################################

stdev <- metrics_rs %>%
  group_by(ReferenceSite) %>%
  summarize(stdev = sd(MMI.2024))

    # ref = 0.103
    # most = 0.245


 ref.percents <- metrics_rs %>%
   filter(ReferenceSite == 'REFERENCE') %>%
   summarize(quantile(MMI.2024, probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)))
 
    # 5th = 0.55
    # 10th = 0.60
 
 mets.most <- metrics_rs %>%
  filter(ReferenceSite == 'MOST DISTURBED') # 158
 
 
 mets.most_.60 <- mets.most %>%
   filter(MMI.2024 < 0.6) # 113
  
 110/158*100 # 70%
 
#####
 
 # bias = rf models of MMI at ref sites only, using natural preds

#####
 
 mmi.ref <- metrics_rs %>%
   filter(ReferenceSite == 'REFERENCE') %>%
   select(SAMPLEID, MMI.2024) %>%
   left_join(predictorsdf) %>%
   select(-MLocID, -ReferenceSite, -COMID)
 
 
rfmod_mmi.ref = randomForest(MMI.2024 ~ AREASQKM+SAND+CLAY+ELEV+BFI+KFFACT+TMAX8110+PRECIP8110+COMPSTRGTH+INORGNWETDEP_2008+N+HYDRLCOND+MGO+K2O+NA2O+SIO2+CAO+P2O5+S+FE2O3+PERM+RCKDEP+OM+MSST_mean08.14+MWST_mean08.14+PCTICE_mean01.19+SLOPE,
          data=mmi.ref, ntree=2000, importance=TRUE, norm.votes=TRUE, keep.forest=TRUE)
 
 
rfmod_mmi.ref 



### 

#       Standardize MMI scores to compare to O/E models

###


ref.X <- mean(mmi.ref$MMI.2024) # 0.727

mmi.ref_stand.mean <- (mmi.ref$MMI.2024)/ref.X

mean(mmi.ref_stand.mean) # 1
sd(mmi.ref_stand.mean) # 0.142

quantile(mmi.ref_stand.mean, probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)) # 0.82 (10th)



###########################################

#    Percentiles and means for 4 metrics -- for use in re-scaling

############################################

perc05 <- function(x){
  quantile(x, 0.05)
}
 
perc95 <- function(x){
  quantile(x, 0.95)
}

mets.percents <- metrics %>% 
  group_by(ReferenceSite) %>%  
  summarise(across(pt_tv_intol_resid:pi_EPTNoHydro_resid,list(perc05 = perc05, perc95 = perc95), .names = "{.col}.{.fn}"))


as.data.frame(mets.percents)


                          #    ReferenceSite pt_tv_intol_resid.perc05 pt_tv_intol_resid.perc95 nt_habitat_rheo_resid.perc05 nt_habitat_rheo_resid.perc95 pt_ti_stenocold_cold_cool_resid.perc05
                          # 1 MOST DISTURBED                -50.83933                 11.60285                   -23.714250                     4.172515                              -39.30112
                          # 2      REFERENCE                -14.82727                 14.11435                    -7.815749                     9.185201                              -17.04776
                          #   pt_ti_stenocold_cold_cool_resid.perc95 pi_EPTNoHydro_resid.perc05 pi_EPTNoHydro_resid.perc95
                          # 1                               12.90728                  -54.54288                   24.13227
                          # 2                               14.89715                  -29.43413                   24.32300                              
                           # need to rescale based on percentiles of metrics
                                    # in the final model, all 4 metrics have ref means > most disturbed means
                                          # metric response = decrease with disturbance
                                    
                                    # here are the values to hard-code in:
                                              # 
                                              #                       REF_95th      MOST_5th
                                              # pt_tv_intol           14.11435      -50.83933
                                              # nt_habitat_rheo       9.185201      -23.714250
                                              # pt_ti_steno...        14.89715      -39.30112
                                              # pi_EPTNoHydro         24.32300      -54.54288


##########################################################################################################################################################################
######################################################################################
##########################################################################################################################################################################
######################################################################################
##########################################################################################################################################################################

#         RUN MMI FOR NEW DATASET

##########################################################################################################################################################################
######################################################################################
##########################################################################################################################################################################
######################################################################################
##########################################################################################################################################################################

library(randomForest)
library(VSURF)
library(psych)
library(dplyr)
library(tibble)


#######
    ####### BRING IN METRICS
#######
 
# must bring in metrics calculated from SUBSAMPLED bug data (to 300 count)
load('bugs analyses/MMI/_2024 model build/bug.metrics_ref.status.Rdata')
metricsdf <- bug.metrics_ref.status %>%
  select(SAMPLEID, pt_tv_intol, nt_habitat_rheo, pt_ti_stenocold_cold_cool, pi_EPTNoHydro)
rm(bug.metrics_ref.status)



#######
    ######## BRING IN PREDICTORS
#######

# bring in as many predictors (StreamCat) as you want, code will select the correct predictors to keep

load('bugs analyses/MMI/_2024 model build/mmi.pred.mets_414_final.Rdata')
predictorsdf <- mmi.pred.mets_414_final %>%
  select(SAMPLEID, TMAX8110, CLAY, OM, KFFACT, PRECIP8110, ELEV, MSST_mean08.14,  
         PERM, AREASQKM, SLOPE, P2O5)
rm(mmi.pred.mets_414_final)




@@@@@@ dont do NAs -- see if model can accept NAs

#######
    ####### JOIN METRICS AND PREDICTORS
#######

rfdat_all <- metricsdf %>% 
        left_join(predictorsdf, by='SAMPLEID')

Drfdat <- rfdat_all




#######
    ######## RUN RANDOM.FORESTS MODELS - FOR EACH OF 4 METRICS
#######

# LOAD MODELS
load('bugs analyses/MMI/_2024 model build/rfmod_pt_tv_intol.Rdata' )
load('bugs analyses/MMI/_2024 model build/rfmod_nt_habitat_rheo.Rdata' )          
load('bugs analyses/MMI/_2024 model build/rfmod_pt_ti_stenocold_cold_cool.Rdata' )          
load('bugs analyses/MMI/_2024 model build/rfmod_pi_EPTNoHydro.Rdata' )                    
 

# which rf models to use
rfmodels <- c('rfmod_pt_tv_intol', 'rfmod_nt_habitat_rheo', 'rfmod_pt_ti_stenocold_cold_cool',
                   'rfmod_pi_EPTNoHydro')

## test site predictions


Dpredictions=list()
for (i in 1:length(rfmodels)){
  tryCatch({Dpredictions[[paste0("E.",rfmodels[i])]]<- round(predict(eval(parse(text =paste0(rfmodels[i]))), Drfdat, type = "response"),digits=4)
  }, error =function (e){
    cat(paste0("/n/tERROR calculating: ",paste0(names(rfdat)[i],"_pred"),"/n"))
    str(e,indent.str = "   "); cat("/n")
  })
}
predictionsdf=as.data.frame(do.call(cbind,Dpredictions))
#join predictions into master dataframe
Drfdat2=cbind(Drfdat,predictionsdf)




#######
    ######## CALCULATE RESIDUALS
#######

resid=list()
for (i in 2:5){
  tryCatch({resid[[paste0(colnames(Drfdat2)[i],"_resid")]]=Drfdat2[,i]- Drfdat2[,paste0("E.rfmod_",colnames(Drfdat2)[i])]
  
  }, error =function (e){
    cat(paste0("/n/tERROR calculating: ",paste0(Drfdat2[i],"_resid"),"/n"))
    str(e,indent.str = "   "); cat("/n")
  })
  
}

                
                
residualsdf=as.data.frame(do.call(cbind,resid))

rfdat_all_final4=cbind(Drfdat2,residualsdf)



#######
    ########    RESCALE TO 0 - 1, 
#######


# Select SAMPLEID + 4 metrics.residuals
candmetrics <- rfdat_all_final4 %>%
  select(SAMPLEID, pt_tv_intol_resid, nt_habitat_rheo_resid,
          pt_ti_stenocold_cold_cool_resid, pi_EPTNoHydro_resid)

 
                     
          # need to rescale based on percentiles of metrics
          # in the final model, all 4 metrics have ref means > most disturbed means
                # metric response = decrease with disturbance
          
           # need to rescale based on percentiles of metrics
          # in the final model, all 4 metrics have ref means > most disturbed means
                # metric response = decrease with disturbance
          
          # here are the values to hard-code in:
                    # 
                    #                       REF_95th      MOST_5th
                    # pt_tv_intol           14.11435      -50.83933
                    # nt_habitat_rheo       9.185201      -23.714250
                    # pt_ti_steno...        14.89715      -39.30112
                    # pi_EPTNoHydro         24.32300      -54.54288
          
          
          
          metrics=candmetrics  
          min = c(-50.83933, -23.714250, -39.30112, -54.54288)
          max = c(14.11435, 9.185201, 14.89715, 24.32300)                                                                                 
          metrics_rs=matrix(nrow=dim(metrics)[1],ncol=0)
          for(n in 3:6){
            metric=metrics[,n]
          metric_rs=(metric-min)/(max-min)
          }
            
         @@@ What we need: for each of the 4 metrics in "metrics", 
                              first subtract the min from the metric, 
                              second subtract min from the max, 
                              then divide first by second
                              RESULT = scaled between 0 -1
                              
                              
          @@@@@ Results of the above are all 
            
            
            #if(mean(ref_metric)<mean(mostdeg_metric)){
            #  min=quantile(ref_metric,0.05)
            #  max=quantile(mostdeg_metric,0.95)
            #  metric_rs=1-((metric-min)/(max-min))}
            metric_rs[metric_rs>1]=1
            metric_rs[metric_rs<0]=0
            metrics_rs=cbind(metrics_rs,metric_rs)
            }
          
          
          
          
            
          
          colnames(metrics_rs)=colnames(metrics[3:7]) # 7
          row.names(metrics_rs)=rownames(metrics)
          
          metrics_rs=as.data.frame(metrics_rs)

          
          
          
# FINAL MMI 
          
          #then average across rescaled metrics
          metrics_rs <- metrics_rs %>%
            rownames_to_column('SAMPLEID') %>%
            left_join(site.type, by='SAMPLEID')%>%
            relocate(ReferenceSite, .before = 2)   %>%
            mutate(MMI.2024 = (pt_habitat_rheo_resid + nt_habit_cling_resid +
                    pt_ti_stenocold_cold_cool_resid + pi_tv_intol_resid + pi_Pleco_resid)/5)
          
          
          















                                                                                                    #' 