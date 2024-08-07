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
        select(-met.type, -REGIONID)
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

                                     # drop metrics with 'Inf' values or only 0 for results
                                     #     metricsdf <- metricsdf %>%
                                      #      select(-ri_ti_sccc_wsw, -nt_habit_climb, -pi_habit_climb, -pt_habit_climb, -nt_Tubif,
                                       #            -pi_Juga, -pi_JugaFlumi, -nt_dni, -pi_dni, -pt_dni)# ri_ti_sccc_wsw -- this metric has 'Inf' values
                                    
                                     
#-------------------------------------------------------------------------------------------------------#
# Step 3- combine predictors and metrics into two dataframes, one with only reference sites and one with all sites
#-------------------------------------------------------------------------------------------------------#
#rfdat_all=dplyr::left_join(metricsdf,predictorsdf,by="sampleId")
#rfdat=subset(rfdat_all,reference=='Y')

      
      
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
        assign(paste0("rfdat",i),rfdat[,c(i,278:306)])
        species.vsurf = VSURF(rfdat[,278:306], rfdat[,i])
        names = as.data.frame(names(rfdat[,c(278:306,i)]))
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
    metrics_good.mods <- metrics_good.mods[!metrics_good.mods %in%  c("nfam_Coleo_resid","nfam_Ephem_resid","nfam_Trich_resid")]
    
    # the following metrics were dropped from rf mods:
    # "nt_Isop" "pi_Caen" "pi_Corb" "pi_Isop" "pt_Isop"
    
    
#----------------------------------------------------------------------------------------------------#
#Step 5- Get predicted values out of model objects for reference sites and predict values for degraded sites
#----------------------------------------------------------------------------------------------------#
##reference site predictions
#select random forest models of interest from workspace
#rfmodels=objects()[243:249]

                              rfmodels=objects()[302:587]
      # better to call by name? select the top performing models, based on 'randomforest_results'?

      
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

@@@@@ not understanding this: looks like take the value of a metric and subtract E.rfmod value, to get residual 
done by indexing, but if I look at colnames of rfdat_all_final, the metrics columns (1:286) are not in the same
order as E.rfmod cols (316: 601)



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





      #create a new dframe of only resids and orig.<10, use this in both 7 & 8
      
      rfdat_all_final3 <- rfdat_all_final4 %>%
            select(SAMPLEID, ReferenceSite, all_of(c(metrics_poor.mods, metrics_good.mods))) 

#----------------------------------------------------------------------------------------------------#
#Step 7- deterimine how well residuals discrimiate between reference and degraded sites with t-tests
#----------------------------------------------------------------------------------------------------#


tvalues=list()

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ what does 436:624 represent?????? ---residuals????
  
          # Get ReferenceSite in the dframe
          site.type <- metricsdf %>%
            select(SAMPLEID, ReferenceSite)

          rfdat_all_final4 <- rfdat_all_final4 %>%
            rownames_to_column('SAMPLEID') %>%
            left_join(site.type, by = 'SAMPLEID') %>%
            relocate(ReferenceSite, .before = 2)

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


576 - 841 = only resids.  need to add in originals for poor mods
          
          for (i in 576:841){     
            tryCatch({t=t.test(rfdat_all_final4[,i]~rfdat_all_final4$ReferenceSite)
            tvalues[[paste0(colnames(rfdat_all_final4)[i])]]=unlist(t$statistic[[1]])
            }, error =function (e){
              cat(paste0("/n/tERROR calculating: ",paste0(names(rfdat_all_final4)[i],"_tvalue"),"/n"))
              str(e,indent.str = "   "); cat("/n")
            })
          }
          tvalues=as.data.frame(do.call(rbind,tvalues)) 
          
          tvalues <- tvalues %>% rownames_to_column('metric')
          
          
          
          write.csv(tvalues,"bugs analyses/MMI/_2024 model build/tvalues.csv")

          # 266 metrics













#----------------------------------------------------------------------------------------------------#
#Step 8- Use PCA to select 5 metric residuals that are least correlated and have highest t values
#----------------------------------------------------------------------------------------------------#
#select metric residuals for metrics with randomforest mode R2>0.10 and if not use original metric values instead

only want residuals and originals.<10 for ref only (see end of step 6 df)           
          
        
              
              
          
# master=rfdat_all_final3
# reference=subset(master, reference=="Y")
# select=principal(reference[c(-1,-2)],nfactors=5,covar=FALSE, rotate='varimax',scores=TRUE)
# select$loadings
        

          master=rfdat_all_final3   
          reference=subset(master, ReferenceSite=="REFERENCE")
          select=principal(reference[c(-1,-2)],nfactors=5,covar=FALSE, rotate='varimax',scores=TRUE)
          select$loadings

          
          # make a df out of pca loads, then combine with tvalues to select metrics
          pca.loads <- as.data.frame(select$loadings[1:269, 1:5]) %>%
            rownames_to_column('metric')

          
          
          pca.tval <- pca.loads %>%
            left_join(tvalues, by = 'metric')


          write.csv(pca.tval, 'bugs analyses/MMI/_2024 model build/pca.tval.csv')




#----------------------------------------------------------------------------------------------------#
#Step 9- rescale selected 5 metrics to be on same scale, then average them for a final MMI value
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

          



          #candmetrics=master[,1:7]   # sampleid, ref, 5 mets
          
          
          
          candmetrics <- master %>%
            select(SAMPLEID, ReferenceSite, pi_tv_intol4_resid, nt_volt_semi_resid,
                    pt_ti_stenocold_cold_cool_resid, pt_habit_cling_resid,
                    pi_Insect_resid)

          
          
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
          colnames(metrics_rs)=colnames(metrics[3:7])
          row.names(metrics_rs)=rownames(metrics)
          
          metrics_rs=as.data.frame(metrics_rs)
          
          metrics_rs <- metrics_rs %>%
            rownames_to_column('SAMPLEID') %>%
            left_join(site.type, by='SAMPLEID')%>%
            relocate(ReferenceSite, .before = 2)   %>%
            mutate(MMI.2024 = (pi_tv_intol4_resid+nt_volt_semi_resid+
                    pt_ti_stenocold_cold_cool_resid+pt_habit_cling_resid+
                    pi_Insect_resid)/5)
          
          
          #then average across rescaled metrics
          write.csv(metrics_rs,'bugs analyses/MMI/_2024 model build/final_MMI.csv')
          
          
          
          boxplot(metrics_rs$MMI.2024 ~ metrics_rs$ReferenceSite)
          