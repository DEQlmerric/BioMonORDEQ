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

      
      
      @@@@@@@@@@@ leave ReferenceSite in !!!!!
      
      
      rfdat_all <- metricsdf %>%
        select(-MLocID, -ReferenceSite, -INDEX_CLASS, -INDEX_NAME, -ri_ti_sccc_wsw) %>% 
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

      metrics=names(rfdat[1:286])
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
        assign(paste0("rfdat",i),rfdat[,c(i,287:315)])
        species.vsurf = VSURF(rfdat[,287:315], rfdat[,i])
        names = as.data.frame(names(rfdat[,c(287:315,i)]))
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
    
    
    
    
#----------------------------------------------------------------------------------------------------#
#Step 5- Get predicted values out of model objects for reference sites and predict values for degraded sites
#----------------------------------------------------------------------------------------------------#
##reference site predictions
#select random forest models of interest from workspace
#rfmodels=objects()[243:249]

      rfmodels=objects()[307:592]
                          # better to call by name? select the top performing models, based on 'randomforest_results'?
                    
                          rfmodels <- c('rfmod_pt_ti_stenocold','rfmod_nt_ti_cowa_warm_stenowarm', 'rfmod_nt_tv_intol','rfmod_pt_tv_intol4',
                                        'rfmod_nt_POET','rfmod_nt_ti_stenocold','rfmod_nt_EPT','rfmod_x_Becks','rfmod_pt_ti_warm_stenowarm',
                                        'rfmod_pt_Coleo','rfmod_pt_EPT','rfmod_nt_habitat_rheo','rfmod_nt_habit_climbcling',
                                        'rfmod_nt_habit_cling','rfmod_nt_ffg_pred_scrap_shred')


     # @@@@@ - below (step 6) calculating residuals and calling by indexing (not by name)....just do it for all metrics at once?
        
        

df=list()
for (i in 1:length(rfmodels)){
  df[[paste0("E.",rfmodels[i])]]=eval(parse(text =paste0(rfmodels[i])))$predicted
}
Rpredicteddf=as.data.frame(do.call(cbind,df))
#join predictions into master dataframe
rfdat2=cbind(rfdat,Rpredicteddf)


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
                for (i in 1:286){
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



          
          for (i in 604:889){     
            tryCatch({t=t.test(rfdat_all_final4[,i]~rfdat_all_final4$ReferenceSite)
            tvalues[[paste0(colnames(rfdat_all_final4)[i])]]=unlist(t$statistic[[1]])
            }, error =function (e){
              cat(paste0("/n/tERROR calculating: ",paste0(names(rfdat_all_final4)[i],"_tvalue"),"/n"))
              str(e,indent.str = "   "); cat("/n")
            })
          }
          tvalues=as.data.frame(do.call(rbind,tvalues))
          write.csv(tvalues,"bugs analyses/MMI/_2024 model build/tvalues.csv")















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
            select(SAMPLEID, ReferenceSite, nt_EPT_resid, pt_ti_cowa_warm_stenowarm_resid,
                  pi_habitat_rheo, pi_NonIns, pi_Colesens_resid)

          
          
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
            mutate(MMI.2024 = (nt_EPT_resid+pt_ti_cowa_warm_stenowarm_resid+
                pi_habitat_rheo+pi_NonIns+pi_Colesens_resid)/5)
          
          
          #then average across rescaled metrics
          write.csv(metrics_rs,'bugs analyses/MMI/_2024 model build/final_MMI.csv')
          
          
          
          boxplot(metrics_rs$MMI.2024 ~ metrics_rs$ReferenceSite)
          