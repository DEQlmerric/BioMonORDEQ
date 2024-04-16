library(randomForest)
library(VSURF)
library(psych)
library(dplyr)

#-------------------------------------------------------------------------------------------------------#
# Step 1- Read in predictors
#-------------------------------------------------------------------------------------------------------#
predictorsdf=read.csv("predictors.csv")
#-------------------------------------------------------------------------------------------------------#
# Step 2- Read in macroinvertebrate metrics 
#-------------------------------------------------------------------------------------------------------#
 metricsdf=read.csv("macroinvert_metrics.csv")
#-------------------------------------------------------------------------------------------------------#
# Step 3- combine predictors and metrics into two dataframes, one with only reference sites and one with all sites
#-------------------------------------------------------------------------------------------------------#
rfdat_all=dplyr::left_join(metricsdf,predictorsdf,by="sampleId")
rfdat=subset(rfdat_all,reference=='Y')
#----------------------------------------------------------------------------------------------------#
#Step 4- create random forest models to predict natural variation in all metrics
#----------------------------------------------------------------------------------------------------#
# run Vsurf in a loop to select predictors using data frame with only reference sites
metrics=names(rfdat[2:152])
formulas=list()
variance_explained=list()

for (i in 1:length(metrics)){
  assign(paste0("rfdat",i),rfdat[,c(i,199:253)])
  species.vsurf = VSURF(rfdat[,199:253], rfdat[,i])
  names = as.data.frame(names(rfdat[,c(199:253,i)]))
  selected.pred=names[species.vsurf$varselect.pred,]
  assign(paste0("rfmod_",names(rfdat)[i]),
         randomForest(as.formula(paste0(names(rfdat)[i],"~",paste(selected.pred,collapse="+"))), data=eval(parse(text =paste0("rfdat",i))), ntree=2000, importance=TRUE, norm.votes=TRUE, keep.forest=TRUE)) 
  print(paste0(names(rfdat)[i],"~",paste(selected.pred,collapse="+")))
  print(eval(parse(text =paste0("rfmod_",names(rfdat)[i]))))
  
  formulas[[names(rfdat)[i]]]<-print(paste0(names(rfdat)[i],"~",paste(selected.pred,collapse="+")))
  variance_explained[[names(rfdat)[i]]]<-eval(parse(text =paste0("rfmod_",names(rfdat)[i],"$rsq[2000]")))
}
nestedlist=list(unlist(formulas),unlist(variance_explained))

randomforest_results=as.data.frame(do.call(cbind,nestedlist))
write.csv(randomforest_results,"randomforest_results.csv")

#----------------------------------------------------------------------------------------------------#
#Step 5- Get predicted values out of model objects for reference sites and predict values for degraded sites
#----------------------------------------------------------------------------------------------------#
##reference site predictions
#select random forest models of interest from workspace
rfmodels=objects()[243:249]

df=list()
for (i in 1:length(rfmodels)){
  df[[paste0("E.",rfmodels[i])]]=eval(parse(text =paste0(rfmodels[i])))$predicted
}
Rpredicteddf=as.data.frame(do.call(cbind,df))
#join predictions into master dataframe
rfdat2=cbind(rfdat,Rpredicteddf)


##degraded site predictions
Drfdat=subset(rfdat_all, reference=="N")

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
write.csv(rfdat_all_final,"rfdat_all_final.csv")

#----------------------------------------------------------------------------------------------------#
# Step 6 Calculate residuals
#----------------------------------------------------------------------------------------------------#
resid=list()
for (i in 2:190){
  tryCatch({resid[[paste0(colnames(rfdat_all_final)[i],"_resid")]]=rfdat_all_final[,i]- rfdat_all_final[,paste0("E.rfmod_",colnames(rfdat_all_final)[i])]
  
  }, error =function (e){
    cat(paste0("/n/tERROR calculating: ",paste0(rfdat_all_final[i],"_resid"),"/n"))
    str(e,indent.str = "   "); cat("/n")
  })
  
}

residualsdf=as.data.frame(do.call(cbind,resid))

rfdat_all_final4=cbind(rfdat_all_final,residualsdf)

#----------------------------------------------------------------------------------------------------#
#Step 7- deterimine how well residuals discrimiate between reference and degraded sites with t-tests
#----------------------------------------------------------------------------------------------------#
tvalues=list()
for (i in 436:624){
  tryCatch({t=t.test(rf_dat_all_final4[,i]~rf_dat_all_final4$reference)
  tvalues[[paste0(colnames(rf_dat_all_final4)[i])]]=unlist(t$statistic[[1]])
  }, error =function (e){
    cat(paste0("/n/tERROR calculating: ",paste0(names(rf_dat_all_final4)[i],"_tvalue"),"/n"))
    str(e,indent.str = "   "); cat("/n")
  })
}
tvalues=as.data.frame(do.call(rbind,tvalues))
write.csv(tvalues,"tvalues.csv")

#----------------------------------------------------------------------------------------------------#
#Step 8- Use PCA to select 5 metric residuals that are least correlated and have highest t values
#----------------------------------------------------------------------------------------------------#
#select metric residuals for metrics with randomforest mode R2>0.10 and if not use original metric values instead
master=rfdat_all_final3
reference=subset(master, reference=="Y")
select=principal(reference[c(-1,-2)],nfactors=5,covar=FALSE, rotate='varimax',scores=TRUE)
select$loadings

#----------------------------------------------------------------------------------------------------#
#Step 9- rescale selected 5 metrics to be on same scale, then average them for a final MMI value
#----------------------------------------------------------------------------------------------------#
candmetrics=master[,1:7]
metrics=candmetrics
row.names(metrics)=metrics$sampleId
ref_metrics=subset(candmetrics, reference=="Y")
mostdeg_metrics=subset(candmetrics, reference=="N")
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
colnames(metrics_rs)=colnames(metrics[3:8])
row.names(metrics_rs)=rownames(metrics)

metrics_rs=as.data.frame(metrics_rs)
#then average across rescaled metrics
write.csv(metrics_rs,'final_MMI.csv')