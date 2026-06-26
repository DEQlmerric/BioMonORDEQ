#### R script for taxa tolerance analyses (GAM Analysis)
# Code developed by Mark Fernandez, Tetra Tech; Mark.Fernandez@tetratech.com
# Updated by Ben Block, Tetra Tech; Ben.Block@tetratech.com
# Date created: 03/07/2025
# Date updated: 03/07/2025

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R version 4.4.1 (2024-06-14) -- "Race for Your Life"


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6/24/26: Modified by Shannon Hubler for OR and WA fines dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Libraries ####
library(readr)
library(dplyr)
library(tidyr)
library(data.table)   # [
library(ggplot2)      # ggplot()
library(ggforce)      # facet_wrap_paginate()
library(mgcv)         # gam()
library(sf)           # st_as_sf()
# Options ####
# options(digits=5L)
par(mar=c(4.5,4.5,1,1),bty='l')
theme_set(theme_classic())
theme_update(legend.position='bottom')

# Settings
na.strings=c(NA,'NA','N/A','#N/A','','None','<Null>','NULL')
seed=27709L
pointsize=9L
txtSize=12L
num_threads=10L

# Plots
breaksProb=seq(0,1,by=0.1)
labelsProb=breaksProb
labelsProb=c(0,'',0.2,'',0.4,'',0.6,'',0.8,'',1)

is.nan.data.frame=function(x) do.call(cbind,lapply(x,is.nan))
is.infinite.data.frame=function(x) do.call(cbind,lapply(x,is.infinite))

# labels for plot ticks
labelsPretty=function(x){
  formatC(x,digits=NULL,big.mark=',',format='fg')
}

# Set arguments
formals(melt.data.table)$na.rm=T
formals(melt.data.table)$variable.factor=F
formals(dcast.data.table)$fill=NA
formals(dcast.data.table)$value.var='value'
formals(fread)$sep=','
formals(fread)$skip=0
formals(fread)$header=T
formals(fread)$check.names=F # Keep FALSE
formals(fread)$na.strings=na.strings
formals(png)$units='in'
formals(png)$res=300
formals(png)$type='cairo'
formals(png)$pointsize=pointsize
formals(expand.grid)$KEEP.OUT.ATTRS=F
formals(expand.grid)$stringsAsFactors=F
formals(rbindlist)$use.names=T
formals(rbindlist)$fill=T

# Logit transformation
# ====================.
if(F){
  x=c(0.2,0.4,0.9)
  x
  y=qlogis(x) # Prob => Real
  plogis(y)   # Real => Prob	
}

# Declare directories ####
wd <- getwd()
myDate <- format(Sys.Date(), "%Y%m%d")
input.dir <- "bugs analyses/stressor/TolAnal_forShannon/Input_Data"
state.dir <- "MD_MBSS"
output.dir <- "bugs analyses/stressor/TolAnal_forShannon/Output_Data"
results.dir <- paste0("/Taxa_Tolerance_Results_", state.dir,"_",myDate,"/")

# create results folder
boo_Results <- dir.exists(file.path(wd, output.dir, results.dir))
if(boo_Results==FALSE){
  dir.create(file.path(wd, output.dir, results.dir), recursive = TRUE) #SLH 6.24.26: I had to add recursive piece
}


# specify input files
      #fn.data1 <- "_TolAnal_MD_MBSS_Run1_20250307.csv"

fn.data1 <- "_TolAnal_MD_20250305_All.csv"


# Read data files ####
df_orig <- read_csv(file.path(wd, input.dir, state.dir, fn.data1)
               , na = c("NA",""), trim_ws = TRUE, skip = 0
               , col_names = TRUE, guess_max = 100000)

# cleanup
rm(boo_Results, input.dir, fn.data1)

# Data processing ####
# state
state=c('MD_MBSS')
stateName=c('MD_MBSS')

## Transform data ####
# df_preds <- df_orig %>%
#   select(BugSampleID, MSST:ELEVCAT_m) %>%
#   distinct()
# 
# df_preds_sqrt <- df_preds %>%
#   mutate(across(where(is.numeric), ~ sqrt(.), .names = "{.col}_sqrt")) %>%
#   select(ends_with("sqrt"))
# 
# df_preds_log <- df_preds %>%
#   mutate(across(where(is.numeric), ~ log(.), .names = "{.col}_log")) %>%
#   select(ends_with("log"))
# 
# df_preds_v2 <- cbind(df_preds, df_preds_sqrt, df_preds_log)
# 
# df_preds_v3 <- df_preds_v2 %>%
#   select(sort(names(df_preds_v2)))
# 
# ## Plot data
# df_preds_v4 <- df_preds_v3 %>%
#   pivot_longer(!c(BugSampleID), names_to = "Parameter", values_to = "Value")
# 
# params <- unique(df_preds_v4$Parameter)
# 
# plot_list <- list()
# counter <- 0
# 
# for(i in params){
# 
#   counter <- counter + 1
#   ## filter data
# 
#   df_loop <- df_preds_v4 %>%
#     filter(Parameter == i)
# 
#   Param_Name <- i
# 
#   # plot
#   plot <- ggplot(data = df_loop, aes(x = Value))+
#     geom_histogram(fill="#69b3a2", alpha=0.8)+
#     labs(x = paste(Param_Name)
#          , title = paste0("N Samples: ", nrow(df_loop), "/874"))+
#     theme_bw()
# 
#   plot_list[[i]] <- plot
# 
# } # End loop
# 
# ### Export plots
# # create pdf
# pdf(file = file.path(wd, output.dir, results.dir
#                      , paste0("Predictor_Plots_"
#                               , myDate, ".pdf")))
# for (i in 1:length(plot_list)) {
#   print(plot_list[[i]])
# }
# dev.off()
# 
# # cleanup
# rm(plot, plot_list, df_loop, df_preds, df_preds_log, df_preds_sqrt, df_preds_v2
#    , df_preds_v3, df_preds_v4, i, counter, params, Param_Name)

# Transformations
# Check for value=0
my_preds <- c("MSST", "IWI_v2_1", "WSAREASQKM", "pSLOPE", "ELEVCAT_m")

tmp <- df_orig %>% 
  select(BugSampleID, one_of(my_preds)) %>% 
  distinct() %>% 
  pivot_longer(!c(BugSampleID), names_to = "Predictor", values_to = "Value") %>% 
  filter(Value == 0) %>% 
  count(Predictor)
rm(tmp, my_preds)

# no zero values in dataset, otherwise, run code below.

# Get min non-zero values
# rm(tmp)
# tmp <- df_orig %>% 
#   select(BugSampleID, one_of(my_preds)) %>% 
#   distinct() %>% 
#   pivot_longer(!c(BugSampleID), names_to = "Predictor", values_to = "Value") %>% 
#   filter(Value != 0 & !is.na(Value)) %>% 
#   group_by(Predictor) %>% 
#   summarize(min_value = min(Value))
# 
# print(tmp)
# rm(tmp)

# Transformations
# df_input_v2 <- df_orig %>% 
#   mutate(ELEVCAT_m_log = log(ELEVCAT_m)
#          , IWI_v2_1_logNeg = log(1-IWI_v2_1)
#          , pSLOPE_log = log(pSLOPE)
#          , WSAREASQKM_log = log(WSAREASQKM)
#          # , MSST = "identity"
#          )

# Transformations
df_input_v2 <- df_orig %>%
  mutate(#ELEVCAT_m = "identity"
         # , IWI_v2_1 = "identity"
         , pSLOPE_log = log(pSLOPE)
         , WSAREASQKM_log = log(WSAREASQKM)
         # , MSST = "identity"
         )

## SiteDateRA table ####
SiteDateRA <- df_input_v2 %>% 
  select(-c( Source, Count, RMN_StationID, WaterbodyName, COMID_Final # Select_Run1,
            , Year, Month, CollDate, CollMeth, Plot_Label, Taxon_Group)) %>% # remove unnecessary fields
  relocate(OTU_TolAnal, .after = last_col()) %>%
  pivot_wider(., names_from = OTU_TolAnal, values_from = RA
              , values_fill = 0) %>% 
  rename(SampleID = BugSampleID)

SiteDateRA <- as.data.table(SiteDateRA)
nrow(SiteDateRA)# 874

## OTU table ####
otuTable_State <- df_input_v2 %>% 
  mutate(TaxaGroup = Taxon_Group
         , TaxaGroup = gsub("OTHER NON-INSECT", "OTHER_NON_INSECT", TaxaGroup)) %>%
  select(TaxaGroup, OTU_TolAnal, Plot_Label) %>% 
  distinct() %>% 
  rename(OTU = OTU_TolAnal
         , OTU_Label = Plot_Label) %>% 
  arrange(TaxaGroup, OTU, OTU_Label) %>% 
  mutate(SortState = row_number()
         , nchar = nchar(OTU))

otuTable_State <- as.data.table(otuTable_State)
## SiteDatePA table ####
# otusState
otusState=sort(unique(otuTable_State$OTU))
length(otusState) # 718

rm(SiteDatePA)
SiteDatePA=copy(SiteDateRA)
SiteDatePA[,.(CHIRONOMINAE,CHIRONOMINI,CONCHAPELOPIA)] # Check first few taxa
SiteDatePA[,(otusState):=lapply(.SD,function(x) ifelse(x>0,1,0)),.SDcols=otusState]
SiteDatePA[,(otusState):=lapply(.SD,function(x) as.integer(x)),.SDcols=otusState]
SiteDatePA[,.(CHIRONOMINAE,CHIRONOMINI,CONCHAPELOPIA)] # Check P/A transformation

# Counts
nrow(otuTable_State)              # 670 taxa
nrow(SiteDatePA)                  # 3408 SampleIDs
uniqueN(otuTable_State$OTU)       # 670 taxa (includes nOcc <30)
uniqueN(otuTable_State$TaxaGroup) # 14 taxaGroups

## Things to update list ####
# Loop variables - Starts Line 255
# breaks & labels - Starts Line 450
# digits - Starts line 1355

## Loop Variables ####

# vars
rm(vars,varsLab,varsTrans,varsScale)
vars=c("ELEVCAT_m", "IWI_v2_1", "MSST", "pSLOPE", "WSAREASQKM")

varsLab=c("Catchment Elevation (m)", "Index of Watershed Integrity"
          , "Mean Summer Stream Temperature", "Percent Stream Slope"
          , "Watershed Area (Sq km)")

varsTrans=c("ELEVCAT_m", "IWI_v2_1", "MSST"
            , "pSLOPE_log", "WSAREASQKM_log")

varsScale=c('identity','identity','identity','log','log') # identity
cbind(varsScale,varsTrans,vars,varsLab)
length(vars) # 5 vars

# taxaGroups_State
rm(taxaGroups_State)
taxaGroups_State=sort(unique(otuTable_State$TaxaGroup))
length(taxaGroups_State) # 14 taxaGroups

# regions
rm(regions)
regions=c('MD_MBSS')
length(regions) # 1 regions

###################################*
### Directories ####
###################################*

# Directories
# dir.create(paste(output.dir, results.dir, 'Explore',sep='/'),recursive=T)
#
r=v=1L
for(r in 1:length(regions)){
  for(v in 1:length(vars)){
    dir.create(paste(output.dir, results.dir, regions[r],vars[v],sep='/')
               ,recursive=T)
    dir.create(paste(output.dir,results.dir, 'DistributionMaps',vars[v],sep='/')
               ,recursive=T)
  }
}
warnings()

###################################*
# *** LOOP *** ####
###################################*

# GAM settings
ci=0.90 # EPA 2006 used 90% CI. 
mult=qnorm(1-(1-ci)/2)
min_CI=0.1 # For "POC_Max_CI"
K=8L # Use k=10 for ORWA MWMT. Use 8 for ORWA-IWI. Use 5-8 for other projects. 

# map
rm(map)
map=st_as_sf(maps::map('state',fill=T,plot=F))
map=map[map$ID %in% c("maryland"),]
nrow(map) # 7

gc(); save.image()


## Loop controls ####
#### Loop yyy
plot(sample(99,1))
plots=T # controls whether plots are printed or not
bpjL=F
regionalVarsPlotsOnly=F
r=v=g=o=i=1L
r=1 # regions
v=6 # vars
g=3 # taxaGroups_RegionVar
# o=1 # otusRegionVarGroup
# i=1 # plot loops
tock=Sys.time()
# j=1
for(r in 1:length(regions)){ # Loop across regions   1:length(regions)
  gc()
  cat('\n\n',regions[r],' ')
  
  # Subset
  rm(dfRegion)
  dfRegion=copy(SiteDatePA)
  # if(regions[r]!='Statewide') dfRegion=dfRegion[Region==regions[r],]
  nrow(dfRegion) # 2,609
  
  #### Do these after filtering for vars:
  # Remove taxa with nOcc=0
  # Flag taxa with nOcc<30
  
  ###################################*
  ## Boxplots ####
  ###################################*
  
  if(regionalVarsPlotsOnly){
    
    # dfLong
    rm(dfLong)
    dfLong=melt(dfRegion,id.vars=c('SampleID'),measure.vars=vars)
    dfLong[,.N,by=variable]
    nrow(dfLong) # 6246
    
    # Settings
    nrow=3
    title=paste0(stateName,' - ',regions[r])
    
    # Boxplots - Arith
    rm(plt)
    plt=ggplot(dfLong,aes(y=value,x=''))+
      geom_jitter(position=position_jitter(w=0.15,h=0),alpha=0.5)+
      geom_boxplot(outlier.shape=NA,fill=NA)+
      scale_y_continuous(trans='identity',labels=labelsPretty)+
      labs(y='Value',x=NULL,title=title)+
      facet_wrap(~variable,nrow=nrow,scales='free_y')+
      theme_bw()+
      theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme(legend.position='none')+
      theme(text=element_text(size=txtSize))
    png(paste0(state,'/',regions[r],'/Boxplots_Arith.png'),width=20,height=10)
    print(plt)
    dev.off()
    
    # Boxplots - Sqrt
    plt=plt+scale_y_continuous(trans='sqrt',labels=labelsPretty)
    png(paste0(state,'/',regions[r],'/Boxplots_Sqrt.png'),width=20,height=10)
    print(plt)
    dev.off()
    
    # Boxplots - Log
    plt=plt+scale_y_continuous(trans='log10',labels=labelsPretty)
    png(paste0(state,'/',regions[r],'/Boxplots_Log.png'),width=20,height=10)
    print(plt)
    dev.off()
    
    
    
    
    
    ###################################*
    ### Correlation ####
    ###################################*
    
    # Correlogram
    rm(M)
    M=psych::corr.test(dfRegion[,..varsTrans],method='pearson',use='pairwise',adjust='none',ci=F)
    
    # Plot
    png(paste0(state,'/',regions[r],'/Correlation.png'),width=6.5,height=6.5)
    # par(mar=c(4.5,4.5,1,1)) # Not respected.
    corrplot::corrplot(M$r,type='lower',method='number',order='hclust',hclust.method='complete',diag=T,tl.pos='n',cl.pos='r',main=NULL)
    corrplot::corrplot(M$r,add=T,order='hclust',hclust.method='complete',type='upper',method='circle',tl.pos='d',tl.col=1,tl.cex=0.9,cl.pos='n')
    dev.off()
    
    # Append pvalues
    rm(tmp)
    tmp=rbind(M$r,M$p)
    tmp=as.data.table(tmp,keep.rownames=T)
    setnames(tmp,'rn','variable')
    tmp[,Type:=rep(c('Correlation','Pvalue'),each=nrow(M$r))]
    
    # Export to csv
    fwrite(tmp,paste0(state,'/',regions[r],'/Correlation.csv'))
    
    next
  } # END if(regionalVarsPlotsOnly)
  
  
  
  
  
  ###################################*
  ##*** vars Loop *** ####
  ###################################*
  
  for(v in 1:length(vars)){ # Loop across vars  1:length(vars)
    cat('\n\n',varsTrans[v],' ')
    
    # Check
    # if(vars[v] %in% c('Precip8110Cat','WS_Area_sqKm','Elev_m'
    #                   ,'pSLOPE_NHD','MWMT_C') & regions[r]!='Statewide') next
    
    # bpjL
    # bpjL=ifelse(vars[v]=='MWMT_C',T,F) 
    
    # K - Legacy
    # K=ifelse(vars[v]=='MWMT_C',10L,8L)
    
    # path
    rm(path)
    path=paste0(output.dir, results.dir,regions[r],'/',vars[v],'/')
    
    # breaks & labels
    rm(labels,breaks)
    
    #keep the MSST breaks the same across all datasets; don't transform MSST?
    
    if(varsTrans[v]== "ELEVCAT_m") {
      labels = c(5, 200, 400, 600, 835)
      breaks = labels
    } else if(varsTrans[v]== "IWI_v2_1"){
      labels = c(0.1, 0.25, 0.5, 0.75, 0.9)
      breaks = labels
    } else if(varsTrans[v]== "MSST"){
      labels = c(10, 14, 18, 22, 26)
      breaks = labels
    } else if(varsTrans[v]== "pSLOPE_log"){
      labels = c(0.001, 0.5, 1, 5, 10)
      breaks = log(labels)
    } else if(varsTrans[v]== "WSAREASQKM_log"){
      labels = c(1, 10, 100, 500, 1000, 2000, 5000, 15000)
      breaks = log(labels)
    } else stop('CHECK breaks')
    
    # dfRegionVar
    rm(dfRegionVar)
    dfRegionVar=copy(dfRegion)
    nrow(dfRegionVar)
    # Note: "dfRegionVar" IS different than "dfRegion", because blank predictor values will be removed from "dfRegionVar" but not "dfRegion"!
    
    # PredTrans
    dfRegionVar[,PredTrans:=NA_real_]
    dfRegionVar[,PredTrans:=get(varsTrans[v])]
    
    # Remove NAs
    dfRegionVar[is.na(PredTrans),.N] # 0 removed
    dfRegionVar=dfRegionVar[!is.na(PredTrans),]
    nrow(dfRegionVar)
    
    # Pred
    dfRegionVar[,Pred:=NA_real_]
    dfRegionVar[,Pred:=get(vars[v])]
    
    # nOccsRegionVar
    rm(nOccsRegionVar)
    nOccsRegionVar=colSums(dfRegionVar[,..otusState])
    nOccsRegionVar=data.table(OTU=names(nOccsRegionVar),nOcc=as.integer(nOccsRegionVar))
    nrow(nOccsRegionVar) # 860
    
    # nTotal
    nOccsRegionVar[,nTotal:=NA_integer_]
    nOccsRegionVar[,nTotal:=nrow(dfRegionVar)]
    
    # AlwaysPresent
    nOccsRegionVar[,AlwaysPresent:=0L]
    nOccsRegionVar[(nTotal-nOcc)<1,AlwaysPresent:=1]
    nOccsRegionVar[,.N,by=AlwaysPresent]
    
    # Plot
    nOccsRegionVar[,Plot:=0L]
    nOccsRegionVar[nOcc>=30,Plot:=1]
    nOccsRegionVar[AlwaysPresent==1,Plot:=0]
    nOccsRegionVar[,.N,by=Plot]
    
    # Join
    otuTable_State$nOcc=NULL
    otuTable_State$nTotal=NULL
    otuTable_State$AlwaysPresent=NULL
    otuTable_State$Plot=NULL
    intersect(names(otuTable_State),names(nOccsRegionVar)) # OTU
    otuTable_State=merge(otuTable_State,nOccsRegionVar,by=c('OTU'),all.x=T) # Left join
    
    # otusRegionVar
    rm(otusRegionVar)
    otusRegionVar=nOccsRegionVar[nOcc>0,sort(unique(OTU))]
    length(otusRegionVar) 
    
    # Remove taxa with nOcc=0
    nOccsRegionVar[nOcc==0,.N] # 115 taxa removed
    rm(tmp)
    tmp=setdiff(names(dfRegionVar),nOccsRegionVar[nOcc==0,OTU])
    dfRegionVar=dfRegionVar[,..tmp]
    
    # RA_RegionVar - For WAopt
    rm(RA_RegionVar)
    RA_RegionVar=copy(dfRegionVar[,..otusRegionVar])
    # rm(otusRegionVar)
    RA_RegionVar=as.matrix(RA_RegionVar)
    nrow(RA_RegionVar)
    
    
    
    
    ###################################*
    ### WAopt ####
    ###################################*
    
    # Could run WAopt at the taxaGroup level, but it's more efficient to run it at the region level, as there is no need to create "RA_RegionVarGroup". 
    
    
    # Fit
    rm(fit.wa)
    fit.wa=analogue::wa(RA_RegionVar,env=dfRegionVar$PredTrans,deshrink='classical'
                        ,tol.dw=F,useN2=T) # monotonic # classical inverse
    # WAopt are NOT affected by deshrink, NOT affected by tolDW, and YES affected by useN2. 
    # MTTI paper used deshrink='classical', tol.dw=T, and useN2=T. 
    
    # Diagnostic plot
    if(F){
      par(mfrow=c(1,2))
      plot(fit.wa)
      par(mfrow=c(1,1))
      
      # Coefs
      coef(fit.wa) # These ARE affected by "deshrink"
      # 13.7491777  0.2294058
      
      # WAopt
      as.matrix(head(fit.wa$wa.optima)) # NOT affected by deshrink. NOT affected by tolDW. YES affected by useN2. 
      #                     [,1]
      # Ablabesmyia     21.52460
      # Acricotopus     23.86821
      
      # Tolerance
      as.matrix(head(fit.wa$tolerances)) # NOT affected by deshrink. NOT affected by tolDW. YES affected by useN2. 
      #                     [,1]
      # Ablabesmyia     3.716501
      # Acricotopus     1.209153
    } # END diagnostics
    
    
    # WAopt
    rm(waRegionVar) # Do this, rather than creating "waRegionVarGroup". More efficient. 
    waRegionVar=cbind(fit.wa$wa.optima,fit.wa$tolerances)
    waRegionVar=as.data.table(waRegionVar,keep.rownames=T)
    waRegionVar[is.nan(waRegionVar)]=NA
    names(waRegionVar)=c('OTU','WAoptTrans','WAtolTrans')
    nrow(waRegionVar)
    
    # WAlwrTrans
    waRegionVar[,':='(WAlwrTrans=NA_real_,WAuprTrans=NA_real_)]
    waRegionVar[,WAlwrTrans:=WAoptTrans-WAtolTrans]
    waRegionVar[,WAuprTrans:=WAoptTrans+WAtolTrans]
    if(varsScale[v]=='logNeg'){
      waRegionVar[,WAlwrTrans:=WAoptTrans+WAtolTrans]
      waRegionVar[,WAuprTrans:=WAoptTrans-WAtolTrans]
    }
    
    # Join TaxaGroup
    waRegionVar[,TaxaGroup:=NULL]
    intersect(names(waRegionVar),names(otuTable_State)) # OTU
    waRegionVar=merge(waRegionVar,otuTable_State[,.(OTU,TaxaGroup)]
                      ,by=c('OTU'),all.x=T) # Left join
    
    # WAopt
    waRegionVar[,WAopt:=NA_real_]
    if(varsScale[v]=='identity'){
      waRegionVar[,WAopt:=WAoptTrans]
    } else if(varsScale[v]=='sqrt'){
      waRegionVar[,WAopt:=WAoptTrans^2]
    } else if(varsScale[v]=='log'){
      waRegionVar[,WAopt:=exp(WAoptTrans)]
    } else if(varsScale[v]=='log1p'){
      waRegionVar[,WAopt:=exp(WAoptTrans)-1]
    } else if(varsScale[v]=='logNeg'){
      waRegionVar[,WAopt:=1-exp(WAoptTrans)]
    } else stop('CHECK WAopt backtransform')
    
    # WAtol - Backtransforming can give wonky results. Rely on WAlwr and WAupr instead. 
    waRegionVar[,WAtol:=NA_real_]
    if(varsScale[v]=='identity'){
      waRegionVar[,WAtol:=WAtolTrans]
    } else if(varsScale[v]=='sqrt'){
      waRegionVar[,WAtol:=WAtolTrans^2]
    } else if(varsScale[v]=='log'){
      waRegionVar[,WAtol:=exp(WAtolTrans)]
    } else if(varsScale[v]=='log1p'){
      waRegionVar[,WAtol:=exp(WAtolTrans)-1]
    } else if(varsScale[v]=='logNeg'){
      waRegionVar[,WAtol:=1-exp(WAtolTrans)]
    } else stop('CHECK WAtol backtransform')
    
    # WAlwr
    waRegionVar[,WAlwr:=NA_real_]
    if(varsScale[v]=='identity'){
      waRegionVar[,WAlwr:=WAlwrTrans]
    } else if(varsScale[v]=='sqrt'){
      waRegionVar[,WAlwr:=WAlwrTrans^2]
    } else if(varsScale[v]=='log'){
      waRegionVar[,WAlwr:=exp(WAlwrTrans)]
    } else if(varsScale[v]=='log1p'){
      waRegionVar[,WAlwr:=exp(WAlwrTrans)-1]
    } else if(varsScale[v]=='logNeg'){
      waRegionVar[,WAlwr:=1-exp(WAlwrTrans)]
    } else stop('CHECK WAlwr backtransform')
    
    # WAupr
    waRegionVar[,WAupr:=NA_real_]
    if(varsScale[v]=='identity'){
      waRegionVar[,WAupr:=WAuprTrans]
    } else if(varsScale[v]=='sqrt'){
      waRegionVar[,WAupr:=WAuprTrans^2]
    } else if(varsScale[v]=='log'){
      waRegionVar[,WAupr:=exp(WAuprTrans)]
    } else if(varsScale[v]=='log1p'){
      waRegionVar[,WAupr:=exp(WAuprTrans)-1]
    } else if(varsScale[v]=='logNeg'){
      waRegionVar[,WAupr:=1-exp(WAuprTrans)]
    } else stop('CHECK WAupr backtransform')
    
    # Export - Not needed
    
    
    
    
    
    
    ###################################*
    ### xgrid ####
    ###################################*
    
    # xgridTrans
    rm(xgridTrans)
    xgridTrans=seq(min(dfRegionVar$PredTrans),max(dfRegionVar$PredTrans)
                   ,length=200) # Use 200 for good resolution. 
    
    # xgrid
    rm(xgrid)
    if(varsScale[v]=='identity'){
      xgrid=xgridTrans
    } else if(varsScale[v]=='sqrt'){
      xgrid=xgridTrans^2
    } else if(varsScale[v]=='log'){
      xgrid=exp(xgridTrans)
    } else if(varsScale[v]=='log1p'){
      xgrid=exp(xgridTrans)-1
    } else if(varsScale[v]=='logNeg'){
      xgrid=1-exp(xgridTrans)
    } else stop('CHECK xgrid backtransform')
    # head(cbind(xgrid,xgridTrans))
    
    # PredTransBin
    rm(tmp)
    tmp=(max(xgridTrans)-min(xgridTrans))/8 # "8" is good
    dfRegionVar[,PredTransBin:=NA_real_]
    dfRegionVar[,PredTransBin:=round(PredTrans/tmp,0) * tmp]
    # dfRegionVar[,.(PredTrans,PredTransBin)]
    
    # newdata - Create here, BEFORE taxaGroups_RegionVar loop
    rm(newdata)
    newdata=data.table(PredTrans=xgridTrans,Pred=xgrid)
    # nrow(newdata) # 200
    
    # predTransSumStats - Presence AND Absence data! (Different than "PCTL10", etc)
    rm(predTransSumStats)
    predTransSumStats=dfRegionVar[,.(Min=min(PredTrans),
                                     P01=quantile(PredTrans,0.01),
                                     P05=quantile(PredTrans,0.05),
                                     Median=median(PredTrans),
                                     Mean=mean(PredTrans),
                                     P95=quantile(PredTrans,0.95),
                                     P99=quantile(PredTrans,0.99),
                                     Max=max(PredTrans))]
    predTransSumStats=as.numeric(predTransSumStats)
    names(predTransSumStats)=c('Min','P01','P05','Median','Mean','P95','P99','Max')
    
    # predSumStats - Do NOT backtransform "predTransSumStats"
    rm(predSumStats)
    predSumStats=dfRegionVar[,.(Min=min(Pred),
                                P01=quantile(Pred,0.01),
                                P05=quantile(Pred,0.05),
                                Median=median(Pred),
                                Mean=mean(Pred),
                                P95=quantile(Pred,0.95),
                                P99=quantile(Pred,0.99),
                                Max=max(Pred))]
    predSumStats=as.numeric(predSumStats)
    names(predSumStats)=c('Min','P01','P05','Median','Mean','P95','P99','Max')
    # cbind(predSumStats,predTransSumStats)
    
    # taxaGroups_RegionVar
    rm(taxaGroups_RegionVar)
    taxaGroups_RegionVar=otuTable_State[OTU %in% otusRegionVar,sort(unique(TaxaGroup))]
    length(taxaGroups_RegionVar) # 9 taxaGroups_RegionVar
    
    
    
    #### For hi-res figures only
    if(F){
      g=aaa; o=aaa  # taxa1
    } # END if(F)
    
    
    
    
    
    ###################################*
    ## *** groups Loop *** ####
    ###################################*
    
    # Clear out any old csvs, before adding new ones
    rm(tmp)
    tmp=list.files(path,pattern='.csv$',recursive=T,full.names=T)
    length(tmp) # 9 files
    file.remove(tmp)
    
    for(g in 1:length(taxaGroups_RegionVar)){ # Loop across taxaGroups_RegionVar   1:length(taxaGroups_RegionVar)
      # gc()
      cat('\n',taxaGroups_RegionVar[g],' ')
      
      # dfRegionVarGroup - No need to create. Instead, melt "dfRegionVar" and filter for OTUs. 
      
      # otusRegionVarGroup
      rm(otusRegionVarGroup)
      otusRegionVarGroup=otuTable_State[TaxaGroup==taxaGroups_RegionVar[g] 
                                        & OTU %in% otusRegionVar,sort(unique(OTU))]
      length(otusRegionVarGroup) # 56
      
      # otusRegionVarGroupPlot
      rm(otusRegionVarGroupPlot)
      otusRegionVarGroupPlot=otuTable_State[OTU %in% otusRegionVarGroup 
                                            & Plot==1,sort(unique(OTU))]
      length(otusRegionVarGroupPlot) # 20
      
      # otuTable_RegionVarGroup - This is the final exported csv
      rm(otuTable_RegionVarGroup)
      otuTable_RegionVarGroup=copy(otuTable_State[OTU %in% otusRegionVarGroup,])
      nrow(otuTable_RegionVarGroup) # 56
      
      # Sort
      # Some taxa may have been dropped due to filtering for regions, etc, so do NOT use "SortState"
      setorder(otuTable_RegionVarGroup,SortState)
      otuTable_RegionVarGroup[,Sort:=NA_integer_]
      otuTable_RegionVarGroup[,Sort:=.I]
      otuTable_RegionVarGroup[,SortState:=NULL]
      
      # Join WAopt
      otuTable_RegionVarGroup[,':='(WAoptTrans=NULL,WAopt=NULL,WAtolTrans=NULL
                                    ,WAtol=NULL,WAlwr=NULL,WAupr=NULL)]
      intersect(names(otuTable_RegionVarGroup),names(waRegionVar)) # OTU
      otuTable_RegionVarGroup=merge(otuTable_RegionVarGroup
                                    ,waRegionVar[,.(OTU,WAoptTrans,WAtolTrans
                                                    ,WAopt,WAtol,WAlwr,WAupr)]
                                    ,by=c('OTU'),all.x=T) # Left join
      
      # RESET
      otuTable_RegionVarGroup[,SD:=NA_real_]
      otuTable_RegionVarGroup[,GAM:=NA_real_]
      otuTable_RegionVarGroup[,AUC:=NA_real_]
      otuTable_RegionVarGroup[,POC_Range:=NA_real_] # Used for CurveShapePre1
      otuTable_RegionVarGroup[,POC_Max:=NA_real_] # Used for CurveShapePre1
      otuTable_RegionVarGroup[,POC_Max_CI:=NA_real_] # Used for CurveShape
      
      # PredTrans summary Stats - Presence data ONLY!
      otuTable_RegionVarGroup[,PredMinTrans:=NA_real_]
      otuTable_RegionVarGroup[,PCTL01Trans:=NA_real_]
      otuTable_RegionVarGroup[,PCTL05Trans:=NA_real_]
      otuTable_RegionVarGroup[,PCTL10Trans:=NA_real_]
      otuTable_RegionVarGroup[,PCTL50Trans:=NA_real_]
      otuTable_RegionVarGroup[,PCTL90Trans:=NA_real_]
      otuTable_RegionVarGroup[,PCTL95Trans:=NA_real_]
      otuTable_RegionVarGroup[,PCTL99Trans:=NA_real_]
      otuTable_RegionVarGroup[,PredMaxTrans:=NA_real_]
      
      # Pred summary Stats - Presence data ONLY!
      otuTable_RegionVarGroup[,PredMin:=NA_real_]
      otuTable_RegionVarGroup[,PCTL01:=NA_real_]
      otuTable_RegionVarGroup[,PCTL05:=NA_real_]
      otuTable_RegionVarGroup[,PCTL10:=NA_real_]
      otuTable_RegionVarGroup[,PCTL50:=NA_real_]
      otuTable_RegionVarGroup[,PCTL90:=NA_real_]
      otuTable_RegionVarGroup[,PCTL95:=NA_real_]
      otuTable_RegionVarGroup[,PCTL99:=NA_real_]
      otuTable_RegionVarGroup[,PredMax:=NA_real_]
      
      # CurveShape & Category
      otuTable_RegionVarGroup[,CurveShapePre1:=NA_character_]
      otuTable_RegionVarGroup[,CurveShapePre2:=NA_character_]
      otuTable_RegionVarGroup[,CurveShapePre3:=NA_character_]
      otuTable_RegionVarGroup[,CurveShape:=NA_character_]
      otuTable_RegionVarGroup[,CurveShapeTmp:=NA_character_]
      # otuTable_RegionVarGroup$CurveShape_BPJ:=NULL  # Joined in
      # otuTable_RegionVarGroup$Category_BPJ:=NULL  # Joined in
      # otuTable_RegionVarGroup[,Category_PreBPJ:=NA_character_]
      # otuTable_RegionVarGroup[,Category:=NA_character_]
      # otuTable_RegionVarGroup[,Thermal_indicator:=NA_character_] # For "BioMonTools" package
      
      
      
      
      
      #### BPJ Overwrite
      if(bpjL){
        # Load
        rm(bpj,tmp)
        bpj=fread(paste0(state,'/BPJ/OptimaTaxaList_CurveShapeBPJ_20240130.csv'))
        tmp=fread(paste0(state,'/BPJ/_Optima_BPJoverrides_ThermPref_v2.csv'))
        nrow(bpj) # 860
        nrow(tmp) # 860
        
        # Drop/rename
        bpj[,Taxon:=NULL]
        tmp[,BPJ_Thermal_indicator:=NULL]
        setnames(tmp,'BPJ_ThermalIndicator','Category_BPJ')
        
        # Join
        intersect(names(bpj),names(tmp)) # OTU
        bpj=merge(bpj,tmp,by=c('OTU'),all.x=T) # Left join
        rm(tmp)
        nrow(bpj) # 860
        
        # Add asterisk - for GAM plot
        bpj[!is.na(CurveShape_BPJ),CurveShape_BPJ:=paste0(CurveShape_BPJ,'*')]
        bpj[!is.na(Category_BPJ),Category_BPJ:=paste0(Category_BPJ,'*')]
        
        # Check
        bpj[,.N,by=CurveShape_BPJ]
        bpj[,.N,by=Category_BPJ]
        
        # Join
        intersect(names(otuTable_RegionVarGroup),names(bpj)) # OTU
        otuTable_RegionVarGroup=merge(otuTable_RegionVarGroup,bpj,by=c('OTU'),all.x=T) # Left join
      } # END if(F)
      
      
      
      
      
      
      
      ###################################*
      ## *** otus Loop *** ####
      ###################################*
      
      # o=1L
      for(o in 1:length(otusRegionVarGroup)){ # Loop across otusRegionVarGroup
        # cat(otusRegionVarGroup[o],' ')
        if(o %% 20 == 0) cat(o,' ')
        
        # Taxa (the response)
        dfRegionVar$Taxa=NA_integer_ # Use hard reset
        dfRegionVar[,Taxa:=get(otusRegionVarGroup[o])]
        
        # SD
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o]
                                ,SD:=dfRegionVar[Taxa==1,sd(PredTrans)]]
        
        # Skip if taxa was very rarely observed
        tmp=dfRegionVar[,sum(Taxa)]
        if(tmp<2) next                     
        
        # Skip if taxa was always observed
        if(otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o]
                                   ,AlwaysPresent]==1) next
        # if((nrow(dfRegionVar)-tmp)<1){
        #   otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],Plot:=0L]
        #   otusRegionVarGroupPlot=otuTable_RegionVarGroup[Plot==1,OTU]
        #   next
        # }
        
        # Skip if taxa has SD=0
        tmp=otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],SD]
        if(tmp==0) next
        
        
        
        
        
        ###################################*
        ### Fit ####
        ###################################*
        
        # Check data
        if(F) plot(Taxa~PredTrans,data=dfRegionVar)
        
        # Fit
        rm(fit)
        fit=gam(Taxa~s(PredTrans,bs='tp',k=K,fx=F),
                data=dfRegionVar,family=binomial(link='logit'),method='REML')
        
        # Check
        if(F){
          summary(fit)
          plot(fit,scale=-1,pages=1,residuals=F,shade=T,all.terms=T)
        }
        
        # AUC
        rm(yhat,tmp)
        yhat=as.numeric(predict(fit,type='response')) # Probability
        tmp=suppressMessages(pROC::roc(dfRegionVar$Taxa,yhat))
        tmp=as.numeric(tmp$auc)
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],AUC:=tmp]
        rm(yhat)
        
        # Hard reset newdata for new taxa
        newdata$yhat=NA_real_
        newdata$lwr=NA_real_
        newdata$upr=NA_real_
        newdata$CI_width=NA_real_
        newdata$maxPOCIsLessThanUpr=NA_real_
        
        # yhat. Use "type=link" so that SE is correct.
        rm(tmp)
        # setorder(newdata,PredTrans) # Not needed
        tmp=predict(fit,newdata=newdata,type='link',se.fit=T)
        newdata[,yhat:=tmp$fit]
        newdata[,lwr :=yhat-tmp$se.fit*mult]
        newdata[,upr :=yhat+tmp$se.fit*mult]
        
        # Back-transform binomial logit link
        newdata[,yhat:=1/(1+exp(-yhat))]
        newdata[,lwr :=1/(1+exp(-lwr))]
        newdata[,upr :=1/(1+exp(-upr))]
        
        # CI_width
        newdata[,CI_width:=upr-lwr]
        
        # optimaTrans
        rm(optimaTrans)
        optimaTrans=rep(NA_real_,2)
        names(optimaTrans)=c('WAopt','GAM')
        optimaTrans['WAopt']=otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],WAoptTrans]
        optimaTrans['GAM']=newdata[which.max(yhat),PredTrans]
        
        # optima
        rm(optima)
        if(varsScale[v]=='identity'){
          optima=optimaTrans
        } else if(varsScale[v]=='sqrt'){
          optima=optimaTrans^2
        } else if(varsScale[v]=='log'){
          optima=exp(optimaTrans)
        } else if(varsScale[v]=='log1p'){
          optima=exp(optimaTrans)-1
        } else if(varsScale[v]=='logNeg'){
          optima=1-exp(optimaTrans)
        } else stop('CHECK optima backtransform')
        # rbind(optima,optimaTrans)
        
        # Save GAM
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],GAM:=optima['GAM']]
        
        
        
        
        
        ###################################*
        ## Curve Attributes ####
        ###################################*
        
        # Save POCs
        rm(POCs)
        POCs=c(
          approx(newdata$PredTrans,y=newdata$yhat,xout=predTransSumStats['Min'])$y,
          newdata[,max(yhat)],
          approx(newdata$PredTrans,y=newdata$yhat,xout=predTransSumStats['Max'])$y)
        names(POCs)=c('POC_MinPredictor','POC_Max','POC_MaxPredictor')
        # POCs
        
        # POC_Range - Used for CurveShapePre1
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o]
                                ,POC_Range:=newdata[,max(yhat)-min(yhat)]]
        
        # POC_Max - Used for CurveShapePre1
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o]
                                ,POC_Max:=newdata[,max(yhat)]]
        
        # POC_Max_CI - Used for CurveShape
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o]
                                ,POC_Max_CI:=newdata[CI_width<min_CI,max(yhat)]]
        
        
        
        
        ###################################*
        ## PredTrans Summary Stats ####
        ###################################*
        
        # PredTrans SumStats - Presence data ONLY! This is different than "predTransSumStats"!
        rm(tmp)
        tmp=dfRegionVar[Taxa==1,.(Min=min(PredTrans),
                                  P01=quantile(PredTrans,0.01),
                                  P05=quantile(PredTrans,0.05),
                                  P10=quantile(PredTrans,0.10),
                                  Median=median(PredTrans),
                                  P90=quantile(PredTrans,0.90),
                                  P95=quantile(PredTrans,0.95),
                                  P99=quantile(PredTrans,0.99),
                                  Max=max(PredTrans))]
        # tmp
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],PredMinTrans:=tmp$Min]
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],PCTL01Trans:=tmp$P01]
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],PCTL05Trans:=tmp$P05]
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],PCTL10Trans:=tmp$P10]
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],PCTL50Trans:=tmp$Median]
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],PCTL90Trans:=tmp$P90]
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],PCTL95Trans:=tmp$P95]
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],PCTL99Trans:=tmp$P99]
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],PredMaxTrans:=tmp$Max]
        
        # Pred SumStats - Presence data ONLY! This is different than "predTransSumStats"!
        rm(tmp)
        tmp=dfRegionVar[Taxa==1,.(Min=min(Pred),
                                  P01=quantile(Pred,0.01),
                                  P05=quantile(Pred,0.05),
                                  P10=quantile(Pred,0.10),
                                  Median=median(Pred),
                                  P90=quantile(Pred,0.90),
                                  P95=quantile(Pred,0.95),
                                  P99=quantile(Pred,0.99),
                                  Max=max(Pred))]
        # tmp
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],PredMin:=tmp$Min]
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],PCTL01:=tmp$P01]
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],PCTL05:=tmp$P05]
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],PCTL10:=tmp$P10]
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],PCTL50:=tmp$Median]
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],PCTL90:=tmp$P90]
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],PCTL95:=tmp$P95]
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],PCTL99:=tmp$P99]
        otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],PredMax:=tmp$Max]
        
        
        
        
        
        ###################################*
        ## CurveShapePre ####
        ###################################*
        
        # CurveShapePre1 - For extreme GAMopts
        if(optimaTrans['GAM']<=(predTransSumStats['P01'])) otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],CurveShapePre1:='Decreaser']
        if(optimaTrans['GAM']>=(predTransSumStats['P99'])) otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],CurveShapePre1:='Increaser']
        # otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],]
        
        # CurveShapePre1 - Unimodal or Concave Up
        if(POCs['POC_Max']>POCs['POC_MinPredictor'] & POCs['POC_Max']>POCs['POC_MaxPredictor']) otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],CurveShapePre1:='Unimodal']
        if(POCs['POC_Max']<POCs['POC_MinPredictor'] & POCs['POC_Max']<POCs['POC_MaxPredictor']) otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],CurveShapePre1:='Concave Up'] # Note: "Concave Up" is unlikely in the real world
        
        # CurveShapePre1 - Flat - Overwrites other non-NA assignments!
        otuTable_RegionVarGroup[POC_Range<0.02,CurveShapePre1:='Flat'] # Could increase from 2% to 5%?
        
        ## CurveShapePre2 - Increaser
        # See if lower ci at max(PredTrans) (Right) is greater than upper ci at min(PredTrans) (Left):
        # (Right-Left)>0 => Increaser
        rm(left,right)
        left=approx(newdata$PredTrans,y=newdata$upr,xout=predTransSumStats['P01'])$y
        right=approx(newdata$PredTrans,y=newdata$lwr,xout=predTransSumStats['P99'])$y 
        if((right-left)>0) otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],CurveShapePre2:='Increaser']
        
        ## CurveShapePre2 - Decreaser
        # See if lower ci at min(PredTrans) (Left) is greater than upper ci at max(PredTrans) (Right):
        # (Left-Right)>0 => Decreaser
        rm(left,right)
        left=approx(newdata$PredTrans,y=newdata$lwr,xout=predTransSumStats['P01'])$y
        right=approx(newdata$PredTrans,y=newdata$upr,xout=predTransSumStats['P99'])$y 
        if((left-right)>0) otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],CurveShapePre2:='Decreaser']
        
        ## Check
        # otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],]
        
        
        
        
        ###################################*
        ## CurveShapePre3 ####
        ###################################*
        
        # This chunk could go AFTER the otus loop, except that "CurveShape" is needed for the old pdf plots.  
        
        
        # First pass
        otuTable_RegionVarGroup[CurveShapePre1=='Decreaser',CurveShapePre3:='Decreaser']
        otuTable_RegionVarGroup[CurveShapePre1=='Increaser',CurveShapePre3:='Increaser']
        otuTable_RegionVarGroup[CurveShapePre1=='Unimodal',CurveShapePre3:='Unimodal']
        otuTable_RegionVarGroup[CurveShapePre1=='Concave Up',CurveShapePre3:='Concave Up']
        otuTable_RegionVarGroup[CurveShapePre1=='Flat',CurveShapePre3:='Flat']
        
        # Mixture
        otuTable_RegionVarGroup[CurveShapePre1=='Unimodal' & CurveShapePre2=='Decreaser',CurveShapePre3:='Unimodal/Decreaser']
        otuTable_RegionVarGroup[CurveShapePre1=='Unimodal' & CurveShapePre2=='Increaser',CurveShapePre3:='Unimodal/Increaser']
        otuTable_RegionVarGroup[CurveShapePre1=='Flat' & CurveShapePre2=='Decreaser',CurveShapePre3:='Flat']
        otuTable_RegionVarGroup[CurveShapePre1=='Flat' & CurveShapePre2=='Increaser',CurveShapePre3:='Flat']
        otuTable_RegionVarGroup[CurveShapePre1=='Concave Up' & CurveShapePre2=='Decreaser',CurveShapePre3:='Concave Up']
        otuTable_RegionVarGroup[CurveShapePre1=='Concave Up' & CurveShapePre2=='Increaser',CurveShapePre3:='Concave Up']
        
        # Less sure assignments. Do this AFTER others but BEFORE "Flat"
        otuTable_RegionVarGroup[CurveShapePre1=='Decreaser' & CurveShapePre2=='Increaser',CurveShapePre3:='Unclear']
        otuTable_RegionVarGroup[CurveShapePre1=='Increaser' & CurveShapePre2=='Decreaser',CurveShapePre3:='Unclear']
        
        # Overwrites EVERYTHING!
        otuTable_RegionVarGroup[POC_Max_CI<0.15,CurveShapePre3:='Unclear']
        
        # Retain "CurveShapePre3" as it allows me to track what "CurveShape_BPJ" overwrote. 
        
        # CurveShape
        otuTable_RegionVarGroup[,CurveShape:=CurveShapePre3]
        
        # CurveShape BPJ override
        if(bpjL){
          otuTable_RegionVarGroup[!is.na(CurveShape_BPJ),CurveShape:=CurveShape_BPJ]
        }
        
        
        
        
        
        ###################################*
        ## Category ####
        ###################################*
        
        # This chunk could go AFTER the otus loop, except that "CurveShape" is needed for the old pdf plots.  
        
        
        # CurveShapeTmp
      #   otuTable_RegionVarGroup[,CurveShapeTmp:=sub('\\*','',CurveShape)] # "CurveShapeTmp" is only used for calculating "Category_PreBPJ"
      #   otuTable_RegionVarGroup[,Category_PreBPJ:='Inconclusive']
      #   
      #   if(varsTrans[v]=='MWMT_C'){
      #     
      #     otuTable_RegionVarGroup[WAopt<21.5 & WAopt>17.5 & PCTL10>14    & PCTL90<25   & !CurveShapeTmp %in% c('Increaser','Decreaser'),Category_PreBPJ:='Cool-Warm'] # Run this regime first!
      #     otuTable_RegionVarGroup[WAopt<20                & PCTL10<=16   & PCTL90<=22  & !grepl('Increaser',CurveShape), Category_PreBPJ:='Cool']
      #     otuTable_RegionVarGroup[WAopt<18                & PCTL10<=14   & PCTL90<=20  & !grepl('Increaser',CurveShape), Category_PreBPJ:='Cold']
      #     otuTable_RegionVarGroup[WAopt<16                & PCTL10<=12   & PCTL90<=18  & !grepl('Increaser',CurveShape), Category_PreBPJ:='Cold Stenotherm']
      #     otuTable_RegionVarGroup[WAopt>19                & PCTL10>14.5  & PCTL90>23.5 & !grepl('Decreaser',CurveShape), Category_PreBPJ:='Warm']
      #     otuTable_RegionVarGroup[WAopt>23                & PCTL10>=19   & PCTL90>=26  & !grepl('Decreaser',CurveShape), Category_PreBPJ:='Warm Stenotherm']
      #     otuTable_RegionVarGroup[                          PCTL10<=14.5 & PCTL90>=22, Category_PreBPJ:='Eurythermal'] # Run last
      #     # otuTable_RegionVarGroup[,.N,by=Category_PreBPJ]
      #     # otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],.(WAopt,PCTL10,PCTL90)]
      #     
      #     # Category BPJ override
      #     otuTable_RegionVarGroup[,Category:=Category_PreBPJ]
      #     if(bpjL) otuTable_RegionVarGroup[!is.na(Category_BPJ),Category:=Category_BPJ]
      #     
      #     # Thermal_indicator - For "BioMonTools" package
      #     otuTable_RegionVarGroup[,Thermal_indicator:=Category]
      #     # otuTable_RegionVarGroup[,Thermal_indicator:=sub('\\*','',Thermal_indicator)]   # KEEP asterisk for "BioMonTools"!
      #     otuTable_RegionVarGroup[,Thermal_indicator:=stringr::str_to_lower(Thermal_indicator)]
      #     otuTable_RegionVarGroup[Thermal_indicator=='cold stenotherm',Thermal_indicator:='stenoc']
      #     otuTable_RegionVarGroup[Thermal_indicator=='cool-warm',Thermal_indicator:='cowa']
      #     otuTable_RegionVarGroup[Thermal_indicator=='warm stenotherm',Thermal_indicator:='stenow']
      #     # otuTable_RegionVarGroup[,.N,by=Thermal_indicator]
      #     
      #   } else if(varsTrans[v]=='logIWI_neg'){
      #     
      #     otuTable_RegionVarGroup[              PCTL10<0.65  & PCTL90>=0.87, Category_PreBPJ:='Attr IV'] # Intermediate Tolerance
      #     otuTable_RegionVarGroup[WAopt>=0.80 & PCTL10>=0.60 & PCTL90>=0.87 & !grepl('Increaser',CurveShape), Category_PreBPJ:='Attr III'] # Sensitive Ubiquitous
      #     otuTable_RegionVarGroup[WAopt>=0.83 & PCTL10>=0.74 & PCTL90>=0.88 & !grepl('Increaser',CurveShape), Category_PreBPJ:='Attr II'] # Highly Sensitive
      #     otuTable_RegionVarGroup[WAopt<0.80 &  PCTL10<=0.45  & PCTL90<0.90 & !grepl('Decreaser',CurveShape), Category_PreBPJ:='Attr V'] # Most Tolerant
      #     # otuTable_RegionVarGroup[,.N,by=Category_PreBPJ]
      #     # otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],.(WAopt,PCTL10,PCTL90)]
      #     
      #   } else if(varsTrans[v]=='sqrtPctUrbCat'){
      #     
      #     otuTable_RegionVarGroup[WAopt>=3 & WAopt<10 & PCTL90<70, Category_PreBPJ:='Medium High']
      #     otuTable_RegionVarGroup[WAopt<3             & PCTL90<=30, Category_PreBPJ:='Medium']
      #     otuTable_RegionVarGroup[WAopt<0.5           & PCTL90<5 & !grepl('Increaser',CurveShape), Category_PreBPJ:='Low']
      #     otuTable_RegionVarGroup[WAopt>=10           & PCTL90>=70 & !grepl('Decreaser',CurveShape), Category_PreBPJ:='High']
      #     otuTable_RegionVarGroup[,.N,by=Category_PreBPJ]
      #     # otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],.(WAopt,PCTL10,PCTL90)]
      #     
      #   } else if(varsTrans[v]=='sqrtPctAgCat'){
      #     
      #     # None provided yet.
      #     
      #   } else if(varsTrans[v]=='sqrtRdDensCat'){
      #     
      #     # None provided yet.
      #     
      #   } else if(varsTrans[v]=='sqrtPrecip8110Cat'){
      #     
      #     otuTable_RegionVarGroup[WAopt>=100 & WAopt<190 & PCTL10>30 & PCTL90>125, Category_PreBPJ:='Moderate']
      #     otuTable_RegionVarGroup[WAopt<100                          & PCTL90<235 & !grepl('Increaser',CurveShape), Category_PreBPJ:='Low']
      #     otuTable_RegionVarGroup[WAopt<65                           & PCTL90<185 & !grepl('Increaser',CurveShape), Category_PreBPJ:='Very Low']
      #     otuTable_RegionVarGroup[WAopt>=190             & PCTL10>75 & PCTL90>275 & !grepl('Decreaser',CurveShape), Category_PreBPJ:='High']
      #     # otuTable_RegionVarGroup[,.N,by=Category_PreBPJ]
      #     # otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],.(WAopt,PCTL10,PCTL90)]
      #     
      #   } else if(varsTrans[v]=='logWS_Area_sqKm'){
      #     
      #     otuTable_RegionVarGroup[WAopt>=15 & WAopt<30              & PCTL90>45 , Category_PreBPJ:='Small-Mid']
      #     otuTable_RegionVarGroup[WAopt<15             & PCTL10<5   & PCTL90<100 & !grepl('Increaser',CurveShape), Category_PreBPJ:='Very Small']
      #     otuTable_RegionVarGroup[WAopt>=30            & PCTL10>2.7 & PCTL90>100 , Category_PreBPJ:='Large']
      #     otuTable_RegionVarGroup[WAopt>100            & PCTL10>6   & PCTL90>9000  & !grepl('Decreaser',CurveShape), Category_PreBPJ:='Very Large']
      #     # otuTable_RegionVarGroup[,.N,by=Category_PreBPJ]
      #     # otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],.(WAopt,PCTL10,PCTL90)]
      #     
      #   } else if(varsTrans[v]=='sqrtElev_m'){
      #     
      #     otuTable_RegionVarGroup[WAopt>=400 & WAopt<750 & PCTL10>50  & PCTL90>800, Category_PreBPJ:='Moderate']
      #     otuTable_RegionVarGroup[WAopt<300              & PCTL10<125 & PCTL90<1100 & !grepl('Increaser',CurveShape), Category_PreBPJ:='Low']
      #     otuTable_RegionVarGroup[WAopt>=750             & PCTL10>200 & PCTL90>1200 & !grepl('Decreaser',CurveShape) , Category_PreBPJ:='High']
      #     # otuTable_RegionVarGroup[,.N,by=Category_PreBPJ]
      #     # otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],.(WAopt,PCTL10,PCTL90)]
      #     
      #   } else if(varsTrans[v]=='sqrtpSLOPE_NHD'){
      #     
      #     otuTable_RegionVarGroup[WAopt>=1 & WAopt<5               & PCTL90<15, Category_PreBPJ:='Medium']
      #     otuTable_RegionVarGroup[WAopt<1            & PCTL10<0.25 & PCTL90<3.7 & !grepl('Increaser',CurveShape), Category_PreBPJ:='Low']
      #     otuTable_RegionVarGroup[WAopt>=5           & PCTL10>0.6  & PCTL90>10 , Category_PreBPJ:='High']
      #     otuTable_RegionVarGroup[WAopt>8            & PCTL10>2    & PCTL90>15 & !grepl('Decreaser',CurveShape), Category_PreBPJ:='Very High']
      #     # otuTable_RegionVarGroup[,.N,by=Category_PreBPJ]
      #     # otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],.(WAopt,PCTL10,PCTL90)]
      #     
      #   } else stop('CHECK Category_PreBPJ')
      #   
      #   # Category BPJ override
      #   otuTable_RegionVarGroup[,Category:=Category_PreBPJ]
      #   if(bpjL) otuTable_RegionVarGroup[!is.na(Category_BPJ),Category:=Category_BPJ]
      #   
      #   ## Check
      #   # otuTable_RegionVarGroup[OTU==otusRegionVarGroup[o],]
      #   
      #   
      } # END loop across otusRegionVarGroup
      # warnings()
      # 
      # 
      
      
      
      ###################################*
      ## XGBoost Predicted BCG Attr ####
      ###################################*
      
      # # Load data
      # rm(df)
      # df=fread(paste(state,regions[r],'Predictions.csv',sep='/'))
      # df=df[,.(OTU,yhat,Predicted_BCG_Num,Predicted_BCG)]
      # nrow(df) # 
      # 
      # # Join
      # otuTable_RegionVarGroup[,':='(yhat=NULL,Predicted_BCG_Num=NULL,Predicted_BCG=NULL)]
      # intersect(names(otuTable_RegionVarGroup),names(df)) # OTU
      # otuTable_RegionVarGroup=merge(otuTable_RegionVarGroup,df,by=c('OTU'),all.x=T) # Left join
      
      
      
      
      
      ###################################*
      ## Final Flags ####
      ###################################*
      
      ## Flag_Less2pc - No longer used
      # otuTable_RegionVarGroup[,Flag_Less2pc:=0L]
      # otuTable_RegionVarGroup[nOcc<74,Flag_Less2pc:=1]
      # otuTable_RegionVarGroup[,.N,by=Flag_Less2pc]
      
      # nOccLT30
      otuTable_RegionVarGroup[,nOccLT30:=0L]
      otuTable_RegionVarGroup[nOcc<30,nOccLT30:=1]
      otuTable_RegionVarGroup[,.N,by=nOccLT30]
      
      # Low_POC_Max_CI
      otuTable_RegionVarGroup[,Low_POC_Max_CI:=NA_integer_]
      otuTable_RegionVarGroup[POC_Max_CI>=0.15,Low_POC_Max_CI:=0L]
      otuTable_RegionVarGroup[POC_Max_CI<0.15,Low_POC_Max_CI:=1]
      otuTable_RegionVarGroup[,.N,by=Low_POC_Max_CI]
      
      # Fix NaN or Inf
      otuTable_RegionVarGroup[is.infinite(POC_Max_CI),POC_Max_CI:=NA]
      
      
      
      
      
      ###################################*
      ## Map & GAM Plots ####
      ###################################*
      
      # dfLong - Presence and Absense - For GAM plots
      rm(dfLong)
      dfLong=melt(dfRegionVar,id.vars=c('SampleID','Latitude','Longitude'
                                        ,'Pred','PredTrans','PredTransBin')
                  ,measure.vars=otusRegionVarGroupPlot,variable.name='OTU')
      # dfLong[,.N,by=OTU]
      nrow(dfLong) # 66,500
      
      # Filter for Plot==1 - Already done by using "measure.vars=otusRegionVarGroupPlot"
      
      # Join fields
      dfLong[,':='(OTU_Label=NULL,nOcc=NULL,WAopt=NULL,CurveShape=NULL
                   ,Category=NULL,PCTL10=NULL,PCTL90=NULL
                   # ,yhat=NULL,Predicted_BCG_Num=NULL,Predicted_BCG=NULL
                   )]
      intersect(names(dfLong),names(otuTable_RegionVarGroup)) # OTU
      dfLong=merge(dfLong,otuTable_RegionVarGroup[,.(OTU,OTU_Label,nOcc,WAopt
                                                     ,CurveShape
                                                     # ,Category
                                                     ,PCTL10
                                                     ,PCTL90
                                                     # ,yhat
                                                     # ,Predicted_BCG_Num
                                                     # ,Predicted_BCG
                                                     )]
                   ,by=c('OTU'),all.x=T) # Left join
      
      # test <- otuTable_RegionVarGroup %>% 
      #   count(OTU)
      
      
      # Check
      rm(tmp)
      tmp=min(dfLong$nOcc)
      if(tmp<30) stop('CHECK nOcc')
      
      # digits
      rm(digits)
      if(vars[v]=='ELEVCAT_m'){
        digits=2
      } else if(vars[v]=='IWI_v2_1'){
        digits=2
      } else if(vars[v]=='MSST'){
        digits=2
      } else if(vars[v]=='pSLOPE'){
        digits=2
      } else if(vars[v]=='WSAREASQKM'){
        digits=2
      } else stop('CHECK digits')
      
      # Facet
      dfLong[,Facet:=NA_character_]
      dfLong[,Facet:=paste0(regions[r],
                            '\n',OTU_Label,
                            '\nn=',nOcc,'; WAopt=',formatC(WAopt,format='f',digits=digits),
                            '; PctRange=',formatC(PCTL10,format='f',digits=digits),'-',
                            formatC(PCTL90,format='f',digits=digits)
                            # ,'\nMaritime ',Category,'; Predicted ',Predicted_BCG
                            )]
      dfLong[1:5,Facet]
      # otuTable_RegionVarGroup
      
      # dfMap - Presence only
      rm(dfMap)
      dfMap=copy(dfLong[value==1,])
      nrow(dfMap) # 2866
      
      # bin
      rm(bin)
      bin=dfLong[,.(value=mean(value)),
                 by=.(OTU,Facet,PredTransBin)]
      setorder(bin,OTU,PredTransBin)
      setnames(bin,'PredTransBin','PredTrans') # for ggplot
      nrow(bin) # 342
      
      # bar - Presence only
      rm(bar)
      bar=dfLong[value==1,.(x=quantile(PredTrans,0.10),
                            xend=quantile(PredTrans,0.90),
                            y=1.08,
                            yend=1.08),
                 by=.(OTU,Facet)]
      nrow(bar) # 38
      
      # Join Facet to waRegionVar - For GAM plot
      waRegionVar[,Facet:=NULL]
      intersect(names(waRegionVar),names(dfLong)) # OTU & others
      waRegionVar=merge(waRegionVar,unique(dfLong[,.(OTU,Facet)]),by=c('OTU'),all.x=T) # Left join
      
      # Test breaks
      if(F){
        labels=c(0,1,3,5,10,20,50)
        breaks=sqrt(labels)
        
        range(SiteDatePA$pSLOPE_NHD)
        plot(SiteDatePA$pSLOPE_NHD)
      }
      
      
      
      
      
      #### Plots
      if(plots){
        
        # Settings
        nrow=3
        ncol=4
        nPanels=uniqueN(dfLong$Facet) # Do NOT use "length(otusRegionVarGroupPlot)"
        nPages=ceiling(nPanels/nrow/ncol)
        alpha=0.2
        size=1.2
        
        
        
        # GAM plot
        pltList=vector(mode='list',length=nPages)
        i=1
        # i=4
        for(i in 1:nPages){ # Loop across nPages
          rm(plt)
          plt=ggplot(dfLong,aes(x=PredTrans,y=value))+
            geom_hline(yintercept=0.5,color='grey')+
            geom_segment(data=bar,mapping=aes(x=x,xend=xend,y=y,yend=yend)
                         ,linewidth=2,color='green3')+
            geom_jitter(width=0,height=0.05,size=size,alpha=alpha,shape=19
                        ,color=1)+
            geom_vline(data=waRegionVar[!is.na(Facet),]
                       ,mapping=aes(xintercept=WAoptTrans),color='grey'
                       ,linewidth=0.5,linetype=2)+
            geom_smooth(method='gam',formula=y~s(x,bs='tp',k=K,fx=F),se=T,fill=4
                        ,alpha=alpha,level=ci,color=4,linewidth=0.5)+
            geom_point(data=bin,shape=24,fill='orange',size=1.5)+ # Binned data. Place on TOP of line, per reviewer comments :(
            scale_y_continuous(breaks=breaksProb,labels=labelsProb)+
            coord_cartesian(ylim=c(-0.09,1.09))+
            scale_x_continuous(trans='identity',breaks=breaks
                               ,labels=labelsPretty(labels))+
            labs(y='Probability of Occurence',x=varsLab[v]
                 ,title=taxaGroups_RegionVar[g])+
            facet_wrap_paginate(~Facet,page=i,nrow=nrow,ncol=ncol,scales='free'
                                ,shrink=F,dir='h')+
            theme_bw()+
            theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
            theme(legend.position='none')+
            theme(text=element_text(size=8))
          # print(plt)
          pltList[[i]]=plt
        } # END loop across nPages
        
        # Export GAM pdf
        pdf(paste0(path,vars[v],'_',regions[r],'_',taxaGroups_RegionVar[g],'.pdf')
            ,width=11,height=8.5)
        print(pltList)
        dev.off()
        
        # GAM Plot Caption
        # ================.
        # GAM logistic regression plot of P/A vs modeled stream temperature. Gray circles were jittered about 0 (absent) and 1 (present) to minimize overlapping. Blue line represents GAM fit. Light blue ribbon represents the 90% ci. Vertical dashed lines represent temperature optima (black = WAopt; red = GAM). Orange triangles are a "binned" representation of the measured data and were not used in the GAM fitting or optima calculations.
        
        
        
        
        
        #### Map Plots
        # Settings
        nrow=3
        ncol=6
        nPages=ceiling(nPanels/nrow/ncol)
        alpha=0.5
        size=1.0
        
        # Note. Will need to use cowplot or patchwork to make "scales='free'" :(  See https://github.com/tidyverse/ggplot2/issues/2651. 
        
        # Map plot
        pltList=vector(mode='list',length=nPages)
        i=1
        # i=4
        for(i in 1:nPages){
          plt=ggplot(dfMap)+
            geom_point(aes(y=Latitude,x=Longitude),shape=19,color='deepskyblue4'
                       ,size=size,alpha=alpha)+ # dodgerblue4 deepskyblue4 darkorange3
            geom_sf(data=map,fill=NA,color=1,linewidth=0.2)+
            labs(y='Latitude',x='Longitude',title=taxaGroups_RegionVar[g])+
            facet_wrap_paginate(~Facet,page=i,nrow=nrow,ncol=ncol,scales='fixed'
                                ,shrink=F,dir='h')+ 
            theme_bw()+
            theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
            theme(legend.position='none')+
            theme(text=element_text(size=8))
          # print(plt)
          pltList[[i]]=plt
        } # END loop across nPages
        
        # Export map pdf
        pdf(paste0(output.dir, results.dir,'/DistributionMaps/'
                   ,vars[v],'/',regions[r],'_',taxaGroups_RegionVar[g]
                   ,'.pdf'),width=11,height=8.5)
        print(pltList)
        dev.off()
      } # END if(plots)
      
      
      
      
      ###################################*
      #### Export CSV ####
      ###################################*
      
      # Do this last, due to renaming fields
      
      
      # Melt (to rename variable)
      otuTable_RegionVarGroup[,':='(CurveShapeTmp=NULL,nchar=NULL,Plot=NULL,Sort=NULL)]
      otuTable_RegionVarGroup=melt(otuTable_RegionVarGroup
                                   ,id.vars=c('TaxaGroup','OTU','OTU_Label'
                                              ,'CurveShapePre1','CurveShapePre2'
                                              ,'CurveShapePre3','CurveShape'
                                              # ,'Category_PreBPJ','Category','Thermal_indicator'
                                              # ,'yhat','Predicted_BCG_Num','Predicted_BCG'
                                              ))
      otuTable_RegionVarGroup[,.N,variable]
      nrow(otuTable_RegionVarGroup) # 1843
      
      # Pivot back
      otuTable_RegionVarGroup[,variable:=paste(variable,vars[v],sep='_')]
      otuTable_RegionVarGroup=dcast(otuTable_RegionVarGroup,TaxaGroup+OTU+OTU_Label+
                                      CurveShapePre1+CurveShapePre2+CurveShapePre3+CurveShape
                                    ~variable,fun.aggregate=mean,na.rm=T)#Category_PreBPJ+Category+Thermal_indicator+yhat+Predicted_BCG_Num+Predicted_BCG
      nrow(otuTable_RegionVarGroup) # 56
      
      # Rename fields
      setnames(otuTable_RegionVarGroup,'CurveShapePre1',paste('CurveShapePre1',vars[v],sep='_'))
      setnames(otuTable_RegionVarGroup,'CurveShapePre2',paste('CurveShapePre2',vars[v],sep='_'))
      setnames(otuTable_RegionVarGroup,'CurveShapePre3',paste('CurveShapePre3',vars[v],sep='_'))
      # setnames(otuTable_RegionVarGroup,'Category_PreBPJ',paste('Category_PreBPJ',vars[v],regions[r],sep='_'))
      setnames(otuTable_RegionVarGroup,'CurveShape',paste('CurveShape',vars[v],sep='_'))
      # setnames(otuTable_RegionVarGroup,'Category',paste('Category',vars[v],regions[r],sep='_'))
      # setnames(otuTable_RegionVarGroup,'Thermal_indicator',paste('Thermal_indicator',vars[v],regions[r],sep='_'))
      # setnames(otuTable_RegionVarGroup,'yhat',paste('yhat',vars[v],regions[r],sep='_'))
      # setnames(otuTable_RegionVarGroup,'Predicted_BCG_Num',paste('Predicted_BCG_Num',vars[v],regions[r],sep='_'))
      # setnames(otuTable_RegionVarGroup,'Predicted_BCG',paste('Predicted_BCG',vars[v],regions[r],sep='_'))
      
      # Order fields
      rm(tmp)
      tmp=c('TaxaGroup','OTU','OTU_Label', # Info
            paste(c('nOcc','nOccLT30','AlwaysPresent','Low_POC_Max_CI', # Flags
                    'WAopt','WAlwr','WAupr','WAtolTrans','WAtol','GAM','AUC', # Optima
                    'POC_Range','POC_Max','POC_Max_CI', # CI
                    'PredMin','PCTL01','PCTL05','PCTL10','PCTL50','PCTL90','PCTL95','PCTL99','PredMax', # Statistics
                    'CurveShapePre1','CurveShapePre2','CurveShapePre3','CurveShape' # CurveShape
                    # 'Category_PreBPJ','Category','Thermal_indicator',
                    # 'yhat','Predicted_BCG_Num','Predicted_BCG'
                    ),
                  vars[v],sep='_')) # Category
      otuTable_RegionVarGroup=otuTable_RegionVarGroup[,..tmp]
      
      # Export csv
      setorder(otuTable_RegionVarGroup,TaxaGroup,OTU)
      fwrite(otuTable_RegionVarGroup,paste0(path,taxaGroups_RegionVar[g],'.csv'))
      
    } # END loop across taxaGroups_RegionVar
    warnings()
    
    
    
    
    #### Append csvs across taxaGroups
    
    # List files
    rm(tmp)
    tmp=list.files(path,pattern='.csv$',recursive=T,full.names=T)
    length(tmp) # 9 files
    
    # Remove
    # tmp=tmp[grep('Results',tmp,invert=T)]
    
    # Check
    if(length(tmp)>length(taxaGroups_RegionVar)) stop('CHECK appended csv')
    
    # Append
    rm(df)
    df=rbindlist(lapply(tmp,fread),fill=F)
    # df[,.N,by=TaxaGroup]
    nrow(df) # 858
    
    # Export results table for variable
    setorder(df
             # ,TaxaGroup,OTU
             )
    fwrite(df,paste0(path,vars[v],'_',regions[r],'.csv'))
    
    # Delete csvs
    file.remove(tmp)
    
  } # END loop across vars
  
  
} # END loop across regions
warnings()
tick=Sys.time()
tick-tock
# 20 min for East
# 65 min for West
# 3.4 hours for Statewide
# 4.6 hours for all 3 regions



# Check
r # regions
v # vars
g # taxaGroups_RegionVar
o # otusRegionVarGroup
i # plot loops





