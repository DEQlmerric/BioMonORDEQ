# R code to build a RIVPACS-type model. ;
# This version uses Random Forest (RF), instead of discriminant function analysis, to predict group membership;
# Otherwise, this script is the same as model.build.v4.r;

# J.Van Sickle, 02/25/10;

#Version 4.2.1 (10/6/11) adds calculation of replicate sampling SD for a new model;
#---------------------------------------------------------#
#load required packages;
#---------------------------------------------------------#

#install.packages("maps")
#install.packages("dendextend")
library(cluster); 
library(Hmisc);
library(randomForest);
library(maps)
library(dendextend)
library(VSURF)
library(tidyr)
library(RColorBrewer)   
library(tidyverse)
#-------------------------------------------------------------------------#
#STEP 1 -- INITIAL SETUP -- Input and organize the bug and predictor data;
#-------------------------------------------------------------------------#
# 
######### Step 1a - Read and organize predictor data;
# read in predictors from csv not database. once you select final predictors for the model save them in the database
# calc predictors for all sites (reference and degraded and put in one file)
#preds=read.csv("bugs analyses/RIVPACS_2022/z_Exploratory Work/preds_slope_ecoregion.csv")
#select only reference data and exclude bug metrics
                             
load('bugs analyses/RIVPACS_2022/_2024 model build/pred.mets_257_final.Rdata')
                              #  @@@@ SLH 11.2.22
                              # predcal=predcal[predcal$reference=='Y',c(1,97:150)]
                              # @@@@ currently only reference data is included in the preds file, so this either not needed OR we need a column for 'reference' added
preds <- pred.mets_257_final

                              # remove NBR sites, plus remove outlier sites (ref sites continually scoring very low)
preds <- preds %>%
        dplyr:: filter(EcoRegion3 != '80') %>%
        dplyr::filter(act_id !='35618-ORDEQ:20000702:R:SR') 
                                                                                        # dplyr::filter(act_id !='21814-ORDEQ:20010912:R:SR') %>%  
                                                                                        # dplyr::filter(act_id !='30343-ORDEQ:20030814:R:SR') %>% 
                                                                                        # dplyr::filter(act_id !='32555-ORDEQ:20050801:R:SR') %>%
                                                                                        # dplyr::filter(act_id !='123437') %>%
                                                                                        # dplyr::filter(act_id !='35813-ORDEQ:20000901:R:SR')
                                                                                        # 

rownames(preds)=preds$act_id


########
########### Step 1b - Input the assemblage data (bug data), as a site-by-taxa matrix;
########
# First, create a Presence/absence (1/0) matrix (site by taxa) for the bugs;

                              # @@@@ SLH: 11.2.22-this all seems USU specific, change to work with csv input file received from Trip on 11.2.22
                              # create a project in the database with all samples to be used
                              # query database for all bug data in that project, select the OTU/ translation to be used for the model, and fixed count
                              # bugsOTU=query("sampleTaxaTranslationRarefied",projectId=387,translationId=2,fixedCount=300)



# bring in DEQ bug data from 265 reference sites
  # data already summed across OTUs
  # rarified to 300 count
  # matrix, counts
  # only samples with > 200 count were retained

load('bugs analyses/RIVPACS_2022/_2024 model build/bugs.mat_257.ref.Rdata')
bugsOTU <- bugs.mat_257.ref %>%
  rename(sampleId = Sample)

# subset the bug data to only the reference predictor data file for now 
bugsOTU=subset(bugsOTU, sampleId %in% rownames(preds))


###
####### prep preds file: remove non-predictor columns
###
preds_257 <- preds %>%
  dplyr::select(-c(act_id, org_id, COMID, MLocID, StaDes,SampleStart_Date,tot.abund,OTU.abund,HUC12_Name,
                   GNIS_Name,AU_ID,Reachcode,Measure,REGIONID, met.type ))

 
predcal <- preds_257
                       
###
######## create P/A dataframes
###

# convert counts greater than 1 to 1
bugs_257 <- bugsOTU %>% mutate_if(is.numeric, ~1 * (. != 0))
bugs_257 <- as.data.frame(bugs_257)
rownames(bugs_257)<-bugs_257$sampleId
# remove sampleID as a column
bugcal.pa<-bugs_257[,-1]

                                                                                    

  
############ Step 1c - Align bug and predictor data, by site/sample;
#check sample(row) alignment of bug and predictor data;

bugcal.pa= bugcal.pa[order(rownames(bugcal.pa)),];
predcal = predcal[order(rownames(predcal)),];

########### Step 1d - remove rare taxa

# First, get a vector containing the proportion of calibration sites at which each taxon occurs;
psite.occ<-colSums(bugcal.pa)/dim(bugcal.pa)[[1]];

# Now get a vector of names of taxa occuring at greater than 5% of calibration sites;Will use only these taxa in clustering;
nonrare.taxa<-names(psite.occ)[psite.occ>0.05];

#Now subset the site-by-taxa matrix to create a new one containing only the nonrare taxa;
bugcal.pa.nonrare<- bugcal.pa[,nonrare.taxa]; dim(bugcal.pa.nonrare);


    #   @@@@@ SLH 08.24.23(200 count): only 108 taxa that are found at > 5% of cal sites?  Seems low!
    #   @@@@@ SLH 08.24.23 (250 ct): only 111 taxa
    
    #   @@@@@ SLH 02.26.24(200 ct): only 96 taxa
                                    #     44.9% of taxa retained
              # NBR removed = 102 taxa, 47.4%  
 

# FYI: Print the percentage of total taxa that were retained after removing rare taxa?
pct.taxa.used.message = paste0((round(length(nonrare.taxa)/length(psite.occ),4)*100),"%"," of total taxa are retained after excluding rare taxa."); print(pct.taxa.used.message, quote=F)






# Now can use the nonrare version, instead of bugcal or bugcal.pa, in subsequent dissimilarity calculations below;


#-------------------------------------------------------------------------#
#STEP 2 -- DISSIMILARITIES AND CLUSTER ANALYSIS;;
#-------------------------------------------------------------------------#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# In his original script, John VanSickle presented multiple options for creating a dissimilarity matrix, but Chuck and Scott insisted on only using Sorenson's method, so I've removed John's other options from this script.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### compute dissimilarity

# Here, I use the generalized outer product function dapply() and choose the desired dissimilarity measure as a called function;
# dapply() output is an (n(n-1)/2) length vector storing ;
# the lower triangle of the site dissimilarity matrix in column major order;
# can be input directly to R clustering functions;

source("bugs analyses/RIVPACS_2022/_2024 model build/dapply.r")

# Option 2 -- Sorenson dissimilarity for P/A data;
# function computes Sorenson P/A dissimilarity between one site pair,  siti and sitj;

  sornfun<-function(siti,sitj) {
              shared<-sum((siti>0)&(sitj>0));
              uniquei<-sum((siti>0)&(sitj==0));
              uniquej<-sum((siti==0)&(sitj>0));
              1-(2*shared/(2*shared+uniquei+uniquej)); #return Sorenson dissimilarity;
                  } #end of function;

# compute Sorenson dissimilarities among all calibration sites;
# (Notice, I substituted in the nonrare matrix - Christian 5/18/17)
# (Last time, this took 5.9 seconds to run.)

start.time = Sys.time()
dissim<-dapply(bugcal.pa.nonrare,1,bugcal.pa.nonrare,1,sornfun); 
end.time = Sys.time()
time.taken.dissim = end.time - start.time
time.taken.dissim

#Proceed to clustering;

####################################;
# Clustering of calibration sites;
# Use flexible-Beta method, with Beta=-0.6;
# See R documentation on agnes() in "cluster" package;
# When using agnes() with Flexible Beta strategy, set Beta=(1-2*Alpha) in Lance-Williams formula;
# A single value for par.method value specifies alpha, so alpha=0.8 gives Beta=-0.6;
# ?agnes()  # "agglomerative nesting (hierarchical clustering)

clus1<-agnes(x=dissim,diss=T,method="flexible", par.method=0.8,keep.diss=F,keep.data=F); class(clus1); str(clus1)

####################################;
# DENDROGRAM PLOTTING FUN!
# There are many ways to plot the dendrogram.  We explore some options here:
# Also, here are some helpful websites: https://cran.r-project.org/web/packages/dendextend/vignettes/FAQ.html, https://cran.r-project.org/web/packages/dendextend/vignettes/introduction.html


####### generic dendrogram #####

windows()
# I [CHRISTIAN] prefer site codes as dendrogram labels (but see John Van Sickle's code below if you prefer row names):
plot(clus1,which.plots=2,labels=bugcal.pa[,1],cex=.35,main=paste0("Dendrogram 
Created on ",Sys.time())); 

plot(clus1,which.plots=2,labels=rownames(bugcal.pa),cex=.35,main=paste0("Dendrogram 
Created on ",Sys.time())); 

# Relate number of clusters to horizontal line height:
# If you want to add horizontal bars that delineate the number of clusters (i.e., relate the number of clusters to the height cutoff)
zoom.dendro = as.dendrogram(as.hclust(clus1))

# Last time, this took 14.45 seconds:
start.time = Sys.time()
dendrogram.numclust.height = heights_per_k.dendrogram(zoom.dendro)
end.time = Sys.time()

time.taken.dendro.heights = end.time - start.time
time.taken.dendro.heights

# review the output:
dendrogram.numclust.height


#### add line for where to cut
# using 25 clusters as an example here:
num.clusters = 7

# report the height associated with that number of clusters:
dendrogram.numclust.height[num.clusters]

# create a plot and add a horizontal line at that height:
plot(clus1,which.plots=2,labels=bugcal.pa[,1],cex=.35,main=paste0("Dendrogram, created on ",Sys.time(),
    ", with a horizontal line delineating ", num.clusters, " cluster breaks.")); 
abline(h=dendrogram.numclust.height[num.clusters],col=4,lty=2);



########## color version  #######
# First, regardless of which option you choose, set the value here to reflect the number of clusters you want:
num.clusters = 8

# Option 1: Label dendrogram leaves with SiteCodes...
zoom.dendro = as.dendrogram(as.hclust(clus1))
labels(zoom.dendro) = as.vector(bugcal.pa[,1])[labels(zoom.dendro)]
zoom.dendro = color_labels(zoom.dendro,k = num.clusters)
zoom.dendro = color_branches(zoom.dendro,k = num.clusters)

plot(zoom.dendro, main=paste0("Dendrogram 
Created on ",Sys.time(),".
","(Leaf labels are Site Codes)"));abline(h=dendrogram.numclust.height[num.clusters],col=4,lty=2);


############# prune ##########
# Now that you've visualized the clustering options by plotting the dendrogram in various ways, you can proceed with actually pruning the dendrogram using the clustering scheme of your choice.

# John Van Sickle provided a simple 3-line code if you know exactly how many clusters you want to use:
grps.5<-cutree(clus1,k=5); #vector of group assignments is in the order of sites in the clustered data;
table(grps.5); #count number of sites in each group;
groups5=cbind(row.names(predcal),grps.5); #list calibration sites and their group assignments;


grps.6<-cutree(clus1,k=6); #vector of group assignments is in the order of sites in the clustered data;
table(grps.6); #count number of sites in each group;
groups6=cbind(row.names(predcal),grps.6); #list calibration sites and their group assignments;


grps.7<-cutree(clus1,k=7); #vector of group assignments is in the order of sites in the clustered data;
table(grps.7); #count number of sites in each group;
groups7=cbind(row.names(predcal),grps.7); #list calibration sites and their group assignments;


grps.8<-cutree(clus1,k=8); #vector of group assignments is in the order of sites in the clustered data;
table(grps.8); #count number of sites in each group;
groups8=cbind(row.names(predcal),grps.8); #list calibration sites and their group assignments;

grps.9<-cutree(clus1,k=9); #vector of group assignments is in the order of sites in the clustered data;
table(grps.9); #count number of sites in each group;
groups9=cbind(row.names(predcal),grps.9); #list calibration sites and their group assignments;

grps.10<-cutree(clus1,k=10); #vector of group assignments is in the order of sites in the clustered data;
table(grps.10); #count number of sites in each group;
groups10=cbind(row.names(predcal),grps.10); #list calibration sites and their group assignments;


grps.11<-cutree(clus1,k=11); #vector of group assignments is in the order of sites in the clustered data;
table(grps.11); #count number of sites in each group;
groups11=cbind(row.names(predcal),grps.11); #list calibration sites and their group assignments;


grps.12<-cutree(clus1,k=12); #vector of group assignments is in the order of sites in the clustered data;
table(grps.12); #count number of sites in each group;
groups12=cbind(row.names(predcal),grps.12); #list calibration sites and their group assignments;



########### display points on a map color coded by groups ########
rfdat<-data.frame(predcal,Groups=grps.9);
#windows()
m=sf::st_as_sf(rfdat, coords = c("Longitude", "Latitude"), crs = 4269,agr = "constant")
mapview::mapview(m['Groups'],col.regions=brewer.pal(8, "Spectral"))



#--------------------------------------------------------------------#
#STEP 3 . BUILD RANDOM fOREST MODEL TO PREDICT GROUP MEMBERSHIP;
#--------------------------------------------------------------------#



      #@@@ NA values in StreamCat metrics dont allow code to work....replace with '0' for now and see if the rest of the code works...hear back from trip on how to handle
      #predcal[is.na(predcal)] <- 0

# if all data is already present (no imputation needed), include categorical variables
rfdat<-data.frame(predcal,grps.8, grps.9, grps.10);

                                                                                  # predictors as chosen through PCA analyses--MAST/MSST values excluded, due to high numbers of missing values 
                                                                                  rfdat <- rfdat %>%
                                                                                    dplyr::select(Tmean8110Ws,Precip8110Ws,CaOWs,SLOPE,WsAreaSqKm,perStrm,SiO2Ws,PermWs,
                                                                                           Fe2O3Ws,ElevCat,Eco2,Eco3,Eco4_code,East.West, grps.5, grps.6, grps.7)

# 1 sample missing PermWs...need to impute
rfdat$PERM <- na.roughfix(rfdat$PERM)
rfdat$SAND <- na.roughfix(rfdat$SAND)                                                                       
rfdat$CLAY <- na.roughfix(rfdat$CLAY)
rfdat$RCKDEP <- na.roughfix(rfdat$RCKDEP)
rfdat$OM <- na.roughfix(rfdat$OM)
rfdat$MSST_mean08.14 <- na.roughfix(rfdat$MSST_mean08.14)
rfdat$MWST_mean08.14 <- na.roughfix(rfdat$MWST_mean08.14)
rfdat$SLOPE <- na.roughfix(rfdat$SLOPE)


rfdat$grps.8<-factor(rfdat$grps.8);
rfdat$grps.9<-factor(rfdat$grps.9);
rfdat$grps.10<-factor(rfdat$grps.10);


# Build RF model;

                                                                                  # VSURF procedure       
                                                                                  
                                                                                  species.vsurf.8 = VSURF(rfdat[,3:36], rfdat[,39])
                                                                                  summary(species.vsurf.8)
                                                                                  
                                                                                      #seeing what the variables are
                                                                                      names = as.data.frame(names(rfdat))
                                                                                      selected.pred=names[species.vsurf.8$varselect.pred,]
                                                                                      interp.pred=names[species.vsurf.8$varselect.interp,]
                                                                                      threshold.pred=names[species.vsurf.8$varselect.thres,]
                                                                                      
                                                                                      paste(selected.pred,collapse="+")
                                                                                      # [1] "PCTALKINTRUVOL+OM+CLAY+PCTCOLLUVSED"                                                                              
                                                                                  
                                                                                  
                                                                                      
                                                                                  species.vsurf.9 = VSURF(rfdat[,3:36], rfdat[,40])
                                                                                  summary(species.vsurf.9)
                                                                                  
                                                                                      #seeing what the variables are
                                                                                      names = as.data.frame(names(rfdat))
                                                                                      selected.pred=names[species.vsurf.9$varselect.pred,]
                                                                                      interp.pred=names[species.vsurf.9$varselect.interp,]
                                                                                      threshold.pred=names[species.vsurf.9$varselect.thres,]
                                                                                      
                                                                                      paste(selected.pred,collapse="+")
                                                                                      # [1] "PCTALKINTRUVOL+CLAY+PRECIP09+PCTCOLLUVSED+MWST_mean08.14"
                                                                                      
                                                                                  
                                                                                  species.vsurf.10 = VSURF(rfdat[,3:36], rfdat[,41])
                                                                                  summary(species.vsurf.10)
                                                                                  
                                                                                      #seeing what the variables are
                                                                                      names = as.data.frame(names(rfdat))
                                                                                      selected.pred=names[species.vsurf.10$varselect.pred,]
                                                                                      interp.pred=names[species.vsurf.10$varselect.interp,]
                                                                                      threshold.pred=names[species.vsurf.10$varselect.thres,]
                                                                                      
                                                                                      paste(selected.pred,collapse="+")
                                                                                      # [1] "PCTALKINTRUVOL+CLAY+SAND+PRECIP09+PCTCOLLUVSED+MWST_mean08.14"
                                                                                  
                                                                                      
                                                                                  species.vsurf.11 = VSURF(rfdat[,3:36], rfdat[,41])
                                                                                  summary(species.vsurf.11)
                                                                                  
                                                                                      #seeing what the variables are
                                                                                      names = as.data.frame(names(rfdat))
                                                                                      selected.pred=names[species.vsurf.11$varselect.pred,]
                                                                                      interp.pred=names[species.vsurf.11$varselect.interp,]
                                                                                      threshold.pred=names[species.vsurf.11$varselect.thres,]
                                                                                      
                                                                                      paste(selected.pred,collapse="+")
                                                                                      # [1] "PCTALKINTRUVOL+CLAY+SAND+PRECIP09+PCTCOLLUVSED+MWST_mean08.14"
                                                                                      
                                                                                  species.vsurf.12 = VSURF(rfdat[,3:36], rfdat[,41])
                                                                                  summary(species.vsurf.12)
                                                                                  
                                                                                      #seeing what the variables are
                                                                                      names = as.data.frame(names(rfdat))
                                                                                      selected.pred=names[species.vsurf.12$varselect.pred,]
                                                                                      interp.pred=names[species.vsurf.12$varselect.interp,]
                                                                                      threshold.pred=names[species.vsurf.12$varselect.thres,]
                                                                                      
                                                                                      paste(selected.pred,collapse="+")
                                                                                      # [1] "PCTALKINTRUVOL+CLAY+SAND+PRECIP09+PERM+PCTCOLLUVSED+MWST_mean08.14+INORGNWETDEP_2008"
                                                                                      
                                                                                    
                                                                                    # all predictors, for full models
                                                                                    predictors <- names[1:38, 1]
                                                                                    paste(predictors,collapse="+")
                                                                                    # Latitude+Longitude+AREASQKM+SAND+CLAY+ELEV+BFI+KFFACT+PCTGLACTILCRS+PCTCARBRESID+PCTALLUVCOAST+PCTALKINTRUVOL+PCTCOLLUVSED+TMAX8110+PRECIP8110+COMPSTRGTH+PCTBL2004+INORGNWETDEP_2008+N+HYDRLCOND+MGO+K2O+NA2O+SIO2+CAO+P2O5+S+FE2O3+PERM+RCKDEP+OM+PRECIP09+MSST_mean08.14+MWST_mean08.14+PCTICE_mean01.19+SLOPE+EcoRegion2+EcoRegion3
                                                                                                    
######################                
                
#                         run RF modeling

######################                                                                                        
                                                                                        
                                                                                    rf.mod.best.from.VSURF8 = randomForest(grps.8 ~ PCTALKINTRUVOL+OM+CLAY+PCTCOLLUVSED, data=rfdat, ntree=2000, importance=TRUE, norm.votes=TRUE, keep.forest=TRUE)
                                                                                    rf.mod.best.from.VSURF9 = randomForest(grps.9 ~ PCTALKINTRUVOL+CLAY+PRECIP09+PCTCOLLUVSED+MWST_mean08.14, data=rfdat, ntree=2000, importance=TRUE, norm.votes=TRUE, keep.forest=TRUE)
                                                                                    rf.mod.best.from.VSURF10 = randomForest(grps.10 ~ PCTALKINTRUVOL+CLAY+SAND+PRECIP09+PCTCOLLUVSED+MWST_mean08.14, data=rfdat, ntree=2000, importance=TRUE, norm.votes=TRUE, keep.forest=TRUE)
                                                                                    rf.mod.best.from.VSURF11 = randomForest(grps.11 ~ PCTALKINTRUVOL+CLAY+SAND+PRECIP09+PCTCOLLUVSED+MWST_mean08.14, data=rfdat, ntree=2000, importance=TRUE, norm.votes=TRUE, keep.forest=TRUE)
                                                                                    rf.mod.best.from.VSURF12 = randomForest(grps.12 ~ PCTALKINTRUVOL+CLAY+SAND+PRECIP09+PERM+PCTCOLLUVSED+MWST_mean08.14+INORGNWETDEP_2008, data=rfdat, ntree=2000, importance=TRUE, norm.votes=TRUE, keep.forest=TRUE)
                                                                                    
                                                                                    print(rf.mod.best.from.VSURF8)
                                                                                    print(rf.mod.best.from.VSURF9)
                                                                                    print(rf.mod.best.from.VSURF10)                                                                                  
                                                                                    print(rf.mod.best.from.VSURF11)  
                                                                                    print(rf.mod.best.from.VSURF12)  

# VSURF models are showing odd results--predictors with little variable importance are being selected.
# run a full model, look at VIP, select top predictors, run again with 'reduced' predictors
# full
rf_full = randomForest(grps.9 ~ AREASQKM+SAND+CLAY+ELEV+BFI+KFFACT+PCTGLACTILCRS+PCTCARBRESID+PCTALLUVCOAST+PCTALKINTRUVOL+PCTCOLLUVSED+TMAX8110+PRECIP8110+COMPSTRGTH+PCTBL2004+INORGNWETDEP_2008+N+HYDRLCOND+MGO+K2O+NA2O+SIO2+CAO+P2O5+S+FE2O3+PERM+RCKDEP+OM+PRECIP09+MSST_mean08.14+MWST_mean08.14+PCTICE_mean01.19+SLOPE,
          data=rfdat, ntree=2000, importance=TRUE, norm.votes=TRUE, keep.forest=TRUE)

print(rf_full)
varImpPlot(rf_full, type=1,n.var=33);
# reduced
rf_reduced = randomForest(grps.9 ~ TMAX8110+MWST_mean08.14+BFI+ELEV+PRECIP8110+CLAY,
                                       data=rfdat, ntree=2000, importance=TRUE, norm.votes=TRUE, keep.forest=TRUE)
                                                                                                             
                                                                                  
print(rf_reduced)

varImpPlot(rf_reduced, type=1,n.var=6);

# Variable Importance Plot (VIP):
                                                                                    varImpPlot(rf.mod.best.from.VSURF8, type=1,n.var=4);
                                                                                    varImpPlot(rf.mod.best.from.VSURF9, type=1,n.var=5);
                                                                                    varImpPlot(rf.mod.best.from.VSURF10, type=1,n.var=6);
                                                                                    varImpPlot(rf.mod.best.from.VSURF11, type=1,n.var=6);
                                                                                    varImpPlot(rf.mod.best.from.VSURF12, type=1,n.var=8);






# Variable Importance Table
importance(rf.mod.best.from.VSURF8)
importance(rf_full)



# the "type=1" argument below tells it to focus on the 1st metric of importance, the %IncMSE.  
var.imp <- data.frame(importance(rf.200ct_5grps, type=1));  var.imp
var.imp$Variables <- row.names(var.imp)
str(var.imp)
# order the variables by Gini importance index (most important variables at the top):
var.imp[order(var.imp$MeanDecreaseAccuracy, decreasing = T),]
imp.names = rownames(var.imp[order(var.imp$MeanDecreaseAccuracy,decreasing = T),]); imp.names


                                      # Partial Dependence Plots:
                                      sapply(unique(rfdat$Groups),function(grp){
                                        partialPlot(rf_full,pred.data=rfdat,x.var="Tmean8110Ws",which.class=grp,main=paste("Group ",grp))});
                                      
                                      sapply(unique(rfdat$Groups),function(grp){
                                        partialPlot(rf.250ct_7grps,pred.data=rfdat,x.var="longitude",which.class=grp,main=paste("Group ",grp))});






      
## Looping over variables ranked by importance:
# Y-axis is Probability of being in Class 1:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Notice that I'm restricting it to just the first 25 important variables! %
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# If you imputed, replace "rfdat" with "rf.imputed" in the code below.
# Last time this took 2.20 minutes!
windows(height=10,width=12)

start.time = Sys.time()
op <- par(mfrow=c(3, 3))
for (i in seq_along(imp.names[c(1:7)])) {
  partialPlot(rf.200ct_5grps, rfdat, which.class = "5", imp.names[i], xlab=imp.names[i],
              main=paste("on", imp.names[i])
              )# ylim=c(-5, 6)
}
end.time = Sys.time()

time.taken.PDP1 = end.time - start.time
time.taken.PDP1




#------------------------------------------------------------#
#STEP 4 - Save the final RF predictive model, for future use;
#------------------------------------------------------------#

# To specify the entire, final RF model, you need to store 4 things as an R object;
# 4.1) The site-by-taxa matrix of observed presence/absence at calibration sites (bugcal.pa, already available);  # I'll want bugcal.pa.nonrare!
# 4.2) Specify the vector of final group membership assignments at calibration sites(grps.final);
grps.final<-grps.9;


# 4.3) Specify the final predictor variables ;Tmean8110Ws+Precip8110Ws+WsAreaSqKm+SiO2Ws+Fe2O3Ws+ElevCat+Eco3
# full model
preds.final_full=c('AREASQKM','SAND','CLAY','ELEV','BFI','KFFACT','PCTGLACTILCRS','PCTCARBRESID','PCTALLUVCOAST','PCTALKINTRUVOL','PCTCOLLUVSED',
                    'TMAX8110','PRECIP8110','COMPSTRGTH','PCTBL2004','INORGNWETDEP_2008','N','HYDRLCOND','MGO','K2O','NA2O','SIO2','CAO','P2O5','S',
                    'FE2O3','PERM','RCKDEP','OM','PRECIP09','MSST_mean08.14','MWST_mean08.14','PCTICE_mean01.19', 'SLOPE')
                                
# reduced model, top predictors from Var Imp Plots
preds.final_reduced=c('TMAX8110', 'BFI','ELEV','MWST_mean08.14','CLAY','PRECIP8110')

      
      #preds.final=c('LONG','MSST_mean08091314','ElevCat','Tmax8110Ws','Precip8110Ws','length_46006','OmWs','DOY','WsAreaSqKm')
      #preds.final<-interp.pred
      #preds.final<-c('prdzdy','LONG','RunoffWs','Precip8110Ws','MSST_mean08091314','Tmax8110Ws','Tmin8110Ws')
    #preds.final<-c('Tmax8110Ws','Precip8110Ws','MSST_mean08091314','LONG')
#      preds.final<-tryit$"Names of included predictors in each RF iteration"[[1]];  # And this is the full model! 
     # preds.final = names(predall)[5:41]; #Use this if you didn't run variable reduction at all and you want to save the full model
# 4.4) The final RF model (rfmod);
#bugcal.pa=read.csv("C:/Users/jenni/OneDrive/Documents/ordinations/bugcal.pa.csv")

bugcal.pa = bugcal.pa

# Set predictors for full and reduced models
# full model
predcal.full = rfdat %>%
            select(AREASQKM,SAND,CLAY,ELEV,BFI,KFFACT,PCTGLACTILCRS,PCTCARBRESID,PCTALLUVCOAST,PCTALKINTRUVOL,PCTCOLLUVSED,
                   TMAX8110,PRECIP8110,COMPSTRGTH,PCTBL2004,INORGNWETDEP_2008,N,HYDRLCOND,MGO,K2O,NA2O,SIO2,CAO,P2O5,S,
                   FE2O3,PERM,RCKDEP,OM,PRECIP09,MSST_mean08.14,MWST_mean08.14,PCTICE_mean01.19, SLOPE )
        
    
# reduced model
predcal.reduced = rfdat %>% select(TMAX8110,BFI,ELEV,MWST_mean08.14,CLAY,PRECIP8110)

    
    
# Set model datasets to use: based on full or reduced predictors
    
predcal <- predcal.reduced
preds.final <- preds.final_reduced
    
model.final <-  rf_reduced
    
    

# Save the model components together in a single .Rdata file.;
    # Any R user can load this file, along with model.predict.RF.r, to make predictions from the model;
#detach("package:NAMCr", unload = TRUE)

save(bugcal.pa, predcal, grps.final, preds.final, ranfor.mod=model.final, file=('bugs analyses/RIVPACS_2022/_2024 model build/RIVPACS.2024__NoNBR_6out_9grps_6preds.Rdata')); # Notice I'm using the most parsimonious model (with 8 predictors)



    #save(bugcal.pa.nonrare, predcal, grps.final, preds.final, rf.mod, file=('My.RF.Model.Version1.fullmodel.Rdata')); # full model
    #NOTE - Predcal is not needed to define the model, but is included so that users see the 
    #       required format for predictor data;

      
#-------------------------------------------------------------------#
#Step 5 - calculate OE scores for reference sites for chosen model;
#-------------------------------------------------------------------#

load('bugs analyses/RIVPACS_2022/_2024 model build/RIVPACS.2024__NoNBR_6out_9grps_6preds.Rdata')
     

#Option 5.1 - Make predictions of E and O/E for calibration (reference) sites. Examine O/E statistics and plots;
  # To do this, run the model.predict.RanFor.4.2 function, using the calibration data as the 'new' data;
  # See Step 7 below, for more info on making predictions;
  # Also see internal documentation of model.predict.Ran.For.4.2;
source("bugs analyses/RIVPACS_2022/_2024 model build/model.predict.RanFor.4.2.r");
     #source("bugs analyses/RIVPACS_2022/model.predict.RanFor.r");
     
# CHRISTIAN: probably worth checking to make sure all these files have the same number of records...
dim(bugcal.pa)[1]
length(grps.final)
str(preds.final) # Just check that you have the correct predictors here (not the number of records)
dim(predcal)
summary(row.names(bugcal.pa)==row.names(predcal))

#Option 5.1.5 - Calculate replicate sampling SD of O/E, as a "perfect model" lower bound for SD(O/E) on calibration data;
# reference is Van Sickle et al. null model paper;
#first, compile the following function;
rep.sam.sd<-function(occprb.cal,Pc) {
  cal.cut<-(occprb.cal>=Pc); #TRUE/FALSE matrix denoting which taxa are above predicted P cutoff;
  #use occurrence probabilities only of taxa above the cutoff (cal.cut='TRUE');
  E.cal<-apply(occprb.cal*cal.cut,1,sum); #vector of predicted E ;
  # numerator of site-specific replicate sampling var. Result is a site vector
  RS.cal<-apply(occprb.cal*cal.cut,1,function(x)sum(x*(1-x)));
  SDRS<-sqrt(mean(RS.cal/(E.cal^2))); #replicate sampling SD is sqrt(mean(site-specific replicate sampling variances));
  print(' ',quote=F)
  print(' Replicate sampling SD of O/E: ',quote=F)
  print(SDRS,digits=4);  }; #end of function;
#Then execute the above function, using either in-bag or OOB predicted occurrence probs ('Capture probs') for the calibration data;
#rep.sam.sd(occprb.cal=OE.assess.cal$Capture.Probs,Pc=0.5);


# SLH 11.14.22: Trip sent me code for 'model.predict.RanFor', code below originally read 'model.predict.RanFor.4.2'

OE.assess.cal<-model.predict.RanFor.4.2(bugcal.pa,grps.final,preds.final, ranfor.mod=model.final,prednew=predcal,bugnew=bugcal.pa,Pc=0.5, Cal.OOB=TRUE); 
rep.sam.sd(occprb.cal=OE.assess.cal$Capture.Probs,Pc=0.5);



            
str(OE.assess.cal)
RESULTS=cbind(OE.assess.cal$OE.scores,predcal,grps.final)




 write.csv(RESULTS, 'bugs analyses/RIVPACS_2022/RIVPACS.2024_FINAL_ref.build_OE.csv')

 quantile(RESULTS$OoverE,0.10)

    ####
 #            10th percentile of ref O/E scores = 0.80
    ####







#Other parts of the results that may be used for further analysis
head(OE.assess.cal$Capture.Probs); #predicted capture probabilties, 1st 5 rows;
head(OE.assess.cal$Group.Occurrence.Probs); #predicted group occurrence probabilities, 1st 5 rows;

#-----------------------------------------------------------------------------------------------#
#Step 6 - Make predictions for degraded sites and compare scores between reference and degraded;
#-----------------------------------------------------------------------------------------------#
prednew=read.csv("C:/Users/jenni/Box/Nonperennial streams/Bugdata/rerun of all models_for pub/metrics_predictors.csv")

prednew = preds_RIV24_no.nbr

#select only reference data and exclude bug metrics
prednew=prednew[,c(1,97:150)]
rownames(prednew)=prednew$sampleId
prednew=prednew[,c(-1,-2)]

bugsOTU=query("sampleTaxaTranslationRarefied",projectId=387,translationId=2,fixedCount=300)
sumrarefiedOTUTaxa = bugsOTU  %>%
  dplyr::group_by(sampleId, otuName) %>%
  dplyr::summarize(sumSplitCount = sum(splitCount)) # why are multiple records exported here per OTU???

sumrarefiedOTUTaxa$presence = ifelse(sumrarefiedOTUTaxa$sumSplitCount >=1, 1, 0)
bugnew = tidyr::pivot_wider(sumrarefiedOTUTaxa,id_cols = "sampleId", names_from = "otuName",values_from = "presence")
bugnew[is.na(bugnew)]<-0
bugnew=as.data.frame(bugnew)
rownames(bugnew)<-bugnew$sampleId
bugnew<-bugnew[,-1]

bugnew= bugnew[order(rownames(bugnew)),];
prednew = prednew[order(rownames(prednew)),];
OE.assess.cal<-model.predict.RanFor.4.2(bugcal.pa,grps.final,preds.final, ranfor.mod=rf.mod.best.from.VSURF9,prednew=prednew,bugnew=bugnew,Pc=0.5,Cal.OOB=FALSE);

RESULTS=cbind(OE.assess.cal$OE.scores,prednew)

finalresultcompare=read.csv("C:/Users/jenni/Box/Nonperennial streams/Bugdata/rerun of all models_for pub/finalresult_compare.csv")
boxplot(OoverE_9groups~reference,data=finalresultcompare)
boxplot(OoverE_10groups~reference,data=finalresultcompare)

finalresultcomparesub=subset(finalresultcompare,reference %in% c("Y","N"))
t.test(OoverE_9groups~reference,data=finalresultcomparesub)
t=t.test(OoverE_10groups~reference,data=finalresultcomparesub)


tvalues=list()
for (i in 2:3){
  t=t.test(finalresultcomparesub[,i]~finalresultcomparesub$reference)
  tvalues[[paste0(colnames(finalresultcomparesub)[i])]]=unlist(t$statistic[[1]])
}

tvaluesdf<-as.data.frame(tvalues)
tvaluesdft=data.table::transpose(tvaluesdf,keep.names='metric')

reference_only=subset(finalresultcompare,reference %in% c("Y"))
quantile(reference_only$OoverE_10groups,0.10)










####################################################################################

#   Model Validation

#####################################################################################


####
    ##### Compare new model results to old scores
####

# bring in saved bug data, downloaded from AWQMS in early February by LAM
# AWQMS pulls haven't been working, this was a temporary work-around, suitable for model building but not permament
load('bugs analyses/RIVPACS_2022/_2024 model build/raw_bugs.Rdata') 


# join to taxonomy and OTUs
taxonomy <- read.xlsx('bugs analyses/Taxonomy/ODEQ_Taxonomy_dec22.xlsx') 

taxonomy.otu <- taxonomy %>%
  select(DEQ_Taxon = DEQ_TAXON, Taxon, OTU_RIV_24)

taxonomy.otu$DEQ_Taxon <- as.character(taxonomy.otu$DEQ_Taxon)


raw.bugs_taxonomy <- raw_bugs %>%
  left_join(taxonomy.otu, by='DEQ_Taxon')

# sum abundances across OTUs, drop 'DNI' taxa

raw.bugs_OTUs <- as.data.frame(raw.bugs_taxonomy %>%
                                     group_by(Sample=act_id, MLocID, OTU=OTU_RIV_24) %>%
                                     summarise(Count=sum(Result_Numeric)) %>%
                                     filter(OTU != 'DNI'))



#### READY for SUBSAMPLING

#####
#####
#       1.2 rarify to 300 count
#####
#####

# rarify to 300 count per sample --> this standardizes 'effort' across all samples 
# since O/E is basically 'reference taxa richness', it is highly related to total count


# load the 'reshape2' package and source in the 'rarify' script for subsampling to 300

source('bugs analyses/RIVPACS_2022/_2024 model build/rarify_w_seed.R')


b.rare.seed <- rarify.seed(na.omit(raw.bugs_OTUs), 'Sample', 'Count', 300) 



#####
#####
#       1.3 get total abundance of rarified samples for PREDATOR
######
######

tot.abund_raw.bugs<-aggregate(b.rare.seed$Count, list(Sample=b.rare.seed$Sample, MLocID=b.rare.seed$MLocID), sum)
tot.abund_raw.bugs <- dplyr::rename(tot.abund_raw.bugs, tot.abund_raw.bugs = x)

#####
#####
#       1.4 Matrify: convert from long format (many rows per sample), to site X species matrix (one row per sample)
######
######

dm.rare <- tidyr::pivot_longer(b.rare.seed, Count, names_to = "variable", values_to = "value" )

# data.table::setDT(dm.rare)
# bugs.all <- data.table::dcast(dm.rare, Sample+MLocID+Eco2+Eco3 ~ OTU, fun.aggregate = sum) # Sample x OTU doesn't seem to be unique so you need the fun.aggregate.  
# head(bugs.all)

bugs.mat_raw.bugs <- dm.rare %>% pivot_wider(               # new tidy method, replacing dcast (data.table)
  names_from = OTU,
  id_cols = c(Sample),
  values_from = value,
  values_fill = 0,
  names_repair = "check_unique",
  names_sort = TRUE)

# export matrified bug data file to load directly in model building phase

save(bugs.mat_raw.bugs, file='bugs analyses/RIVPACS_2022/_2024 model build/bugs.mat_raw.bugs.Rdata')







##########

#               BUG DATA: MODEL READY

##########

# bring in saved bug data, downloaded from AWQMS in early February by LAM
# AWQMS pulls haven't been working, this was a temporary work-around, suitable for model building but not permament
load('bugs analyses/RIVPACS_2022/_2024 model build/raw_bugs.Rdata') 


# load 'matrified, subsamled raw_bugs

load('bugs analyses/RIVPACS_2022/_2024 model build/bugs.mat_raw.bugs.Rdata')






##########

#             PREDICTOR DATA: assemble

##########

# load StreamCat predictors --- saved in "ASSEMBLE DATA"

load('bugs analyses/RIVPACS_2022/_2024 model build/metrics_WS.OTHER_OR.CA.NV.Rdta' )

load('bugs analyses/RIVPACS_2022/_2024 model build/metrics_CAT.OTHER_OR.CA.NV.Rdta' )



#####

#   Bring in COMIDs that were reviewed and, as necessary, manually assigned by Adam Thompson

#####

bio.mlocids.awqms_ALT <- read.xlsx('bugs analyses/RIVPACS_2022/_2024 model build/Bio_MlocIDs_AWQMS.xlsx') 



mlocids_WS <- bio.mlocids.awqms_ALT %>%
  filter(COMID != 'NULL') %>%
  select(MLocID, COMID) %>%
  unique()

mlocids_WS$COMID <- as.character(mlocids_WS$COMID)


mlocids_CAT <- bio.mlocids.awqms_ALT %>%
  filter(COMID == 'NULL') %>%
  select(MLocID, COMID = Nearby_COMID) %>%
  drop_na() %>%
  unique()

mlocids_CAT$COMID <- as.character(mlocids_CAT$COMID)

# get sample IDs (act_id) and MLocIDs

act.MLoc <- raw_bugs %>%
  select(act_id, MLocID,COMID, SampleStart_Date, Sample_Method, Char_Name) %>%
  filter(SampleStart_Date > "1998-01-01") %>%
  filter(Sample_Method %in% 'Benthic Kick - Riffle' | Sample_Method %in% 'Benthic Kick - Targeted Riffle'| Sample_Method %in% 'Benthic Kick - Transect') %>%
  filter(Char_Name == 'Count') %>%
  mutate( month = format(SampleStart_Date,"%m")) %>%
  filter(month %in% '06' | month %in% '07' | month %in% '08' | month %in% '09' | month %in% '10') %>%
  unique()


act.MLoc$COMID <- as.character(act.MLoc$COMID)

# join samples with appropriate scale: WS or CAT

act.MLoc_WS <- act.MLoc %>%
  inner_join(mlocids_WS, by = c('MLocID','COMID'))

act.MLoc_CAT <- act.MLoc %>%
  inner_join(mlocids_CAT, by = c('MLocID')) %>%
  select(-COMID.x) %>%
  rename(COMID = COMID.y)


# join with streamcat tables

preds.WS <- act.MLoc_WS %>%
  left_join(metrics_WS.OTHER_OR.CA.NV, by = 'COMID')

preds.CAT <- act.MLoc_CAT %>%
  left_join(metrics_CAT.OTHER_OR.CA.NV, by = 'COMID')



preds.WS <- preds.WS %>%
  rename(AREASQKM = WSAREASQKM, SAND = SANDWS, CLAY = CLAYWS, ELEV = ELEVWS,            
         BFI = BFIWS, KFFACT = KFFACTWS, PCTGLACLAKEFINE = PCTGLACLAKEFINEWS,
         PCTGLACTILCLAY = PCTGLACTILCLAYWS, PCTGLACTILLOAM = PCTGLACTILLOAMWS,  
         PCTCOASTCRS = PCTCOASTCRSWS, PCTGLACLAKECRS = PCTGLACLAKECRSWS,
         PCTEXTRUVOL = PCTEXTRUVOLWS, PCTEOLFINE = PCTEOLFINEWS, PCTEOLCRS = PCTEOLCRSWS, 
         PCTGLACTILCRS = PCTGLACTILCRSWS, PCTSALLAKE = PCTSALLAKEWS, 
         PCTCARBRESID = PCTCARBRESIDWS, PCTALLUVCOAST = PCTALLUVCOASTWS,      
         PCTALKINTRUVOL = PCTALKINTRUVOLWS, PCTHYDRIC = PCTHYDRICWS,
         PCTCOLLUVSED = PCTCOLLUVSEDWS, TMAX8110 = TMAX8110WS, TMIN8110 = TMIN8110WS,
         PRECIP8110 = PRECIP8110WS, TMEAN8110 = TMEAN8110WS, COMPSTRGTH = COMPSTRGTHWS,        
         PCTBL2004 = PCTBL2004WS, INORGNWETDEP_2008 = INORGNWETDEP_2008WS,
         NO3_2008 = NO3_2008WS, NH4_2008 = NH4_2008WS, N = NWS,
         HYDRLCOND = HYDRLCONDWS, MGO = MGOWS, K2O = K2OWS, AL2O3 = AL2O3WS,
         NA2O = NA2OWS, SIO2 = SIO2WS, CAO = CAOWS, P2O5 = P2O5WS,             
         S = SWS, FE2O3 = FE2O3WS, PERM = PERMWS, RCKDEP = RCKDEPWS, OM = OMWS,
         PCTBL2001 = PCTBL2001WS, PRECIP08 = PRECIP08WS, PRECIP09 = PRECIP09WS )    

preds.CAT <- preds.CAT %>%
  rename(AREASQKM = CATAREASQKM, SAND = SANDCAT, CLAY = CLAYCAT, ELEV = ELEVCAT,            
         BFI = BFICAT, KFFACT = KFFACTCAT, PCTGLACLAKEFINE = PCTGLACLAKEFINECAT,
         PCTGLACTILCLAY = PCTGLACTILCLAYCAT, PCTGLACTILLOAM = PCTGLACTILLOAMCAT,  
         PCTCOASTCRS = PCTCOASTCRSCAT, PCTGLACLAKECRS = PCTGLACLAKECRSCAT,
         PCTEXTRUVOL = PCTEXTRUVOLCAT, PCTEOLFINE = PCTEOLFINECAT, PCTEOLCRS = PCTEOLCRSCAT, 
         PCTGLACTILCRS = PCTGLACTILCRSCAT, PCTSALLAKE = PCTSALLAKECAT, 
         PCTCARBRESID = PCTCARBRESIDCAT, PCTALLUVCOAST = PCTALLUVCOASTCAT,      
         PCTALKINTRUVOL = PCTALKINTRUVOLCAT, PCTHYDRIC = PCTHYDRICCAT,
         PCTCOLLUVSED = PCTCOLLUVSEDCAT, TMAX8110 = TMAX8110CAT, TMIN8110 = TMIN8110CAT,
         PRECIP8110 = PRECIP8110CAT, TMEAN8110 = TMEAN8110CAT, COMPSTRGTH = COMPSTRGTHCAT,        
         PCTBL2004 = PCTBL2004CAT, INORGNWETDEP_2008 = INORGNWETDEP_2008CAT,
         NO3_2008 = NO3_2008CAT, NH4_2008 = NH4_2008CAT, N = NCAT,
         HYDRLCOND = HYDRLCONDCAT, MGO = MGOCAT, K2O = K2OCAT, AL2O3 = AL2O3CAT,
         NA2O = NA2OCAT, SIO2 = SIO2CAT, CAO = CAOCAT, P2O5 = P2O5CAT,             
         S = SCAT, FE2O3 = FE2O3CAT, PERM = PERMCAT, RCKDEP = RCKDEPCAT, OM = OMCAT,
         PCTBL2001 = PCTBL2001CAT, PRECIP08 = PRECIP08CAT, PRECIP09 = PRECIP09CAT )    

preds_raw.bugs <- bind_rows(preds.WS, preds.CAT)


# limit to model predictors

preds_raw.bugs_mod <- preds_raw.bugs %>%
  select(act_id, TMAX8110,BFI,ELEV,MWST_mean08.14,CLAY,PRECIP8110) 

  
# drop samples with incomplete predictors

preds_raw.bugs_mod <- preds_raw.bugs_mod[complete.cases(preds_raw.bugs_mod), ] # drops about 70 samples



  
#-------------------------------------------------------------------#
#Step 5 - calculate OE scores for reference sites for chosen model;
#-------------------------------------------------------------------#
#######

# match bug file and predictors file

# subset the bug data to only the reference predictor data file for now 
bugs.mat_raw.bugs_matching <- subset(bugs.mat_raw.bugs, Sample %in% preds_raw.bugs_mod$act_id) 

preds_raw.bugs_mod <-subset(preds_raw.bugs_mod, act_id %in% bugs.mat_raw.bugs$Sample)


bugs.mat_raw.bugs_matching<- bugs.mat_raw.bugs_matching %>% column_to_rownames(var='Sample')

rownames(preds_raw.bugs_mod) <- preds_raw.bugs_mod[,1]  # tidy way wasn't working
preds_raw.bugs_mod <- preds_raw.bugs_mod %>%  select(-act_id)


############ Step 1c - Align bug and predictor data, by site/sample;
#check sample(row) alignment of bug and predictor data;

bugs.mat_raw.bugs_matching= bugs.mat_raw.bugs_matching[order(rownames(bugs.mat_raw.bugs_matching)),];
preds_raw.bugs_mod = preds_raw.bugs_mod[order(rownames(preds_raw.bugs_mod)),];


# probably worth checking to make sure all these files have the same number of records...
dim(bugs.mat_raw.bugs_matching)[1]
dim(preds_raw.bugs_mod)
summary(row.names(bugs.mat_raw.bugs_matching)==row.names(preds_raw.bugs_mod))




# load the model to run
load('bugs analyses/RIVPACS_2022/_2024 model build/RIVPACS.2024__NoNBR_6out_9grps_6preds.Rdata')






#Option 5.1 - Make predictions of E and O/E for calibration (reference) sites. Examine O/E statistics and plots;
# To do this, run the model.predict.RanFor.4.2 function, using the calibration data as the 'new' data;
# See Step 7 below, for more info on making predictions;
# Also see internal documentation of model.predict.Ran.For.4.2;
source("bugs analyses/RIVPACS_2022/_2024 model build/model.predict.RanFor.4.2.r");
#source("bugs analyses/RIVPACS_2022/model.predict.RanFor.r");



# run model with new dataset
OE.AWQMS<-model.predict.RanFor.4.2(bugcal.pa,grps.final,preds.final, ranfor.mod=model.final,
                prednew=preds_raw.bugs_mod,
                bugnew=bugs.mat_raw.bugs_matching,
                Pc=0.5, Cal.OOB=FALSE); 
      ########
            ###### Cal.OOB must be FALSE for test sites (true only for calibration sites)
      ########

test.RESULTS <- cbind(OE.AWQMS$OE.scores,prednew)


# get MLocID back into O/E results
OE.raw_bugs <- test.RESULTS %>%
  rownames_to_column(var='act_id')


# MLocID associated with act_id
OE.raw_bugs2 <- OE.raw_bugs %>%
  left_join(act.MLoc, by = 'act_id') %>%
  select(-Char_Name, -COMID) %>%
  unique()


# Get MLocID and reference into O/E results
  # bring in ref/disturbed designations
  bio.mlocids<- read.xlsx('bugs analyses/RIVPACS_2022/_2024 model build/Bio_MlocIDs_AWQMS.xlsx') %>%
    select(-org_id, -Project1, -MonLocType) %>%
    unique()


OE.raw_bugs_stations <- OE.raw_bugs2 %>% 
  left_join(bio.mlocids, by = 'MLocID') %>%
  filter(ReferenceSite == 'REFERENCE' | ReferenceSite == 'MODERATELY DISTURBED'| ReferenceSite == 'MOST DISTURBED')

      @@@@@@@@ duplpicated act_id = 'dfw_2451:20080811:R:SR'

        OE.raw_bugs_stations[duplicated(OE.raw_bugs_stations$act_id),]
        
        OE.raw_bugs_stations <- OE.raw_bugs_stations[-2992, ]

      @@@@@@@ duplicated as well???  '103WER026' 'Hardscrabble Creek above Coldwater Creek'

ref.order <- c('REFERENCE', 'MODERATELY DISTURBED', 'MOST DISTURBED')


bp <- ggplot(OE.raw_bugs_stations, aes(x=factor(ReferenceSite, level = ref.order), y=OoverE)) + 
  geom_boxplot( fill = 'gray') +    ylim(0,2) +
  theme_bw() + 	labs(x='', y = '', size=10) +
  theme(axis.text.y = element_text(size = 12,  face = "bold"),
        axis.title=element_text(size=10,face="bold"))  


bp+ facet_wrap(~EcoRegion2)
bp+ facet_wrap(~EcoRegion3)


######

#       Compare new model scores to old PREDATOR scores

#####


# bring in old O/E scores

oe.old <- read.csv('bugs analyses/RIVPACS_2022/_2024 model build/SUM_BUGS_2023-08-07.csv') %>%
  select(act_id = Sample, OE_old = OoverE, E_old = E)

oe_old.vs.new <- OE.raw_bugs_stations %>%
  select(act_id, MLocID, SampleStart_Date, ReferenceSite, OE_new = OoverE, E_new = E) %>%
  left_join(oe.old, by = 'act_id') %>%
  filter(complete.cases(.)) %>%
  filter(SampleStart_Date > '1998-01-01') %>%
  mutate(ReferenceSite = fct_relevel(ReferenceSite, 'MOST DISTURBED', 'MODERATELY DISTURBED', 'REFERENCE'))


ggplot(oe_old.vs.new, aes(x = OE_old, y = OE_new, color = ReferenceSite)) +
  geom_point() + geom_abline(slope = 1, intercept = 0   )+ 
  geom_smooth(method = "lm", se = FALSE)


#####

#       SENSITIVITY

#####

most.disturbed <- oe_old.vs.new %>%
  filter(ReferenceSite == 'MOST DISTURBED')

sensitvity <- most.disturbed %>%
  filter(OE_new < 0.80)

N = 178

sensitivity = 178/300*100


59%











