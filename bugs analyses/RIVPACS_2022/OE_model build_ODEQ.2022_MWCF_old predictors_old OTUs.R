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

#-------------------------------------------------------------------------#
#STEP 1 -- INITIAL SETUP -- Input and organize the bug and predictor data;
#-------------------------------------------------------------------------#
# 
######### Step 1a - Read and organize predictor data;
# read in predictors from csv not database. once you select final predictors for the model save them in the database
# calc predictors for all sites (reference and degraded and put in one file)
preds=read.csv("bugs analyses/RIVPACS_2022/run old models with new ref sites/2005preds_225ref_new sites.csv")
#select only reference data and exclude bug metrics
                             

                              #  @@@@ SLH 11.2.22
                              # predcal=predcal[predcal$reference=='Y',c(1,97:150)]
                              # @@@@ currently only reference data is included in the preds file, so this either not needed OR we need a column for 'reference' added


# remove NBR sites
preds <- preds %>%
           dplyr:: filter(Eco2 == 'MWCF')



rownames(preds)=preds$sampleId
########
########### Step 1b - Input the assemblage data (bug data), as a site-by-taxa matrix;
########
# First, create a Presence/absence (1/0) matrix (site by taxa) for the bugs;

                              # @@@@ SLH: 11.2.22-this all seems USU specific, change to work with csv input file received from Trip on 11.2.22
                              # create a project in the database with all samples to be used
                              # query database for all bug data in that project, select the OTU/ translation to be used for the model, and fixed count
                              # bugsOTU=query("sampleTaxaTranslationRarefied",projectId=387,translationId=2,fixedCount=300)

bugsOTU <- read.csv('bugs analyses/RIVPACS_2022/run old models with new ref sites/ODEQ22Matrix2005otu.csv')                        
                              
# subset the bug data to only the reference predictor data file for now 
bugsOTU=subset(bugsOTU, sampleId %in% rownames(preds))
       

# database exports multiple records per OTU so need to group them first
sumrarefiedOTUTaxa = bugsOTU  %>%
  dplyr::group_by(sampleId, otuName2005) %>%
  dplyr::summarize(sumSplitCount = sum(splitCount)) 
  # SLH 11/9/22: sumrare = 1081 fewer records than bugsOTU

# SLH 11/9/22: calculate total abundance, use to drop sites below a certain threshold (e.g., <200, <250)

tot.abund = sumrarefiedOTUTaxa  %>%
  dplyr::group_by(sampleId) %>%
  dplyr::summarize(tot_abund = sum(sumSplitCount))
          # use this to plot locations and abundances, look for patterns
          # tot.abund_location <- predcal %>%
          #  dplyr::left_join(tot.abund, by="sampleId")

          # write.csv(tot.abund_location, 'bugs analyses/RIVPACS_2022/tot.abund_location.csv')

# all bug samples with total abundance 200 or more 
tot.abund_200 <- tot.abund %>%
  dplyr::filter(tot_abund > 199)
  # 206 samples

# all bug samples with total abundance 200 or more 
tot.abund_250 <- tot.abund %>%
  dplyr::filter(tot_abund > 249)
# 186 samples


# create bug and predictor dataframes to use in modeling
bugs.mod_200 <- sumrarefiedOTUTaxa %>%
  dplyr::filter(sampleId %in% tot.abund_200$sampleId)

bugs.mod_250 <- sumrarefiedOTUTaxa %>%
  dplyr::filter(sampleId %in% tot.abund_250$sampleId)

preds_200 <- preds %>%
  dplyr::filter(sampleId %in% tot.abund_200$sampleId)
rownames(preds_200)=preds_200$sampleId


preds_250 <- preds %>%
  dplyr::filter(sampleId %in% tot.abund_250$sampleId)
rownames(preds_250)=preds_250$sampleId

###
####### prep preds file: remove sampleId and reference columns
###
preds_200 <- preds_200 %>%
  dplyr::select(-c(siteName, sampleId, COMID, waterbodyName))

preds_250 <- preds_250 %>%
  dplyr::select(-c(siteName, sampleId, COMID, waterbodyName))

###
######## create P/A dataframes
###

######## 200 count
# convert counts to presence absence
bugs.mod_200$presence = ifelse(bugs.mod_200$sumSplitCount >=1, 1, 0)

# pivot into wide format with taxa as columns and sites as rows
bugcal.pa_200 = tidyr::pivot_wider(bugs.mod_200,id_cols = "sampleId", names_from = "otuName2005",values_from = "presence")
# fill in nulls as 0
bugcal.pa_200[is.na(bugcal.pa_200)]<-0
bugcal.pa_200=as.data.frame(bugcal.pa_200)
rownames(bugcal.pa_200)<-bugcal.pa_200$sampleId
# remove sampleID as a column
bugcal.pa_200<-bugcal.pa_200[,-1]


######### 250 count
# convert counts to presence absence
bugs.mod_250$presence = ifelse(bugs.mod_250$sumSplitCount >=1, 1, 0)

# pivot into wide format with taxa as columns and sites as rows
bugcal.pa_250 = tidyr::pivot_wider(bugs.mod_250,id_cols = "sampleId", names_from = "otuName2005",values_from = "presence")
# fill in nulls as 0
bugcal.pa_250[is.na(bugcal.pa_250)]<-0
bugcal.pa_250=as.data.frame(bugcal.pa_250)
rownames(bugcal.pa_250)<-bugcal.pa_250$sampleId
# remove sampleID as a column
bugcal.pa_250<-bugcal.pa_250[,-1]



###########
##########
###########

# EXPLORATORY WORK = choose total abundance level HERE, for modeling moving forward--repeat for each threshold (200, 250) and compare results
#                    after deciding on final model, clean this up and remove multiple models

bugcal.pa <- bugcal.pa_200
predcal   <- preds_200

##########
##########
##########
  
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


    #   @@@@@ SLH 11.9.22 (200 count): only 104 taxa that are found at > 5% of cal sites?  Seems low!
    #   @@@@@ SLH 11.14.22 (250 ct): only 105 taxa


 

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

source("bugs analyses/RIVPACS_2022/dapply.r")

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
plot(clus1,which.plots=2,labels=rownames(bugcal.pa),cex=.35,main=paste0("Dendrogram, created on ",Sys.time(),
    ", with a horizontal line delineating ", num.clusters, " cluster breaks.")); 
abline(h=dendrogram.numclust.height[num.clusters],col=4,lty=2);



########## color version  #######
# First, regardless of which option you choose, set the value here to reflect the number of clusters you want:
num.clusters = 7

# Option 1: Label dendrogram leaves with SiteCodes...
zoom.dendro = as.dendrogram(as.hclust(clus1))
labels(zoom.dendro) = rownames(bugcal.pa)[labels(zoom.dendro)]
zoom.dendro = color_labels(zoom.dendro,k = num.clusters)
zoom.dendro = color_branches(zoom.dendro,k = num.clusters)

plot(zoom.dendro, main=paste0("Dendrogram 
Created on ",Sys.time(),".
","(Leaf labels are Site Codes)"));abline(h=dendrogram.numclust.height[num.clusters],col=4,lty=2);


############# prune ##########
# Now that you've visualized the clustering options by plotting the dendrogram in various ways, you can proceed with actually pruning the dendrogram using the clustering scheme of your choice.

# John Van Sickle provided a simple 3-line code if you know exactly how many clusters you want to use:
grps.2<-cutree(clus1,k=2); #vector of group assignments is in the order of sites in the clustered data;
table(grps.2); #count number of sites in each group;
groups2=cbind(row.names(predcal),grps.2); #list calibration sites and their group assignments;


grps.3<-cutree(clus1,k=3); #vector of group assignments is in the order of sites in the clustered data;
table(grps.3); #count number of sites in each group;
groups3=cbind(row.names(predcal),grps.3); #list calibration sites and their group assignments;


########### display points on a map color coded by groups ########
rfdat<-data.frame(predcal,Groups=grps.2);

m=sf::st_as_sf(rfdat, coords = c("longitude", "latitude"), crs = 4269,agr = "constant")
mapview::mapview(m['Groups'],col.regions=brewer.pal(7, 'Set1'), label=TRUE, use.layer.names=TRUE)



#--------------------------------------------------------------------#
#STEP 3 . BUILD RANDOM fOREST MODEL TO PREDICT GROUP MEMBERSHIP;
#--------------------------------------------------------------------#



      #@@@ NA values in StreamCat metrics dont allow code to work....replace with '0' for now and see if the rest of the code works...hear back from trip on how to handle
      #predcal[is.na(predcal)] <- 0

# if all data is already present (no imputation needed), include categorical variables
rfdat<-data.frame(predcal,grps.2); #, grps.9, grps.10, grps.11


rfdat <- rfdat %>%
  dplyr::select(daynum, long, grps.2)#,grps.9,grps.10,grps.11



                                # if some predictors are missing data, use randomforest package to impute NAs
                                
                                
                                #     @@@@@@@   = doesnt work when categorical included, would need to split categorical out, then recombine after imputing
                                
                                predcal_imputed <- na.roughfix(predcal)
                                
                                ##############
                                rfdat<-data.frame(predcal_imputed,grps.7, grps.8, grps.9, grps.10, grps.11);




rfdat$grps.2<-factor(rfdat$grps.2);


 
# Build RF model;


    
                                                                    # VSURF procedure       
                                                                    species.vsurf.7 = VSURF(rfdat[,1:14], rfdat[,15])
                                                                    species.vsurf.8 = VSURF(rfdat[,1:14], rfdat[,16])
                                                                    species.vsurf.9 = VSURF(rfdat[,1:14], rfdat[,17])
                                                                    species.vsurf.10 = VSURF(rfdat[,1:14], rfdat[,18])
                                                                    species.vsurf.11 = VSURF(rfdat[,1:14], rfdat[,19])
                                                                    
                                          
                                                                    summary(species.vsurf.8)

                                                                    #seeing what the variables are
                                                                    names = as.data.frame(names(rfdat))
                                                                    selected.pred=names[species.vsurf.11$varselect.pred,]
                                                                    interp.pred=names[species.vsurf.11$varselect.interp,]
                                                                    threshold.pred=names[species.vsurf.11$varselect.thres,]
                                                                    
                                                                    paste(selected.pred,collapse="+")
                                                                    
                                                                        # all predictors, for full models
                                                                        predictors <- names[1:37, 1]
                                                                        paste(predictors,collapse="+")




# run RF modeling
                                                                    # VSURF models
                                                                    rf.mod.best.from.VSURF7_250 = randomForest(grps.7 ~ Tmean8110Ws+ElevCat+Eco3, data=rfdat, ntree=2000, importance=TRUE, norm.votes=TRUE, keep.forest=TRUE)
                                                                    rf.mod.best.from.VSURF8_250 = randomForest(grps.8 ~ Tmean8110Ws+ElevCat+Eco3, data=rfdat, ntree=2000, importance=TRUE, norm.votes=TRUE, keep.forest=TRUE)
                                                                    rf.mod.best.from.VSURF9_250 = randomForest(grps.9 ~ Tmean8110Ws+ElevCat+Eco3, data=rfdat, ntree=2000, importance=TRUE, norm.votes=TRUE, keep.forest=TRUE)
                                                                    rf.mod.best.from.VSURF10_250 = randomForest(grps.10 ~ Tmean8110Ws+ElevCat+Eco3, data=rfdat, ntree=2000, importance=TRUE, norm.votes=TRUE, keep.forest=TRUE)
                                                                    rf.mod.best.from.VSURF11_250 = randomForest(grps.11 ~ Tmean8110Ws+ElevCat+Eco3, data=rfdat, ntree=2000, importance=TRUE, norm.votes=TRUE, keep.forest=TRUE)
                                                                              rf.mod.best.from.VSURF12 = randomForest(grps.12 ~ . , data=rfdat, ntree=2000, importance=TRUE, norm.votes=TRUE, keep.forest=TRUE)
                                                                    # rf.mod.best.from.VSURF9 = randomForest(grps.9 ~ PctIce2011Ws+MAST_mean08091314+LAT+RckdepWs+Tmax8110Ws+length_46003+SWs+A1_3, data=rfdat, ntree=2000, importance=TRUE, norm.votes=TRUE, keep.forest=TRUE)
                                                                    # rf.mod.best.from.VSURF11 = randomForest(grps.10 ~ MAST_mean08091314+LAT+PctIce2011Ws+WsAreaSqKm+RckdepWs+Tmax8110Ws+length_46003+MSST_mean08091314+SWs, data=rfdat, ntree=2000, importance=TRUE, norm.votes=TRUE, keep.forest=TRUE)
                                                                    print(rf.mod.best.from.VSURF8_250)


                                                                    
                                                                    
rf.200ct_2grps = randomForest(grps.2 ~ daynum + long,
                              data=rfdat, ntree=2000, importance=TRUE, norm.votes=TRUE, keep.forest=TRUE)





print(rf.200ct_2grps)

# Variable Importance Plot (VIP):


varImpPlot(rf.200ct_2grps, type=1,n.var=2);



 

#varImpPlot(rf.mod, type=1,n.var=48);



# Variable Importance Table
importance(rf.250ct_7grps)
# the "type=1" argument below tells it to focus on the 1st metric of importance, the %IncMSE.  
var.imp <- data.frame(importance(rf.250ct_7grps, type=1));  var.imp
var.imp$Variables <- row.names(var.imp)
str(var.imp)
# order the variables by Gini importance index (most important variables at the top):
var.imp[order(var.imp$MeanDecreaseAccuracy, decreasing = T),]
imp.names = rownames(var.imp[order(var.imp$MeanDecreaseAccuracy,decreasing = T),]); imp.names


# Partial Dependence Plots:
sapply(unique(rfdat$Groups),function(grp){
  partialPlot(rf.mod,pred.data=rfdat,x.var="El_m_sqt",which.class=grp,main=paste("Group ",grp))});
@@@@@@@ rf.mod doesnt exist
@@@@@@@ El_m_sqt doesnt exist---what value should be entered?  Below doesnt work

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
for (i in seq_along(imp.names[c(1:8)])) {
  partialPlot(rf.250ct_7grps, rfdat, which.class = "7", imp.names[i], xlab=imp.names[i],
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
      grps.final<-grps.2;
# 4.3) Specify the final predictor variables ;
      preds.final=c('daynum', 'long')
      
      #preds.final=c('LONG','MSST_mean08091314','ElevCat','Tmax8110Ws','Precip8110Ws','length_46006','OmWs','DOY','WsAreaSqKm')
      #preds.final<-interp.pred
      #preds.final<-c('prdzdy','LONG','RunoffWs','Precip8110Ws','MSST_mean08091314','Tmax8110Ws','Tmin8110Ws')
    #preds.final<-c('Tmax8110Ws','Precip8110Ws','MSST_mean08091314','LONG')
#      preds.final<-tryit$"Names of included predictors in each RF iteration"[[1]];  # And this is the full model! 
     # preds.final = names(predall)[5:41]; #Use this if you didn't run variable reduction at all and you want to save the full model
# 4.4) The final RF model (rfmod);
#bugcal.pa=read.csv("C:/Users/jenni/OneDrive/Documents/ordinations/bugcal.pa.csv")

bugcal.pa = bugcal.pa_200
predcal = rfdat[,c(1:2)]
# Save the model components together in a single .Rdata file.;
    # Any R user can load this file, along with model.predict.RF.r, to make predictions from the model;
#detach("package:NAMCr", unload = TRUE)
     save(bugcal.pa, predcal, grps.final, preds.final, rf.250ct_2grps, file=('bugs analyses/RIVPACS_2022/MWCF_rf.mod_05preds.05otu_200.Rdata')); # Notice I'm using the most parsimonious model (with 8 predictors)
    #save(bugcal.pa.nonrare, predcal, grps.final, preds.final, rf.mod, file=('My.RF.Model.Version1.fullmodel.Rdata')); # full model
    #NOTE - Predcal is not needed to define the model, but is included so that users see the 
    #       required format for predictor data;

      
#-------------------------------------------------------------------#
#Step 5 - calculate OE scores for reference sites for chosen model;
#-------------------------------------------------------------------#

load("bugs analyses/RIVPACS_2022/MWCF_rf.mod_05preds.05otu_200.Rdata")
     

#Option 5.1 - Make predictions of E and O/E for calibration (reference) sites. Examine O/E statistics and plots;
  # To do this, run the model.predict.RanFor.4.2 function, using the calibration data as the 'new' data;
  # See Step 7 below, for more info on making predictions;
  # Also see internal documentation of model.predict.Ran.For.4.2;
source("bugs analyses/RIVPACS_2022/model.predict.RanFor.4.2.r");
     
     
# CHRISTIAN: probably worth checking to make sure all these files have the same number of records...
dim(bugcal.pa)[1]
length(grps.final)
str(preds.final) # Just check that you have the correct predictors here (not the number of records)
dim(predcal)
summary(row.names(bugcal.pa)==row.names(predcal))



# SLH 11.14.22: Trip sent me code for 'model.predict.RanFor', code below originally read 'model.predict.RanFor.4.2'

OE.assess.cal<-model.predict.RanFor.4.2(bugcal.pa,grps.final,preds.final, ranfor.mod=rf.200ct_2grps,prednew=predcal,bugnew=bugcal.pa,Pc=0.5, Cal.OOB=TRUE); 
rep.sam.sd(occprb.cal=OE.assess.cal$Capture.Probs,Pc=0.5);

    


            OE.assess.cal<-model.predict.RanFor(bugcal.pa,grps.final,preds.final, ranfor.mod=rf.200ct_8grps,prednew=predcal,bugnew=bugcal.pa,Pc=0.5); # , Cal.OOB=TRUE
            rep.sam.sd(occprb.cal=OE.assess.cal$Capture.Probs,Pc=0.5);
            



str(OE.assess.cal)
RESULTS=cbind(OE.assess.cal$OE.scores,predcal,grps.final)


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
rep.sam.sd(occprb.cal=OE.assess.cal$Capture.Probs,Pc=0.5);



#Other parts of the results that may be used for further analysis
head(OE.assess.cal$Capture.Probs); #predicted capture probabilties, 1st 5 rows;
head(OE.assess.cal$Group.Occurrence.Probs); #predicted group occurrence probabilities, 1st 5 rows;

#-----------------------------------------------------------------------------------------------#
#Step 6 - Make predictions for degraded sites and compare scores between reference and degraded;
#-----------------------------------------------------------------------------------------------#
prednew=read.csv("C:/Users/jenni/Box/Nonperennial streams/Bugdata/rerun of all models_for pub/metrics_predictors.csv")
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


