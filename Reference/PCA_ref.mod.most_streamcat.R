# SLH: 8.14.24

# objective: finalize the PCA analysis with all sites (including CA) 
#             and StreamCat predictors


library(tidyverse)
################################################

-STEP 1: run the 'FrontEnd_v2.R', lines 47:88
          - generate 'sample_info'




##################################################
-STEP 2: associate StreamCat with sample_info


library(StreamCatTools)

#####

#   Query StreamCat database

#####

region_params <- sc_get_params(param='areaOfInterest')
name_params <- sc_get_params(param='name')
name_params <- sort(name_params)  


# Get data for the three Hydroregions covering: 17= OR+WA+ID, 16 = NV, 18 = CA

# Watershed + Other (stream temp mets)
metrics_WS.OTHER_OR.CA.NV <- sc_get_data(metric='al2o3,bfi,cao,clay,compstrgth,elev,fe2o3,hydrlcond,
                       inorgnwetdep_2008,k2o,kffact,mast_2008,mast_2009,mast_2013,mast_2014,
                       msst_2008,msst_2009,msst_2013,msst_2014,mwst_2008,mwst_2009,mwst_2013,mwst_2014,  
                       mgo,n,na2o,nh4_2008,no3_2008,om,p2o5,pctalkintruvol,pctalluvcoast,pctbl2001,
                       pctbl2004,pctcarbresid,pctcoastcrs,pctcolluvsed,pcteolcrs,pcteolfine,pctextruvol,
                       pctglaclakecrs,pctglaclakefine,pctglactilclay,pctglactilcrs,pctglactilloam,
                       pcthydric,pctice2001,pctice2004,pctice2006,pctice2008,pctice2011,
                       pctice2013,pctice2016,pctice2019,pctsallake,perm,
                       precip8110,rckdep,s,sand,sio2,tmax8110,tmean8110,tmin8110', 
                       aoi='watershed, other', region='16,17,18')

# convert multiple years to means
metrics_WS.OTHER_OR.CA.NV <- metrics_WS.OTHER_OR.CA.NV %>%
  #mutate(MAST_mean08.14 = (MAST_2008+MAST_2009+MAST_2013+MAST_2014)/4) %>%
 # mutate(MSST_mean08.14 = (MSST_2008+MSST_2009+MSST_2013+MSST_2014)/4) %>% 
  #mutate(MWST_mean08.14 = (MWST_2008+MWST_2009+MWST_2013+MWST_2014)/4) %>%
  mutate(PCTICE_mean01.19 = (PCTICE2001WS+PCTICE2004WS+PCTICE2006WS+PCTICE2008WS+PCTICE2011WS+
                               PCTICE2013WS+PCTICE2016WS+PCTICE2019WS)/8) %>%
  select(#-MAST_2008, -MAST_2009,-MAST_2013,-MAST_2014,-MSST_2008,-MSST_2009,
         #-MSST_2013,-MSST_2014,-MWST_2008,-MWST_2009,-MWST_2013,-MWST_2014, 
         -PCTICE2001WS,-PCTICE2004WS,-PCTICE2006WS,-PCTICE2008WS,-PCTICE2011WS,
         -PCTICE2013WS,-PCTICE2016WS,-PCTICE2019WS)




###################################################
#-STEP 3: associate streamcat to comids


# get unique stations
sta.comid <- sample_info %>%
  filter(Wade_Boat == 'wadeable') %>%
  select(MLocID, COMID, EcoRegion3, ReferenceSite) %>%
  distinct(MLocID, .keep_all = TRUE)

sta.comid2 <- na.omit(sta.comid)



sta.comid_kitty <- sta.comid %>%
  left_join(metrics_WS.OTHER_OR.CA.NV, by = "COMID") %>%
  select(-REGIONID)


###################################################
#-STEP 4: run PCA



# remove columns with "zero variance"


sta.comid_kitty[,c(5:47)] %>% 
  summarise_all(var) %>% 
  select_if(function(.) . == 0) %>% 
  names()


# missing data blows up the PCA -- remove rows with missing data
sta.comid_kitty_complete <- sta.comid_kitty[complete.cases(sta.comid_kitty), ]


# data transformations for PCA: normal distributions an assumption for linear relationships
source("//deqlab1/Biomon/R Stats/chris parker scripts/transform.variables[1]_SH divide by 100.r") #

transform.view(sta.comid_kitty_complete)  # [,c(5:47)]

@@@@ this isnt working




library(DataExplorer)


plot_missing(sta.comid_kitty_complete)
plot_histogram(sta.comid_kitty_complete)
#drop the following: low variability/range of values
sta.comid_kitty_complete2 <- sta.comid_kitty_complete %>%
  select(-c(PCTALKINTRUVOLWS, PCTALLUVCOASTWS, PCTCARBRESIDWS, PCTEOLFINEWS, PCTEOLCRSWS, PCTEXTRUVOLWS,
            PCTHYDRICWS, PCTGLACLAKECRSWS, PCTGLACLAKEFINEWS, PCTGLACTILCRSWS, PCTSALLAKEWS, PCTBL2004WS, 
            PCTCOLLUVSEDWS, PCTBL2001WS, PCTICE_mean01.19, PCTGLACTILCLAYWS, PCTGLACTILLOAMWS,
            PCTCOASTCRSWS))


# get ReferenceSite levels ready for plotting in proper order and color
sta.comid_kitty_complete2$ReferenceSite <- as.factor(sta.comid_kitty_complete2$ReferenceSite)

sta.comid_kitty_complete2$ReferenceSite <- factor(sta.comid_kitty_complete2$ReferenceSite, 
                                          levels = c("REFERENCE", "MODERATELY DISTURBED", "MOST DISTURBED"))



                      #windows()
                      plot_qq(sta.comid_kitty_complete2)
                      
                             # no transformation needed:
                            SIO2WS 
                            TMEAN8110WS 
                            K2OWS 
                            COMPSTRGTHWS 
                            CLAYWS 
                            TMAX8110WS 
                            KFFACTWS
                            BFI 
                      
                      # look at log plots to see which improve
                      log_qq_data <- update_columns(sta.comid_kitty_complete2, 5:29, function(x) log10(x + 1))
                      plot_qq(log_qq_data[, 5:29])
                      
                            # variables improved by Log + 1
                            
                            SANDWS
                            WSAREAKM
                            TMAX8110WS
                            WSAREASQKM
                            PERMWS
                            ELEVWS
                      
                      # look at sqrt plots to see which improve
                      sqrt_qq_data <- update_columns(sta.comid_kitty_complete2, 5:29, function(x) sqrt(x))
                      plot_qq(sqrt_qq_data[, 5:29])
                      
                      
                            # variables improved by sqrt
                          
                            OMWS
                      
                      # look at asin.sqrt plots
                      asin.sqrt_qq_data <- update_columns(sta.comid_kitty_complete2, 5:29, function(x) asin(sqrt(x/100)))
                      plot_qq(asin.sqrt_qq_data[, 5:29])
                      
                            # variables improved by asin.sqrt
                            #RCKDEPWS ---- no! too many NaNs produced



log.10p1 <- function(x, na.rm = FALSE) (log10(x +1)) #, na.rm = na.rm))
log.10 <- function(x, na.rm = FALSE) (log10(x))      #, na.rm = na.rm))
asin.sqrt.100 <- function(x, na.rm = FALSE) (asin(sqrt(x/100))) #, na.rm = na.rm)))
sqroot <- function(x, na.rm = FALSE) (sqrt(x))         #, na.rm = na.rm))

#trans_sta.kitty <- sta.comid_kitty_complete2 %>% 
 # mutate_at(c('RCKDEPWS'), asin.sqrt.100)  


trans_sta.kitty <- sta.comid_kitty_complete2 %>% 
  mutate_at(c('OMWS'), sqroot)  %>% 
  mutate_at(c('SANDWS', 'WSAREASQKM','TMAX8110WS', 'PERMWS', 'ELEVWS'), log.10p1)  # %>% 
#   mutate_at(c('SWs', 'NWs','HydrlCondWs'), log.10)  



                                                                                    # 
                                                                                    # 
                                                                                    # # create a new df for assessing PCA with transformed variables
                                                                                    # ref.cat_trans <- ref.cat_complete
                                                                                    # 
                                                                                    # # remove variables with little info  
                                                                                    # ref.cat_trans <- ref.cat_trans %>%   
                                                                                    #     select (-c(PctCarbResidWs, PctGlacTilClayWs,	PctGlacTilLoamWs,	PctGlacTilCrsWs, PctGlacLakeCrsWs,
                                                                                    #              PctGlacLakeFineWs,	PctHydricWs,	PctEolCrsWs,	PctEolFineWs,	PctSalLakeWs,	PctAlluvCoastWs,
                                                                                    #              PctCoastCrsWs,	PctWaterWs,	AgKffactWs))
                                                                                    # 
                                                                                    # 
                                                                                    # ref.cat_complete <- ref.cat_complete %>%   
                                                                                    #   select (-c(PctCarbResidWs, PctGlacTilClayWs,	PctGlacTilLoamWs,	PctGlacTilCrsWs, PctGlacLakeCrsWs,
                                                                                    #              PctGlacLakeFineWs,	PctHydricWs,	PctEolCrsWs,	PctEolFineWs,	PctSalLakeWs,	PctAlluvCoastWs,
                                                                                    #              PctCoastCrsWs,	PctWaterWs,	AgKffactWs))
                                                                                    # 
                                                                                    #   
                                                                                    # # data transformations
                                                                                    # ref.cat_trans$PctNonCarbResidWs <- asin(sqrt(ref.cat_trans$PctNonCarbResidWs/100))
                                                                                    # 
                                                                                    # 


  
# run PCA
library(FactoMineR)  
library("factoextra")
library("corrplot")
pca.allsites <- PCA(trans_sta.kitty[,c(5:29)], graph = FALSE)

summary(pca.allsites)
str(pca.allsites)
print(pca.allsites)
eig.val <- get_eigenvalue(pca.allsites) # Extract the eigenvalues/variances of principal components
fviz_eig(pca.allsites, addlabels = TRUE, ylim = c(0, 50) ) # Visualize the eigenvalues


# Extract the results for individuals and variables, respectively.
ind <- get_pca_ind(pca.allsites); 
var <- get_pca_var(pca.allsites) 

# Visualize the results individuals and variables, respectively.
fviz_pca_ind(pca.allsites)
fviz_pca_var(pca.allsites, col.var = "blue") 

fviz_pca_biplot(pca.allsites) # Make a biplot of individuals and variables.


corrplot(var$cos2, is.corr=FALSE)  # ???????

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(pca.allsites, choice = "var", axes = 1:2)


# Color by cos2 values: quality on the factor map
fviz_pca_var(pca.allsites, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)


fviz_pca_var(pca.allsites, alpha.var = "cos2") # Change the transparency by cos2 values

corrplot(var$contrib, is.corr=FALSE)  

fviz_contrib(pca.allsites, choice = "var", axes = 1, top = 15)
fviz_contrib(pca.allsites, choice = "var", axes = 2, top = 15)
fviz_contrib(pca.allsites, choice = "var", axes = 1:2, top = 15)

# The most important (or, contributing) variables can be highlighted on the correlation plot as follow:
fviz_pca_var(pca.allsites, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

            






# Color by groups


# Eco3
fviz_pca_ind(pca.allsites,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = trans_sta.kitty$EcoRegion3, # color by groups
             palette = 'lancet', #c('violet', 'blue', 'green', 'gray', 'orange', 'red', 'pink', 'black', 'forest green'),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)

# Ref 2020
fviz_pca_ind(pca.allsites,
             geom.ind = 'point', # show points only (nbut not "text")
             col.ind = trans_sta.kitty$ReferenceSite, # color by groups
             palette = c('blue', 'forest green', 'red'),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = 'Ref Class 2016')






# scientific journal palettes from ggsci R package, e.g.: "npg", "aaas", "lancet", "jco", "ucscgb", "uchicago", "simpsons" and "rickandmorty".


# Change the size of arrows an labels
fviz_pca_var(pca.allsites, arrowsize = 1, labelsize = 5, 
             repel = TRUE)



fviz_pca_biplot(pca.allsites, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2,
                fill.ind = sta.comid_kitty_complete2$EcoRegion3,
                col.ind = "black",
                # Color variable by groups
                #col.var = factor(c("sepal", "sepal", "petal", "petal")),
                
                legend.title = list(fill = "Eco3", color = "Clusters"),
                repel = TRUE        # Avoid label overplotting
)+
  ggpubr::fill_palette("lancet")+      # Indiviual fill color
  ggpubr::color_palette("npg")      # Variable colors     # Variable colors





fviz_pca_ind(pca.allsites, 
             # Fill individuals by groups
             geom.ind = "point",
             pointshape = 21,
             pointsize = 2,
             fill.ind = sta.comid_kitty_complete2$EcoRegion3, 
             addEllipses = TRUE,
             #col.ind = "black",
             # Color variable by groups
             #col.var = factor(c("sepal", "sepal", "petal", "petal")),
             
             legend.title = list(fill = "Eco3"), #, color = "Clusters"),
             repel = TRUE        # Avoid label overplotting
)+
  ggpubr::fill_palette("lancet") #+      # Indiviual fill color
# ggpubr::color_palette("npg")      # Variable colors     # Variable colors


fviz_pca_ind(pca.allsites,
             geom.ind = "point", # show points only (but not "text")
             pointshape = 21,
             pointsize = sta.comid_kitty_complete2$ReferenceSite,
             fill.ind = sta.comid_kitty_complete2$EcoRegion3, # color by groups
             palette = c('purple', 'blue', 'green', 'orange', 'red', 'gray', 'pink', 'black', 'forest green'), 
             addEllipses = FALSE, # Concentration ellipses
             legend.title = "Groups"
) 



fviz_pca_biplot(pca.allsites,
                geom.ind = "point", # show points only (nbut not "text")
                pointshape = 21,
                pointsize = sta.comid_kitty_complete2$ReferenceSite,
                fill.ind = sta.comid_kitty_complete2$EcoRegion3, # color by groups
                palette = c('purple', 'blue', 'green', 'orange', 'red', 'gray', 'black','yellow',  'forest green'), 
                addEllipses = FALSE, # Concentration ellipses
                select.var = list(cos2  = 0.5),  # opnly include variables with cos2 > 0.5
                legend.title = "Groups"
) 


###################################################

# summary stats table

library(vtable)

sta.kitty_sum <- sta.comid_kitty_complete2[,c(4:29)]


st(sta.kitty_sum, group = 'ReferenceSite')




########### 

# box plots


# wsarea has big diffs between most disturbed and others -- rescale

sta.kitty_sum.scale <- sta.kitty_sum %>%
  mutate(WSAREASQKM.log = log10(WSAREASQKM)) %>%
  select(!WSAREASQKM)

sta.kitty_long <- sta.kitty_sum.scale %>%
  pivot_longer(!ReferenceSite, names_to = "metric", values_to = "value") 

                sta.kitty_long <- as.data.frame(sta.kitty_long)
                
                sta.kitty_long <- sta.kitty_long %>%
                  recode(ReferenceSite, REFERENCE = 'REF')

sta.kitty_long$ReferenceSite <- as.character(sta.kitty_long$ReferenceSite)                
                
sta.kitty_long[sta.kitty_long == 'REFERENCE'] <- 'REF'
sta.kitty_long[sta.kitty_long == 'MODERATELY DISTURBED'] <- 'MOD'
sta.kitty_long[sta.kitty_long == 'MOST DISTURBED'] <- 'MOST'


sta.kitty_long$ReferenceSite <- as.factor(sta.kitty_long$ReferenceSite)                
sta.kitty_long$ReferenceSite <- factor(sta.kitty_long$ReferenceSite, 
                                          levels = c("REF", "MOD", "MOST"))



ggplot(data = sta.kitty_long, 
       mapping = aes(x = ReferenceSite, y = value)) +
           geom_jitter(alpha = 0.05, color = "blue") +
           geom_boxplot(alpha = 0) +  # Do not show outliers
         
          theme_bw()+
    facet_wrap(facets = vars(metric), scales = "free")






