

# Principal Components Analysis (PCA) to examine comparability of CAL and VAL Datasets

# PCA

library(factoextra)
library(tidyverse)

# need CAL and VAL designations added as a column
tss.cal <- tss.cal %>%
  mutate(dataset = 'CAL')

tss.val <- tss.val %>%
  mutate(dataset = 'VAL')

# need to join CAL and VAL datasets together for plotting in PCA
tss.all <- rbind(tss.cal, tss.val)

# limit dframe to numeric variables, select only predictors used in RF_red15 (final reduced ALL SITES model)


tss.preds_all_pca <- tss.all %>%
  select(MLocID, dataset, WDRW_LDWS,PCTCONIF2001WS,CLAYWS,PESTIC1997WS,SANDWS,PCTCROP2001WS,POPDEN2010WS,PRECIP9120WS,PCTAGSLPMID2001WS,PCT_EROD,SW_FLUXWS,PCTIMP2001WS,RDCRSWS,WTDEPWS,ELEVWS)




# remove columns with "zero variance"
which(apply(tss.preds_all_pca, 2, var)==0)  



# remove incomplete cases
tss.preds_all_pca<- tss.preds_all_pca[complete.cases(tss.preds_all_pca), ]


#@@@@@@@@@@@@@@@@@@@@@@
# data transformations
#@@@@@@@@@@@@@@@@@@@@@@								#colnames(site.data_pca)

source("Benchmarks/Water Chemistry/TSS CLass Project/transform.variables[2_asinsqrt].r")
transform.view2(tss.preds_all_pca)

@@@@@@@@@@@@@@@@@ not working, do manually



                                                                                hist(tss.preds_all_pca$WTDEPWS)
                                                                                hist(asin(sqrt(tss.preds_all_pca$WTDEPWS/100)))
                                                                                hist(log(tss.preds_all_pca$WTDEPWS+1))
                                                                                hist(log10((tss.preds_all_pca$WTDEPWS+1)))
                                                                                hist(sqrt(tss.preds_all_pca$PCTCONIF2001WS))
                                                                                hist(log(sqrt(tss.preds_all_pca$WTDEPWS)))
                                                                                hist(asin(sqrt(tss.preds_all_pca$WTDEPWS)))
                                                                                


tss.preds_all_pca_trans <- tss.preds_all_pca %>%
  mutate(WDRW_LDWS = log(WDRW_LDWS+1)) %>%
  mutate(PCTCONIF2001WS = asin(sqrt(PCTCONIF2001WS/100))) %>%
  mutate(CLAYWS = sqrt(CLAYWS)) %>%
  mutate(PESTIC1997WS = log(PESTIC1997WS+1)) %>%
  mutate(SANDWS = log(SANDWS+1)) %>%
  mutate(PCTCROP2001WS = log(PCTCROP2001WS+1)) %>%
  mutate(POPDEN2010WS = log(POPDEN2010WS+1)) %>%
  mutate(PRECIP9120WS = sqrt(PRECIP9120WS)) %>%
  mutate(PCTAGSLPMID2001WS = log(PCTAGSLPMID2001WS+1)) %>%
  mutate(PCT_EROD = asin(sqrt(PCT_EROD/100))) %>%
  mutate(SW_FLUXWS = log(SW_FLUXWS+1)) %>%
  mutate(PCTIMP2001WS = log(PCTIMP2001WS+1)) %>%
  mutate(RDCRSWS = log(RDCRSWS+1)) %>%
  mutate(WTDEPWS = sqrt(WTDEPWS)) %>%
  mutate(ELEVWS = sqrt(ELEVWS))




                                                                                  # reshape the data so that all columns are condensed into a metric column and results in another
                                                                                  
                                                                                  #tss.preds_all_pca_long <- tss.preds_all_pca_trans %>%
                                                                                  #  pivot_longer(!c(MLocID)  , names_to = "metric", values_to = "value")


# run pca 
library(FactoMineR)  
library("factoextra")
library("corrplot")

# center and scale, before PCA
tss.preds_all_pca_s.c <- scale(tss.preds_all_pca_trans[,c(3:17)],center=T,scale=T)

pca.allsites <- PCA(tss.preds_all_pca_s.c, graph = FALSE, scale.unit = TRUE)



summary(pca.allsites)


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
fviz_contrib(pca.allsites, choice = "var", axes = 3, top = 15)
fviz_contrib(pca.allsites, choice = "var", axes = 1:3, top = 15)


# The most important (or, contributing) variables can be highlighted on the correlation plot as follow:
fviz_pca_var(pca.allsites, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)


#############################

# FINAL PCA biplots

############################

library(patchwork)

grp <- as.factor(tss.preds_all_pca_trans$dataset)
# Color variables by groups



p1 <- fviz_pca_biplot(pca.allsites, axes=1:2, labelsize = 7, pointsize = 2,
                      col.ind = grp, # color by groups
                      palette = c("steelblue1",  "red"),#c("#00AFBB",  "#FC4E07")
                      # habillage = site.data_pca$calval,
                      # addEllipses = TRUE, # Concentration ellipses
                      # ellipse.type = "confidence",
                      legend.title = "Dataset",
                      repel = TRUE, #label='var', 
                      geom.ind = c("point"), geom.var = c("arrow", "text"),
                      col.var = 'black',
                      alpha.ind = 1, # transparency of individuals 
                      title = ""
) + theme(legend.position = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 14))


p2 <- fviz_pca_biplot(pca.allsites, axes=c(2,3), labelsize = 7, pointsize = 2,
                      col.ind = grp, # color by groups
                      palette = c("steelblue1",  "red"),#c("#00AFBB",  "#FC4E07")
                      # habillage = site.data_pca$calval,
                      # addEllipses = TRUE, # Concentration ellipses
                      # ellipse.type = "confidence",
                      legend.title = "Dataset",
                      repel = TRUE, #label='var', 
                      geom.ind = c("point"), geom.var = c("arrow", "text"),
                      col.var = 'black',
                      alpha.ind = 1, # transparency of individuals 
                      title=''
)+ theme(legend.position = "none", axis.text = element_text(size = 14), axis.title = element_text(size = 14))

p3 <- fviz_pca_biplot(pca.allsites, axes=c(3,4),
                      col.ind = grp, # color by groups
                      palette = c("steelblue1",  "red"),#c("#00AFBB",  "#FC4E07")
                      # habillage = site.data_pca$calval,
                      # addEllipses = TRUE, # Concentration ellipses
                      # ellipse.type = "confidence",
                      legend.title = "Dataset",
                      repel = TRUE, #label='var', 
                      geom.ind = c("point"), geom.var = c("arrow", "text"),
                      col.var = 'black',
                      alpha.ind = 1, # transparency of individuals 
                      title=''
)


p1 + p2

  
  
  
  
  
  
  
  