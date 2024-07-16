#this code will run functions multiple times and return stats on mean and varience of Bioassessment componadnts
#currently only works for OE, but can be easily modifed for the other componants. 



num_runs <- 20

# OE_multirun -----------------------------------------------------------------------------------------------------

oe_multi <- function(){
  
    rand_subsample <- random_subsample(bug_tax_data_filtered)
    
    rand_stats <- rand_subsample |> 
      group_by(Sample) |> 
      summarise(num_taxa = n_distinct(OTU),
                num_indiv = sum(Count))
    
    
    OE_results <- OE_modelrun(df_bugs = bug_tax_data_filtered,
                              df_rand = rand_subsample)
    
    
    OE_scores <- OE_results$OE_Scores |> 
      left_join(rand_stats, by = c('act_id' = 'Sample')) |> 
      dplyr::arrange(act_id)
    
    return(OE_scores)
}


result_list_OE <- purrr::map(1:num_runs, ~ oe_multi())


# View(result_list[[1]])
# View(result_list[[2]])

result_stats_OE <- data.frame(sample = result_list_OE[[1]]$act_id,
                     mean_indivduals = rowMeans(sapply(result_list_OE, function(df) df$num_indiv)),
                     sd_ind = apply(sapply(result_list_OE, function(df) df$num_indiv), 1, sd),
                     mean_num_taxa =  rowMeans(sapply(result_list_OE, function(df) df$num_taxa)),
                     sd_taxa = apply(sapply(result_list_OE, function(df) df$num_taxa), 1, sd),
                     mean_OE = rowMeans(sapply(result_list_OE, function(df) df$OoverE)),
                     sd_OE = apply(sapply(result_list_OE, function(df) df$OoverE), 1, sd),
                     var_OE = apply(sapply(result_list_OE, function(df) df$OoverE), 1, var))



ggplot(result_stats, aes(x=sd_OE)) + geom_histogram()



# MMI multirun ----------------------------------------------------------------------------------------------------


MMI_multi <- function(){
  
  rand_subsample <- random_subsample(bug_tax_data_filtered)
  
  rand_stats <- rand_subsample |> 
    group_by(Sample) |> 
    summarise(num_taxa = n_distinct(OTU),
              num_indiv = sum(Count))
  
  MMI_results <- MMI_run(rand_subsample, sample_info)
  
  MMI_scores <- MMI_results$MMI_result |> 
    left_join(rand_stats, by = c('SAMPLEID' = 'Sample')) |> 
    dplyr::arrange(SAMPLEID)
  
  

  
  return(MMI_scores)
}


result_list_MMI <- purrr::map(1:num_runs, ~ MMI_multi())


result_stats_MMI <- data.frame(sample = result_list_MMI[[1]]$SAMPLEID,
                           mean_indivduals = rowMeans(sapply(result_list_MMI, function(df) df$num_indiv)),
                           sd_ind = apply(sapply(result_list_MMI, function(df) df$num_indiv), 1, sd),
                           mean_num_taxa =  rowMeans(sapply(result_list_MMI, function(df) df$num_taxa)),
                           sd_taxa = apply(sapply(result_list_MMI, function(df) df$num_taxa), 1, sd),
                           mean_MMI = rowMeans(sapply(result_list_MMI, function(df) df$MMI)),
                           sd_MMI = apply(sapply(result_list_MMI, function(df) df$MMI), 1, sd),
                           var_MMI = apply(sapply(result_list_MMI, function(df) df$MMI), 1, var))

ggplot(result_stats_OE, aes(x=sd_OE)) + 
  geom_histogram() +
  labs(title = paste("Histogram of O:E standard deviation over",num_runs, "sampling runs" ))+
  ylab("Number of samples")+
  theme_minimal()


ggplot(result_stats_MMI, aes(x=sd_MMI)) + 
  geom_histogram() +
  labs(title = paste("Histogram of MMI standard deviation over",num_runs, "subsampling runs" ))+
  ylab("Number of samples")+
  theme_minimal()
  
results_together <- result_stats_OE |> 
  select(sample, sd_OE) |> 
  left_join( select(result_stats_MMI, sample, sd_MMI)) |> 
  pivot_longer(cols = c(sd_OE, sd_MMI),
               names_to = 'type',
               values_to = 'st_dev')



ggplot(results_together, aes(x=st_dev, fill = type, color=type)) + 
  geom_histogram(alpha = 0.5,
                 aes(x=st_dev, fill = type, color=type),
                 position='identity') +
  labs(title = paste("Histogram of standard deviation over",num_runs, "subsampling runs" ))+
  ylab("Number of samples")+
  theme_minimal()

