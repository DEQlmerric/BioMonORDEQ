#this code will run functions multiple times and return stats on mean and varience of Bioassessment componadnts
#currently only works for OE, but can be easily modifed for the other componants. 



num_runs <- 10

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


result_list <- purrr::map(1:num_runs, ~ oe_multi())


View(result_list[[1]])
View(result_list[[2]])

result_stats <- data.frame(sample = result_list[[1]]$act_id,
                     mean_indivduals = rowMeans(sapply(result_list, function(df) df$num_indiv)),
                     sd_ind = apply(sapply(result_list, function(df) df$num_indiv), 1, sd),
                     mean_num_taxa =  rowMeans(sapply(result_list, function(df) df$num_taxa)),
                     sd_taxa = apply(sapply(result_list, function(df) df$num_taxa), 1, sd),
                     mean_OE = rowMeans(sapply(result_list, function(df) df$OoverE)),
                     sd_OE = apply(sapply(result_list, function(df) df$OoverE), 1, sd),
                     var_OE = apply(sapply(result_list, function(df) df$OoverE), 1, var))
