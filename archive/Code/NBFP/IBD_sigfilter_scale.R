#this function does a significant p-value filter and scales to 1
#the column of log10(FPR) needs to be named "log_fp"
#NOTE: it renames the column names to be able to pass through dplyr functions 

IBD.sigfilter.scale <- function(final.table = NULL, 
                                pval.col = "pv", 
                                logpv.col = "log_p") {
  
  final.table.renamed <- final.table %>% 
    rename(pv = {{ pval.col }},
           log_p = {{ logpv.col }}) %>%  
    filter(pv < 0.05) %>% 
    mutate(log_p1 = log_p/max(log_p),
           log_fp1 = log_fp/max(log_fp))

  
  return(final.table.renamed)
}
