#this function is to prepare the table for the heatmap by arranging genes by their total score and melting the table
#the columns MUST match the previous tables 

IBD_order_melt <- function(heatmap.table){
  
  #turn insignificant p values to zero in fold change columns (it is easier to grey out zeros in ggplot. 
  #NOTE: this was for visualization purposes ONLY, 
  #and this is the ONLY time this is done, 
  #and in no way were any values changed to a different value that changed their meaning)
  heatmap.table <- heatmap.table %>% 
    mutate(APCfc = replace(APCfc, which(APCpv > 0.05), 0),
         BCELLfc = replace(BCELLfc, which(BCELLpv > 0.05), 0),
         CD4fc   = replace(CD4fc, which(CD4pv > 0.05), 0), 
         CD8fc   = replace(CD8fc, which(CD8pv > 0.05), 0), 
         TREGfc  = replace(TREGfc, which(TREGpv > 0.05), 0)) %>% 
    rename(APC = APCfc, 
           BCELL = BCELLfc, 
           CD4 = CD4fc, 
           CD8 = CD8fc,
           TREG = TREGfc) %>% 
    mutate(max = pmax(APC.total.score, BCELL.total.score, CD4.total.score, CD8.total.score, TREG.total.score)) %>% 
    arrange(max) %>% 
    select(Symbol, APC, BCELL, CD4, CD8, TREG) 
  
  #melt
  heatmap.table.melt <- reshape2::melt(heatmap.table, id.vars = "Symbol", measure.vars = c("APC", "BCELL", "CD4", "CD8", "TREG"))
  heatmap.table.melt$Symbol <- factor(heatmap.table.melt$Symbol, levels = heatmap.table$Symbol)
  return(heatmap.table.melt)
}

