#this function plots -log10(DE p-value) and -log10(FPR)

IBD_gene_plotr <- function(SIG.final.table, 
                           pref, 
                           top10, 
                           data.genename.col = "Symbol",
                           pref.genename.col = "Symbol",
                           top10.genename.col = "Symbol",
                           x.pv.col = "APC.log_p1", 
                           y.fpr.col = "log_fp1", 
                           title = "title") {
  
  data_renamed <- SIG.final.table %>% 
    rename(symbol = {{ data.genename.col }}, 
           log_p1 = {{ x.pv.col }}, 
           log_fp1 = {{ y.fpr.col }})
  
  pref_renamed <- pref %>% 
    rename(symbol = {{ pref.genename.col }},
           log_p1 = {{ x.pv.col }}, 
           log_fp1 = {{ y.fpr.col }})
  
  top10_renamed <- top10 %>% 
    rename(symbol = {{ top10.genename.col }},
           log_p1 = {{ x.pv.col }}, 
           log_fp1 = {{ y.fpr.col }})
  
  #logical array for pareto front labels:
  subset.pref.labels <- data_renamed$symbol %in% pref_renamed$symbol
  #list for top10 genes MINUS intersecting pref genes (to make sure there are not duplicate overlaps for label)
  top10.minuspref <- top10_renamed$symbol[!(top10_renamed$symbol %in% pref_renamed$symbol)]
  #logical array for top10 labels (minus pref genes): 
  subset.top10.labels <- data_renamed$symbol %in% top10.minuspref
  
  
  ggplot(data = data_renamed,
         aes(x = log_p1, y = log_fp1, label = symbol)) +
    geom_point(shape = 21, color = "gray20") +
    geom_step(data = pref_renamed, direction = "vh", color = "lightblue", size = 1.5) +
    geom_point(data = pref_renamed, shape = 20, size = 10, color = "orange") +
    geom_point(data = top10_renamed, shape = 20, size = 5.5, color = "tomato3") +
    geom_text(aes(),
              subset(data_renamed, 
                     subset = subset.pref.labels), hjust = -0.2, vjust = -0.5, angle = 15, size = 5) +
    geom_text(aes(),
              subset(data_renamed, 
                     subset = subset.top10.labels), hjust = -0.2, vjust = -0.5, angle = 15, size = 5) +
    labs(title = title,
         y = "-log10 SVM FP Rate", 
         x = "-log10 Differential Gene Expression p-value") +
    coord_equal() +
    theme(axis.title = element_text(size = 30), axis.text = element_text(size = 15)) 
}
