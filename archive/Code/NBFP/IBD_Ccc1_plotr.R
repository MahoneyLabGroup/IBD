#this function plots mean gene position and -log10(FPR) for Ccc1 locus ONLY
#column names are specific!! (sorry that's kind of lazy)

IBD_Ccc1_plotr <- function(Ccc1.final.table) {
  
  #melt
  Ccc1.final.table.melt <- melt(Ccc1.final.table, 
                                id.vars = c("Symbol", "mean.gene.position", "logpv1"), 
                                measure.vars = c("int.logfp1", "hemo.logfp1"))
  
  #top10 int
  top10.int <- Ccc1.final.table %>% slice_max(int.logfp1, n = 10)
  #top10 hemo
  top10.hemo <- Ccc1.final.table %>% slice_max(hemo.logfp1, n = 10)
  #logical array for labels
  subset.top10.int.labels <- Ccc1.final.table.melt$Symbol %in% top10.int$Symbol
  subset.top10.hemo.labels <- Ccc1.final.table.melt$Symbol %in% top10.hemo$Symbol
  
  
  #plot
  ggplot(Ccc1.final.table.melt,
          aes(x = mean.gene.position,
              y = value,
              color = factor(variable),
              size = logpv1,
              shape = variable,
              label = Symbol)) +
    geom_point() +
    geom_text(aes(x = mean.gene.position,
                  y = value, 
                  label = Symbol), 
              subset(Ccc1.final.table.melt, subset = subset.top10.int.labels), hjust = "bottom", angle = 15, size = 8) +
    geom_text(aes(x = mean.gene.position,
                  y = value, 
                  label = Symbol), 
              subset(Ccc1.final.table.melt, subset = subset.top10.hemo.labels), hjust = "bottom", angle = 15, size = 8) +
    labs(title = "Ccc1",
         x = "Mean Gene Position on Chr 12",
         y = "Normalized -log10(SVM FP Rate)") +
    theme(axis.title = element_text(size = 25), 
          axis.text = element_text(size = 15), 
          title = element_text(size = 25), 
          legend.title = element_text(size = 15), 
          legend.text = element_text(size = 15)) + 
    scale_color_manual(name = "Network", values = c("seagreen", "turquoise3"), labels = c("Hemolymphoid", "Intestinal")) +
    scale_size(name = "p value") +
    guides(shape = FALSE)
}
