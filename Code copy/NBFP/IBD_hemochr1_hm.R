#this function is to make a heatmap of specifically chr 1 from hemolymphoid SVM
#the columns MUST match the previous tables 

HEMO.chr1.heatmap <- function(heatmap.melt){
  
  #used range() for min break value
  #range(heatmap.melt$value) 
  #-266.05600    2.83787
  #### min break = 3
  
  #used quantile() for the max value BELOW the extreme values
  #quantile(data, c(0.05, 0.95)) to determine 5% and 95%
  # 5% -13.422010 and 95% is 2.114978
  #### max break = -14
  
  #eg
  # #find where to put the breaks - this is just to visualize and map what number to put for which break value
  # BreaksChrlist <- as.data.frame(seq(4, -7.1))
  # BreaksChrlist$values <- seq(-100, 0, length.out = 12) 
  
  ##gradientn
  #in between gray is zero, whites and grays have to be extremely close, 
  #in between white and blue is bottom value before purple (use the quantile)
  pals <- c("tomato2", "white", "white", "gray90", "gray90", "white", "white", "deepskyblue2", "deepskyblue2", "purple", "purple")
  vals <- c(1, 0.826, 0.825, 0.824, 0.82352941, 0.823, 0.822, 0.05, 0.0499, 0.0498, 0)
  # zero at -82.352941
  brks <- c(3, 0, -13)
  
  ggplot(heatmap.melt, 
         aes(variable, Symbol, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradientn(name = "fold \nchange",
                         colours = pals,
                         values = vals,
                         limits = c(-14, 3), 
                         na.value = "purple",
                         breaks = brks) +
    labs(title = "Chr 1 (hemo)", 
         x = "Cell Type",
         y = "Chr 1 Genes") +
    theme(axis.text = element_text(size=30), title = element_text(size = 30),
          axis.title= element_text(size=30), 
          legend.text = element_text(size=30),
          legend.title = element_text(size = 30),
          panel.background = element_rect(fill = "white"), 
          axis.ticks.y = element_blank()) +
    coord_fixed(ratio = 1/3)
  
  
  
}

