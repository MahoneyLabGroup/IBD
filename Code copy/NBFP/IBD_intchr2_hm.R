#this function is to make a heatmap of specifically chr 2 from intestine SVM
#the columns MUST match the previous tables 

INT.chr2.heatmap <- function(heatmap.melt){
  
  #used range() for min break value
  #range(heatmap.melt$value) 
  #-5.9713 52.2411 (-6, 53)
  #### min break = -6
  
  #used quantile() for the max value BELOW the extreme values
  #quantile(data, c(0.05, 0.95)) to determine 5% and 95%
  # 5% -4.487582  and 95% is 6.801857 
  #### max break = 6
  
  ##gradientn
  #vals are in between pals, and zero is between white and grey on the left, use quant for between plum and tomato
  #in between gray is zero, whites and grays have to be extremely close, 
  #in between white and tomato is bottom value before plum (use the quantile)
  pals <- c("plum2", "plum2", "tomato2", "white", "white", "white", "gray90","gray90", "white", "white", "deepskyblue2") 
  #zero at -46.153846
  vals <- c(1, 0.975, 0.9749, 0.464, 0.463, 0.462, 0.46153846, 0.460, 0.459, 0.458, 0)
  # 1, 0.985, 0.9849, 0.529, 0.528, 0.527, 0.52631579, 0.525, 0.524, 0.523, 0) #zero at -52.631579
  brks <- c(6, 0, -6)
  
  ggplot(heatmap.melt, 
         aes(variable, Symbol, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradientn(name = "fold \nchange",
                         colours = pals,
                         values = vals,
                         limits = c(-6, 7), 
                         na.value = "plum2",
                         breaks = brks) +
    labs(title = "Chr 2 (int)", 
         x = "Cell Type",
         y = "Chr 2 Genes") +
    theme(axis.text = element_text(size=30), title = element_text(size = 30),
          axis.title= element_text(size=30), 
          legend.text = element_text(size=30),
          legend.title = element_text(size = 30),
          panel.background = element_rect(fill = "white"), 
          axis.ticks.y = element_blank()) +
    coord_fixed(ratio = 1/3)
  
  
  
}

