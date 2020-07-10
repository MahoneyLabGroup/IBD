#this function is to make a heatmap of specifically chr 1 from intestine SVM
#the columns MUST match the previous tables 

INT.chr1.heatmap <- function(heatmap.melt){
  
  #used range() for min break value
    #range(heatmap.melt$value) 
    #-266.05600    3.75085
  #### min break = 4
  
  #used quantile() for the max value BELOW the extreme values
    #quantile(data, c(0.05, 0.95)) to determine 5% and 95%
    # -9.453514  2.401371 
  #### max break = -9
  
  #eg
  # #find where to put the breaks - this is just to visualize and map what number to put for which break value
  # BreaksChrlist <- as.data.frame(seq(4, -7.1))
  # BreaksChrlist$values <- seq(-100, 0, length.out = 12) 
  
  ##gradientn
  #in between gray is zero, whites and grays have to be extremely close, 
  #in between white and blue is bottom value before purple (use the quantile)
  pals <- c("tomato2", "white", "white", "gray90", "gray90", "white", "white", "deepskyblue2", "deepskyblue2", "purple", "purple")
  vals <- c(1, 0.717, 0.716, 0.715, 0.71428571, 0.713, 0.712, 0.05, 0.0499, 0.0498, 0) 
  # zero at -71.428571
  brks <- c(4, 0, -9)
  
  ggplot(heatmap.melt, 
         aes(variable, Symbol, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradientn(name = "fold \nchange",
                         colours = pals,
                         values = vals,
                         limits = c(-10, 4), 
                         na.value = "purple",
                         breaks = brks) +
    labs(title = "Chr 1 (int)", 
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

