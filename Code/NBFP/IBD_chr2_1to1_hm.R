#this function is to make the chr2 1:1 heatmap
#the columns MUST match the previous tables 

chr2.1to1.heatmap <- function(heatmap.melt){
  
  #used range() for min break value
  #range(heatmap.melt$value) 
  #-6.62971  2.67240
  #### min break = -7
  
  #used quantile() for the max value BELOW the extreme values
  #quantile(data, c(0.05, 0.95)) to determine 5% and 95%
  #-6.005778  and 95% is 1.671511 
  #quantile(data), there are no extremes so just use 100%
  #### max break = 3
  
  # #find where to put the breaks - this is just to visualize and map what number to put for which break value
  # BreaksChrlist <- as.data.frame(seq(4, -7.1))
  # BreaksChrlist$values <- seq(-100, 0, length.out = 12) 
  
  pals <- c("tomato3", "white", "gray85", "white", "deepskyblue2")
  vals <- c(1, 0.71, 0.70, 0.69, 0)
  
  ggplot(heatmap.melt, 
         aes(variable, Symbol, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradientn(name = "fold \nchange",
                         colours = pals,
                         values = vals,
                         limits = c(-7, 3), 
                         na.value = "gray90",
                         breaks = c(-7, 0, 3)) +
    labs(title = "1:1 Chr 2", 
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

