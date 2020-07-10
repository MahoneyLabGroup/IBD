#this function is to make a heatmap of specifically chr 2 from hemolymphoid SVM
#the columns MUST match the previous tables 

HEMO.chr2.heatmap <- function(heatmap.melt){
  
  #used range() for min break value
  #range(heatmap.melt$value) 
  #-9.35005 52.24110 
  #### min break = -10
  
  #used quantile() for the max value BELOW the extreme values
  #quantile(data, c(0.05, 0.95)) to determine 5% and 95%
  # 5% -5.68544 and 95% is 8.668773
  #### max break = 8
  
  #eg
  # #find where to put the breaks - this is just to visualize and map what number to put for which break value
  # BreaksChrlist <- as.data.frame(seq(4, -7.1))
  # BreaksChrlist$values <- seq(-100, 0, length.out = 12) 
  
  ##gradientn
  #in between gray is zero, whites and grays have to be extremely close, 
  #in between white and tomato is bottom value before plum (use the quantile)
  # pals2 <- c("plum2", "plum2", "tomato2", "white", "white", "gray90", "white", "white", "deepskyblue2") #zero at -46.153846
  # vals2 <- c(1, 0.985, 0.9849, 0.528, 0.52631579, 0.52631579, 0.526315789, 0.525, 0.524, 0) #zero at -52.631579
  # brks2 <- c(8, 0, -10)
  pals <- c("plum2", "tomato2", "white",  "gray90",  "white", "deepskyblue2") 
  #zero at -46.153846
  vals <- c(0.98590002, 0.98580001, 0.526315791, 0.52631579, 0.52631578, 0.00000000) 
  #zero at -52.631579
  brks <- c(8, 0, -10)
  
  ggplot(heatmap.melt, 
         aes(variable, Symbol, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradientn(name = "fold \nchange",
                         colours = pals,
                         values = vals,
                         limits = c(-10, 9), 
                         na.value = "plum2",
                         breaks = brks) +
    labs(title = "Chr 2 (hemo)", 
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

