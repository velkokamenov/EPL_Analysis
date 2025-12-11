# Define function for ggplo2 horizontal bar charts

plotHorizontalBarChartsGgplot = function(StatsDF, ycol, xcol, graphTitle){
  
  library(ggplot2)
  
  colYPos = which(colnames(StatsDF)==ycol)
  colXPos = which(colnames(StatsDF)==xcol)
  
  plotdf = StatsDF %>%
    select(colYPos, colXPos)
  
  colnames(plotdf)[1:2] <- c("PlayerName", "VarName")
  
  plotdf = plotdf %>%
    arrange(desc(VarName)) %>%
    top_n(10)
  
  for (i in 1:nrow(plotdf)){
    if (i == 1){
      
      plotdf[1,3] = 1
      
    } else{
      
      if (plotdf[i,2] == plotdf[i-1,2]){
        
        plotdf[i,3] = plotdf[i-1,3]
        
      } else{
        
        plotdf[i,3] = plotdf[i-1,3] + 1
        
      }
    }
  }
  
  # Name the third column
  colnames(plotdf)[3] <- "V3"
  
  plotdfpositions = plotdf %>%
    mutate(PlayerName = paste0(V3,". ",PlayerName)) %>%
    select(PlayerName, VarName)
  
  
  ggplot(data=plotdfpositions, aes(x=reorder(PlayerName, VarName),y=VarName)) +
    geom_bar(stat="identity",fill = "royalblue3") + 
    geom_text(aes(label=VarName), vjust=0,hjust=-0, size = 3.5) +
    coord_flip() +
    ggtitle(graphTitle)+ 
    theme(plot.margin = margin(1.25,0.5,1.25,0.5, "cm"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(size=18),
          axis.ticks = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_text(size=10)
    )
  
}
