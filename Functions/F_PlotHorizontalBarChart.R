### Plotting function 

plotHorizontalBarChartsPlotly = function(dataDF, xcat, ycat, titlePlot){
  
  library(plotly)
  library(dplyr)
  
  dataDF = dataDF %>%
    arrange(desc(dataDF[[xcat]])) 
  
  dataDF = dataDF[1:10,]
  
  readyPlot = plot_ly(dataDF, 
                      x = as.factor(dataDF[[xcat]]), 
                      y = dataDF[[ycat]], 
                      type = 'bar', 
                      orientation = 'h',
                      text = dataDF[[xcat]],
                      textposition = 'auto'
  ) %>%
    layout(title = titlePlot,
           xaxis = list(title = ""
           ),
           yaxis = list(title = "",
                        categoryorder = "array",
                        categoryarray  = c(dataDF %>%
                                             arrange(dataDF[[xcat]])%>%
                                             select(1))[[1]])
           ,margin = list(l = 350, r = 100, b = 100, t = 50, pad = 0)
    )
  
  return(readyPlot)
  
}



