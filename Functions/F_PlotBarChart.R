### Plotting function 

plotBarChartsPlotly = function(dataDF, xcat, ycat, titlePlot){
  
  library(plotly)
  library(dplyr)
  
  readyPlot = plot_ly(dataDF, x = dataDF[[xcat]], y = dataDF[[ycat]], type = 'bar') %>%
    layout(title = titlePlot,
           xaxis = list(title = "",
                        tickangle = 45,
                        categoryorder = "array",
                        categoryarray  = c(dataDF %>%
                                             arrange(desc(dataDF[[ycat]]))%>%
                                             select(1))[[1]]
                        ),
           yaxis = list(title = ""),
           margin = list(l = 200, r = 200, b = 100, t = 50, pad = 8))
  
  return(readyPlot)
  
}





