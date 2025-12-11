# Define function for plotly scatter plots 

plotlyScatters = function(dataDF, xcat, ycat,labelcat, titlePlot){
  
  ScatterPlot = plot_ly(data = dataDF, 
          x = ~dataDF[[xcat]], 
          y = ~dataDF[[ycat]],
          #mode = "text",
          text = ~dataDF[[labelcat]],
          textposition = 'right',
          marker = list(size = 10,
                        color = 'rgba(25, 181, 254, 1)',
                        line = list(color = 'rgba(31, 58, 147, 1)',
                                    width = 2))) %>%
    layout(title = titlePlot,
           yaxis = list(zeroline = FALSE,
                        title = ycat),
           xaxis = list(zeroline = FALSE,
                        title = xcat),
           margin = list(l = 250, r = 250, b = 75, t = 50, pad = 8))
  
  return(ScatterPlot)
  
  
}



