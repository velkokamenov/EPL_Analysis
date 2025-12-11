# Define function for plotly scatter plots with polynomial regression fitting

plotlyScatters = function(dataDF, xcat, ycat, labelcat, titlePlot){
  
  # Remove any NA values
  clean_data <- dataDF[!is.na(dataDF[[xcat]]) & !is.na(dataDF[[ycat]]), ]
  
  # Function to fit polynomial models and select the best one
  fit_best_polynomial <- function(x, y, max_degree = 3) {
    n <- length(x)
    # Don't fit polynomials higher than n-2 to avoid overfitting
    max_degree <- min(max_degree, n - 2, 3)
    
    models <- list()
    r_squared_values <- numeric()
    adj_r_squared_values <- numeric()
    
    # Fit polynomial models of different degrees using raw polynomials
    for (degree in 1:max_degree) {
      tryCatch({
        if (degree == 1) {
          model <- lm(y ~ x)
        } else {
          # Create polynomial terms manually for better control
          poly_terms <- sapply(1:degree, function(d) x^d)
          model <- lm(y ~ poly_terms)
        }
        models[[degree]] <- model
        r_squared_values[degree] <- summary(model)$r.squared
        adj_r_squared_values[degree] <- summary(model)$adj.r.squared
      }, error = function(e) {
        # If polynomial fitting fails, use linear
        if (degree == 1) {
          model <- lm(y ~ x)
          models[[1]] <<- model
          r_squared_values[1] <<- summary(model)$r.squared
          adj_r_squared_values[1] <<- summary(model)$adj.r.squared
        }
      })
    }
    
    # Select best model based on adjusted R-squared
    valid_models <- !is.na(adj_r_squared_values)
    if (sum(valid_models) == 0) {
      # Fallback to simple linear
      model <- lm(y ~ x)
      return(list(
        model = model, 
        degree = 1,
        r_squared = summary(model)$r.squared,
        adj_r_squared = summary(model)$adj.r.squared
      ))
    }
    
    best_degree <- which.max(adj_r_squared_values[valid_models])
    
    # Only use polynomial if it improves adjusted R-squared by at least 0.05
    if (best_degree > 1 && adj_r_squared_values[best_degree] - adj_r_squared_values[1] < 0.05) {
      best_degree <- 1
    }
    
    return(list(
      model = models[[best_degree]], 
      degree = best_degree,
      r_squared = r_squared_values[best_degree],
      adj_r_squared = adj_r_squared_values[best_degree]
    ))
  }
  
  # Fit the best polynomial model
  best_fit <- fit_best_polynomial(clean_data[[xcat]], clean_data[[ycat]])
  
  # Create smooth prediction line
  x_range <- range(clean_data[[xcat]], na.rm = TRUE)
  x_seq <- seq(x_range[1], x_range[2], length.out = 100)
  
  # Generate predictions based on degree
  if (best_fit$degree == 1) {
    # Linear prediction
    pred_df <- data.frame(x = x_seq)
    y_pred <- predict(best_fit$model, newdata = pred_df)
    model_type <- "Linear"
  } else {
    # Polynomial prediction - manually calculate using coefficients
    coeffs <- coef(best_fit$model)
    if (length(coeffs) >= 2) {
      # Calculate polynomial values
      y_pred <- coeffs[1] # intercept
      for (i in 2:length(coeffs)) {
        y_pred <- y_pred + coeffs[i] * x_seq^(i-1)
      }
      model_type <- paste0("Polynomial (degree ", best_fit$degree, ")")
    } else {
      # Fallback to linear if coefficients are weird
      simple_model <- lm(clean_data[[ycat]] ~ clean_data[[xcat]])
      pred_df <- data.frame(x = x_seq)
      names(pred_df)[1] <- xcat
      y_pred <- predict(simple_model, newdata = pred_df)
      model_type <- "Linear (fallback)"
      best_fit$r_squared <- summary(simple_model)$r.squared
      best_fit$adj_r_squared <- summary(simple_model)$adj.r.squared
    }
  }
  
  # Create the scatter plot
  ScatterPlot = plot_ly(data = clean_data, 
          x = ~clean_data[[xcat]], 
          y = ~clean_data[[ycat]],
          text = ~clean_data[[labelcat]],
          textposition = 'right',
          type = 'scatter',
          mode = 'markers',
          name = 'Teams',
          marker = list(size = 10,
                        color = 'rgba(25, 181, 254, 1)',
                        line = list(color = 'rgba(31, 58, 147, 1)',
                                    width = 2))) %>%
    # Add the best-fit curve
    add_lines(x = x_seq, 
              y = y_pred, 
              name = paste0(model_type, ' (R² = ', round(best_fit$r_squared, 3), ')'),
              line = list(color = 'rgba(255, 0, 0, 0.8)', width = 3),
              inherit = FALSE) %>%
    layout(title = paste0(titlePlot, '<br><sub>', model_type, ' Fit - R²: ', round(best_fit$r_squared, 3), ', Adj. R²: ', round(best_fit$adj_r_squared, 3), '</sub>'),
           yaxis = list(zeroline = FALSE,
                        title = ycat),
           xaxis = list(zeroline = FALSE,
                        title = xcat),
           margin = list(l = 250, r = 250, b = 75, t = 75, pad = 8),
           showlegend = TRUE,
           legend = list(x = 0.02, y = 0.98))
  
  return(ScatterPlot)
}



