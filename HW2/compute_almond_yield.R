#' Almond yield anomaly model
#'
#' This function computes maximum, minimum, and mean yield anomaly for almonds
#' given a time series of minimum, maximum daily temperatures and precipitation
#' 
#' @param min_temp minimum temperature for february (degrees Celsius)
#' @param precip precipitation during the month of january (mm)
#' @return yield anomaly  (ton acre ^-1)
#' 




almond_yield = function(min_temp, precip) {
  yield = -0.015*(min_temp) - 0.0046*(min_temp^2) - 0.07*(precip) + 0.0043*(precip^2) + 0.28
  
  max_yield = max(yield)
  min_yield = min(yield)
  mean_yield = mean(yield)
    
  x <- list(max_yield, min_yield, mean_yield)
  
  return(x)
}
