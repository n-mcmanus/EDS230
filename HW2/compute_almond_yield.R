#' Almond yield anomaly model
#'
#' This function computes maximum, minimum, and mean yield anomaly for almonds
#' given a time series of minimum, maximum daily temperatures and precipitation
#' 
#' @param min_temp minimum temperature for February (degrees Celsius)
#' @param precip precipitation during the month of January (mm)
#' @return yield anomaly  (tons/acre)


almond_yield = function(min_temp, precip) {
  
  ### Calculate the yield anomalies based on provided formula
  yield = (-0.015*(min_temp) - 0.0046*(min_temp^2) - 0.07*(precip) + 0.0043*(precip^2) + 0.28)
  
  ### Find max, min, mean yield anomalies
  max_yield = max(yield)
  min_yield = min(yield)
  mean_yield = mean(yield)
  
  ### Return values as list
  yield_stats <- as.list(data.frame(max_yield, min_yield, mean_yield))
  return(yield_stats)
}
