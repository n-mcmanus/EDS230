#' Almond yield anomaly model
#'
#' This function computes maximum, minimum, and mean yield anomaly for almonds
#' given a time series of minimum, maximum daily temperatures and precipitation
#' 
#' @param min_temp minimum temperature for February (degrees Celsius)
#' @param precip precipitation during the month of January (mm)
#' @param tcoeff1 min Feb temp coefficient 1 (default = -0.015)
#' @param tcoeff2 min Feb temp coefficient 2 (default = -0.0046)
#' @param pcoeff1 summed Jan precip coefficient 1 (default = -0.07)
#' @param pcoeff2 summed Jan precip coefficient 2 (default = 0.0043)
#' @return list of maximum, minimum, and average yield anomalies  (tons/acre)


almond_yield = function(min_temp, precip, tcoeff1 = -0.015, tcoeff2 = -0.0046, pcoeff1 = -0.07, pcoeff2 = 0.0043) {
  
  ### Error checking:
  ##### STOP fxn if negative precip values given
  ifelse (precip < 0, stop("Negative precipitation value(s) supplied"), NA)
  ##### WARN user if precip value of 0 given
  ifelse (precip == 0, warning("Precipitation value(s) of 0 supplied"), NA)
  #### WARN user if min temp values are 
  ifelse (min_temp >=10, warning("Minimum temperature value(s) over 10 degrees C"), NA)
  
  
  ### Calculate the yield anomalies based on provided formula
  yield = ((tcoeff1)*(min_temp) + (tcoeff2)*(min_temp^2) + (pcoeff1)*(precip) + (pcoeff2)*(precip^2) + 0.28)
  
  
  ### Find max, min, mean yield anomalies
  max_yield = max(yield)
  min_yield = min(yield)
  mean_yield = mean(yield)
  
  
  ### Return values as list
  yield_stats <- as.list(data.frame(max_yield, min_yield, mean_yield))
  return(yield_stats)
}
