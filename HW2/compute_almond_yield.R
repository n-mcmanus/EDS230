#' Almond yield anamoly model
#'
#' This function computes maximum, minimum, and mean yield anamoly for almonds
#' given a time series of minimum, maximum daily temperatures and precipitation
#' 
#' @param min_temp minimum temperature for february (degrees Celsius)
#' @param precip precipitation during the month of january (mm)
#' @return yield anamoly  (ton acre ^-1)
#' 

almond_yield = function(min_temp, precip) {
  yield = -0.015*(min_temp) - 0.0046*(min_temp^2) - 0.07*(precip) + 0.0043*(precip^2) + 0.28
  return(yield)
}
