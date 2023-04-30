#' Almond profit model
#'
#' This function computes the profit of almond production based on the yield anomaly. 
#' @param price_ton price of almonds in 2021 (default = 3520 USD/ton/acre)
#' @param base_profit baseline profit for average almond yield (default = 3168 USD/ton/acre). Assumption that average yield of 0.9 ton/acre
#' @param yield_anomaly the yield anomaly per year based on almond yield model (ton/acre).
#' @return gross profit per year  (USD/tons/acre)


almond_profit = function(yield_anomaly, base_profit = 3168, price_ton = 3520) {
  profit = (base_profit) + (yield_anomaly*price_ton)
  
  return(profit)
}