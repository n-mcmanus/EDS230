#' Almond yield anomaly and profit model
#'
#' This function computes the mean annual almond yield anomaly and profit across 
#' an entire time series of monthly precipitation and minimum temperatures. 
#' 
#' @authors Nick McManus and Katheryn Moya
#' @param tmin minimum temperature for February (degrees Celsius)
#' @param precip summed precipitation during the month of January (mm)
#' @param tcoeff1 min Feb temp coefficient 1 (default = -0.015)
#' @param tcoeff2 min Feb temp coefficient 2 (default = -0.0046)
#' @param pcoeff1 summed Jan precip coefficient 1 (default = -0.07)
#' @param pcoeff2 summed Jan precip coefficient 2 (default = 0.0043)
#' @param price_ton price of almonds in 2021 (default = 3520 USD/ton/acre)
#' @param base_profit baseline profit for average almond yield (default = 3168 USD/ton/acre). Assumption that average yield of 0.9 ton/acre
#' @return almond yield anomaly (tons/acre) and profit per year (USD/tons/acre)

almond_yield_profit = function(tmin, precip, tcoeff1 = -0.015, tcoeff2 = -0.0046, pcoeff1 = -0.07, pcoeff2 = 0.0043, base_profit = 3168, price_ton = 3520) {


      ### First calculate the almond yield anomaly
      almond_yield = function(tmin, precip, tcoeff1, tcoeff2, pcoeff1, pcoeff2) {
            ## Error checking:
            #### STOP fxn if negative precip values given
            ifelse (precip < 0, stop("Negative precipitation value(s) supplied"), NA)
            #### WARN user if precip value of 0 given
            ifelse (precip == 0, warning("Precipitation value(s) of 0 supplied"), NA)
            #### WARN user if min Feb temp value is over 10C (50F)
            ifelse (tmin >=10, warning("Minimum temperature value(s) over 10 degrees C"), NA)
        
        ### Calculate the yield anomalies based on provided formula
        yield_anomaly <<- ((tcoeff1)*(tmin) + (tcoeff2)*(tmin^2) + (pcoeff1)*(precip) + (pcoeff2)*(precip^2) + 0.28)
     
        return(yield_anomaly)
      }
    
### Calculate the yield anomaly for a given year and store as variable
yield_anomaly = almond_yield(tmin, precip, tcoeff1, tcoeff2, pcoeff1, pcoeff2)

### Find the mean yield
mean_yield = mean(yield_anomaly)

  
      ### Use yield anomaly to then calculate profit
      almond_profit = function(yield_anomaly, base_profit = 3168, price_ton = 3520) {
        profit <<- (base_profit) + (yield_anomaly*price_ton)
        return(profit)
      }
      

### Calculate the profit using the yield anomaly for given year
profit = almond_profit(yield_anomaly, base_profit, price_ton)
mean_profit = mean(profit)
  

## Return list of of yield and profit for each year    
yield_stats <- as.list(data.frame(mean_yield, mean_profit))
    
return(yield_stats)

}