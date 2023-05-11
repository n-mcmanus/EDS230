
#' @param m modeled data
#' @param o observed data
#' @param wy water year
#' @return the RMSE between max flows per water year
combo_rmse <- function(m, o, wy) {
  ## bind data together
  data <- cbind(m, o, wy)
  data_df <- as.data.frame(data)
  
  ## find the max flows
  data_max <- data_df %>% 
    group_by(wy) %>% 
    summarize(max_m = max(m),
              max_o = max(o)) %>% 
    mutate(sq_diff = (max_o - max_m)^2)
  rmse_max <- sqrt(mean(data_max$sq_diff))
  
  ## find min flows and rmse
  data_min <- data_df %>% 
    group_by(wy) %>% 
    summarize(min_m = min(m),
              min_o = min(o)) %>% 
    mutate(sq_diff = (min_o - min_m)^2)
  
  rmse_min <- sqrt(mean(data_min$sq_diff))
  
  ## combine metrics
  norm_max = 1 - min((rmse_max/mean(data_max$max_o)), 1)
  norm_min = 1 - min((rmse_min/mean(data_min$min_o)), 1)
  
  combo_rmse = 0.5*norm_max + 0.5*norm_min
  return(combo_rmse)
  
}