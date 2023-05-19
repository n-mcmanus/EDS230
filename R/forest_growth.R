#' Forest growth model
#'
#' This differential function computes forest growth in relation to a canopy closure threshold 
#' @authors Nick McManus 
#' @param time time since start
#' @param params list of four parameter values (r, g, K, closure)
#' @param C size of forest (measured in kg of carbon)
#' @param K carrying capacity
#' @param r exponential growth rate (before canopy closure)
#' @param g linear growth rate (after canopy closure
#' @return forest size over time

forest_growth = function(time, C, params) {
  ## Rate of growth depending on canopy closure threshold
  dC = ifelse(C < params$closure, 
              params$r * C, 
              params$g * (1-(C/params$K)))
  
  ## Rate of growth also depending on carrying capacity
  # dC = ifelse(C >= params$K, 0, dC)
 
  return(list(dC)) 
}