#' Kit Fox Resistance Across a Landscape
#' 
#' This function determines kit fox resistance across a landscape given habitat suitability and sensitivity
#' @param r resistance coefficient (default = 100)
#' @param m resistance coefficient 2 (default = 99)
#' @param c kit fox sensitivity to habitat suitability score 
#' @param h habitat suitability score 
#' @return resistance (ohm)
#' 
#function definition
kit_fox_resistance = function(r=100, m=99, c, h) {
  result = r-m*((1-exp(-c*h))/(1-exp(-c)))
  return(result)
}

