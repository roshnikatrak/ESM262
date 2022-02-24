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
  # make sure that habitat suitability inputs are between 0 and 1 
  h = ifelse(h >= 0, ifelse(h <=1, h, NA), NA)
  #h = ifelse((h < 0 & h > 1), NA, h) 
  #if (h > 1) return ("Habitat suitability score cannot be greater than 1")
  #if (h < 0) return("Habitat suitability score cannot be less than 0")
  # make sure that sensitivity inputs are greater than 0 
  c = ifelse((c < 0), return ("Sensitivity must be greater than 0"), c)
  result = r-m*((1-exp(-c*h))/(1-exp(-c)))
  return(result)
}

