#' Kit Fox Resistance Across a Landscape
#' 
#' This function determines automobile power generation
#' @param r resistance coefficient (100)
#' @param l resistance coefficient 2 (99)
#' @param c kit fox sensitivity to habitat suitability score
#' @param h habitat suitability score

#function definition
kit_fox_resistance = function(r=100, l=99, c, h) {
  result = r - l * ((1 - exp(-c * h)) / (1 - exp(-c)))
  return(result)
}

