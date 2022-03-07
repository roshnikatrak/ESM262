#' Return most common item, rarest item, and total number of items
#' 
#' This function determines kit fox resistance across a landscape given habitat suitability and sensitivity
#' @param r resistance coefficient (default = 100)
#' @param m resistance coefficient 2 (default = 99)
#' @param c kit fox sensitivity to habitat suitability score. Must be greater than or equal to 0.
#' @param h habitat suitability score. Must be greater than or equal to 0 and less than or equal to 1.
#' @return resistance (ohm)
#' 
#function definition
fish = function(item) {
  #return most common item
  most_common = names(which.max(summary(as.factor(item))))

  #return rarest item
  rare = names(which.min(summary(as.factor(item))))
  
  #return total number of items
  total = summary(item)
    
  return(list(most_common=most_common, rare=rare, total=total))
}

