#' Risks from Extreme Temperature Events
#' 
#' This function determines a level of risk associated with extreme temperature events.
#' @param temperature_threshold Thresholds at which temperature is categorized as extreme
#' @param income
#' @param age
#' @param occupation Occupation categorized as primarily outdoor, primarily indoor, or combination
#' @return risk (very low, low, medium, high, very high) 

#function definition
temp_risk = function(occupation, income) {
  
  #occupation risk scale (0-10); added risk based on occupation
  occupation_risk_value = case_when(occupation == "outdoor" ~ 10,
                                    occupation == "indoor" ~ 0,
                                    occupation == "combination" ~ 5,
                                    occupation == "none" ~ 0)
  #TRUE ~ as.numeric(occupation))
  
  #income risk scale (0-10); added risk based on income
  income_risk_value = case_when(income == "high_income" ~ 0,
                                income == "medium_income" ~ 5,
                                income == "low_income" ~ 10)
  #TRUE ~ as.numeric(income))
  
  risk_value = occupation_risk_value + income_risk_value
  
  #risk categories (very low, low, medium, high, very high)
  risk = case_when(risk_value < 30 ~ "very low", #low temp risk, high other categories
                   risk_value >= 30 & risk_value <= 50 ~ "low", #medium temp risk, low other categories
                   risk_value > 50 & risk_value <= 80 ~ "medium", #medium temp risk, medium/high other categories
                   risk_value >=80 & risk_value <= 100 ~ "high", #high temp risk, low other categories
                   risk_value > 100 ~ "very high") #high temp risk, medium/high other categories
                   #TRUE ~ as.character(risk_value))
  
  return(risk=risk) 
}

