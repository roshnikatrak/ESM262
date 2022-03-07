#' Risks from Extreme Temperature Events
#' 
#' This function determines a level of risk associated with extreme temperature events.
#' @param temperature_threshold Thresholds at which temperature is categorized as extreme
#' @param income
#' @param age
#' @param occupation Occupation categorized as primarily outdoor, primarily indoor, or combination
#' @return risk (very low, low, medium, high, very high) 


#function definition
temp_risk = function(temperature_threshold_extreme = 35, temperature_threshold = 30, temperature, occupation_risk_value, age_risk_value, income_risk_value, risk_value) {
  
  # a while loop is useful here
  days = 0
  i=1
  # we use while here because we want to exit our loop any time we get more than 5 days with
  # air temperature greater than the threshold
  
  while ((days < 5) && (i <= length(temperature))) {
    if (temperature[i] > temperature_threshold)
      # we have another day with greater than 10 so accumulate
      numb10 = numb10+1
    else
      # we have to start over
      numb10 = 0
    # remember with while loops we need to increment our counter
    i = i+1
    
  }
  
  # temperature risk scale (0-100) if temperature has been 32 degrees or more for 3 or more consecutive days
  temp_risk_value = case_when(temperature < temperature_threshold ~ 0, #low risk
                     temperature >= temperature_threshold & temperature < temperature_threshold_extreme ~ 50, #medium risk
                     temperature >= temperature_threshold_extreme ~ 100) #high risk
  
  #occupation risk scale (0-10); added risk based on occupation
  occupation_risk_value = case_when(outdoor ~ 10,
                              indoor ~ 0,
                              combination ~ 5)
  
  #age risk scale (0-10); added risk based on age
  age_risk_value = case_when(child ~ 10,
                             adult ~ 0,
                             elder ~ 10)
  
  #income risk scale (0-10); added risk based on income
  income_risk_value = case_when(high_income ~ 0,
                                medium_income ~ 5,
                                low_income ~ 10)
  
  risk_value = temp_risk_value + occupation_risk_value + age_risk_value + income_risk_value
  
  #risk categories (very low, low, medium, high, very high)
  risk = case_when(risk_value < 30 ~ "very low", #low temp risk, high other categories
                   risk_value = 50 ~ "low", #medium temp risk, low other categories
                   risk_value > 50 & risk_value <= 80 ~ "medium", #medium temp risk, medium/high other categories
                   risk_value = 100 ~ "high", #high temp risk, low other categories
                   risk_value > 100 ~ "very high") #high temp risk, medium/high other categories
  
  return(temp_risk_value) 
}
