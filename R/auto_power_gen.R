#' Automobile Power Generation
#' 
#' This function determines automobile power generation
#' @param m vehicle mass (kg)
#' @param V vehicle speed (assuming no headwind) in m/s (or mps)
#' @param A surface area of car (m^2)
#' @param g acceleration due to gravity (9.8m/s^2)
#' @param p_air density of air (1.2kg/m^3)
#' @param crolling rolling coefficient
#' @param cdrag aerodynamic resistive coefficient
#' @return power (Watts)

#function definition
auto_power_gen = function(crolling=0.015, m, g=9.8, V, A, p_air=1.2, cdrag=0.3) {
  result = crolling * m * g * V + 0.5 * A * p_air * cdrag * V^3
  return(result)
}