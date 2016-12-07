#' Real Return Function
#'
#' This function calculates real returns given nominal
#' @param nom_rate nominal rate
#' @param inf_rate inflation rate
#' @keywords nominal inflation
#' @export
#' @examples
#' nom2real()

nom2real = function(nom_rate, inf_rate)
{
  real_return = (1+nom_rate)/(1+inf_rate) - 1
  approx_real_return = nom_rate - inf_rate
  
  out = c(real_return, approx_real_return)
  names(out) = c("Real Return", "Approx Real Return")
  
  return(out)
}

