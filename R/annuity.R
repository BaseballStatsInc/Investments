#' Present and Future Values of an Annuity
#'
#' This function calculates annuities 
#' @param pmt payment
#' @param int_rate interest rate
#' @param growth growth rate
#' @param n number of years
#' @keywords annuity
#' @export
#' @examples
#' annuity()

annuity = function(pmt, int_rate, growth, n)
{
  PV = (pmt/(int_rate - growth)) * (1 - ((1+growth)/(1+int_rate))^n)
  FV = pmt * ((1+int_rate)^n - (1+growth)^n)/(int_rate - growth)
  out = c(PV,FV)
  names(out) = c("Present Value", "Future Value")
  return(out)
}

