#' Future Value Calculator
#'
#' This function calculates future values 
#' @param present_value present value at time 0
#' @param int_rate interest rate
#' @param m times compounding per year, "inf" if continuous
#' @param n number of years
#' @keywords future value
#' @export
#' @examples
#' future_val()

future_val = function(present_val, int_rate, m, n)
{
  if (m == "inf")
  {
    FV = present_val*exp(n*int_rate)
  }
  else
  {
    FV = present_val*(1 + int_rate/months_comp_year)^(months_comp_year*n)
  }
  return(FV)
}

