#' Present Value Calculator
#'
#' This function calculates present values 
#' @param future_value future value at time 0
#' @param int_rate interest rate
#' @param m times compunding per year, "inf" if continuous
#' @param n number of years
#' @keywords present value
#' @export
#' @examples
#' present_val()

present_val = function(future_val, int_rate, m, n)
{
  if (m == "inf")
  {
    PV = future_val*exp(-n*int_rate)
  }
  else
  {
    PV = future_val/(1 + int_rate/months_comp_year)^(months_comp_year*n)
  }
  return(PV)
}

