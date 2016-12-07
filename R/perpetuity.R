#' Present Value of a Perpetuity
#'
#' This function calculates perpetuities 
#' @param pmt payment
#' @param int_rate interest rate
#' @param growth growth rate
#' @keywords perpetuity
#' @export
#' @examples
#' perpetuity()

perpetuity = function(pmt, int_rate, growth)
{
  PV = pmt/(int_rate - growth)
  return(PV)
}

