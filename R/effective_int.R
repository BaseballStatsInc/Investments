#' Effective Interest Rate
#'
#' This function calculates effective interest rates 
#' @param int_rate interest rate
#' @param m times compunding per year, "inf" if continuous
#' @keywords effective interest
#' @export
#' @examples
#' effective_int()

effective_int = function(int_rate, m)
{
  if (m == "inf")
  {
    eir = exp(int_rate) - 1
  }
  else
  {
    eir = (1+int_rate/m)^m - 1
  }
  return(eir)
}

