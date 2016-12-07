#' Return Means (Arithmetic and Geometric)
#'
#' This function calculates return means
#' @param t number of periods
#' @param r vector of returns
#' @keywords returns
#' @keywords geometric
#' @keywords arithmetic
#' @keywords mean
#' @export
#' @examples
#' mean_returns()

mean_returns = function(t, r)
{
  sum = 0
  for (i in 1:t)
  {
    term = r[i]
    sum = sum + term
  }
  arithmetic_mean = sum/t
  
  prod = 1
  for (i in 1:t)
  {
    term = 1+r[i]
    prod = prod*term
  }
  geometric_mean = prod^(1/t) - 1
  
  out = c(arithmetic_mean, geometric_mean)
  names(out) = c("Arithmetic", "Geometric")
  return(out)
}

