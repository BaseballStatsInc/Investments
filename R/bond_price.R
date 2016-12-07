#' Price of different kinds of Bonds
#'
#' This function calculates bond prices
#' @param C coupon
#' @param r rate
#' @param n number of years
#' @param Fv face value
#' @keywords bond
#' @keywords coupon
#' @keywords consol
#' @keywords discount
#' @export
#' @examples
#' bond_price()

bond_price = function(C, r, n, Fv)
{
  sum = 0
  for (t in 1:n)
  {
    term = C/(1+r)^t
    sum = sum + term
  }
  last_term = Fv/(1+r)^n
  coupon_bond = sum + last_term
  
  consol_bond = C/r
  discount_bond = Fv/(1+r)^n
  
  out = c(coupon_bond, consol_bond, discount_bond)
  names(out) = c("Coupon", "Consol", "Discount")
  return(out)
}

