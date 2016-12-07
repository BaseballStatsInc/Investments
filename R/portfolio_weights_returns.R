#' Portfolio Weights - 2 risky assets and 1 risk-free asset
#'
#' This function calculates portfolio weights for two risky assets and a risk free asset
#' @param var_x variance of x
#' @param var_y variance of y
#' @param cov_x_y covariance of x and y
#' @param rf risk-free rate
#' @param return_x mean return of x
#' @param return_y mean return of y
#' @param weight_x weight on x if one risky and one risk free
#' @param gamma risk aversion coefficient
#' @keywords mean
#' @keywords variance
#' @keywords tangency
#' @keywords risk
#' @export
#' @examples
#' portfolio_weights_returns()

portfolio_weights_returns = function(var_x, var_y, cov_x_y, rf, return_x, 
                             return_y, weight_x, gamma)
{
  # one risky and one risk free asset
  if (!missing(rf) & !missing(weight_x)){
  return_1and1 = rf + weight_x*(return_x - rf)
  var_1and1 = weight_x*var_x
  sharpe = (return_x - rf)/sqrt(var_x)
  one_risky_one_rf = list(return_1and1, var_1and1, sharpe)
  names(one_risky_one_rf) = c("Return", "Variance", "Sharpe Ratio")
  }
  else{
    return_1and1 = "NULL"
    var_1and1 = "NULL"
    sharpe = "NULL"
    one_risky_one_rf = list(return_1and1, var_1and1, sharpe)
    names(one_risky_one_rf) = c("Return", "Variance", "Sharpe Ratio")
  }
  
  # minimum variance of two risky assets
  if (!missing(var_y) & !missing(cov_x_y) & !missing(return_y)){
  min_var_x = (var_y - cov_x_y)/(var_x + var_y - 2*cov_x_y)
  min_var_y = 1 - min_var_x
  min_var_return = min_var_x*return_x + min_var_y*return_y
  min_var_variance = min_var_x^2*var_x + min_var_y*var_y + 
    2*min_var_x*min_var_y*cov_x_y
  minimum_variance = list(min_var_x, min_var_y, min_var_return, min_var_variance)
  names(minimum_variance) = c("Weight on X", "Weight on Y", "Return", "Variance")
  }
  else{
    min_var_x = "NULL"
    min_var_y = "NULL"
    min_var_return = "NULL"
    min_var_variance = "NULL"
    minimum_variance = list(min_var_x, min_var_y, min_var_return, min_var_variance)
    names(minimum_variance) = c("Weight on X", "Weight on Y", "Return", "Variance")
  }
  
  # tangency portfolio
  if (!missing(rf) & !missing(cov_x_y) & !missing(return_y) & !missing(gamma)
      & !missing(var_y)){
  tangency_x = ((return_x-rf)*var_y - (return_y-rf)*cov_x_y)/
    ((return_x-rf)*var_y + (return_y-rf)*var_x - (return_x + return_y - 2*rf)*cov_x_y)
  tangency_y = 1 - tangency_x
  tangency_return = tangency_x*return_x + tangency_y*return_y
  tangency_variance = tangency_x^2*var_x + tangency_y^2*var_y +
    2*tangency_x*tangency_y*cov_x_y
  weight_tangency = (tangency_return - rf)/(gamma*tangency_variance)
  weight_tangency_x = weight_tangency*tangency_x
  weight_tangency_y = weight_tangency*tangency_y
  weight_tangency_rf = 1 - weight_tangency
  sharpe_tangency = (tangency_return-rf)/sqrt(tangency_variance)
  optimal_sd = sqrt(weight_tangency_x^2*var_x + weight_tangency_y*var_y + 
                      2*weight_tangency_x*weight_tangency_y*cov_x_y)
  optimal = rf + sharpe_tangency*optimal_sd
  tangency = list(tangency_x, tangency_y, tangency_return, tangency_variance, 
                  weight_tangency, weight_tangency_x, weight_tangency_y, 
                  weight_tangency_rf, optimal_sd^2, optimal)
  names(tangency) = c("Weight on X", "Weight on Y", "Return", "Variance", 
                     "Optimal weight on tangency", "Optimal weight on X", 
                     "Optimal weight on Y", "Optimal weight on rf", 
                     "Optimal variance", "Optimal Return")
  }
  else{
    tangency_x = "NULL" 
    tangency_y = "NULL"
    tangency_return = "NULL" 
    tangency_variance = "NULL"
    weight_tangency = "NULL"
    weight_tangency_x = "NULL"
    weight_tangency_y = "NULL" 
    weight_tangency_rf = "NULL"
    optimal_sd^2 = "NULL" 
    optimal = "NULL"
    tangency = list(tangency_x, tangency_y, tangency_return, tangency_variance, 
                    weight_tangency, weight_tangency_x, weight_tangency_y, 
                    weight_tangency_rf, optimal_sd^2, optimal)
    names(tangency) = c("Weight on X", "Weight on Y", "Return", "Variance", 
                       "Optimal weight on tangency", "Optimal weight on X", 
                       "Optimal weight on Y", "Optimal weight on rf", 
                       "Optimal variance", "Optimal Return")
  }
  
  output = list(one_risky_one_rf, minimum_variance, tangency)
  names(output) = c("One Risky and One Risk-Free Asset", "Minimum Variance", 
                   "Tangency")
  
  return(output)
}

