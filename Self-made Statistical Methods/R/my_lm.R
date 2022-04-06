#' Linear Model Function
#' 
#' A function that is used to fit linear models. Can be used to carry out 
#' regression, single stratum analysis of variance, and anaylsis of covariance
#' 
#' @param formula a formula class object, similar to lm()
#' @param data input data frame. 
#' 
#' @keywords inference prediction
#' 
#' @return Returns a table similar to the coefficient table from summary() 
#'   with rows for each coefficient and columns for the Estimate, Std. Error, 
#'   t value, and p value
#' 
#' @importFrom stats model.frame model.matrix model.response terms
#' 
#' @examples 
#' data("mtcars")
#' my_lm(formula = mpg ~ hp + wt, data = mtcars)
#' 
#' @export

my_lm <- function(formula, data) {
  # Extract a model matrix X
  X <- model.matrix(formula, data)
  # Extract model frame
  model_frame <- model.frame(formula, data)
  # Extract model response Y
  Y <- model.response(model_frame)
  df <- length(Y) - ncol(X)
  # Solve for coefficients and standard error
  solve_val <- solve(t(X) %*% X)
  coefficient <- solve_val %*% t(X) %*% Y
  estimation <- sum(((Y - (X %*% coefficient))^2) / df)
  standard_error <- diag(sqrt(estimation * abs(solve_val)))
  # Calculate the test statistic and p value for each estimation
  t_value <- (coefficient - 0) / standard_error
  p_value <- 2 * pt(abs(t_value), df, lower.tail = FALSE)
  
  # Create a result data frame
  result <- data.frame("Estimate" = coefficient,
                       "Std. Error" = standard_error,
                       "t value" = t_value,
                       "p value" = p_value)
  return(result)
}