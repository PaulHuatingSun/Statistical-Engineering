#' T-Test Function
#' 
#' A function that performs one and two sample t-tests on vectors of data
#' 
#' @param x a numeric vector of data.
#' @param alternative a character string specifying the alternative hypothesis. 
#'   This should only accept "two.sided", "less", or "greater". 
#' @param mu a number indicating the null hypothesis value of the mean.
#' 
#' @keywords inference
#' 
#' @return List with elements,
#' - \code{test_stat}: the numeric test statistic,
#' - \code{df}: the degrees of freedom, 
#' - \code{alternative}: the value of the parameter,
#' - \code{p_val}: the numeric p-value.
#' 
#' @importFrom stats pt sd
#' 
#' @examples
#' test = c(1:25)
#' my_t.test(test, alternative = "less", mu = 10)
#' my_t.test(test, alternative = "two.sided", mu = 10)
#' 
#' @export

my_t.test <- function(x, alternative, mu) {
  # Calculate degree of freedom and sample size 
  n <- length(x)
  df <- n - 1
  # Compute test statistic value
  test_stat <- (mean(x) - mu) / (sd(x) / sqrt(n))
  # Based on the alternative method, perform different t tests
  if (alternative == "two.sided") {
    p_val <- 2 * pt(abs(test_stat), df, lower.tail = FALSE)
  } else if (alternative == "greater") {
    p_val <- pt(test_stat, df, lower.tail = FALSE)
  } else if (alternative == "less") {
    p_val <- pt(test_stat, df, lower.tail = TRUE)
    # if the alternative method is invalid, print error message
  } else {
    stop('Choose an alternative method: "two.sided", "less", or "greater"')
  }
  # Return a list of items
  return(list(test_stat = test_stat, df = df, alternative = alternative, p_val = p_val))
}