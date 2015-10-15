#' Calculates standard errors 
#'
#' This function calculates the standard error of a vector of values 
#' @param x Vector of values for which the standard error is calculated
#' @keywords standard error 
#' @export
#' @examples
#' test_data <- c(2.3, 1.9, 3.2, NA, 1.7, 2.0, 2.6)
#' standard_error <- se(test_data)

se <- function(x) {
  sd(x, na.rm=TRUE) / sqrt(sum(!is.na(x)))
}
