#' Function for sum removing NAs
#'
#' @param x Object to sum
#'
#' @export sum2
#'
#' @examples
#' sum2(c(1, 2, 3, NA))
sum2 <- function(x) {
  sum(x, na.rm = T)
}
