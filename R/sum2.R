#' Function for sum removing NAs
#' @export sum2
#'
#' @examples
#' sum2(c(1, 2, 3, NA))
sum2 <- function(x) {
  sum(x, na.rm = T)
}
