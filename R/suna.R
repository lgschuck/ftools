
#' Sum taking NA's into account
#'
#' @param x Object to be summed
#' @param na_rm If TRUE or T removes NA's. Default value is T.
#' @examples
#' suna(c(1, 2, 3, NA))
#' suna(c(1, 2, 3, NA), na_rm = FALSE)
#'
#' @export
#'
suna <- function(x, na_rm = TRUE){
  sum(x, na.rm = na_rm)
}
