
#' Last n value taking NA's into account
#'
#' If all values are NA returns NA
#'
#' @param x Object with values
#' @param n Position to return, default is 1 (last). If zero or less it will be corrected to 1.
#' @param na_rm If TRUE or T removes the NA's. Default value is T.
#'
#' @examples
#' lana(c(5, 1, 2, 3, NA))
#' lana(c(NA, 1, 2, 3, NA), na_rm = F)
#' lana(c(5, 1, 2, 3, NA), n = 3)
#' lana(c(5, 1, 2, 3, NA), n = 3)
#' lana(c(NA, NA, NA), na_rm = T)
#' lana(c(NA, NA, NA), na_rm = F)
#'
#' @export
#'
lana <- function(x, n = 1, na_rm = T){

  if(all(is.na(x))){
    NA
  } else {
    n <- max(as.integer(n), 1)
    if(isTRUE(na_rm)) x <- x[!is.na(x)]
    x[length(x) - n + 1]
  }
}
