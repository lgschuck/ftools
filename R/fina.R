
#' First n value taking NA's into account
#'
#' If all values are NA returns NA
#'
#' @param x Objeto com valores
#' @param n Position to return, default is 1 (first). If zero or less it will be corrected to 1
#' @param na_rm If TRUE or T will remove NA's. Default value is T.
#' @examples
#' fina(c(5, 1, 2, 3, NA)) # returns 5
#' fina(c(5, 1, 2, 3, NA), n = 2) # returns 1
#' fina(c(NA, 1, 2, 3, NA), na_rm = T) # returns 1
#' fina(c(NA, 1, 2, 3, NA), na_rm = F) # returns NA
#' fina(c(NA, NA, NA), na_rm = T) # returns NA
#' fina(c(NA, NA, NA), na_rm = F) # returns NA
#'
#' @export
#'
fina <- function(x, n = 1L, na_rm = T){

  if(all(is.na(x))){
    NA
  } else {
    n <- max(as.integer(n), 1)
    if(isTRUE(na_rm)) x <- x[!is.na(x)]
    x[n]
  }
}
