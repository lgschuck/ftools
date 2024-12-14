
#' Minimum value taking NA's into account
#'
#' @param x Object with values
#' @param na_rm If TRUE or T removes NA's. Default value is T.
#' @examples
#' mina(c(-1, 2, 3, NA))
#' mina(c(1, 2, 3, NA), na_rm = FALSE)
#' mina(c(1, 2, 3, NA, NaN), na_rm = FALSE)
#'
#' @export
#'
mina <- function(x, na_rm = TRUE){
  if(all(is.na(x))) NA else min(x, na.rm = na_rm)
}
