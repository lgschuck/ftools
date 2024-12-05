
#' Minimum value taking NA's into account
#'
#' @param x Object with values
#' @param na_rm If TRUE or T removes NA's. Default value is T.
#' @examples
#' mina(c(-1, 2, 3, NA)) # returns -1
#' mina(c(1, 2, 3, NA), na_rm = F) # returns NA
#' mina(c(1, 2, 3, NA, NaN), na_rm = F) # returns NA
#'
#' @export
#'
mina <- function(x, na_rm = T){
  if(all(is.na(x))) NA else min(x, na.rm = na_rm)
}
