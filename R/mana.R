
#' Maximum value taking NA's into account
#'
#' @param x Object with values
#' @param na_rm If TRUE or T removes NA's. Default value is T.
#' @examples
#' mana(c(-1, 2, 3, NA))
#' mana(c(1, 2, 3, NA), na_rm = F)
#' mana(c(1, 2, 3, NA, NaN), na_rm = F)
#'
#' @export
#'
mana <- function(x, na_rm = T){
  if(all(is.na(x))) NA else max(x, na.rm = na_rm)
}
