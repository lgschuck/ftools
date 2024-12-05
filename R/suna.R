
#' Sum taking NA's into account
#'
#' @param x Object to be summed
#' @param na_rm If TRUE or T removes NA's. Default value is T.
#' @examples
#' suna(c(1, 2, 3, NA)) # returns 6
#' suna(c(1, 2, 3, NA), sem_na = F) # returns NA
#'
#' @export
#'
suna <- function(x, na_rm = T){
  sum(x, na.rm = na_rm)
}
