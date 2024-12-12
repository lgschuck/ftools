
#' Format number with abreviation and separators
#'
#' Return is character class
#'
#' @param x Object with values
#' @param big Thousand separator
#' @param dec Decimal separator
#' @param thousand Abreviation for thousands
#' @param million Abreviation for millions
#' @param billion Abreviation for billions
#' @param dig Digits after decimal mark
#'
#' @examples
#' f_num(12345678956, billion = 'G')
#' f_num(512347896, million = 'Mi')
#' f_num(9995198, thousand = 'm', dig = 3, dec = ',', big = '.')
#' f_num(55566312345678956, billion = 'G')
#' f_num(Inf)
#' f_num(-Inf)
#'
#' @export
#' @importFrom data.table fcase

f_num <- function(x, big = ',', dec = '.', thousand = 'K',
                  million = 'M', billion = 'B', dig = 0){

  fcase(
    is.infinite(x), paste(x),
    x > 1e9,
    paste(format(round(x/1e9, digits = dig),
                 decimal.mark = dec, big.mark = big, scientific = F), billion),
    x > 1e6,
    paste(format(round(x/1e6, digits = dig),
                 decimal.mark = dec, big.mark = big, scientific = F), million),
    x > 1e3,
    paste(format(round(x/1e3, digits = dig),
                 decimal.mark = dec, big.mark = big, scientific = F), thousand),
    default = format(round(x, dig), scientific = F)
    )
}
