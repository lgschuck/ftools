
#' Fill leading zeros
#'
#' Fill leading zeros until informed length. The default value is the largest value in x.
#'
#' @param x Object with values
#' @param len Desired length
#' @examples
#' zfill('a', 3)
#' zfill(1:3, 3)
#' zfill(c(1, 12, 123))
#'
#' mtcars |>
#'   mutate(hp_fill = zfill(hp, 6))
#'
#' # Mind cientific notation
#'
#' options(scipen = 0)
#' 1e6 |> zfill(largura = 9) # return "00001e+06"
#'
#' options(scipen = 999)
#' 1e6 |> zfill(largura = 9) # return "001000000"
#'
#' @export
#'

zfill <- function(x, len = max(nchar(x))){

  lapply(x, function(arg1) {
    paste0(
      paste0(rep('0', len - nchar(arg1)), collapse = ''),
      arg1
    )}) |> unlist()
}
