#' Functions to calculate percentiles
#'
#' @param y Object with values
#' @param p Probs as in quantile function
#' @param na_rm Removes NAs
#' @param ... as in quantile function
#'
#' @examples
#'
#' p25(1:3)
#' p75(1:4)
#' p99(0:100)
#'
#' @importFrom stats quantile

#' @export
#' @rdname percentile
pn <- function(y, p, na_rm = T, ...) {
  quantile(x = y, probs = p, na.rm = na_rm, ...)
}

#' @export
#' @rdname percentile
p10 <- function(y, na_rm = T, ...) {
  quantile(x = y, probs = 0.1, na.rm = na_rm, ...)
}

#' @export
#' @rdname percentile
p25 <- function(y, na_rm = T, ...) {
  quantile(x = y, probs = 0.25, na.rm = na_rm, ...)
}

#' @export
#' @rdname percentile
p75 <- function(y, na_rm = T, ...) {
  quantile(x = y, probs = 0.75, na.rm = na_rm, ...)
}

#' @export
#' @rdname percentile
p90 <- function(y, na_rm = T, ...) {
  quantile(x = y, probs = 0.9, na.rm = na_rm, ...)
}

#' @export
#' @rdname percentile
p95 <- function(y, na_rm = T, ...) {
  quantile(x = y, probs = 0.95, na.rm = na_rm, ...)
}

#' @export
#' @rdname percentile
p99 <- function(y, na_rm = T, ...) {
  quantile(x = y, probs = 0.99, na.rm = na_rm, ...)
}
