#' Functions to calculate percentiles
#'
#' @param y Object with values
#' @param ... as in quantile function
#'
#' @examples
#'
#' p25(1:3)
#' p75(1:4)
#' p99(0:100)
#'
#' @rdname percentile
#'
#' @importFrom stats quantile
#' @export
p10 <- function(y, ...) {
  quantile(x = y, probs = 0.1, ...)
}

#' @rdname percentile
#' @export
p25 <- function(y, ...) {
  quantile(x = y, probs = 0.25, ...)
}

#' @rdname percentile
#' @export
p75 <- function(y, ...) {
  quantile(x = y, probs = 0.75, ...)
}

#' @rdname percentile
#' @export
p90 <- function(y, ...) {
  quantile(x = y, probs = 0.9, ...)
}

#' @rdname percentile
#' @export
p95 <- function(y, ...) {
  quantile(x = y, probs = 0.95, ...)
}

#' @rdname percentile
#' @export
p99 <- function(y, ...) {
  quantile(x = y, probs = 0.99, ...)
}
