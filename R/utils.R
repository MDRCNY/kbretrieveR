#' Null-coalescing operator
#'
#' Return `y` when `x` is `NULL` or of length 0; otherwise return `x`.
#'
#' This is an internal helper used throughout the package.
#'
#' @param x,y Values to coalesce.
#' @return Either `x` (when present) or `y`.
#' @name %||%
#' @keywords internal
NULL

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}
