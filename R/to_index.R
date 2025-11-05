#' Convert a numeric series to an index with a chosen base year
#'
#' Scales a numeric vector so that the value at a specified base year
#' (or the first non-missing observation by default) equals 100.
#'
#' @param x Numeric vector.
#' @param year Optional numeric vector of the same length as x, giving the year of each observation.
#' @param base_year Year to use as the 100-point base. If NULL, the first non-missing value is used.
#' @return Numeric vector rescaled to a base of 100.
#' @examples
#' to_index(c(50, 75, 100), year = c(1985, 1990, 1995), base_year = 1990)
#' @export
to_index <- function(x, year = NULL, base_year = NULL) {
  # Determine base value
  if (!is.null(base_year) && !is.null(year)) {
    base <- x[which(year == base_year)[1]]
  } else {
    base <- x[which(!is.na(x))[1]]
  }

  # Handle edge cases
  if (is.na(base) || base == 0) {
    return(rep(NA_real_, length(x)))
  }

  (x / base) * 100
}
