#' @title Estimate growth rates
#' @name growth_rate
#'
#' @note For growth rate estimation, the data is assumed to be sorted for vectors, for data.frames
#' data is sorted internally.
#'
#' @param x Numeric vector containing counts or related metric to estimate growth rates.
#' It can also be a data frame containing `n` and `time` variables (for data frames only).
#' @param n Variable name of count data. Quoted or unquoted.
#' @param time Variable name of the date variable (any format that is sortable). Quoted or unquoted.
#' @param by Grouping variable
#' @param ... Further arguments passed to other methods.
#'
#' @return When input value is a vector, then a vector of the same length that input parameter
#' is returned. When input value is a data.frame, tibble or data.table, a data.table is returned.
#'
#' @details
#'
#'   More info about this growth rate equation can be seen next [...]
#'
#' @export
growth_rate <- function(x, ...) {
  UseMethod("growth_rate")
}

#' @rdname growth_rate
#' @export
growth_rate.default <- function(x, ...) {

  if (any(is.na(x))) {
    warning("There are missing values in `x`")
  }

  # r <- data.table::shift(
  #   log(x / data.table::shift(x, type = "lag")),
  #   type = "lead"
  # )

  r <- log(x / data.table::shift(x, type = "lag"))

  return(r)
}

#' @rdname growth_rate
#' @export
growth_rate.data.frame <- function(x, n, time, by = NULL, ...) {

  n <- get_params(n);
  time <- get_params(time);
  by <- get_params(by, env = environment());

  by_call <- c(by, time)

  xdt <- data.table::as.data.table(x)

  cell <- xdt[, .N, keyby = by_call][["N"]]
  if (any(cell > 1)) {
    warning("There're more than 1 value per time measurement.\n",
            "This values will be aggregated", call. = FALSE)

    xdt <- xdt[j = lapply(.SD, sum), keyby = by_call, .SDcols = n]
  }

  ind <- do.call(
    what = order,
    args = mget(by_call, envir = as.environment(xdt))
  )

  time <- as.name(time); n <- as.name(n)

  dti <- substitute(expr = order(time))
  dtj <- substitute(expr = list(time, n, "r" = growth_rate(n)))

  output <- xdt[eval(dti), eval(dtj), by = by]

  class(output) <- c(class(output), "growth_rate")

  return(output)
}

