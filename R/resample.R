#' Resample or subsample data rows
#'
#' Resample or subsample data rows from a \code{data.frame} or a list containing
#' \code{data.frame}s, vectors and/or matrices.
#'
#' @inheritParams get_stability_paths
#' @return the subsampled or resampled \code{data}
#' @param ... further arguments handed over to methods
#' @export
resample <- function(data, method = c("subsample", "bootstrap"), strata = NULL,
    fraction = 0.5, ...) {
  UseMethod("resample")
}

#' @describeIn resample method for \code{data.frames}
resample.data.frame <- function(data, method = c("subsample", "bootstrap"),
  strata = NULL, fraction = 0.5) {
  method <- match.arg(method)
  nrows <- nrow(data)
  rows <- resample_rows(nrows, method, strata, fraction)
  data[rows, ]
}

#' @describeIn resample entries in \code{data} have to be numeric vectors,
#' matrices or data.frames that all have the same length or number of rows.
resample.list <- function(data, method = c("subsample", "bootstrap"),
  strata = NULL, fraction = 0.5) {
  method <- match.arg(method)
  stopifnot(
    length(unique(lapply(data, NROW))) == 1L,
    all(unlist(sapply(data, class)) %in% c("integer", "numeric", "matrix",
      "data.frame")))
  nrows <- NROW(data[[1]])
  rows <- resample_rows(nrows, method, strata, fraction)
  lapply(data, function(x, rows) {
    if(is.null(dim(x))) {
      x[rows]
    } else {
      x[rows,]
    }
  }, rows = rows)
}

# utils ------------------------------------------------------------------------

resample_rows <- function(nrows, method, strata = NULL, fraction = 0.5) {
  if(method == "bootstrap") {
    sample_with_replacement(nrows, strata)
  } else {
    sample_without_replacement(nrows, strata, fraction = fraction)
  }
}

sample_with_replacement <- function(nrows, strata = NULL) {
  if(is.null(strata)) {
    sample(nrows, replace = TRUE)
  } else {
    stopifnot(length(strata) == nrows)
    rows <- tapply(X = seq_len(nrows), INDEX = strata, FUN = sample,
      replace = TRUE)
    as.vector(rows)
  }
}

sample_without_replacement <- function(nrows, strata = NULL, fraction = 0.5) {
  stopifnot(fraction > 0 & fraction <= 1)
  if(is.null(strata)) {
    sample(1:nrows, size = ceiling(nrows * fraction),  replace = FALSE)
  } else {
    stopifnot(length(strata) == nrows)
    strata_sizes <- ceiling(table(strata) * fraction)
    strata_index <- split(seq_len(nrows), strata)
    rows <- mapply(sample, x = strata_index, size = strata_sizes,
      replace = FALSE)
    as.vector(rows)
  }
}
