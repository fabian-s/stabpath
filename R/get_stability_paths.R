#' Extract stability paths
#'
#' Extract stability paths from refits of a model object on resampled or
#' subsampled data.
#'
#' List-shaped \code{data} arguments are necessary a) for models that use
#' weights or offsets and/or b) for modeling functions that receive the design
#' matrix and the response vector as arguments directly instead of a model
#' formula and a corresponding dataset. In these cases \code{data} has to be a
#' named list with names corresponding to the modelling function's respective
#' arguments (for example, \code{list(x = <design matrix>, y = <response>,
#' weights = <model weights>, offset = <model offsets>)} or
#' \code{list(data = <dataframe>, weights = <model weights>)})
#'
#' @param model a fitted model object for which \code{\link{refit}} and
#'   \code{\link{get_selected}} methods are defined
#' @param data the data used for the model fit, a \code{data.frame} or a list
#'   containing \code{data.frame}s, vectors and/or matrices. See Details.
#' @param reps how many refits of \code{model} on resampled \code{data} to
#'   perform
#' @param method resampling method, either "subsample" (without replacement) or
#'   "bootstrap" (with replacement)
#' @param strata for subsampling, a vector with length nrow(data) defining the
#'   strata
#' @param fraction subsampling fraction
#' @return a <max. subsetsize + 1> x <covariates> matrix of relative selection
#'   frequencies with class \code{stabpath}. First row is the null model (no
#'   covariates, i.e., all 0s)
#' @export
get_stability_paths <- function(model, data, reps = 100, method = "subsample",
  strata = NULL, fraction = 0.5) {

  # TODO: input checks
  # TODO: this loop should be parallelized (at least optionally)
  selected <- list()
  for(i in seq_len(reps)) {
    new_data <- resample(data, method=method, strata=strata, fraction=fraction)
    new_model <- refit(model, new_data)
    selected[[i]] <- get_selected(new_model)
  }
  stability_paths <- make_paths(selected)
  class(stability_paths) <- c("stabpath", class(stability_paths))
  stability_paths
}

make_paths <- function(selected) {
  Reduce(`+`, selected) / length(selected)
}
