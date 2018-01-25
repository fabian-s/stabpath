#' Extract selected model terms from a (regularized) model
#'
#' @inheritParams get_stability_paths
#' @param ... other arguments to pass on to methods
#' @return a logical matrix that summarizes
#' which covariates (=columns) were in the model in each iteration or for each
#' value of the regularization parameter (=rows). First row is the null model
#' (typically all FALSE).
#' @export
get_selected <- function(model, ...){
  UseMethod("get_selected")
}

#' @describeIn get_selected method for \code{\link[leaps]{regsubsets}}
get_selected.regsubsets <- function(model, ...) {
  selected <- summary(model)$which
  # add row for null model / maximal regularization: intercept only
  selected <- rbind(rep(FALSE, ncol(selected)),
    selected)
  if("(Intercept)" %in% colnames(selected)) {
    selected[, "(Intercept)" != colnames(selected)]
  } else {
    selected
  }
}

#' @details \code{get_selected.gbm} also counts variables used (only) for surrogate
#' splits as selected.
#' @describeIn get_selected method for \code{\link[gbm]{gbm}} objects
get_selected.gbm <- function(model, ...) {

  # TODO: see ?gbm::pretty.gbm.tree for explanation of tree-structure

}

#' @describeIn get_selected method for \code{\link[glmnet]{glmnet}} objects
get_selected.glmnet <- function(model, ...) {

  # TODO
}
