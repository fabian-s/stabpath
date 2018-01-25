#' Refit a model on a new dataset
#'
#' This is roughly synonymous to \code{\link[stats]{update}} with
#' \code{update(model, data = new_data)}.
#'
#' @inheritParams get_stability_paths
#' @param new_data the new data
#' @param ... further arguments handed over to methods
#' @return the fitted model object for \code{newdata}
#' @export
refit <- function(model, new_data, ...) {
  UseMethod("refit")
}

#' @describeIn refit method for \code{\link[leaps]{regsubsets}}
refit.regsubsets <- function(model, new_data, ...) {
  modelcall <- model$call
  if (!is.null(modelcall$weights)) {
    if(is.null(new_data$weights))
      stop("model weights not supplied in 'new_data'")
    modelcall$data <- new_data$data
    modelcall$weights <- new_data$weights
  } else {
    modelcall$data <- new_data
  }
  # use regsubsets-generic instead of regsubsets.formula or other method as
  # methods are not exported by leaps:
  # use as.name on string instead of function name directly to avoid R CMD CHECK
  # complaining about undeclared global variable ....
  modelcall[[1]] <-  as.name("leaps::regsubsets")
  eval(modelcall)
}

#' @describeIn refit method for \code{\link[gbm]{gbm}} objects.
refit.gbm <- function(model, new_data, ...) {
  # TODO
}

#' @describeIn refit method for \code{\link[glmnet]{glmnet}} objects
refit.glmnet <- function(model, new_data, ...) {
  # TODO
}
