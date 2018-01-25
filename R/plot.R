#' Plot stability paths
#'
#' @param x a \code{stability_path}-object
#' @param y not used
#' @param label_position where to put the covariate labels: use rank or mean
#'  inclusion probability (scaled to [0,1])
#' @param path_pars named list with optional arguments for \code{\link[graphics]{matplot}}-plotting of the stabiltiy path lines.
#'  See source code for defaults.
#' @param label_pars named list with optional arguments for covariate labels via \code{\link[graphics]{mtext}}.
#'  See source code for defaults.
#' @param ... optional additional arguments handed over to \code{par}.
#' @return invisibly returns the plotted object
#' @export
plot.stabpath <- function(x, y=NULL, label_position = c("rank", "mean"),
    path_pars=list(), label_pars=list(),  ...) {
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  label_position <- match.arg(label_position)
  new_pars <- list(...)
  default_pars <- list(mar = opar$mar + c(0,0,0,1))
  do.call(par, modifyList(default_pars, new_pars))

  n_cov <- ncol(x)
  n_step <- nrow(x)

  # default qualitative palette:
  col <- hcl(h = seq(0, 360, l=n_cov+1) + 15,
    c= 100, l = 65)[-(n_cov+1)]

  default_path_pars <- list(lty = 1, type = "l", bty = "n", col = col,
    x = 0:(n_step - 1), pch = 19, xlab = "regularization",
    ylab = expression(Pi), ylim = c(0,1))
  path_pars <- c(list(y = x), modifyList(default_path_pars, path_pars))
  do.call(matplot, path_pars)

  if (label_position == "rank") {
    rank_coef <- rank(colMeans(x), ties.method = "first")
    label_pos <- seq(0, 1, l = n_cov)[rank_coef]
    } else {
      # arrange labels according to mean inclusion probability
      label_pos <- colMeans(x)
    }

  default_label_pars <- list(text = colnames(x), side = 4, at = label_pos,
    col = col, las=1)
  label_pars <- modifyList(default_label_pars, label_pars)

  do.call(mtext, label_pars)
  invisible(x)
}
