#' Nonparametric effect sizes for all group pairs (AUC / Cliff's delta)
#'
#' Computes pairwise nonparametric effect sizes between groups:
#' \itemize{
#' \item \strong{AUC} (a.k.a. probability of superiority): \eqn{P(X>Y) + 0.5 P(X=Y)}
#' \item \strong{Cliff's delta}: \eqn{(wins - losses)/(n_1 n_2)} in \eqn{[-1, 1]}
#' }
#'
#' @param x Numeric vector of observations.
#' @param g Factor/character group labels (same length as \code{x}).
#' @param measure One of \code{"auc"} or \code{"cliff"}.
#' @param ci Logical; if \code{TRUE}, return bootstrap percentile confidence intervals.
#' @param level Confidence level for CIs (default \code{0.95}).
#' @param nboot Number of bootstrap resamples used when \code{ci = TRUE}.
#' @param seed Optional integer seed for reproducibility of the bootstrap.
#'
#' @return A data.frame with columns:
#' \describe{
#' \item{group1, group2}{Pair of groups compared}
#' \item{measure}{\code{"auc"} or \code{"cliff"}}
#' \item{effect}{Estimated effect}
#' \item{ci_low, ci_high}{Bootstrap CI bounds if \code{ci=TRUE}, otherwise \code{NA}}
#' }
#'
#' @details The bootstrap uses simple percentile intervals. Examples are kept
#' small so they run quickly.
#'
#' @examples
#' set.seed(1)
#' x <- c(rnorm(10, 0), rnorm(12, 0.6), rnorm(9, 0))
#' g <- factor(rep(c("A","B","C"), c(10,12,9)))
#' np_effect_size(x, g, measure = "auc", ci = FALSE)
#' np_effect_size(x, g, measure = "cliff", ci = TRUE, nboot = 200, seed = 1)
#'
#' @export
#' @importFrom stats quantile
np_effect_size <- function(x, g, measure = c("auc","cliff"),
                           ci = TRUE, level = 0.95, nboot = 2000, seed = NULL) {
  measure <- match.arg(measure)
  stopifnot(length(x) == length(g))
  g <- as.factor(g)
  if (nlevels(g) < 2L) stop("Need at least two groups.")
  pairs <- utils::combn(levels(g), 2, simplify = FALSE)

  rows <- lapply(pairs, function(pair) {
    xi <- x[g == pair[1]]
    yi <- x[g == pair[2]]
    eff <- .rank_effect_two(xi, yi, measure = measure)
    if (isTRUE(ci)) {
      ci2 <- .rank_effect_two_boot(xi, yi, measure = measure, level = level,
                                   nboot = nboot, seed = seed)
      data.frame(group1 = pair[1], group2 = pair[2],
                 measure = measure, effect = eff,
                 ci_low = ci2[1], ci_high = ci2[2],
                 stringsAsFactors = FALSE)
    } else {
      data.frame(group1 = pair[1], group2 = pair[2],
                 measure = measure, effect = eff,
                 ci_low = NA_real_, ci_high = NA_real_,
                 stringsAsFactors = FALSE)
    }
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

