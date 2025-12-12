#' Equivalence / non-inferiority on probabilistic nonparametric effects
#'
#' Uses bootstrap CIs for AUC or Cliff's delta to decide equivalence / non-inferiority
#' for all group pairs. "No-difference" center: 0.5 (AUC) or 0 (Cliff).
#' @param g Factor/character group labels (same length as x).
#' @param nboot Integer number of bootstrap resamples for the CI.
#' @param seed Optional integer seed for reproducibility.
#' @param x Numeric vector; @param g group labels (same length as x).
#' @param delta Non-negative margin.
#' @param measure "auc" or "cliff".
#' @param alternative "equivalence","noninferiority","noninferiority_rev".
#' @param level CI level; @param nboot bootstrap reps; @param seed optional seed.
#' @return data.frame with: group1, group2, measure, effect, ci_low, ci_high,
#'         center, delta, alternative, decision
#' @examples
#' set.seed(2)
#' x <- c(rnorm(15,0), rnorm(15,0.05)); g <- factor(rep(c("A","B"), each=15))
#' equivalence_np(x, g, delta = 0.05, measure = "auc",
#'                alternative = "equivalence", nboot = 200, seed = 1)
#' @export
equivalence_np <- function(x, g, delta,
                           measure = c("auc","cliff"),
                           alternative = c("equivalence","noninferiority","noninferiority_rev"),
                           level = 0.95, nboot = 2000, seed = NULL) {
  measure <- match.arg(measure)
  alternative <- match.arg(alternative)
  stopifnot(is.numeric(delta), length(delta) == 1, delta >= 0)
  stopifnot(length(x) == length(g))
  g <- as.factor(g); if (nlevels(g) < 2L) stop("Need at least two groups.")
  pairs <- utils::combn(levels(g), 2, simplify = FALSE)
  center <- if (measure == "auc") 0.5 else 0.0

  rows <- lapply(pairs, function(pr) {
    xi <- x[g == pr[1]]; yi <- x[g == pr[2]]
    eff <- .rank_effect_two(xi, yi, measure = measure)
    ci2 <- .rank_effect_two_boot(xi, yi, measure = measure, level = level,
                                 nboot = nboot, seed = seed)
    lo <- ci2[1]; hi <- ci2[2]
    decision <- switch(
      alternative,
      equivalence        = if (lo >= (center - delta) && hi <= (center + delta)) "equivalent" else "not_equivalent",
      noninferiority     = if (lo >= (center - delta)) "non_inferior"         else "not_non_inferior",
      noninferiority_rev = if (hi <= (center + delta)) "non_inferior_rev"     else "not_non_inferior_rev"
    )
    data.frame(group1 = pr[1], group2 = pr[2], measure = measure,
               effect = eff, ci_low = lo, ci_high = hi,
               center = center, delta = delta,
               alternative = alternative, decision = decision,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows); rownames(out) <- NULL; out
}
