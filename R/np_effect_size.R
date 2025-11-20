#' Nonparametric effect sizes for all pairs (AUC or Cliff's delta)
#'
#' @inheritParams pairwise_rank_sum
#' @param measure "auc" (probability of superiority) or "cliff"
#' @param ci Logical; placeholder (CIs added later).
#' @return data.frame with: group1, group2, measure, effect
#' @export
np_effect_size <- function(x, g, measure = c("auc","cliff"), ci = FALSE,
                           level = 0.95, nboot = 2000, seed = NULL) {
  measure <- match.arg(measure)
  g <- as.factor(g)
  pairs <- utils::combn(levels(g), 2, simplify = FALSE)
  calc <- function(a, b) {
    cmp <- outer(a, b, "-")
    wins <- sum(cmp > 0); ties <- sum(cmp == 0); losses <- sum(cmp < 0)
    N <- length(a) * length(b)
    if (measure == "auc") (wins + 0.5 * ties) / N else (wins - losses) / N
  }
  rows <- lapply(pairs, function(pr) {
    a <- x[g == pr[1]]; b <- x[g == pr[2]]
    data.frame(group1 = pr[1], group2 = pr[2], measure = measure,
               effect = calc(a, b), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
