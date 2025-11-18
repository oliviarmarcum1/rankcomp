#' Pairwise rank-based comparisons with adjusted p-values
#'
#' Compares every pair of groups using the Wilcoxon/Mann–Whitney test and
#' returns a table with raw and adjusted p-values.
#'
#' @param x Numeric vector of observations.
#' @param g Factor/character vector of group labels (same length as x).
#' @param method P-value adjustment method; default "holm".
#' @param exact Logical or NULL. If TRUE, request exact p-values when feasible.
#' @param nperm Ignored in this minimal version (placeholder).
#' @param seed Optional seed (placeholder for permutation mode).
#'
#' @return data.frame with columns: group1, group2, stat, p, p_adj, method, notes
#' @export
#' @examples
#' set.seed(1)
#' x <- c(rnorm(10,0), rnorm(12,0.6), rnorm(9,0))
#' g <- rep(c("A","B","C"), c(10,12,9))
#' pairwise_rank_sum(x, g)
pairwise_rank_sum <- function(x, g, method = "holm", exact = NULL, nperm = 10000, seed = NULL) {
  stopifnot(length(x) == length(g))
  g <- as.factor(g)
  pairs <- utils::combn(levels(g), 2, simplify = FALSE)
  out <- lapply(pairs, function(pr) {
    i <- g %in% pr
    xx <- x[i]; gg <- droplevels(g[i])
    wt <- stats::wilcox.test(xx ~ gg, exact = isTRUE(exact), conf.int = FALSE)
    data.frame(
      group1 = pr[1], group2 = pr[2],
      stat = unname(wt$statistic), p = wt$p.value,
      method = if (isTRUE(exact)) "exact" else "asymptotic",
      notes = NA_character_, stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)
  out$p_adj <- stats::p.adjust(out$p, method = method)
  out
}
