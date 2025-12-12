#' SDCF-style all-pairs comparison (Wilcoxon + multiplicity control surrogate)
#'
#' @description
#' This function provides a working approximation for the Steel–Dwass–Critchlow–Fligner
#' (SDCF) all-pairs procedure by running Wilcoxon/Mann–Whitney tests for every pair
#' of groups and applying a p-value adjustment (Holm by default) to control the familywise
#' error rate.
#'
#' A full SDCF implementation is planned for a future version.
#'
#'#' @details
#' `sdcf_test()` currently provides an SDCF-style working approximation using Wilcoxon all-pairs
#' with familywise error control; formal SDCF variance/tie corrections are scheduled for v0.2.
#'
#'
#' @inheritParams pairwise_rank_sum
#' @param adjust P-value adjustment method (e.g., "holm", "hochberg", "bonferroni", "BH").
#'   Default is "holm".
#' @param type One of
#'   \code{"dwass"} or \code{"critchlow-fligner"}.
#'
#' @return A data.frame with columns:
#'   \code{group1, group2, n1, n2, stat, p, p_adj, mode, notes, method_label}
#'   where \code{method_label} is \code{"wilcoxon+FWER (SDCF-approx)"}.
#'
#' @examples
#' set.seed(1)
#' x <- c(rnorm(10,0), rnorm(12,0.6), rnorm(9,0))
#' g <- factor(rep(c("A","B","C"), c(10,12,9)))
#' sdcf_test(x, g)
#'
#' @export
#' @importFrom stats wilcox.test p.adjust
sdcf_test <- function(x, g,
                      adjust = "holm",
                      type = c("dwass","critchlow-fligner"),
                      exact = NULL) {
  type <- match.arg(type)  # reserved for future true-SDCF variants

  base <- pairwise_rank_sum(x, g, method = adjust, exact = exact)

  if (!all(c("n1","n2") %in% names(base))) {
    g <- as.factor(g)
    levels_g <- levels(g)
    pairs <- utils::combn(levels_g, 2, simplify = FALSE)
    n_tbl <- lapply(pairs, function(pr) {
      n1 <- sum(g == pr[1], na.rm = TRUE)
      n2 <- sum(g == pr[2], na.rm = TRUE)
      data.frame(group1 = pr[1], group2 = pr[2], n1 = n1, n2 = n2, stringsAsFactors = FALSE)
    })
    n_tbl <- do.call(rbind, n_tbl)
    base <- merge(base, n_tbl, by = c("group1","group2"), all.x = TRUE)
    base <- base[, c("group1","group2","n1","n2", setdiff(names(base), c("group1","group2","n1","n2")))]
  }

  base$method_label <- "wilcoxon+FWER (SDCF-approx)"
  if ("notes" %in% names(base)) {
    base$notes <- ifelse(is.na(base$notes),
                         "SDCF approximation via Wilcoxon + p-adjust",
                         paste0(base$notes, "; SDCF approximation via Wilcoxon + p-adjust"))
  }

  base
}
