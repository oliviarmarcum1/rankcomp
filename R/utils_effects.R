#' @keywords internal
#' @noRd
.rank_effect_two <- function(x, y, measure = c("auc","cliff")) {
  measure <- match.arg(measure)
  x <- x[is.finite(x)]; y <- y[is.finite(y)]
  n1 <- length(x); n2 <- length(y)
  if (n1 == 0 || n2 == 0) return(NA_real_)
  cmp <- outer(x, y, "-")
  wins <- sum(cmp > 0); ties <- sum(cmp == 0); losses <- sum(cmp < 0)
  N <- n1 * n2
  if (measure == "auc") (wins + 0.5 * ties) / N else (wins - losses) / N
}

#' @keywords internal
#' @noRd
.rank_effect_two_boot <- function(x, y, measure, level = 0.95, nboot = 2000, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n1 <- length(x); n2 <- length(y)
  if (n1 == 0 || n2 == 0) return(c(NA_real_, NA_real_))
  boots <- numeric(nboot)
  for (b in seq_len(nboot)) {
    xb <- sample(x, n1, replace = TRUE)
    yb <- sample(y, n2, replace = TRUE)
    boots[b] <- .rank_effect_two(xb, yb, measure = measure)
  }
  a <- (1 - level) / 2
  stats::quantile(boots, probs = c(a, 1 - a), names = FALSE, type = 7)
}
