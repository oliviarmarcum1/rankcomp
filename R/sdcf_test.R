#' Steel–Dwass–Critchlow–Fligner all-pairs test (stub)
#'
#' @inheritParams pairwise_rank_sum
#' @param type "dwass" or "critchlow-fligner"
#' @return data.frame (to be implemented)
#' @export
sdcf_test <- function(x, g, type = c("dwass","critchlow-fligner"),
                      exact = NULL, nperm = 10000) {
  type <- match.arg(type)
  stop("SDCF implementation in progress; will handle ties and variance corrections.")
}
