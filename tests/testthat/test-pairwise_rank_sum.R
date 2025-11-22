test_that("pairwise_rank_sum returns expected rows/cols", {
  set.seed(123)
  x <- c(rnorm(10), rnorm(12, 0.5), rnorm(9))
  g <- factor(rep(c("A","B","C"), c(10,12,9)))
  res <- pairwise_rank_sum(x, g)
  expect_equal(nrow(res), choose(nlevels(g), 2))
  expect_true(all(c("group1","group2","stat","p","p_adj") %in% names(res)))
})
