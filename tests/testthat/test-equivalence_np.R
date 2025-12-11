test_that("equivalence_np decisions are defined", {
  set.seed(2)
  x <- c(rnorm(20,0), rnorm(20,0.03))
  g <- factor(rep(c("A","B"), each = 20))
  res <- equivalence_np(x, g, delta = 0.06, measure = "auc",
                        alternative = "equivalence", nboot = 200, seed = 2)
  expect_true(all(res$decision %in% c("equivalent","not_equivalent")))
})
