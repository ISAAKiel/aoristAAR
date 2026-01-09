test_that("period_correction conserves total mass (sum == n records)", {
  x <- data.frame(start = c(-10, -10, -8), end = c(-8, -8, -6))
  res <- aorist(x, from = "start", to = "end", method = "period_correction")
  expect_equal(sum(res$sum), nrow(x), tolerance = 1e-12)
})