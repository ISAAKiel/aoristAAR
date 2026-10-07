# Each observation carries a total probability mass of one. With stepwidth = 1
# the time series must therefore sum to the number of observations.

test_data <- data.frame(
  start = c(-3800, -3750, -3500, -4000, -3800, -3800, -3550, -3750, -3800),
  end   = c(-3700, -3400, -3300, -3300, -3500, -3300, -3525, -3650, -3700)
)

test_that("method 'weight' gives each observation a total mass of one", {
  res <- aorist(test_data, from = "start", to = "end", method = "weight")
  expect_equal(sum(res$sum), nrow(test_data))
})

test_that("method 'weight' gives a single interval a mass of one", {
  one <- data.frame(start = -3800, end = -3700)
  res <- aorist(one, from = "start", to = "end", method = "weight")
  expect_equal(sum(res$sum), 1)
  # 101 years (both ends inclusive), each with weight 1/101
  expect_equal(sum(res$sum > 0), 101)
  expect_equal(unique(res$sum[res$sum > 0]), 1 / 101)
})

test_that("method 'weight' handles single-year intervals", {
  one <- data.frame(start = c(-3800, -3800), end = c(-3800, -3800))
  res <- aorist(one, from = "start", to = "end", method = "weight")
  expect_equal(sum(res$sum), 2)
})

test_that("method 'period_correction' gives each observation a total mass of one", {
  res <- aorist(test_data, from = "start", to = "end", method = "period_correction")
  expect_equal(sum(res$sum), nrow(test_data))
})

test_that("'weight' and 'period_correction' agree for non-overlapping periods", {
  # Without overlap the correction has nothing to correct.
  disjoint <- data.frame(
    start = c(-3800, -3800, -3700, -3600),
    end   = c(-3701, -3701, -3601, -3501)
  )
  w <- aorist(disjoint, from = "start", to = "end", method = "weight")
  p <- aorist(disjoint, from = "start", to = "end", method = "period_correction")
  expect_equal(w$date, p$date)
  expect_equal(w$sum, p$sum)
})
