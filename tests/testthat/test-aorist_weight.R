test_that("method_weight distributes constant weight across covered years", {
  x <- data.frame(start = -10, end = -6)
  res <- aorist(x, from = "start", to = "end", method = "weight")
  expect_equal(res$date, -10:-6)
  expect_equal(res$sum, rep(1/4, 5))  # abs(-10 - -6) = 4
})

test_that("method_weight adds weights for overlapping intervals", {
  x <- data.frame(
    start = c(-10, -9),
    end   = c(-8,  -8)
  )
  res <- aorist(x, from = "start", to = "end", method = "weight")
  # weights: abs(-10--8)=2 => 0.5; abs(-9--8)=1 => 1
  # yearly sums: -10:0.5, -9:1.5, -8:1.5
  expect_equal(res$date, -10:-8)
  expect_equal(res$sum, c(0.5, 1.5, 1.5))
})