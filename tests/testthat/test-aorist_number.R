test_that("aorist(method='number') counts inclusive intervals correctly", {
  x <- data.frame(
    start = c(-10, -8),
    end   = c(-9,  -6)
  )

  res <- aorist(x, from = "start", to = "end", method = "number")

  # expected dates: -10:-6
  expect_equal(res$date, -10:-6)

  # interval 1 covers -10,-9
  # interval 2 covers -8,-7,-6
  # totals per year:
  # -10:1, -9:1, -8:1, -7:1, -6:1
  expect_equal(res$sum, c(1, 1, 1, 1, 1))
})

test_that("aorist(method='number') sums overlaps", {
  x <- data.frame(
    start = c(-10, -9),
    end   = c(-8,  -8)
  )

  res <- aorist(x, from = "start", to = "end", method = "number")

  # dates: -10:-8
  # record1 covers -10,-9,-8
  # record2 covers -9,-8
  # sums: -10:1, -9:2, -8:2
  expect_equal(res$date, -10:-8)
  expect_equal(res$sum, c(1, 2, 2))
})

test_that("method_number counts inclusive intervals and overlaps", {
  x <- data.frame(start = c(-10, -9), end = c(-8, -8))
  res <- aorist(x, from = "start", to = "end", method = "number")
  expect_equal(res$date, -10:-8)
  expect_equal(res$sum, c(1, 2, 2))
})

