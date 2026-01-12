test_that("calendar='historical' rejects year 0", {
  x <- data.frame(start = 0, end = 1)
  expect_error(
    aorist(x, from="start", to="end", method="number", calendar="historical"),
    "year 0",
    fixed = FALSE
  )
})

test_that("calendar='astronomical' allows year 0 (default)", {
  x <- data.frame(start = 0, end = 1)
  res <- aorist(x, from="start", to="end", method="number")
  expect_true(all(res$date %in% c(0, 1)))
})