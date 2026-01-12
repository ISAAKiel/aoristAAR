test_that("interval closure controls inclusion at boundaries", {
  x <- data.frame(start = 0, end = 1)

  # [] includes both endpoints: years 0 and 1
  res <- aorist(x, from="start", to="end", method="number", interval="[]")
  expect_equal(res$sum[res$date == 0], 1)
  expect_equal(res$sum[res$date == 1], 1)

  # [) includes start, excludes end: year 0 only
  res <- aorist(x, from="start", to="end", method="number", interval="[)")
  expect_equal(res$sum[res$date == 0], 1)
  expect_equal(res$sum[res$date == 1], 0)

  # (] excludes start, includes end: year 1 only
  res <- aorist(x, from="start", to="end", method="number", interval="(]")
  expect_equal(res$sum[res$date == 0], 0)
  expect_equal(res$sum[res$date == 1], 1)

  # () excludes both: nothing
  res <- aorist(x, from="start", to="end", method="number", interval="()")
  expect_equal(sum(res$sum), 0)
})

test_that("interval closure only changes boundary years", {
  x <- data.frame(start = c(0, 0), end = c(2, 2))

  resA <- aorist(x, from="start", to="end", method="number", interval="[]")
  resB <- aorist(x, from="start", to="end", method="number", interval="[)")

  # interior year 1 unaffected
  expect_equal(resA$sum[resA$date == 1], resB$sum[resB$date == 1])

  # end boundary differs (year 2)
  expect_true(resA$sum[resA$date == 2] > resB$sum[resB$date == 2])
})