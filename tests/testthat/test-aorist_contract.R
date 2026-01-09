test_that("aorist() returns aorist_ts with full date range and no NA sums", {
  x <- data.frame(
    start = c(-10, -8),
    end   = c(-9,  -6)
  )

  res <- aorist(x, from = "start", to = "end", method = "number")

  # S3 contract
  expect_s3_class(res, "aorist_ts")
  expect_true(is.data.frame(res))
  expect_true(all(c("date", "sum") %in% names(res)))

  # date range contract
  expect_equal(min(res$date, na.rm = TRUE), min(x$start, na.rm = TRUE))
  expect_equal(max(res$date, na.rm = TRUE), max(x$end,   na.rm = TRUE))
  expect_equal(nrow(res), length(seq(min(x$start), max(x$end), by = 1)))

  # final output contract: NA replaced by 0
  expect_false(any(is.na(res$sum)))

  # basic metadata (once you add it via new_aorist_ts)
  expect_equal(attr(res, "method"), "number")
  expect_equal(attr(res, "stepwidth"), 1)
  expect_equal(attr(res, "from"), "start")
  expect_equal(attr(res, "to"), "end")
})