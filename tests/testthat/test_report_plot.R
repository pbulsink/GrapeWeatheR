#Testing Report and Plot Functions
context("Testing Report & Plot Functions")

test_that("Rolling Average", {
  expect_equal(rolling(c(1:10), 3), c(1.5, 2:9, 9.5))
  expect_equal(rolling(c(1:4,NA,6:10), 3), c(1.5, 2:3, 3.5, 5, 6.5, 7:9, 9.5))
  expect_equal(rolling(c(1:10), 1), c(1:10))
  expect_equal(rolling(c(1:3, NA, NA, NA, 7:10), 3), c(1.5, 2, 2.5, 3, NA, 7, 7.5, 8, 9, 9.5))
  expect_message(rolling(c(1:10), 2), "Odd rolling window required. Using n=2+1.", fixed=TRUE)
})
