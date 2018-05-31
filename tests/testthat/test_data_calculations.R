#Testing Calculation Functions
context("Testing Calculation Functions")

test_that("Climate constants properly calculate", {
  expect_equal(lat_correction(40)$K_HI, 1.02, tolerance = 0.0005)
  expect_equal(lat_correction(50)$K_HI, 1.06, tolerance = 0.0005)
  expect_equal(lat_correction(33.3)$K_HI, 1.00, tolerance = 0.0005)
  expect_equal(lat_correction(40)$K_BEDD, 1.00, tolerance = 0.0005)
})
