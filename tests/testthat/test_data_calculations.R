#Testing Calculation Functions
context("Testing Calculation Functions")

test_that("Climate constants properly calculated", {
  expect_equal(lat_correction(40)$K_HI, 1.02, tolerance = 0.0005)
  expect_equal(lat_correction(50)$K_HI, 1.06, tolerance = 0.0005)
  expect_equal(lat_correction(33.3)$K_HI, 1.00, tolerance = 0.001)
  expect_equal(lat_correction(40)$K_BEDD, 1.00, tolerance = 0.001)
})

test_that("Climate Indexes are properly calculated", {
  sample_data<-tibble::tibble(
    date = seq.Date(as.Date("2017-01-01"), as.Date("2017-12-31"), by=1),
    max_temp = 25,
    min_temp = 15,
    lat = 45
  )
  sample_data$mean_temp = (sample_data$max_temp + sample_data$min_temp)/2
  sample_data$year = as.integer(format(sample_data$date, "%Y"))
  sample_data$month = as.integer(format(sample_data$date, "%m"))
  sample_data$day = as.integer(format(sample_data$date, "%d"))

  expect_equal(gst(sample_data), list("GSTavg" = 20, "GSTmax" = 25, "GSTmin" = 15, "GST_region" = "Hot"))
  # expect_equal(gst(sample_data)$GSTavg, 20)
  # expect_equal(gst(sample_data)$GSTmax, 25)
  # expect_equal(gst(sample_data)$GSTmin, 15)
  # expect_equal(gst(sample_data)$GST_region, "Hot")

  expect_equal(winkler_index(sample_data), list("WI" = 2140, "WI_region" = "Region IV"))

  expect_equal(huglin_index(sample_data), list("HI" = 2374.333, "HI_region" = "Warm Temperate"), tolerance = 0.001)

  expect_equal(bedd(sample_data), list("BEDD" = 1926, "BEDD_region" = "Warm"))
})
