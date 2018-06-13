#Testing Calculation Functions
context("Testing Calculation Functions")

test_that("Climate constants properly calculated", {
  expect_equal(lat_correction(40)$K_HI, 1.02, tolerance = 0.0005)
  expect_equal(lat_correction(50)$K_HI, 1.06, tolerance = 0.0005)
  expect_equal(lat_correction(33.3)$K_HI, 1.00, tolerance = 0.001)
  expect_equal(lat_correction(40)$K_BEDD, 1.00, tolerance = 0.001)
})

test_that("Climate Indexes are properly calculated", {
  sample_data<-dplyr::tibble(
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

  expect_equal(winkler_index(sample_data), list("WI" = 2140, "WI_region" = "Region IV"))

  expect_equal(huglin_index(sample_data), list("HI" = 2374.333, "HI_region" = "Warm Temperate"), tolerance = 0.001)

  expect_equal(bedd(sample_data), list("BEDD" = 1926, "BEDD_region" = "Warm"))
})

test_that("Annual calculations return ok", {
  ai<-calculate_annual_indicies(weathercan::kamloops_day)  # kamloops_day is not a full year
  expect_true("tbl_df" %in% class(ai))

  expect_equal(ai$GSTavg, 15.9527, tolerance = 0.0001)
  expect_equal(ai$GSTmax, 23.4936, tolerance = 0.0001)
  expect_equal(ai$GSTmin, 8.3582, tolerance = 0.0001)
  expect_equal(ai$WI, 544.6, tolerance = 0.0001)
  expect_equal(ai$HI, 941, tolerance = 0.0001)
  expect_equal(ai$BEDD, 670.9032, tolerance = 0.0001)
  expect_equal(ai$FFD, 11)
  expect_equal(ai$F_Annual, 55)
  expect_equal(ai$F_Growing, 55)
  expect_equal(ai$ND_25, 34)
  expect_equal(ai$ND_30, 12)
  expect_equal(ai$NDT_Min_90p, 19)
  expect_equal(ai$NDT_Max_90p, 18)
  expect_equal(ai$NDT_Min_10p, 19)
  expect_equal(ai$NDT_Max_10p, 19)
  expect_equal(ai$P_Annual, 124.6)
  expect_equal(ai$P_Growing, 70)
  expect_equal(ai$P_Max, 8.4)
  expect_equal(ai$P_I, 9.9)
  expect_equal(ai$P_II, 60.1)
  expect_equal(ai$P_III, 0)
  expect_equal(ai$P_IV, 0)
  expect_equal(ai$P_V, 54.6)
  expect_equal(ai$DPL, 16)
  expect_equal(ai$NP95p, 10)
  expect_equal(ai$PP95p, 58)

})

test_that("Hourly to daily calculations return ok", {
  sample_data<-dplyr::tibble(
    station_id = c(rep(1, 24), rep(2, 24)),
    date = c(rep(as.Date("2018-01-01"), 24*2)),
    station_name = station_id,
    station_operator = "",
    prov = "ON",
    lat = 45,
    lon = -75,
    elev = 100,
    climate_id = station_id,
    WMO_id = station_id,
    TC_id = station_id,
    year = 2018,
    month = 1,
    day = 1,
    time = c(rep(seq.POSIXt(from = as.POSIXct("2018-01-01 0:00:00"), by='hour', length.out = 24), 2)),
    temp = c(rep(c(8, rep(10, 22), 12), 2))
  )

  result<- dplyr::tibble(station_id = c(1,2),
                          date = rep(as.Date("2018-01-01"),2),
                          station_name = c(1, 2),
                          prov = c("ON", "ON"),
                          lat = c(45, 45),
                          lon = c(-75, -75),
                          elev = c(100, 100),
                          climate_id = c(1, 2),
                          WMO_id = c(1, 2),
                          TC_id = c(1, 2),
                          year = c(2018, 2018),
                          month = c(1, 1),
                          day = c(1, 1),
                          max_temp = c(12, 12),
                          min_temp = c(8, 8),
                          mean_temp = c(10, 10))
  expect_equivalent(turn_hour_data_to_daily(sample_data), result)

})
