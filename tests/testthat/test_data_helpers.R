# tests/testthat/test_data_helpers.R

library(testthat)
source("../../R/data_helpers.R")

test_that("build_station_registry returns one row per station", {
  tmp <- make_mock_data_dir()
  on.exit(unlink(tmp, recursive = TRUE))

  registry <- build_station_registry(data_dir = tmp)

  expect_equal(nrow(registry), 1)
  expect_equal(registry$climate_id,   "7017270")
  expect_equal(registry$station_name, "MONTREAL")
  expect_equal(registry$lon,          -73.57)
  expect_equal(registry$lat,           45.52)
})

test_that("build_station_registry returns NULL for empty dir", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  expect_null(build_station_registry(data_dir = tmp))
})

test_that("get_yesterday_summary returns named numeric vector for known date", {
  tmp <- make_mock_data_dir(dates = c("2026-03-31"))
  on.exit(unlink(tmp, recursive = TRUE))

  result <- get_yesterday_summary(
    climate_id = "7017270",
    data_dir   = tmp,
    ref_date   = as.Date("2026-04-01")
  )

  expect_true(is.numeric(result))
  expect_named(result, intersect(names(TOOLTIP_COLS), names(result)))
  expect_equal(unname(result["Mean Temp (C)"]), 1.5)
})

test_that("get_yesterday_summary returns NULL when date not in data", {
  tmp <- make_mock_data_dir(dates = c("2026-03-30"))
  on.exit(unlink(tmp, recursive = TRUE))

  result <- get_yesterday_summary(
    climate_id = "7017270",
    data_dir   = tmp,
    ref_date   = as.Date("2026-04-01")  # yesterday = 2026-03-31, not in data
  )

  expect_null(result)
})
