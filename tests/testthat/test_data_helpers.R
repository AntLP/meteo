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
