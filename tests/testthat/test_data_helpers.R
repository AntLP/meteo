# tests/testthat/test_data_helpers.R

library(testthat)
source("../../R/data_helpers.R")

test_that("build_station_registry returns one row per station", {
  con <- make_mock_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  registry <- build_station_registry(con)

  expect_equal(nrow(registry), 1)
  expect_equal(registry$climate_id,   "7017270")
  expect_equal(registry$station_name, "MONTREAL")
  expect_equal(registry$lon,          -73.57)
  expect_equal(registry$lat,           45.52)
})

test_that("build_station_registry returns NULL for empty observations", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  expect_null(build_station_registry(con))
})

test_that("get_yesterday_summary returns named numeric vector for known date", {
  con <- make_mock_db(dates = c("2026-03-31"))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  result <- get_yesterday_summary(
    con,
    climate_id = "7017270",
    ref_date   = as.Date("2026-04-01")
  )

  expect_true(is.numeric(result))
  expect_named(result, intersect(names(TOOLTIP_COLS), names(result)))
  expect_equal(unname(result["mean_temp"]), 1.5)
})

test_that("get_yesterday_summary returns NULL when date not in data", {
  con <- make_mock_db(dates = c("2026-03-30"))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  result <- get_yesterday_summary(
    con,
    climate_id = "7017270",
    ref_date   = as.Date("2026-04-01")  # yesterday = 2026-03-31, not in data
  )

  expect_null(result)
})

test_that("load_station_data returns all rows sorted by date", {
  con <- make_mock_db(dates = c("2025-12-31", "2026-03-30", "2026-03-31"))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  d <- load_station_data(con, "7017270")

  expect_s3_class(d[["date"]], "Date")
  expect_equal(nrow(d), 3)
  expect_true(all(diff(as.numeric(d[["date"]])) >= 0))  # sorted
})

test_that("load_station_data returns NULL for unknown climate_id", {
  con <- make_mock_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  expect_null(load_station_data(con, "9999999"))
})
