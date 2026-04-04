# tests/testthat/test_plots.R

library(testthat)
source("../../R/data_helpers.R")
source("../../R/plots.R")

test_that("build_chart returns a plotly object for a valid metric", {
  con <- make_mock_db(dates = c("2026-03-30", "2026-03-31"))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  d     <- load_station_data(con, "7017270")
  chart <- build_chart(d, "mean_temp")

  expect_s3_class(chart, "plotly")
})

test_that("build_chart y-axis title is the French column name", {
  con <- make_mock_db(dates = c("2026-03-30", "2026-03-31"))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  d     <- load_station_data(con, "7017270")
  chart <- build_chart(d, "max_temp")

  y_title <- chart$x$layout$yaxis$title
  expect_equal(y_title, FRENCH_COLS[["max_temp"]])
})

test_that("build_table returns a datatable object", {
  con <- make_mock_db(dates = c("2026-03-30", "2026-03-31"))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  d   <- load_station_data(con, "7017270")
  tbl <- build_table(d)

  expect_s3_class(tbl, "datatables")
})

test_that("build_table uses French column names", {
  con <- make_mock_db(dates = c("2026-03-30", "2026-03-31"))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  d   <- load_station_data(con, "7017270")
  tbl <- build_table(d)

  expect_true("Temp. max. (°C)" %in% names(tbl$x$data))
  expect_false("max_temp" %in% names(tbl$x$data))
})
