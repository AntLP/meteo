# tests/testthat/test_plots.R

library(testthat)
source("../../R/data_helpers.R")
source("../../R/plots.R")

test_that("build_chart returns a plotly object for a valid metric", {
  tmp <- make_mock_data_dir(dates = c("2026-03-30", "2026-03-31"))
  on.exit(unlink(tmp, recursive = TRUE))

  d     <- load_station_data("7017270", data_dir = tmp)
  chart <- build_chart(d, "Mean Temp (C)")

  expect_s3_class(chart, "plotly")
})

test_that("build_chart y-axis title is the French column name", {
  tmp <- make_mock_data_dir(dates = c("2026-03-30", "2026-03-31"))
  on.exit(unlink(tmp, recursive = TRUE))

  d     <- load_station_data("7017270", data_dir = tmp)
  chart <- build_chart(d, "Max Temp (C)")

  y_title <- chart$x$layout$yaxis$title
  expect_equal(y_title, FRENCH_COLS[["Max Temp (C)"]])
})

test_that("build_table returns a datatable object", {
  tmp <- make_mock_data_dir(dates = c("2026-03-30", "2026-03-31"))
  on.exit(unlink(tmp, recursive = TRUE))

  d     <- load_station_data("7017270", data_dir = tmp)
  tbl   <- build_table(d)

  expect_s3_class(tbl, "datatables")
})

test_that("build_table uses French column names", {
  tmp <- make_mock_data_dir(dates = c("2026-03-30", "2026-03-31"))
  on.exit(unlink(tmp, recursive = TRUE))

  d   <- load_station_data("7017270", data_dir = tmp)
  tbl <- build_table(d)

  expect_true("Temp. max. (°C)" %in% names(tbl$x$data))
  expect_false("Max Temp (C)" %in% names(tbl$x$data))
})
