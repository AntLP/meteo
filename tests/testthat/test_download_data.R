# tests/testthat/test_download_data.R

library(testthat)
source("../../R/download_data.R")

test_that("parse_csv_filenames filters to QC climate daily pattern", {
  html_links <- c(
    "climate_daily_QC_7017270_1970_P1D.csv",
    "climate_daily_QC_7017270_2025_P1D.csv",
    "climate_daily_ON_1234567_2025_P1D.csv",   # wrong province
    "README.txt",                                # not a CSV
    "../"                                        # parent dir link
  )

  result <- parse_csv_filenames(html_links)

  expect_equal(length(result), 2)
  expect_true(all(grepl("^climate_daily_QC_.+_P1D\\.csv$", result)))
})

test_that("download_qc_data skips files that already exist", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  # Pre-create a file that would be downloaded
  existing_file <- file.path(tmp, "climate_daily_QC_7017270_2025_P1D.csv")
  writeLines("already here", existing_file)
  mtime_before <- file.mtime(existing_file)

  # Mock get_csv_list to return a one-row data.frame
  local_mocked_bindings(
    get_csv_list = function(...) data.frame(
      name          = "climate_daily_QC_7017270_2025_P1D.csv",
      link          = "http://example.com/climate_daily_QC_7017270_2025_P1D.csv",
      last_modified = as.POSIXct("2025-03-13 12:52", format = "%Y-%m-%d %H:%M"),
      stringsAsFactors = FALSE
    ),
    {
      suppressMessages(
        download_qc_data(
          base_url = "http://example.com/",
          data_dir = tmp
        )
      )
    }
  )

  # File was not modified — was skipped
  expect_equal(file.mtime(existing_file), mtime_before)
  expect_equal(readLines(existing_file), "already here")
})
