# R/download_data.R

library(rvest)
library(httr)
library(readr)
library(dplyr)
library(dbplyr)
library(DBI)
library(duckdb)

# Clean snake_case column names for Weather Canada daily climate CSVs.
# Order matches the 31 columns in the file header exactly.
.ECCC_COL_NAMES <- c(
  "lon", "lat", "station_name", "climate_id", "date",
  "year", "month", "day", "data_quality",
  "max_temp", "max_temp_flag",
  "min_temp", "min_temp_flag",
  "mean_temp", "mean_temp_flag",
  "heat_deg_days", "heat_deg_days_flag",
  "cool_deg_days", "cool_deg_days_flag",
  "total_rain", "total_rain_flag",
  "total_snow", "total_snow_flag",
  "total_precip", "total_precip_flag",
  "snow_on_ground", "snow_on_ground_flag",
  "gust_dir", "gust_dir_flag",
  "gust_speed", "gust_speed_flag"
)

# readr col_types spec aligned with .ECCC_COL_NAMES order.
.ECCC_COL_TYPES <- cols(
  lon            = col_double(),
  lat            = col_double(),
  station_name   = col_character(),
  climate_id     = col_character(),
  date           = col_date(format = "%Y-%m-%d"),
  year           = col_integer(),
  month          = col_integer(),
  day            = col_integer(),
  data_quality   = col_character(),
  max_temp       = col_double(),
  max_temp_flag  = col_character(),
  min_temp       = col_double(),
  min_temp_flag  = col_character(),
  mean_temp      = col_double(),
  mean_temp_flag = col_character(),
  heat_deg_days       = col_double(),
  heat_deg_days_flag  = col_character(),
  cool_deg_days       = col_double(),
  cool_deg_days_flag  = col_character(),
  total_rain      = col_double(),
  total_rain_flag = col_character(),
  total_snow      = col_double(),
  total_snow_flag = col_character(),
  total_precip      = col_double(),
  total_precip_flag = col_character(),
  snow_on_ground      = col_double(),
  snow_on_ground_flag = col_character(),
  gust_dir      = col_double(),
  gust_dir_flag = col_character(),
  gust_speed      = col_double(),
  gust_speed_flag = col_character()
)

# Read a Weather Canada daily climate CSV from a URL or file path.
# Skips the original header row and applies .ECCC_COL_NAMES / .ECCC_COL_TYPES directly.
download_file <- function(url) {
  read_csv(
    url,
    col_names = .ECCC_COL_NAMES,
    col_types = .ECCC_COL_TYPES,
    skip      = 1,
    show_col_types = FALSE,
    locale = locale(encoding = "latin1")
  )
}

# Filter a character vector of link hrefs to QC climate daily CSV filenames only.
parse_csv_filenames <- function(links) {
  links[grepl("^climate_daily_QC_.+_\\d{4}_P1D\\.csv$", links)]
}

# Fetch the ECCC directory listing and return a data.frame with columns:
#   name          - CSV filename
#   link          - full URL to the file
#   last_modified - POSIXct timestamp from the server listing
get_csv_list <- function(base_url) {
  html <- httr::content(httr::GET(base_url), as = "text", encoding = "UTF-8")
  pattern <- 'href="(climate_daily_QC_[^"]+\\.csv)"[^>]*>[^<]*</a>\\s+(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2})'
  m <- regmatches(html, gregexpr(pattern, html, perl = TRUE))[[1]]
  if (length(m) == 0) {
    return(data.frame(name = character(), link = character(),
                      last_modified = as.POSIXct(character()), stringsAsFactors = FALSE))
  }
  name <- sub('.*href="([^"]+)".*', "\\1", m)
  ts   <- sub(paste0(".*", "(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}).*"), "\\1", m)
  data.frame(
    name          = name,
    link          = paste0(base_url, name),
    last_modified = as.POSIXct(ts, format = "%Y-%m-%d %H:%M"),
    stringsAsFactors = FALSE
  )
}

# Download CSVs described by a csv_list data.frame (as returned by get_csv_list),
# parse each file into typed observations, and persist both to a DuckDB database.
#
# Tables created / updated:
#   csv_list     - mirrors the input data.frame; gains an ingested_at column
#   observations - one row per daily observation across all files
#
# Files already on disk are not re-downloaded. Files whose name already has a
# non-NULL ingested_at in the database are not re-processed.
# Warns (does not stop) on per-file failures.
download_qc_data <- function(
  csv_list,
  db_path  = "data/meteo.duckdb",
  data_dir = "data"
) {
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  if (!dir.exists(dirname(db_path))) dir.create(dirname(db_path), recursive = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # ── csv_list table ──────────────────────────────────────────────────────────
  if (!DBI::dbExistsTable(con, "csv_list")) {
    DBI::dbWriteTable(con, "csv_list",
      cbind(csv_list, ingested_at = as.POSIXct(NA_character_)))
  } else {
    existing <- tbl(con, "csv_list") |> pull(name)
    new_rows <- csv_list[!csv_list$name %in% existing, , drop = FALSE]
    if (nrow(new_rows) > 0) {
      DBI::dbAppendTable(con, "csv_list",
        cbind(new_rows, ingested_at = as.POSIXct(NA_character_)))
    }
  }

  # Only process files not yet ingested
  ingested   <- tbl(con, "csv_list") |>
    filter(!is.na(ingested_at)) |>
    pull(name)
  to_process <- csv_list[!csv_list$name %in% ingested, , drop = FALSE]
  message(sprintf("%d fichier(s) à traiter.", nrow(to_process)))

  downloaded <- 0L
  skipped    <- 0L

  for (i in seq_len(nrow(to_process))) {
    fname <- to_process$name[i]
    dest  <- file.path(data_dir, fname)

    if (!file.exists(dest)) {
      ok <- tryCatch({
        download.file(to_process$link[i], dest, quiet = TRUE, mode = "wb")
        downloaded <- downloaded + 1L
        TRUE
      }, error = function(e) {
        warning(sprintf("Échec du téléchargement : %s\n%s", fname, conditionMessage(e)))
        FALSE
      })
      if (!ok) next
    } else {
      skipped <- skipped + 1L
    }

    df <- download_file(dest)
    if (!DBI::dbExistsTable(con, "observations")) {
      DBI::dbWriteTable(con, "observations", df)
    } else {
      DBI::dbAppendTable(con, "observations", df)
    }

    DBI::dbExecute(
      con,
      "UPDATE csv_list SET ingested_at = CURRENT_TIMESTAMP WHERE name = ?",
      params = list(fname)
    )
  }

  message(sprintf(
    "Terminé : %d téléchargé(s), %d déjà sur disque.",
    downloaded, skipped
  ))
  invisible(NULL)
}

# Fetch the current ECCC directory listing, compare it with the csv_list table
# in the database, and ingest what has changed:
#
#   - New files      : present on website, absent from the database
#   - Updated files  : already ingested, but website last_modified > ingested_at
#
# For updated files the existing observations rows are deleted and the cached
# file on disk is removed so download_qc_data re-downloads and re-ingests them.
# Warns (does not stop) on per-file failures.
update_data <- function(
  base_url = "https://dd.weather.gc.ca/today/climate/observations/daily/csv/QC/",
  db_path  = "data/meteo.duckdb",
  data_dir = "data"
) {
  message("Récupération de la liste des fichiers depuis le site...")
  website_list <- get_csv_list(base_url)
  message(sprintf("%d fichier(s) disponible(s) sur le site.", nrow(website_list)))

  new_count     <- nrow(website_list)   # conservative default when no db yet
  pending_count <- 0L
  updated_count <- 0L

  if (file.exists(db_path)) {
    con <- DBI::dbConnect(duckdb::duckdb(), db_path)

    if (DBI::dbExistsTable(con, "csv_list")) {
      db_list <- tbl(con, "csv_list") |>
        select(name, last_modified, ingested_at) |>
        collect()

      # New: filename not present in the database at all
      new_count <- sum(!website_list$name %in% db_list$name)

      # Pending: in csv_list but ingestion was never completed (e.g. interrupted)
      pending_count <- sum(db_list$name %in% website_list$name & is.na(db_list$ingested_at))

      # Updated: already ingested but the website copy is newer
      ingested <- db_list[!is.na(db_list$ingested_at), ]
      common   <- merge(
        website_list[, c("name", "last_modified")],
        ingested[, c("name", "last_modified", "ingested_at")],
        by = "name", suffixes = c("_web", "_db")
      )
      updated_rows  <- common[common$last_modified_web > common$ingested_at, ]
      updated_count <- nrow(updated_rows)

      if (updated_count > 0) {
        for (i in seq_len(updated_count)) {
          fname      <- updated_rows$name[i]
          climate_id <- sub("climate_daily_QC_(.+)_\\d{4}_P1D\\.csv$", "\\1", fname)
          year       <- as.integer(sub(
            "climate_daily_QC_.+_(\\d{4})_P1D\\.csv$", "\\1", fname))

          # Remove stale observations
          if (DBI::dbExistsTable(con, "observations")) {
            DBI::dbExecute(con,
              "DELETE FROM observations WHERE climate_id = ? AND year = ?",
              params = list(climate_id, year))
          }

          # Reset tracking: mark as not ingested, store the new last_modified
          DBI::dbExecute(con,
            "UPDATE csv_list SET ingested_at = NULL, last_modified = ? WHERE name = ?",
            params = list(updated_rows$last_modified_web[i], fname))

          # Remove cached file so download_qc_data re-fetches it
          dest <- file.path(data_dir, fname)
          if (file.exists(dest)) file.remove(dest)
        }
      }
    }

    DBI::dbDisconnect(con, shutdown = TRUE)
  }

  message(sprintf(
    "%d nouveau(x), %d en attente, %d mis à jour.",
    new_count, pending_count, updated_count))

  if (new_count == 0L && pending_count == 0L && updated_count == 0L) {
    message("Aucun changement détecté.")
    return(invisible(NULL))
  }

  download_qc_data(website_list, db_path = db_path, data_dir = data_dir)
}
