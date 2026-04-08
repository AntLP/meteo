# R/download_data.R

library(httr)
library(cli)
library(dplyr)
library(dbplyr)
library(DBI)
library(duckdb)

# Named vector mapping snake_case column names → DuckDB SQL types.
# Order matches the 31 columns in the Weather Canada daily climate CSV header.
.ECCC_COLS <- c(
  lon = "DOUBLE",
  lat = "DOUBLE",
  station_name = "VARCHAR",
  climate_id = "VARCHAR",
  date = "DATE",
  year = "INTEGER",
  month = "INTEGER",
  day = "INTEGER",
  data_quality = "VARCHAR",
  max_temp = "DOUBLE",
  max_temp_flag = "VARCHAR",
  min_temp = "DOUBLE",
  min_temp_flag = "VARCHAR",
  mean_temp = "DOUBLE",
  mean_temp_flag = "VARCHAR",
  heat_deg_days = "DOUBLE",
  heat_deg_days_flag = "VARCHAR",
  cool_deg_days = "DOUBLE",
  cool_deg_days_flag = "VARCHAR",
  total_rain = "DOUBLE",
  total_rain_flag = "VARCHAR",
  total_snow = "DOUBLE",
  total_snow_flag = "VARCHAR",
  total_precip = "DOUBLE",
  total_precip_flag = "VARCHAR",
  snow_on_ground = "DOUBLE",
  snow_on_ground_flag = "VARCHAR",
  gust_dir = "DOUBLE",
  gust_dir_flag = "VARCHAR",
  gust_speed = "DOUBLE",
  gust_speed_flag = "VARCHAR"
)

# Pre-built DuckDB columns={} spec and CREATE TABLE column list.
.ECCC_READ_CSV_COLS <- paste(
  sprintf("'%s': '%s'", names(.ECCC_COLS), .ECCC_COLS),
  collapse = ", "
)
.ECCC_CREATE_COLS <- paste(
  sprintf('"%s" %s', names(.ECCC_COLS), .ECCC_COLS),
  collapse = ", "
)

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
    return(data.frame(
      name = character(),
      link = character(),
      last_modified = as.POSIXct(character()),
      stringsAsFactors = FALSE
    ))
  }
  name <- sub('.*href="([^"]+)".*', "\\1", m)
  ts <- sub(paste0(".*", "(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}).*"), "\\1", m)
  data.frame(
    name = name,
    link = paste0(base_url, name),
    last_modified = as.POSIXct(ts, format = "%Y-%m-%d %H:%M"),
    stringsAsFactors = FALSE
  )
}

# Download CSVs described by a csv_list data.frame (as returned by get_csv_list)
# and ingest them directly into DuckDB without loading into R memory.
#
# Downloads are parallelised via curl::multi_download. Ingestion uses DuckDB's
# native read_csv() so data flows file → DuckDB with no R data.frame round-trip.
#
# Tables created / updated:
#   csv_list     - mirrors the input data.frame; gains an ingested_at column
#   observations - one row per daily observation across all files
#
# Files already on disk are not re-downloaded. Files already ingested are skipped.
# Warns (does not stop) on per-file failures.
download_qc_data <- function(
  csv_list,
  db_path = "data/meteo.duckdb",
  data_dir = "data"
) {
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }
  if (!dir.exists(dirname(db_path))) {
    dir.create(dirname(db_path), recursive = TRUE)
  }

  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # ── csv_list table ──────────────────────────────────────────────────────────
  if (!DBI::dbExistsTable(con, "csv_list")) {
    DBI::dbWriteTable(
      con,
      "csv_list",
      cbind(csv_list, ingested_at = as.POSIXct(NA_character_))
    )
  } else {
    existing <- tbl(con, "csv_list") |> pull(name)
    new_rows <- csv_list[!csv_list$name %in% existing, , drop = FALSE]
    if (nrow(new_rows) > 0) {
      DBI::dbAppendTable(
        con,
        "csv_list",
        cbind(new_rows, ingested_at = as.POSIXct(NA_character_))
      )
    }
  }

  # Only process files not yet ingested
  ingested <- tbl(con, "csv_list") |>
    filter(!is.na(ingested_at)) |>
    pull(name)
  to_process <- csv_list[!csv_list$name %in% ingested, , drop = FALSE]
  message(sprintf("%d fichier(s) à traiter.", nrow(to_process)))

  if (nrow(to_process) == 0L) {
    return(invisible(NULL))
  }

  # ── Download ─────────────────────────────────────────────────────────────────
  dests <- file.path(data_dir, to_process$name)
  need_dl <- !file.exists(dests)

  downloaded <- 0L
  failed     <- 0L
  skipped    <- sum(!need_dl)

  if (any(need_dl)) {
    cli::cli_progress_bar(
      "Téléchargement",
      total = sum(need_dl),
      format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} | {cli::pb_eta_str} | {cli::pb_status}"
    )
    for (i in which(need_dl)) {
      cli::cli_progress_update(status = to_process$name[i])
      ok <- tryCatch(
        {
          download.file(to_process$link[i], dests[i], quiet = TRUE, mode = "wb")
          TRUE
        },
        error = function(e) {
          warning(sprintf(
            "Échec du téléchargement : %s\n%s",
            to_process$name[i],
            conditionMessage(e)
          ))
          FALSE
        }
      )
      if (ok) {
        downloaded <- downloaded + 1L
      } else {
        failed <- failed + 1L
        if (file.exists(dests[i])) file.remove(dests[i])
      }
    }
    cli::cli_progress_done()
  }

  if (failed > 0) {
    cli::cli_alert_warning(
      "{downloaded} t\u00e9l\u00e9charg\u00e9(s), {failed} \u00e9chec(s), {skipped} d\u00e9j\u00e0 sur disque."
    )
  } else {
    cli::cli_alert_success(
      "{downloaded} t\u00e9l\u00e9charg\u00e9(s), {skipped} d\u00e9j\u00e0 sur disque."
    )
  }

  # ── Ensure observations table exists ────────────────────────────────────────
  if (!DBI::dbExistsTable(con, "observations")) {
    DBI::dbExecute(
      con,
      sprintf("CREATE TABLE observations (%s)", .ECCC_CREATE_COLS)
    )
  }

  # ── Ingest directly from CSV into DuckDB ────────────────────────────────────
  cli::cli_progress_bar(
    "Ingestion",
    total = nrow(to_process),
    format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} | {cli::pb_eta_str} | {cli::pb_status}"
  )
  for (i in seq_len(nrow(to_process))) {
    fname <- to_process$name[i]
    dest <- normalizePath(dests[i], winslash = "/", mustWork = FALSE)

    cli::cli_progress_update(status = fname)

    if (!file.exists(dest)) {
      next
    } # download failed

    ok <- tryCatch(
      {
        DBI::dbExecute(
          con,
          sprintf(
            "INSERT INTO observations
              SELECT * FROM read_csv('%s',
                header    = false,
                skip      = 1,
                columns   = {%s},
                dateformat = '%%Y-%%m-%%d',
                encoding  = 'latin-1'
         )",
            dest,
            .ECCC_READ_CSV_COLS
          )
        )
        TRUE
      },
      error = function(e) {
        warning(sprintf(
          "Échec de l'ingestion : %s\n%s",
          fname,
          conditionMessage(e)
        ))
        FALSE
      }
    )

    if (ok) {
      DBI::dbExecute(
        con,
        "UPDATE csv_list SET ingested_at = CURRENT_TIMESTAMP WHERE name = ?",
        params = list(fname)
      )
      file.remove(dests[i])
    }
  }
  cli::cli_progress_done()

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
  db_path = "data/meteo.duckdb",
  data_dir = "data"
) {
  message("Récupération de la liste des fichiers depuis le site...")
  website_list <- get_csv_list(base_url)
  message(sprintf(
    "%d fichier(s) disponible(s) sur le site.",
    nrow(website_list)
  ))

  new_count <- nrow(website_list) # conservative default when no db yet
  pending_count <- 0L
  updated_count <- 0L

  if (file.exists(db_path)) {
    con <- tryCatch(
      DBI::dbConnect(duckdb::duckdb(), db_path),
      error = function(e) {
        warning(sprintf(
          "Impossible d'ouvrir la base de données, tous les fichiers seront traités : %s",
          conditionMessage(e)
        ))
        NULL
      }
    )

    if (!is.null(con)) {
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

      if (DBI::dbExistsTable(con, "csv_list")) {
        db_list <- tbl(con, "csv_list") |>
          select(name, last_modified, ingested_at) |>
          collect()

        # New: filename not present in the database at all
        new_count <- sum(!website_list$name %in% db_list$name)

        # Pending: in csv_list but ingestion was never completed (e.g. interrupted)
        pending_count <- sum(
          db_list$name %in% website_list$name & is.na(db_list$ingested_at)
        )

        # Updated: already ingested but the website copy is newer
        ingested <- db_list[!is.na(db_list$ingested_at), ]
        common <- merge(
          website_list[, c("name", "last_modified")],
          ingested[, c("name", "last_modified", "ingested_at")],
          by = "name",
          suffixes = c("_web", "_db")
        )
        updated_rows <- common[common$last_modified_web > common$ingested_at, ]
        updated_count <- nrow(updated_rows)

        if (updated_count > 0) {
          for (i in seq_len(updated_count)) {
            fname <- updated_rows$name[i]
            climate_id <- sub(
              "climate_daily_QC_(.+)_\\d{4}_P1D\\.csv$",
              "\\1",
              fname
            )
            year <- as.integer(sub(
              "climate_daily_QC_.+_(\\d{4})_P1D\\.csv$",
              "\\1",
              fname
            ))

            # Remove stale observations
            if (DBI::dbExistsTable(con, "observations")) {
              DBI::dbExecute(
                con,
                "DELETE FROM observations WHERE climate_id = ? AND year = ?",
                params = list(climate_id, year)
              )
            }

            # Reset tracking: mark as not ingested, store the new last_modified
            DBI::dbExecute(
              con,
              "UPDATE csv_list SET ingested_at = NULL, last_modified = ? WHERE name = ?",
              params = list(updated_rows$last_modified_web[i], fname)
            )

            # Remove cached file so download_qc_data re-fetches it
            dest <- file.path(data_dir, fname)
            if (file.exists(dest)) file.remove(dest)
          }
        }
      }
    }
  }

  message(sprintf(
    "%d nouveau(x), %d en attente, %d mis à jour.",
    new_count,
    pending_count,
    updated_count
  ))

  if (new_count == 0L && pending_count == 0L && updated_count == 0L) {
    message("Aucun changement détecté.")
    return(invisible(NULL))
  }

  download_qc_data(website_list, db_path = db_path, data_dir = data_dir)
}
