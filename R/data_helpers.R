# R/data_helpers.R

library(dplyr)
library(readr)

# All columns shown in the station detail table (in display order)
DISPLAY_COLS <- c(
  "Date/Time",
  "Max Temp (C)",
  "Min Temp (C)",
  "Mean Temp (C)",
  "Heat Deg Days (C)",
  "Cool Deg Days (C)",
  "Total Rain (mm)",
  "Total Snow (cm)",
  "Total Precip (mm)",
  "Snow on Grnd (cm)",
  "Dir of Max Gust (10s deg)",
  "Spd of Max Gust (km/h)"
)

# Metric columns only (excludes Date/Time)
METRIC_COLS <- DISPLAY_COLS[DISPLAY_COLS != "Date/Time"]

# English → French label mapping for table headers and chart y-axis
FRENCH_COLS <- c(
  "Date/Time"                  = "Date",
  "Max Temp (C)"               = "Temp. max. (°C)",
  "Min Temp (C)"               = "Temp. min. (°C)",
  "Mean Temp (C)"              = "Temp. moy. (°C)",
  "Heat Deg Days (C)"          = "Degrés-jours de chauffe (°C)",
  "Cool Deg Days (C)"          = "Degrés-jours de clim. (°C)",
  "Total Rain (mm)"            = "Pluie totale (mm)",
  "Total Snow (cm)"            = "Neige totale (cm)",
  "Total Precip (mm)"          = "Précip. totales (mm)",
  "Snow on Grnd (cm)"          = "Neige au sol (cm)",
  "Dir of Max Gust (10s deg)"  = "Dir. rafale max. (diz. °)",
  "Spd of Max Gust (km/h)"     = "Vitesse rafale max. (km/h)"
)

# Subset shown in hover tooltip on the map.
# Note: "Total Precip (mm)" uses a shorter French label here than in FRENCH_COLS
# to keep tooltip compact.
TOOLTIP_COLS <- c(
  "Max Temp (C)"        = "Temp. max. (°C)",
  "Min Temp (C)"        = "Temp. min. (°C)",
  "Mean Temp (C)"       = "Temp. moy. (°C)",
  "Total Precip (mm)"   = "Précip. (mm)",
  "Snow on Grnd (cm)"   = "Neige au sol (cm)",
  "Spd of Max Gust (km/h)" = "Rafale max. (km/h)"
)

# Build HTML string for Leaflet hover tooltip
# summary: named numeric vector from get_yesterday_summary(), names are English col names
build_tooltip_html <- function(station_name, summary) {
  header <- sprintf("<b>%s</b>", htmltools::htmlEscape(station_name))

  if (is.null(summary) || length(summary) == 0) {
    return(paste(header, "Donn\u00e9es non disponibles", sep = "<br>"))
  }

  # Only show columns defined in TOOLTIP_COLS, in TOOLTIP_COLS order
  cols_to_show <- intersect(names(TOOLTIP_COLS), names(summary))
  lines <- vapply(cols_to_show, function(col) {
    label <- TOOLTIP_COLS[[col]]
    sprintf("%s : %.1f", label, summary[[col]])
  }, character(1))

  paste(c(header, lines), collapse = "<br>")
}

# Scan data_dir, read first row of each station's most recent CSV,
# return data frame: climate_id, station_name, lon, lat
build_station_registry <- function(data_dir = "data") {
  files <- list.files(
    data_dir,
    pattern   = "^climate_daily_QC_.+_\\d{4}_P1D\\.csv$",
    full.names = TRUE
  )

  if (length(files) == 0) return(NULL)

  file_info <- data.frame(
    path       = files,
    climate_id = sub(".*climate_daily_QC_(.+)_\\d{4}_P1D\\.csv$", "\\1", basename(files)),
    year       = as.integer(sub(".*climate_daily_QC_.+_(\\d{4})_P1D\\.csv$", "\\1", basename(files))),
    stringsAsFactors = FALSE
  )

  most_recent <- file_info |>
    group_by(climate_id) |>
    slice_max(year, n = 1, with_ties = FALSE) |>
    ungroup()

  rows <- lapply(seq_len(nrow(most_recent)), function(i) {
    tryCatch({
      d <- read_csv(most_recent$path[i], n_max = 1, show_col_types = FALSE)
      data.frame(
        climate_id   = most_recent$climate_id[i],
        station_name = d[["Station Name"]],
        lon          = d[["Longitude (x)"]],
        lat          = d[["Latitude (y)"]],
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      warning(sprintf("Impossible de lire le fichier %s : %s",
                      most_recent$path[i], conditionMessage(e)))
      NULL
    })
  })

  do.call(rbind, Filter(Negate(is.null), rows))
}

# Return a named numeric vector of non-NA TOOLTIP_COLS values for yesterday.
# ref_date is injectable for testing (defaults to today).
get_yesterday_summary <- function(climate_id, data_dir = "data", ref_date = Sys.Date()) {
  yesterday <- as.character(ref_date - 1)
  year      <- format(ref_date - 1, "%Y")

  pattern <- sprintf("^climate_daily_QC_%s_%s_P1D\\.csv$", climate_id, year)
  files   <- list.files(data_dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) return(NULL)

  d   <- read_csv(files[[1]], show_col_types = FALSE)
  row <- d[d[["Date/Time"]] == yesterday, ]

  if (nrow(row) == 0) return(NULL)

  vals <- vapply(names(TOOLTIP_COLS), function(col) {
    v <- row[[col]]
    if (length(v) == 0 || is.na(v)) NA_real_ else as.numeric(v)
  }, numeric(1))

  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(NULL)
  vals
}
