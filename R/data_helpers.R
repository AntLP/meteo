# R/data_helpers.R

library(dplyr)
library(dbplyr)
library(DBI)
library(duckdb)
library(htmltools)

# All columns shown in the station detail table (in display order)
DISPLAY_COLS <- c(
  "date",
  "max_temp", "min_temp", "mean_temp",
  "heat_deg_days", "cool_deg_days",
  "total_rain", "total_snow", "total_precip",
  "snow_on_ground", "gust_dir", "gust_speed"
)

# Metric columns only (excludes date)
METRIC_COLS <- DISPLAY_COLS[DISPLAY_COLS != "date"]

# snake_case → French label mapping for table headers and chart y-axis
FRENCH_COLS <- c(
  "date"           = "Date",
  "max_temp"       = "Temp. max. (°C)",
  "min_temp"       = "Temp. min. (°C)",
  "mean_temp"      = "Temp. moy. (°C)",
  "heat_deg_days"  = "Degrés-jours de chauffe (°C)",
  "cool_deg_days"  = "Degrés-jours de clim. (°C)",
  "total_rain"     = "Pluie totale (mm)",
  "total_snow"     = "Neige totale (cm)",
  "total_precip"   = "Précip. totales (mm)",
  "snow_on_ground" = "Neige au sol (cm)",
  "gust_dir"       = "Dir. rafale max. (diz. °)",
  "gust_speed"     = "Vitesse rafale max. (km/h)"
)

# Subset shown in hover tooltip on the map.
TOOLTIP_COLS <- c(
  "max_temp"       = "Temp. max. (°C)",
  "min_temp"       = "Temp. min. (°C)",
  "mean_temp"      = "Temp. moy. (°C)",
  "total_precip"   = "Précip. (mm)",
  "snow_on_ground" = "Neige au sol (cm)",
  "gust_speed"     = "Rafale max. (km/h)"
)

# Build HTML string for Leaflet hover tooltip.
# summary: named numeric vector from get_yesterday_summary(), names are snake_case col names.
build_tooltip_html <- function(station_name, summary) {
  header <- sprintf("<b>%s</b>", htmltools::htmlEscape(station_name))

  if (is.null(summary) || length(summary) == 0) {
    return(paste(header, "Donn\u00e9es non disponibles", sep = "<br>"))
  }

  cols_to_show <- intersect(names(TOOLTIP_COLS), names(summary))
  lines <- vapply(
    cols_to_show,
    function(col) {
      label <- TOOLTIP_COLS[[col]]
      sprintf("%s : %.1f", label, summary[[col]])
    },
    character(1)
  )

  paste(c(header, lines), collapse = "<br>")
}

# Query the observations table and return a data frame:
#   climate_id, station_name, lon, lat
# Returns NULL if the table doesn't exist or is empty.
build_station_registry <- function(con) {
  if (!DBI::dbExistsTable(con, "observations")) return(NULL)
  result <- tbl(con, "observations") |>
    group_by(climate_id) |>
    summarise(
      station_name = min(station_name, na.rm = TRUE),
      lon          = min(lon, na.rm = TRUE),
      lat          = min(lat, na.rm = TRUE),
      .groups      = "drop"
    ) |>
    arrange(station_name) |>
    collect()
  if (nrow(result) == 0) NULL else result
}

# Return a named numeric vector of non-NA TOOLTIP_COLS values for yesterday.
# ref_date is injectable for testing (defaults to today).
get_yesterday_summary <- function(con, climate_id, ref_date = Sys.Date()) {
  yesterday <- as.Date(ref_date - 1)
  cid <- climate_id
  row <- tbl(con, "observations") |>
    filter(climate_id == !!cid, date == !!yesterday) |>
    select(max_temp, min_temp, mean_temp, total_precip, snow_on_ground, gust_speed) |>
    collect()
  if (nrow(row) == 0) return(NULL)
  vals <- unlist(row[1, ])
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) NULL else vals
}

# Query all observations for climate_id. Returns a data frame with DISPLAY_COLS
# sorted chronologically, or NULL if the station has no data.
load_station_data <- function(con, climate_id) {
  if (!DBI::dbExistsTable(con, "observations")) return(NULL)
  cid <- climate_id
  result <- tbl(con, "observations") |>
    filter(climate_id == !!cid) |>
    select(date, max_temp, min_temp, mean_temp,
           heat_deg_days, cool_deg_days,
           total_rain, total_snow, total_precip,
           snow_on_ground, gust_dir, gust_speed) |>
    arrange(date) |>
    collect()
  if (nrow(result) == 0) return(NULL)
  result$date <- as.Date(result$date)
  result
}
