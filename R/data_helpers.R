# R/data_helpers.R

library(dplyr)
library(dbplyr)
library(DBI)
library(duckdb)
library(htmltools)

# All columns shown in the station detail table (in display order)
DISPLAY_COLS <- c(
  "date",
  "max_temp",
  "min_temp",
  "mean_temp",
  "heat_deg_days",
  "cool_deg_days",
  "total_rain",
  "total_snow",
  "total_precip",
  "snow_on_ground",
  "gust_dir",
  "gust_speed"
)

# Metric columns only (excludes date)
METRIC_COLS <- DISPLAY_COLS[DISPLAY_COLS != "date"]

# snake_case → French label mapping for table headers and chart y-axis
FRENCH_COLS <- c(
  "date" = "Date",
  "max_temp" = "Temp. max. (°C)",
  "min_temp" = "Temp. min. (°C)",
  "mean_temp" = "Temp. moy. (°C)",
  "heat_deg_days" = "Degrés-jours de chauffe (°C)",
  "cool_deg_days" = "Degrés-jours de clim. (°C)",
  "total_rain" = "Pluie totale (mm)",
  "total_snow" = "Neige totale (cm)",
  "total_precip" = "Précip. totales (mm)",
  "snow_on_ground" = "Neige au sol (cm)",
  "gust_dir" = "Dir. rafale max. (diz. °)",
  "gust_speed" = "Vitesse rafale max. (km/h)"
)

# Build HTML string for Leaflet hover tooltip.
build_tooltip_html <- function(station_name, first_obs, last_obs) {
  sprintf(
    "<b>%s</b><br>Premi\u00e8re observation : %s<br>Derni\u00e8re observation : %s",
    htmltools::htmlEscape(station_name),
    format(first_obs, "%Y-%m-%d"),
    format(last_obs,  "%Y-%m-%d")
  )
}

# Query the observations table and return a data frame:
#   climate_id, station_name, lon, lat
# Returns NULL if the table doesn't exist or is empty.
build_station_registry <- function(con) {
  if (!DBI::dbExistsTable(con, "observations")) {
    return(NULL)
  }
  result <- tbl(con, "observations") |>
    group_by(climate_id) |>
    summarise(
      station_name = min(station_name, na.rm = TRUE),
      lon          = min(lon,          na.rm = TRUE),
      lat          = min(lat,          na.rm = TRUE),
      first_obs    = min(date,         na.rm = TRUE),
      last_obs     = max(date,         na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(station_name) |>
    collect()
  if (nrow(result) == 0) return(NULL)
  result$first_obs <- as.Date(result$first_obs)
  result$last_obs  <- as.Date(result$last_obs)
  result
}

# Query yearly averages (and optional quantiles) across all stations for one metric.
# quantiles: numeric vector of percentile values in [0, 100], e.g. c(10, 25, 75, 90).
# Returns a data frame with columns: year, mean, and one column per quantile named p<value>.
# Returns NULL if the table doesn't exist or has no data.
get_yearly_stats <- function(con, metric, quantiles = numeric(0), months = 1:12) {
  if (!DBI::dbExistsTable(con, "observations")) return(NULL)

  q_cols <- if (length(quantiles) > 0) {
    paste(
      sprintf(
        ", PERCENTILE_CONT(%.10f) WITHIN GROUP (ORDER BY \"%s\") AS \"%s\"",
        quantiles / 100,
        metric,
        paste0("p", gsub("\\.", "_", as.character(quantiles)))
      ),
      collapse = ""
    )
  } else {
    ""
  }

  month_filter <- if (length(months) < 12) {
    sprintf("AND month IN (%s)", paste(as.integer(months), collapse = ", "))
  } else {
    ""
  }

  sql <- sprintf(
    "SELECT year, AVG(\"%s\") AS mean%s
     FROM observations
     WHERE \"%s\" IS NOT NULL %s
     GROUP BY year
     ORDER BY year",
    metric, q_cols, metric, month_filter
  )

  result <- DBI::dbGetQuery(con, sql)
  if (nrow(result) == 0) NULL else result
}

# Aggregate daily station data to week / month / year.
# period: one of "day", "week", "month", "year".
# gust_speed is aggregated with max(); all other metric columns with mean().
summarise_station_data <- function(data, period) {
  if (period == "day") return(data)

  safe_mean <- function(x) { v <- mean(x, na.rm = TRUE); if (is.nan(v))      NA_real_ else v }
  safe_max  <- function(x) { v <- max(x,  na.rm = TRUE); if (is.infinite(v)) NA_real_ else v }

  period_date <- switch(period,
    week  = data$date - (as.integer(format(data$date, "%u")) - 1L),  # ISO Monday
    month = as.Date(format(data$date, "%Y-%m-01")),
    year  = as.Date(format(data$date, "%Y-01-01"))
  )

  avg_cols <- setdiff(METRIC_COLS, "gust_speed")

  data |>
    mutate(date = period_date) |>
    group_by(date) |>
    summarise(
      across(all_of(avg_cols), safe_mean),
      gust_speed = safe_max(gust_speed),
      .groups = "drop"
    ) |>
    select(all_of(DISPLAY_COLS)) |>
    arrange(date)
}

# Query all observations for climate_id. Returns a data frame with DISPLAY_COLS
# sorted chronologically, or NULL if the station has no data.
load_station_data <- function(con, climate_id) {
  if (!DBI::dbExistsTable(con, "observations")) {
    return(NULL)
  }
  cid <- climate_id
  result <- tbl(con, "observations") |>
    filter(climate_id == !!cid) |>
    select(
      date,
      max_temp,
      min_temp,
      mean_temp,
      heat_deg_days,
      cool_deg_days,
      total_rain,
      total_snow,
      total_precip,
      snow_on_ground,
      gust_dir,
      gust_speed
    ) |>
    arrange(date) |>
    collect()
  if (nrow(result) == 0) {
    return(NULL)
  }
  result$date <- as.Date(result$date)
  result
}
