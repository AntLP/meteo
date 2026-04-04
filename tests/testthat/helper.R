# tests/testthat/helper.R

library(DBI)
library(duckdb)

# Creates an in-memory DuckDB connection with an observations table pre-populated
# for the given station and dates. Returns the open connection — caller is
# responsible for disconnecting with DBI::dbDisconnect(con, shutdown = TRUE).
make_mock_db <- function(
  climate_id   = "7017270",
  station_name = "MONTREAL",
  lon          = -73.57,
  lat          = 45.52,
  dates        = c("2026-03-30", "2026-03-31")
) {
  con <- DBI::dbConnect(duckdb::duckdb())

  rows <- lapply(dates, function(d) {
    data.frame(
      lon            = lon,
      lat            = lat,
      station_name   = station_name,
      climate_id     = climate_id,
      date           = as.Date(d),
      year           = as.integer(format(as.Date(d), "%Y")),
      month          = as.integer(format(as.Date(d), "%m")),
      day            = as.integer(format(as.Date(d), "%d")),
      data_quality       = NA_character_,
      max_temp           = 5.2,
      max_temp_flag      = NA_character_,
      min_temp           = -2.1,
      min_temp_flag      = NA_character_,
      mean_temp          = 1.5,
      mean_temp_flag     = NA_character_,
      heat_deg_days      = 16.5,
      heat_deg_days_flag = NA_character_,
      cool_deg_days      = 0.0,
      cool_deg_days_flag = NA_character_,
      total_rain         = 0.0,
      total_rain_flag    = NA_character_,
      total_snow         = 2.3,
      total_snow_flag    = NA_character_,
      total_precip       = 2.3,
      total_precip_flag  = NA_character_,
      snow_on_ground     = 10.0,
      snow_on_ground_flag = NA_character_,
      gust_dir           = 27.0,
      gust_dir_flag      = NA_character_,
      gust_speed         = 48.0,
      gust_speed_flag    = NA_character_,
      stringsAsFactors   = FALSE
    )
  })

  DBI::dbWriteTable(con, "observations", do.call(rbind, rows))
  con
}
