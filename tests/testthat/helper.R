# tests/testthat/helper.R

library(readr)

# Creates a temp data dir with one CSV file for the given station.
# Returns the path to the temp dir.
make_mock_data_dir <- function(
  climate_id   = "7017270",
  station_name = "MONTREAL",
  lon          = -73.57,
  lat          = 45.52,
  dates        = c("2026-03-30", "2026-03-31")
) {
  tmp  <- tempfile()
  dir.create(tmp)
  year <- format(as.Date(dates[[1]]), "%Y")

  rows <- lapply(dates, function(d) {
    data.frame(
      "Longitude (x)"            = lon,
      "Latitude (y)"             = lat,
      "Station Name"             = station_name,
      "Climate ID"               = climate_id,
      "Date/Time"                = d,
      "Year"                     = as.integer(format(as.Date(d), "%Y")),
      "Month"                    = as.integer(format(as.Date(d), "%m")),
      "Day"                      = as.integer(format(as.Date(d), "%d")),
      "Data Quality"             = NA_character_,
      "Max Temp (C)"             = 5.2,
      "Max Temp Flag"            = NA_character_,
      "Min Temp (C)"             = -2.1,
      "Min Temp Flag"            = NA_character_,
      "Mean Temp (C)"            = 1.5,
      "Mean Temp Flag"           = NA_character_,
      "Heat Deg Days (C)"        = 16.5,
      "Heat Deg Days Flag"       = NA_character_,
      "Cool Deg Days (C)"        = 0.0,
      "Cool Deg Days Flag"       = NA_character_,
      "Total Rain (mm)"          = 0.0,
      "Total Rain Flag"          = NA_character_,
      "Total Snow (cm)"          = 2.3,
      "Total Snow Flag"          = NA_character_,
      "Total Precip (mm)"        = 2.3,
      "Total Precip Flag"        = NA_character_,
      "Snow on Grnd (cm)"        = 10.0,
      "Snow on Grnd Flag"        = NA_character_,
      "Dir of Max Gust (10s deg)"= 27.0,
      "Dir of Max Gust Flag"     = NA_character_,
      "Spd of Max Gust (km/h)"   = 48.0,
      "Spd of Max Gust Flag"     = NA_character_,
      check.names      = FALSE,
      stringsAsFactors = FALSE
    )
  })

  df    <- do.call(rbind, rows)
  fname <- sprintf("climate_daily_QC_%s_%s_P1D.csv", climate_id, year)
  write_csv(df, file.path(tmp, fname))
  tmp
}
