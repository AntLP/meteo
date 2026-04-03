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

# Metric columns only (excludes Date)
METRIC_COLS <- DISPLAY_COLS[-1]

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

# Subset shown in hover tooltip on the map
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
  header <- sprintf("<b>%s</b>", station_name)

  if (is.null(summary) || length(summary) == 0) {
    return(paste(header, "Données non disponibles", sep = "<br>"))
  }

  lines <- vapply(names(summary), function(col) {
    label <- TOOLTIP_COLS[col]
    sprintf("%s : %.1f", label, summary[[col]])
  }, character(1))

  paste(c(header, lines), collapse = "<br>")
}
