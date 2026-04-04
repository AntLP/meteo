# R/raster_helpers.R

library(terra)
library(dplyr)
library(dbplyr)
library(DBI)

# Query the mean of a metric per station over a date window.
# Returns a data.frame: climate_id, lon, lat, value  (rows with NA value dropped).
get_station_metric <- function(con, metric, date_from, date_to) {
  col <- sym(metric)
  tbl(con, "observations") |>
    filter(date >= !!date_from, date <= !!date_to) |>
    group_by(climate_id, lon, lat) |>
    summarise(value = mean(!!col, na.rm = TRUE), .groups = "drop") |>
    filter(!is.na(value)) |>
    collect()
}

# Build a Gaussian-kernel-smoothed SpatRaster (EPSG:4326) from station values.
#
# For each grid cell the output is a weighted mean of all station values, where
# weights decay as exp(-d² / 2σ²) and σ = bandwidth (degrees latitude).
# Longitude bandwidth is adjusted for the latitude so the kernel is isotropic
# in real space.  Cells further than ~3 σ from every station are set to NA.
#
# bandwidth  : smoothing radius in degrees lat (~111 km / degree)
# resolution : output grid cell size in degrees
#
# Returns a single-layer SpatRaster, or NULL when fewer than 3 stations
# have non-NA data for the requested period.
build_metric_raster <- function(
  con,
  metric,
  date_from,
  date_to,
  bandwidth = 1.5,
  resolution = 0.25
) {
  stations <- get_station_metric(con, metric, date_from, date_to)
  if (nrow(stations) < 3) {
    return(NULL)
  }

  # Grid extent: station bounds padded by 2× bandwidth
  pad <- 2 * bandwidth
  lon_min <- floor(min(stations$lon) - pad)
  lon_max <- ceiling(max(stations$lon) + pad)
  lat_min <- floor(min(stations$lat) - pad)
  lat_max <- ceiling(max(stations$lat) + pad)

  r <- terra::rast(
    xmin = lon_min,
    xmax = lon_max,
    ymin = lat_min,
    ymax = lat_max,
    resolution = resolution,
    crs = "EPSG:4326"
  )

  xy <- terra::xyFromCell(r, seq_len(terra::ncell(r)))
  gx <- xy[, 1]
  gy <- xy[, 2]

  sx <- stations$lon
  sy <- stations$lat
  sv <- stations$value

  # Isotropic kernel in real space: correct lon bandwidth for latitude
  lat_mid <- (lat_min + lat_max) / 2
  bw_lon <- bandwidth / cos(lat_mid * pi / 180)
  bw_lat <- bandwidth

  # Weight matrix (ncell × nstation)
  dlon <- outer(gx, sx, `-`)
  dlat <- outer(gy, sy, `-`)
  w <- exp(-(dlon^2 / (2 * bw_lon^2) + dlat^2 / (2 * bw_lat^2)))

  # Weighted mean; mask cells with no station within ~3 bandwidths
  vals <- rowSums(sweep(w, 2, sv, `*`)) / rowSums(w)
  vals[apply(w, 1, max) < exp(-4.5)] <- NA_real_

  terra::values(r) <- vals
  r
}
