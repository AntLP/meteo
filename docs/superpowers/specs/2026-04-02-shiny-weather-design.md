# Shiny Weather App — Design Spec
**Date:** 2026-04-02
**Scope:** Quebec historical daily weather observations from dd.weather.gc.ca

---

## Overview

A single-file R Shiny app that displays historical daily weather data for Quebec weather stations. All user-facing text is in French. Data is fetched separately via a download script and stored locally; the app reads only from local files.

---

## File Layout

```
ProMétéoMedia/
├── download_data.R        # Entry point — sources R/download_data.R
├── app.R                  # Shiny app (single-file)
├── R/
│   ├── download_data.R    # Download logic
│   ├── data_helpers.R     # Data loading, station registry, summaries
│   └── plots.R            # Chart rendering (plotly)
└── data/                  # Local CSV storage (populated by download_data.R)
```

---

## Data Source

- **URL:** `https://dd.weather.gc.ca/climate/observations/daily/csv/QC/`
- **File pattern:** `climate_daily_QC_[CLIMATE_ID]_[YEAR]_P1D.csv`
- **Key columns:** Longitude (x), Latitude (y), Station Name, Climate ID, Date/Time, Year, Month, Day, Max Temp (C), Min Temp (C), Mean Temp (C), Heat Deg Days (C), Cool Deg Days (C), Total Rain (mm), Total Snow (cm), Total Precip (mm), Snow on Grnd (cm), Dir of Max Gust (10s deg), Spd of Max Gust (km/h)
- Flag columns (one per measurement) are excluded from user-facing display.

---

## Architecture

### 1. Data Layer — `download_data.R` (root) + `R/download_data.R`

The root `download_data.R` is a minimal launcher that sources `R/download_data.R`.

`R/download_data.R` is responsible for:
- Fetching the directory listing from the ECCC URL
- Parsing all CSV filenames matching the QC pattern
- Downloading missing files to `data/` (skipping already-present files)
- Logging a warning per failed download without aborting the full run

### 2. App Layer — `app.R`

Single-file Shiny app using `navbarPage` with two fixed tabs:

- **Carte** (Map tab)
- **Données de la station** (Station Detail tab)

At startup, `R/data_helpers.R` scans `data/`, reads the first row of each station's most recent CSV to build a station registry (name, climate ID, lat/lon) and computes yesterday's summary for each station.

---

## Tab 1 — Carte

An interactive Leaflet map of Quebec showing all weather stations.

- Markers clustered when zoomed out, expanding on zoom
- **Hover tooltip:** Station name + yesterday's available statistics (Max/Min/Mean Temp, Total Precip, Snow on Grnd, Spd of Max Gust). Only non-missing values are shown. Label: "Données non disponibles" if nothing is available.
- **Click:** Loads the clicked station into the Station Detail tab and switches to it automatically

All tooltip labels in French (e.g., "Temp. max.", "Précip. totales", "Rafale max.").

---

## Tab 2 — Données de la station

Always visible. Allows manual or map-driven station selection.

### Layout (top to bottom)

1. **Dropdown selector** (`selectInput`): Lists all Quebec stations by name in French. Label: "Sélectionner une station". Updated automatically when a map marker is clicked.

2. **Graphique** (Chart): `plotly` line chart showing the selected metric over the full locally-available history for the station. Defaults to Mean Temp (°C) on first load. X-axis: date. Y-axis: metric value with unit.

3. **Tableau** (Table): `DT` datatable showing all daily observations for the station. Displays the 16 weather variables (flag columns excluded). Sortable and scrollable. **Clicking a column header** updates the chart above to show that variable's history.

All column headers and UI labels in French.

---

## Data Flow

1. User runs `download_data.R` (root) to populate `data/`.
2. App starts → `data_helpers.R` builds station registry from local CSVs.
3. Map renders all station markers.
4. User hovers a marker → tooltip shows yesterday's summary.
5. User clicks a marker OR selects from dropdown → app reads all CSVs for that climate ID, binds into a single data frame, renders chart (Mean Temp default) and table.
6. User clicks a column header in the table → chart re-renders for that metric.

---

## Error Handling

| Situation | Behaviour |
|---|---|
| `data/` empty or missing | App shows French message: "Aucune donnée trouvée. Veuillez exécuter le script `download_data.R` d'abord." |
| No data for yesterday at a station | Hover tooltip shows "Données non disponibles" |
| CSV download fails | Warning logged with filename; download continues for remaining files |
| Missing values in data | Shown as `NA` in table; gaps in line chart (no crash) |

---

## Key Packages

| Package | Purpose |
|---|---|
| `shiny` | App framework |
| `leaflet` | Interactive map |
| `plotly` | Interactive line chart |
| `DT` | Interactive table |
| `rvest` or `httr` | Scraping directory listing for download script |
| `dplyr` / `readr` | Data wrangling |

---

## Constraints & Decisions

- **French only:** All user-facing text (labels, tooltips, tab names, column headers, messages) must be in French.
- **Local data only:** The Shiny app never makes HTTP requests — all data must be pre-downloaded.
- **QC only (v1):** Scope is limited to Quebec stations. Other provinces can be added later.
- **Single-file app:** `app.R` contains UI and server. Helper logic lives in `R/` scripts sourced at startup.
- **No flag columns:** Data quality flag columns are hidden from the user.
