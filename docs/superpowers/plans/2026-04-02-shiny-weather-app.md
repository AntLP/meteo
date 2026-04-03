# Shiny Weather App Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a French-language R Shiny app that displays Quebec historical daily weather data on an interactive Leaflet map, with a station detail tab showing a Plotly chart and DT table driven by locally-downloaded ECCC CSVs.

**Architecture:** Single-file app (`app.R`) sources helpers from `R/`. A separate download script (`R/download_data.R`, launched from root `download_data.R`) fetches CSVs from `https://dd.weather.gc.ca/climate/observations/daily/csv/QC/` and stores them in `data/`. The app never makes HTTP requests — it reads only local files. Two fixed navbar tabs: Carte (map) and Données de la station (station detail with dropdown, chart, table).

**Tech Stack:** R, shiny, leaflet, leaflet.extras, plotly, DT, rvest, httr, dplyr, readr, htmltools, testthat, renv

---

## File Map

| File | Responsibility |
|---|---|
| `download_data.R` | Root launcher — sources `R/download_data.R` and calls `download_qc_data()` |
| `R/download_data.R` | Fetch directory listing, download missing CSVs, skip existing, warn on failure |
| `R/data_helpers.R` | Column constants, station registry, yesterday summary, station data loader, tooltip HTML builder |
| `R/plots.R` | Plotly line chart builder, DT table builder with column-click JS |
| `app.R` | Full Shiny app: navbarPage UI + server (map render, station reactivity, chart/table render) |
| `tests/testthat/helper.R` | Shared mock data helper used by all test files |
| `tests/testthat/test_data_helpers.R` | Tests for `R/data_helpers.R` |
| `tests/testthat/test_plots.R` | Tests for `R/plots.R` |
| `tests/testthat/test_download_data.R` | Tests for pure functions in `R/download_data.R` |

---

## Task 1: Project Scaffolding

**Files:**
- Create: `.gitignore`
- Create: `data/.gitkeep`
- Create: `tests/testthat/` (directory)

- [ ] **Step 1: Initialise git and create directory structure**

```bash
cd "/mnt/d/R/ProMétéoMedia"
git init
mkdir -p R data tests/testthat
touch data/.gitkeep
```

- [ ] **Step 2: Write `.gitignore`**

```
# R
.Rproj.user/
.Rhistory
*.Rproj
renv/library/
renv/staging/

# Data (large — do not commit)
data/*.csv

# OS
.DS_Store
Thumbs.db
```

- [ ] **Step 3: Initialise renv (interactive — run in an R session)**

```r
# Run this in an R console opened in the project directory
renv::init(bare = TRUE)
```

Then install required packages in that same R console:

```r
renv::install(c(
  "shiny", "leaflet", "leaflet.extras", "plotly", "DT",
  "rvest", "httr", "dplyr", "readr", "htmltools", "testthat"
))
renv::snapshot()
```

- [ ] **Step 4: Commit**

```bash
git add .gitignore data/.gitkeep renv.lock renv/activate.R renv/settings.json
git commit -m "chore: initialise project with renv and gitignore"
```

---

## Task 2: R/data_helpers.R — Column Constants and Tooltip Builder

**Files:**
- Create: `R/data_helpers.R`

- [ ] **Step 1: Write the constants and `build_tooltip_html` function**

```r
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
  "Max Temp (C)"               = "Temp. max. (\u00b0C)",
  "Min Temp (C)"               = "Temp. min. (\u00b0C)",
  "Mean Temp (C)"              = "Temp. moy. (\u00b0C)",
  "Heat Deg Days (C)"          = "Degr\u00e9s-jours de chauffe (\u00b0C)",
  "Cool Deg Days (C)"          = "Degr\u00e9s-jours de clim. (\u00b0C)",
  "Total Rain (mm)"            = "Pluie totale (mm)",
  "Total Snow (cm)"            = "Neige totale (cm)",
  "Total Precip (mm)"          = "Pr\u00e9cip. totales (mm)",
  "Snow on Grnd (cm)"          = "Neige au sol (cm)",
  "Dir of Max Gust (10s deg)"  = "Dir. rafale max. (diz. \u00b0)",
  "Spd of Max Gust (km/h)"     = "Vitesse rafale max. (km/h)"
)

# Subset shown in hover tooltip on the map
TOOLTIP_COLS <- c(
  "Max Temp (C)"        = "Temp. max. (\u00b0C)",
  "Min Temp (C)"        = "Temp. min. (\u00b0C)",
  "Mean Temp (C)"       = "Temp. moy. (\u00b0C)",
  "Total Precip (mm)"   = "Pr\u00e9cip. (mm)",
  "Snow on Grnd (cm)"   = "Neige au sol (cm)",
  "Spd of Max Gust (km/h)" = "Rafale max. (km/h)"
)

# Build HTML string for Leaflet hover tooltip
# summary: named numeric vector from get_yesterday_summary(), names are English col names
build_tooltip_html <- function(station_name, summary) {
  header <- sprintf("<b>%s</b>", station_name)

  if (is.null(summary) || length(summary) == 0) {
    return(paste(header, "Donn\u00e9es non disponibles", sep = "<br>"))
  }

  lines <- vapply(names(summary), function(col) {
    label <- TOOLTIP_COLS[col]
    sprintf("%s : %.1f", label, summary[[col]])
  }, character(1))

  paste(c(header, lines), collapse = "<br>")
}
```

- [ ] **Step 2: No test needed for constants — proceed to commit**

```bash
git add R/data_helpers.R
git commit -m "feat: add column constants and tooltip builder to data_helpers"
```

---

## Task 3: R/data_helpers.R — build_station_registry

**Files:**
- Modify: `R/data_helpers.R`
- Create: `tests/testthat/helper.R`
- Create: `tests/testthat/test_data_helpers.R`

- [ ] **Step 1: Write the shared test helper**

```r
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
```

- [ ] **Step 2: Write the failing test for `build_station_registry`**

```r
# tests/testthat/test_data_helpers.R

library(testthat)
source("../../R/data_helpers.R")

test_that("build_station_registry returns one row per station", {
  tmp <- make_mock_data_dir()
  on.exit(unlink(tmp, recursive = TRUE))

  registry <- build_station_registry(data_dir = tmp)

  expect_equal(nrow(registry), 1)
  expect_equal(registry$climate_id,   "7017270")
  expect_equal(registry$station_name, "MONTREAL")
  expect_equal(registry$lon,          -73.57)
  expect_equal(registry$lat,           45.52)
})

test_that("build_station_registry returns NULL for empty dir", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  expect_null(build_station_registry(data_dir = tmp))
})
```

- [ ] **Step 3: Run test — expect FAIL**

```bash
Rscript -e "testthat::test_file('tests/testthat/test_data_helpers.R')"
```

Expected: Error — `build_station_registry` not found.

- [ ] **Step 4: Implement `build_station_registry` in `R/data_helpers.R`**

Add after `build_tooltip_html`:

```r
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
    }, error = function(e) NULL)
  })

  do.call(rbind, Filter(Negate(is.null), rows))
}
```

- [ ] **Step 5: Run test — expect PASS**

```bash
Rscript -e "testthat::test_file('tests/testthat/test_data_helpers.R')"
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 2 ]`

- [ ] **Step 6: Commit**

```bash
git add R/data_helpers.R tests/testthat/helper.R tests/testthat/test_data_helpers.R
git commit -m "feat: add build_station_registry with tests"
```

---

## Task 4: R/data_helpers.R — get_yesterday_summary

**Files:**
- Modify: `R/data_helpers.R`
- Modify: `tests/testthat/test_data_helpers.R`

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test_data_helpers.R`:

```r
test_that("get_yesterday_summary returns named numeric vector for known date", {
  tmp <- make_mock_data_dir(dates = c("2026-03-31"))
  on.exit(unlink(tmp, recursive = TRUE))

  result <- get_yesterday_summary(
    climate_id = "7017270",
    data_dir   = tmp,
    ref_date   = as.Date("2026-04-01")
  )

  expect_true(is.numeric(result))
  expect_named(result, intersect(names(TOOLTIP_COLS), names(result)))
  expect_equal(unname(result["Mean Temp (C)"]), 1.5)
})

test_that("get_yesterday_summary returns NULL when date not in data", {
  tmp <- make_mock_data_dir(dates = c("2026-03-30"))
  on.exit(unlink(tmp, recursive = TRUE))

  result <- get_yesterday_summary(
    climate_id = "7017270",
    data_dir   = tmp,
    ref_date   = as.Date("2026-04-01")  # yesterday = 2026-03-31, not in data
  )

  expect_null(result)
})
```

- [ ] **Step 2: Run test — expect FAIL**

```bash
Rscript -e "testthat::test_file('tests/testthat/test_data_helpers.R')"
```

Expected: Error — `get_yesterday_summary` not found.

- [ ] **Step 3: Implement `get_yesterday_summary` in `R/data_helpers.R`**

```r
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
```

- [ ] **Step 4: Run test — expect PASS**

```bash
Rscript -e "testthat::test_file('tests/testthat/test_data_helpers.R')"
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 4 ]`

- [ ] **Step 5: Commit**

```bash
git add R/data_helpers.R tests/testthat/test_data_helpers.R
git commit -m "feat: add get_yesterday_summary with tests"
```

---

## Task 5: R/data_helpers.R — load_station_data

**Files:**
- Modify: `R/data_helpers.R`
- Modify: `tests/testthat/test_data_helpers.R`

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test_data_helpers.R`:

```r
test_that("load_station_data binds all years and returns sorted Date column", {
  tmp <- make_mock_data_dir(dates = c("2026-03-30", "2026-03-31"))
  on.exit(unlink(tmp, recursive = TRUE))

  # Add a second year CSV manually
  second_year <- make_mock_data_dir(
    climate_id = "7017270",
    dates      = c("2025-12-31")
  )
  file.copy(
    list.files(second_year, full.names = TRUE),
    tmp
  )
  unlink(second_year, recursive = TRUE)

  d <- load_station_data("7017270", data_dir = tmp)

  expect_s3_class(d[["Date/Time"]], "Date")
  expect_equal(nrow(d), 3)
  expect_true(all(diff(as.numeric(d[["Date/Time"]])) >= 0))  # sorted
})

test_that("load_station_data returns NULL for unknown climate_id", {
  tmp <- make_mock_data_dir()
  on.exit(unlink(tmp, recursive = TRUE))

  expect_null(load_station_data("9999999", data_dir = tmp))
})
```

- [ ] **Step 2: Run test — expect FAIL**

```bash
Rscript -e "testthat::test_file('tests/testthat/test_data_helpers.R')"
```

Expected: Error — `load_station_data` not found.

- [ ] **Step 3: Implement `load_station_data` in `R/data_helpers.R`**

```r
# Read and bind all yearly CSVs for a climate_id. Returns a data frame
# with Date/Time parsed as Date and rows sorted chronologically.
load_station_data <- function(climate_id, data_dir = "data") {
  pattern <- sprintf("^climate_daily_QC_%s_\\d{4}_P1D\\.csv$", climate_id)
  files   <- list.files(data_dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) return(NULL)

  parts <- lapply(files, function(f) {
    tryCatch(read_csv(f, show_col_types = FALSE), error = function(e) NULL)
  })

  d <- bind_rows(Filter(Negate(is.null), parts))
  d[["Date/Time"]] <- as.Date(d[["Date/Time"]])
  arrange(d, `Date/Time`)
}
```

- [ ] **Step 4: Run test — expect PASS**

```bash
Rscript -e "testthat::test_file('tests/testthat/test_data_helpers.R')"
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 6 ]`

- [ ] **Step 5: Commit**

```bash
git add R/data_helpers.R tests/testthat/test_data_helpers.R
git commit -m "feat: add load_station_data with tests"
```

---

## Task 6: R/plots.R — build_chart

**Files:**
- Create: `R/plots.R`
- Create: `tests/testthat/test_plots.R`

- [ ] **Step 1: Write the failing test**

```r
# tests/testthat/test_plots.R

library(testthat)
source("../../R/data_helpers.R")
source("../../R/plots.R")

test_that("build_chart returns a plotly object for a valid metric", {
  tmp <- make_mock_data_dir(dates = c("2026-03-30", "2026-03-31"))
  on.exit(unlink(tmp, recursive = TRUE))

  d     <- load_station_data("7017270", data_dir = tmp)
  chart <- build_chart(d, "Mean Temp (C)")

  expect_s3_class(chart, "plotly")
})

test_that("build_chart y-axis title is the French column name", {
  tmp <- make_mock_data_dir(dates = c("2026-03-30", "2026-03-31"))
  on.exit(unlink(tmp, recursive = TRUE))

  d     <- load_station_data("7017270", data_dir = tmp)
  chart <- build_chart(d, "Max Temp (C)")

  y_title <- chart$x$layout$yaxis$title
  expect_equal(y_title, FRENCH_COLS[["Max Temp (C)"]])
})
```

- [ ] **Step 2: Run test — expect FAIL**

```bash
Rscript -e "testthat::test_file('tests/testthat/test_plots.R')"
```

Expected: Error — `build_chart` not found.

- [ ] **Step 3: Implement `build_chart` in `R/plots.R`**

```r
# R/plots.R

library(plotly)
library(DT)

# Plotly line chart of metric_col over time for a station's data frame.
# metric_col: one of METRIC_COLS (English name).
build_chart <- function(data, metric_col) {
  y_label <- FRENCH_COLS[[metric_col]]

  plot_ly(
    data = data,
    x    = ~`Date/Time`,
    y    = as.formula(paste0("~`", metric_col, "`")),
    type = "scatter",
    mode = "lines",
    line = list(color = "#1565C0", width = 1.5),
    hovertemplate = paste0("%{x}<br>", y_label, " : %{y}<extra></extra>")
  ) |>
    layout(
      xaxis = list(title = "Date"),
      yaxis = list(title = y_label),
      hovermode = "x unified"
    )
}
```

- [ ] **Step 4: Run test — expect PASS**

```bash
Rscript -e "testthat::test_file('tests/testthat/test_plots.R')"
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 2 ]`

- [ ] **Step 5: Commit**

```bash
git add R/plots.R tests/testthat/test_plots.R
git commit -m "feat: add build_chart with tests"
```

---

## Task 7: R/plots.R — build_table

**Files:**
- Modify: `R/plots.R`
- Modify: `tests/testthat/test_plots.R`

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test_plots.R`:

```r
test_that("build_table returns a datatable object", {
  tmp <- make_mock_data_dir(dates = c("2026-03-30", "2026-03-31"))
  on.exit(unlink(tmp, recursive = TRUE))

  d     <- load_station_data("7017270", data_dir = tmp)
  tbl   <- build_table(d)

  expect_s3_class(tbl, "datatables")
})

test_that("build_table uses French column names", {
  tmp <- make_mock_data_dir(dates = c("2026-03-30", "2026-03-31"))
  on.exit(unlink(tmp, recursive = TRUE))

  d   <- load_station_data("7017270", data_dir = tmp)
  tbl <- build_table(d)

  expect_true("Temp. max. (\u00b0C)" %in% names(tbl$x$data))
  expect_false("Max Temp (C)" %in% names(tbl$x$data))
})
```

- [ ] **Step 2: Run test — expect FAIL**

```bash
Rscript -e "testthat::test_file('tests/testthat/test_plots.R')"
```

Expected: Error — `build_table` not found.

- [ ] **Step 3: Implement `build_table` in `R/plots.R`**

Append to `R/plots.R`:

```r
# DT datatable showing DISPLAY_COLS with French headers.
# A JS initComplete callback sends the clicked column index to Shiny
# via input$selected_col (1-based, 1 = first metric column after Date).
build_table <- function(data) {
  display_data <- data[, DISPLAY_COLS, drop = FALSE]
  colnames(display_data) <- unname(FRENCH_COLS[DISPLAY_COLS])

  datatable(
    display_data,
    rownames  = FALSE,
    selection = "none",
    options   = list(
      pageLength = 25,
      scrollX    = TRUE,
      language   = list(
        search      = "Rechercher :",
        lengthMenu  = "Afficher _MENU_ entr\u00e9es",
        info        = "Entr\u00e9es _START_ \u00e0 _END_ sur _TOTAL_",
        paginate    = list(previous = "Pr\u00e9c\u00e9dent", `next` = "Suivant"),
        zeroRecords = "Aucun r\u00e9sultat trouv\u00e9"
      ),
      initComplete = JS("
        function(settings, json) {
          var table = this.api();
          $(table.table().header()).find('th').on('click', function() {
            var colIdx = $(this).index();
            if (colIdx > 0) {
              Shiny.setInputValue('selected_col', colIdx, {priority: 'event'});
            }
          });
        }
      ")
    )
  )
}
```

- [ ] **Step 4: Run test — expect PASS**

```bash
Rscript -e "testthat::test_file('tests/testthat/test_plots.R')"
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 4 ]`

- [ ] **Step 5: Commit**

```bash
git add R/plots.R tests/testthat/test_plots.R
git commit -m "feat: add build_table with French headers and column-click JS"
```

---

## Task 8: R/download_data.R — get_csv_list

**Files:**
- Create: `R/download_data.R`
- Create: `tests/testthat/test_download_data.R`

- [ ] **Step 1: Write the failing test**

```r
# tests/testthat/test_download_data.R

library(testthat)
source("../../R/download_data.R")

test_that("parse_csv_filenames filters to QC climate daily pattern", {
  html_links <- c(
    "climate_daily_QC_7017270_1970_P1D.csv",
    "climate_daily_QC_7017270_2025_P1D.csv",
    "climate_daily_ON_1234567_2025_P1D.csv",   # wrong province
    "README.txt",                                # not a CSV
    "../"                                        # parent dir link
  )

  result <- parse_csv_filenames(html_links)

  expect_equal(length(result), 2)
  expect_true(all(grepl("^climate_daily_QC_.+_P1D\\.csv$", result)))
})
```

- [ ] **Step 2: Run test — expect FAIL**

```bash
Rscript -e "testthat::test_file('tests/testthat/test_download_data.R')"
```

Expected: Error — `parse_csv_filenames` not found.

- [ ] **Step 3: Implement `get_csv_list` and `parse_csv_filenames` in `R/download_data.R`**

```r
# R/download_data.R

library(rvest)
library(httr)

# Filter a character vector of link hrefs to QC climate daily CSV filenames only.
parse_csv_filenames <- function(links) {
  links[grepl("^climate_daily_QC_.+_\\d{4}_P1D\\.csv$", links)]
}

# Fetch the ECCC directory listing and return a vector of CSV filenames.
get_csv_list <- function(base_url) {
  page  <- read_html(base_url)
  links <- html_attr(html_elements(page, "a"), "href")
  parse_csv_filenames(links)
}

# Download all QC climate daily CSVs that are not already in data_dir.
# Warns (does not stop) on per-file failures.
download_qc_data <- function(
  base_url = "https://dd.weather.gc.ca/climate/observations/daily/csv/QC/",
  data_dir = "data"
) {
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

  message("R\u00e9cup\u00e9ration de la liste des fichiers...")
  csv_files <- get_csv_list(base_url)
  message(sprintf("Trouv\u00e9 %d fichier(s) CSV.", length(csv_files)))

  downloaded <- 0L
  skipped    <- 0L

  for (fname in csv_files) {
    dest <- file.path(data_dir, fname)
    if (file.exists(dest)) {
      skipped <- skipped + 1L
      next
    }

    tryCatch({
      url <- paste0(base_url, fname)
      download.file(url, dest, quiet = TRUE, mode = "wb")
      downloaded <- downloaded + 1L
    }, error = function(e) {
      warning(sprintf("\u00c9chec du t\u00e9l\u00e9chargement : %s\n%s", fname, conditionMessage(e)))
    })
  }

  message(sprintf(
    "Termin\u00e9 : %d t\u00e9l\u00e9charg\u00e9(s), %d ignor\u00e9(s) (d\u00e9j\u00e0 pr\u00e9sents).",
    downloaded, skipped
  ))
  invisible(NULL)
}
```

- [ ] **Step 4: Run test — expect PASS**

```bash
Rscript -e "testthat::test_file('tests/testthat/test_download_data.R')"
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 1 ]`

- [ ] **Step 5: Commit**

```bash
git add R/download_data.R tests/testthat/test_download_data.R
git commit -m "feat: add download script with get_csv_list and tests"
```

---

## Task 9: R/download_data.R — download_qc_data (skip logic test)

**Files:**
- Modify: `tests/testthat/test_download_data.R`

- [ ] **Step 1: Write the failing test for skip logic**

Append to `tests/testthat/test_download_data.R`:

```r
test_that("download_qc_data skips files that already exist", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  # Pre-create a file that would be downloaded
  existing_file <- file.path(tmp, "climate_daily_QC_7017270_2025_P1D.csv")
  writeLines("already here", existing_file)
  mtime_before <- file.mtime(existing_file)

  # Mock get_csv_list to return just this one filename
  local_mocked_bindings(
    get_csv_list = function(...) "climate_daily_QC_7017270_2025_P1D.csv",
    {
      suppressMessages(
        download_qc_data(
          base_url = "http://example.com/",
          data_dir = tmp
        )
      )
    }
  )

  # File was not modified — was skipped
  expect_equal(file.mtime(existing_file), mtime_before)
  expect_equal(readLines(existing_file), "already here")
})
```

- [ ] **Step 2: Run test — expect PASS** (skip logic is already implemented)

```bash
Rscript -e "testthat::test_file('tests/testthat/test_download_data.R')"
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 2 ]`

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test_download_data.R
git commit -m "test: verify download_qc_data skip logic for existing files"
```

---

## Task 10: Root download_data.R

**Files:**
- Create: `download_data.R`

- [ ] **Step 1: Write the root launcher**

```r
# download_data.R
# Launcher — sources the download logic and runs it.
# Run this script from the project root to populate data/.

source("R/download_data.R")
download_qc_data()
```

- [ ] **Step 2: Commit**

```bash
git add download_data.R
git commit -m "feat: add root download_data.R launcher"
```

---

## Task 11: app.R — Full Shiny App

**Files:**
- Create: `app.R`

This task implements the complete app in one shot: UI skeleton, map rendering with tooltips, station detail tab, and all reactive wiring.

- [ ] **Step 1: Write app.R**

```r
# app.R

library(shiny)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(DT)
library(htmltools)

source("R/data_helpers.R")
source("R/plots.R")

# ── Startup: build station registry ──────────────────────────────────────────
station_registry <- build_station_registry()
data_available   <- !is.null(station_registry) && nrow(station_registry) > 0

if (data_available) {
  station_choices <- setNames(station_registry$climate_id, station_registry$station_name)

  # Pre-compute yesterday's summaries for all stations (map tooltips)
  yesterday_summaries <- lapply(
    station_registry$climate_id,
    function(cid) get_yesterday_summary(cid)
  )
  names(yesterday_summaries) <- station_registry$climate_id

  tooltip_html <- mapply(
    function(name, cid) build_tooltip_html(name, yesterday_summaries[[cid]]),
    station_registry$station_name,
    station_registry$climate_id,
    SIMPLIFY = TRUE
  )
}

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  id    = "navbar",
  title = "M\u00e9t\u00e9o Qu\u00e9bec",

  tabPanel(
    "Carte",
    leafletOutput("map", height = "85vh")
  ),

  tabPanel(
    "Donn\u00e9es de la station",
    fluidPage(
      br(),
      selectInput(
        inputId  = "station_select",
        label    = "S\u00e9lectionner une station :",
        choices  = if (data_available) station_choices else character(0),
        width    = "400px"
      ),
      plotlyOutput("chart", height = "400px"),
      br(),
      DTOutput("table")
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Show blocking modal if no data
  observe({
    if (!data_available) {
      showModal(modalDialog(
        title  = "Donn\u00e9es manquantes",
        tagList(
          p("Aucune donn\u00e9e trouv\u00e9e."),
          p("Veuillez ex\u00e9cuter le script ", code("download_data.R"), " d'abord.")
        ),
        easyClose = FALSE,
        footer    = NULL
      ))
    }
  })

  # ── State ─────────────────────────────────────────────────────────────────
  selected_climate_id <- reactiveVal(
    if (data_available) station_registry$climate_id[[1]] else NULL
  )
  selected_metric <- reactiveVal("Mean Temp (C)")

  # ── Map ───────────────────────────────────────────────────────────────────
  output$map <- renderLeaflet({
    m <- leaflet() |>
      addTiles() |>
      setView(lng = -72, lat = 52, zoom = 5)

    if (!data_available) return(m)

    m |> addMarkers(
      data          = station_registry,
      lng           = ~lon,
      lat           = ~lat,
      layerId       = ~climate_id,
      label         = lapply(tooltip_html, HTML),
      labelOptions  = labelOptions(style = list("font-size" = "13px")),
      clusterOptions = markerClusterOptions()
    )
  })

  # Map marker click → switch tab and select station
  observeEvent(input$map_marker_click, {
    cid <- input$map_marker_click$id
    selected_climate_id(cid)
    selected_metric("Mean Temp (C)")
    updateSelectInput(session, "station_select", selected = cid)
    updateNavbarPage(session, "navbar", selected = "Donn\u00e9es de la station")
  })

  # Dropdown change → update state
  observeEvent(input$station_select, {
    selected_climate_id(input$station_select)
    selected_metric("Mean Temp (C)")
  }, ignoreInit = TRUE)

  # Column header click (from DT JS callback) → update chart metric
  observeEvent(input$selected_col, {
    col_idx <- as.integer(input$selected_col)  # 1 = first metric col
    if (col_idx >= 1 && col_idx <= length(METRIC_COLS)) {
      selected_metric(METRIC_COLS[[col_idx]])
    }
  })

  # ── Station data ──────────────────────────────────────────────────────────
  station_data <- reactive({
    req(selected_climate_id())
    load_station_data(selected_climate_id())
  })

  # ── Chart ─────────────────────────────────────────────────────────────────
  output$chart <- renderPlotly({
    d <- station_data()
    req(d)
    build_chart(d, selected_metric())
  })

  # ── Table ─────────────────────────────────────────────────────────────────
  output$table <- renderDT({
    d <- station_data()
    req(d)
    build_table(d)
  })
}

shinyApp(ui, server)
```

- [ ] **Step 2: Run the app to verify it launches without errors** (requires data/ to be populated — if data/ is empty, the modal should appear)

```bash
Rscript -e "shiny::runApp('.', launch.browser = TRUE)"
```

Expected: App opens in browser. If `data/` is empty, the modal "Données manquantes" appears. If data is present, the map shows station markers with tooltips on hover, and clicking a marker navigates to the station detail tab.

- [ ] **Step 3: Run all tests to confirm nothing is broken**

```bash
Rscript -e "testthat::test_dir('tests/testthat')"
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 9 ]`

- [ ] **Step 4: Commit**

```bash
git add app.R
git commit -m "feat: add complete Shiny app with map, station detail, and reactivity"
```

---

## Self-Review

**Spec coverage check:**

| Spec requirement | Covered by |
|---|---|
| Interactive Leaflet map of QC stations | Task 11 — `addMarkers` with cluster |
| Hover tooltip with yesterday's stats in French | Tasks 2, 4, 11 |
| Click marker → switch to Station Detail tab | Task 11 — `observeEvent(map_marker_click)` |
| Two fixed tabs (Carte / Données de la station) | Task 11 — `navbarPage` |
| Dropdown to manually select station | Task 11 — `selectInput` |
| Plotly line chart defaulting to Mean Temp | Tasks 6, 11 |
| DT table with French headers, no flag columns | Tasks 7, 11 |
| Column header click updates chart | Tasks 7, 11 — JS callback + `observeEvent(selected_col)` |
| All UI text in French | Tasks 2, 7, 11 |
| download_data.R in R/ with root launcher | Tasks 8, 9, 10 |
| Skip already-downloaded files | Tasks 8, 9 |
| Warn on download failure, continue | Task 8 |
| Empty data/ → French modal message | Task 11 |
| No data for yesterday → "Données non disponibles" | Tasks 2, 4 |
| Missing values shown as NA / chart gaps | Task 6 — Plotly handles gaps natively |

All spec requirements are covered. No placeholders remain.
