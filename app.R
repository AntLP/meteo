# app.R

library(shiny)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(terra)
library(stars)
library(plotly)
library(DT)
library(htmltools)
library(DBI)
library(duckdb)

source("R/data_helpers.R")
source("R/plots.R")
source("R/raster_helpers.R")

# ── Database connection ───────────────────────────────────────────────────────
db_path <- "data/meteo.duckdb"
con <- if (file.exists(db_path)) {
  tryCatch(
    DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE),
    error = function(e) NULL
  )
} else {
  NULL
}

shiny::onStop(function() {
  if (!is.null(con)) DBI::dbDisconnect(con, shutdown = TRUE)
})

# ── Startup: build station registry ──────────────────────────────────────────
station_registry <- if (!is.null(con)) build_station_registry(con) else NULL
data_available <- !is.null(station_registry) && nrow(station_registry) > 0

if (data_available) {
  station_choices <- setNames(
    station_registry$climate_id,
    station_registry$station_name
  )

  tooltip_html <- build_tooltip_html(
    station_registry$station_name,
    station_registry$first_obs,
    station_registry$last_obs
  )
}

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  id = "navbar",
  title = "Météo Québec",

  tabPanel(
    "Carte",
    leafletOutput("map", height = "85vh")
  ),

  tabPanel(
    "Données de la station",
    fluidPage(
      br(),
      selectInput(
        inputId = "station_select",
        label = "Sélectionner une station :",
        choices = if (data_available) station_choices else character(0),
        width = "400px"
      ),
      plotlyOutput("chart", height = "400px"),
      br(),
      DTOutput("table")
    )
  ),

  tabPanel(
    "Tendances annuelles",
    fluidPage(
      br(),
      fluidRow(
        column(
          width = 3,
          selectInput(
            inputId = "yearly_metric",
            label = "Indicateur :",
            choices = setNames(METRIC_COLS, FRENCH_COLS[METRIC_COLS]),
            selected = "mean_temp"
          ),
          selectizeInput(
            inputId = "yearly_quantiles",
            label = "Quantiles (%) :",
            choices = c(5, 10, 25, 75, 90, 95),
            selected = NULL,
            multiple = TRUE,
            options = list(
              create = TRUE,
              createFilter = "^(100|[0-9]{1,2})(\\.[0-9]+)?$",
              placeholder = "Ajouter un quantile\u2026"
            )
          )
        ),
        column(
          width = 9,
          plotlyOutput("yearly_chart", height = "65vh")
        )
      )
    )
  ),

  tabPanel(
    "Carte raster",
    fluidPage(
      br(),
      fluidRow(
        column(
          width = 3,
          selectInput(
            inputId = "raster_metric",
            label = "Indicateur :",
            choices = setNames(METRIC_COLS, FRENCH_COLS[METRIC_COLS]),
            selected = "mean_temp"
          ),
          radioButtons(
            inputId = "raster_date_mode",
            label = "Période :",
            choices = c("Date unique" = "single", "Intervalle" = "range"),
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.raster_date_mode == 'single'",
            dateInput(
              inputId = "raster_date",
              label = "Date :",
              value = Sys.Date() - 1
            )
          ),
          conditionalPanel(
            condition = "input.raster_date_mode == 'range'",
            dateRangeInput(
              inputId = "raster_date_range",
              label = "Intervalle :",
              start = Sys.Date() - 30,
              end = Sys.Date() - 1
            )
          ),
          sliderInput(
            inputId = "raster_bandwidth",
            label = "Lissage (degrés) :",
            min = 0.5,
            max = 5,
            value = 1.5,
            step = 0.25
          )
        ),
        column(
          width = 9,
          leafletOutput("raster_map", height = "75vh")
        )
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  # Show blocking modal if no data
  observe({
    if (!data_available) {
      showModal(modalDialog(
        title = "Données manquantes",
        tagList(
          p("Aucune donnée trouvée."),
          p(
            "Veuillez exécuter le script ",
            code("download_data.R"),
            " d'abord."
          )
        ),
        easyClose = FALSE,
        footer = NULL
      ))
    }
  })

  # ── State ─────────────────────────────────────────────────────────────────
  selected_climate_id <- reactiveVal(
    if (data_available) station_registry$climate_id[[1]] else NULL
  )
  selected_metric <- reactiveVal("mean_temp")

  # ── Map ───────────────────────────────────────────────────────────────────
  output$map <- renderLeaflet({
    m <- leaflet() |>
      addTiles() |>
      setView(lng = -72, lat = 52, zoom = 5)

    if (!data_available) {
      return(m)
    }

    m |>
      addMarkers(
        data = station_registry,
        lng = ~lon,
        lat = ~lat,
        layerId = ~climate_id,
        label = lapply(tooltip_html, HTML),
        labelOptions = labelOptions(style = list("font-size" = "13px")),
        clusterOptions = markerClusterOptions()
      )
  })

  # Map marker click → switch tab and select station
  observeEvent(input$map_marker_click, {
    cid <- input$map_marker_click$id
    selected_climate_id(cid)
    selected_metric("mean_temp")
    updateSelectInput(session, "station_select", selected = cid)
    updateNavbarPage(session, "navbar", selected = "Données de la station")
  })

  # Dropdown change → update state
  observeEvent(
    input$station_select,
    {
      selected_climate_id(input$station_select)
      selected_metric("mean_temp")
    },
    ignoreInit = TRUE
  )

  # Column header click (from DT JS callback) → update chart metric
  observeEvent(input$selected_col, {
    col_idx <- as.integer(input$selected_col) # 1 = first metric col
    if (col_idx >= 1 && col_idx <= length(METRIC_COLS)) {
      selected_metric(METRIC_COLS[[col_idx]])
    }
  })

  # ── Station data ──────────────────────────────────────────────────────────
  station_data <- reactive({
    req(selected_climate_id())
    load_station_data(con, selected_climate_id())
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

  # ── Yearly trends ─────────────────────────────────────────────────────────
  yearly_data <- reactive({
    req(data_available)
    quantiles <- sort(as.numeric(input$yearly_quantiles))
    quantiles <- quantiles[
      !is.na(quantiles) & quantiles >= 0 & quantiles <= 100
    ]
    get_yearly_stats(con, input$yearly_metric, quantiles)
  })

  output$yearly_chart <- renderPlotly({
    d <- yearly_data()
    req(d)
    quantiles <- sort(as.numeric(input$yearly_quantiles))
    quantiles <- quantiles[
      !is.na(quantiles) & quantiles >= 0 & quantiles <= 100
    ]
    build_yearly_chart(d, input$yearly_metric, quantiles)
  })

  # ── Raster map ────────────────────────────────────────────────────────────
  output$raster_map <- renderLeaflet({
    req(data_available)

    metric <- req(input$raster_metric)

    if (input$raster_date_mode == "single") {
      date_from <- date_to <- req(input$raster_date)
    } else {
      dr <- req(input$raster_date_range)
      date_from <- dr[1]
      date_to <- dr[2]
    }

    r <- withProgress(message = "Calcul du lissage...", {
      build_metric_raster(
        con,
        metric,
        date_from,
        date_to,
        bandwidth = input$raster_bandwidth
      )
    })

    m <- leaflet() |>
      addTiles() |>
      setView(lng = -72, lat = 52, zoom = 5)

    if (is.null(r)) {
      return(m)
    }

    vals <- terra::values(r, na.rm = TRUE)
    pal <- colorNumeric(
      palette = "Spectral",
      domain = range(vals),
      reverse = TRUE,
      na.color = "transparent"
    )
    label <- FRENCH_COLS[[metric]]

    m |>
      leafem::addStarsImage(
        stars::st_as_stars(r),
        colors = pal,
        opacity = 0.7
      ) |>
      addCircleMarkers(
        data = station_registry,
        lng = ~lon,
        lat = ~lat,
        radius = 4,
        color = "black",
        weight = 1,
        fillColor = "white",
        fillOpacity = 0.8,
        label = ~station_name
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = vals,
        title = label
      )
  })
}

shinyApp(ui, server)
