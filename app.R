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
data_available <- !is.null(station_registry) && nrow(station_registry) > 0

if (data_available) {
  station_choices <- setNames(
    station_registry$climate_id,
    station_registry$station_name
  )

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
  selected_metric <- reactiveVal("Mean Temp (°C)")

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
    selected_metric("Mean Temp (°C)")
    updateSelectInput(session, "station_select", selected = cid)
    updateNavbarPage(session, "navbar", selected = "Données de la station")
  })

  # Dropdown change → update state
  observeEvent(
    input$station_select,
    {
      selected_climate_id(input$station_select)
      selected_metric("Mean Temp (°C)")
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
