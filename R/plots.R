# R/plots.R

library(plotly)
library(DT)

# Plotly line chart of metric_col over time for a station's data frame.
# metric_col: one of METRIC_COLS (English name).
build_chart <- function(data, metric_col) {
  y_label <- FRENCH_COLS[[metric_col]]

  plot_ly(
    data = data,
    x = ~date,
    y = as.formula(paste0("~", metric_col)),
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

# Plotly chart of yearly mean (and optional quantile lines) across all stations.
# data: data frame from get_yearly_stats() with columns year, mean, p<n>...
# quantiles: numeric vector of percentile values matching the p-columns in data.
build_yearly_chart <- function(data, metric_col, quantiles = numeric(0)) {
  y_label <- FRENCH_COLS[[metric_col]]

  p <- plot_ly(data, x = ~year) |>
    add_trace(
      y = ~mean,
      type = "scatter",
      mode = "lines+markers",
      name = "Moyenne",
      line = list(color = "#1565C0", width = 2),
      marker = list(color = "#1565C0", size = 5),
      hovertemplate = paste0("%{x}<br>Moyenne : %{y:.2f}<extra></extra>")
    )

  if (length(quantiles) > 0) {
    # Spread colours from orange (extreme) to grey (central)
    pal <- colorRampPalette(c("#EF6C00", "#BDBDBD"))(ceiling(length(quantiles) / 2))
    # Mirror palette so symmetric quantiles share a colour
    colours <- c(pal, rev(pal))[seq_along(quantiles)]

    for (i in seq_along(quantiles)) {
      q   <- quantiles[i]
      col <- paste0("p", gsub("\\.", "_", as.character(q)))
      p <- p |> add_trace(
        y = data[[col]],
        type = "scatter",
        mode = "lines",
        name = paste0(q, "e centile"),
        line = list(color = colours[i], width = 1.2, dash = "dash"),
        hovertemplate = paste0("%{x}<br>", q, "e centile : %{y:.2f}<extra></extra>")
      )
    }
  }

  p |> layout(
    xaxis = list(title = "Ann\u00e9e", dtick = 1),
    yaxis = list(title = y_label),
    hovermode = "x unified",
    legend = list(orientation = "h", y = -0.15)
  )
}

# DT datatable showing DISPLAY_COLS with French headers.
# A JS initComplete callback sends the clicked column index to Shiny
# via input$selected_col (1-based, 1 = first metric column after Date).
build_table <- function(data) {
  display_data <- data[, DISPLAY_COLS, drop = FALSE]
  metric_cols <- setdiff(DISPLAY_COLS, "date")
  has_data <- rowSums(!is.na(display_data[, metric_cols, drop = FALSE])) > 0
  display_data <- display_data[has_data, , drop = FALSE]
  colnames(display_data) <- unname(FRENCH_COLS[DISPLAY_COLS])

  datatable(
    display_data,
    rownames = FALSE,
    selection = "none",
    options = list(
      pageLength = 25,
      scrollX = TRUE,
      language = list(
        search = "Rechercher :",
        lengthMenu = "Afficher _MENU_ entrées",
        info = "Entrées _START_ à _END_ sur _TOTAL_",
        paginate = list(previous = "Précédent", `next` = "Suivant"),
        zeroRecords = "Aucun résultat trouvé"
      ),
      initComplete = JS(
        "
        function(settings, json) {
          var table = this.api();
          $(table.table().header()).find('th').on('click', function() {
            var colIdx = $(this).index();
            if (colIdx > 0) {
              Shiny.setInputValue('selected_col', colIdx, {priority: 'event'});
            }
          });
        }
      "
      )
    )
  )
}
