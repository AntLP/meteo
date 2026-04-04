# R/plots.R

library(plotly)
library(DT)

# Plotly line chart of metric_col over time for a station's data frame.
# metric_col: one of METRIC_COLS (English name).
build_chart <- function(data, metric_col) {
  y_label <- FRENCH_COLS[[metric_col]]

  plot_ly(
    data = data,
    x    = ~date,
    y    = as.formula(paste0("~", metric_col)),
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
        lengthMenu  = "Afficher _MENU_ entrées",
        info        = "Entrées _START_ à _END_ sur _TOTAL_",
        paginate    = list(previous = "Précédent", `next` = "Suivant"),
        zeroRecords = "Aucun résultat trouvé"
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
