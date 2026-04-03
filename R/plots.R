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
    ) |>
    plotly_build()
}
