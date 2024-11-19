# Function to generate an error message plot
error_plot <- function(message = "No plots available") {
  plot_ly() %>%
    layout(
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE,
        range = c(0, 1)
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE,
        range = c(0, 1)
      ),
      annotations = list(
        x = 0.5,
        y = 0.5,
        text = message,
        showarrow = FALSE,
        font = list(size = 20, family = "Inter, sans-serif"),
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "middle"
      ),
      margin = list(l = 0, r = 0, t = 0, b = 0),
      plot_bgcolor = "rgb(245, 245, 245)", # Optional: Light background
      paper_bgcolor = "rgb(245, 245, 245)"
    )
}
