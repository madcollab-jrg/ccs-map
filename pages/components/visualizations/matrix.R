library(dplyr)
library(tidyr)
library(plotly)
library(stringr)

matrix_questions <- function(example_matrix, demographic_variable, q_type) {
  # Debugging: Print inputs
  print(demographic_variable)
  print(q_type)

  # Rename the second column to 'response'
  names(example_matrix)[2] <- "response"

  # Separate 'response' into 'question' and 'answer'
  example_matrix <- example_matrix %>%
    separate_rows(response, sep = "; ") %>%
    separate(response, into = c("question", "answer"), sep = " - ")

  # Convert 'answer' to a factor
  example_matrix$answer <- as.factor(example_matrix$answer)

  example_matrix$question <- str_wrap(example_matrix$question, width = 40)

  # Summarize data with .groups = "drop" to suppress warning
  matrix_summary <- example_matrix %>%
    group_by(!!sym(demographic_variable), question, answer) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(freq = n / sum(n))

  # Filter out NA demographics
  matrix_summary <- matrix_summary %>%
    filter(!is.na(!!sym(demographic_variable)))

  # Define color sets based on q_type
  color_sets <- list(
    frequency = c("#E66101", "#FDB863", "#F7F7F7", "#B2ABD2", "#5E3C99"),
    agree = c("#E66101", "#FDB863", "#F7F7F7", "#B2ABD2", "#5E3C99"),
    agree1 = c("#E66101", "#FDB863", "#F7F7F7", "#B2ABD2", "#5E3C99"),
    agree2 = c("#E66101", "#FDB863", "#F7F7F7", "#B2ABD2", "#5E3C99"),
    responsible = c("#E66101", "#FDB863", "#FAE9D6", "#B2ABD2", "#F7F7F7"),
    awareness = c("#E66101", "#5E3C99"),
    informed = c("#E66101", "#FDB863", "#F7F7F7", "#B2ABD2", "#5E3C99"),
    amount = c("#E66101", "#FDB863", "#F7F7F7", "#B2ABD2", "#5E3C99"),
    binary = c("#E66101", "#5E3C99"),
    importance = c(
      "#E66101", "#FDB863", "#FAE9D6", "#F7F7F7",
      "#B2ABD2", "#6753D7", "#3F009E"
    ),
    change = c("#1B9E77", "#D95F02", "#7570B3"), # Added 'change'
    "agree1" = c("#E66101", "#FDB863", "#F7F7F7", "#B2ABD2", "#5E3C99"),
    "agree2" = c("#E66101", "#FDB863", "#F7F7F7", "#B2ABD2", "#5E3C99")
    # Add more as needed
  )

  # Clean q_type by removing spaces to match color_sets keys
  q_type_clean <- gsub(" ", "", q_type)
  color_set <- color_sets[[q_type_clean]]

  # Handle missing color_set
  if (is.null(color_set)) {
    warning(paste(
      "Color set for q_type:", q_type,
      "is not defined. Using default colors."
    ))
    color_set <- RColorBrewer::brewer.pal(5, "PuOr") # default palette
  }

  # Ensure there are enough colors for all unique answers
  unique_answers <- levels(matrix_summary$answer)
  if (length(color_set) < length(unique_answers)) {
    color_set <- colorRampPalette(color_set)(length(unique_answers))
  }

  # Create a named vector for colors
  answer_colors <- setNames(color_set, unique_answers)

  # Create the plot
  matrix_visualization <- plot_ly(
    data = matrix_summary,
    x = ~freq,
    y = ~question,
    text = ~ paste("<b>Response</b>: ", answer),
    hoverinfo = "text",
    type = "bar",
    orientation = "h",
    color = ~answer, # Color based on 'answer'
    colors = answer_colors, # Assign specific colors
    width = 800,
    height = 400,
    showlegend = TRUE
  ) %>%
    layout(
      xaxis = list(title = "Percentage of Respondents", family = "'Inter'"),
      yaxis = list(title = "Questions", family = "'Inter'"),
      barmode = "stack",
      legend = list(
        x = 0,
        y = -0.2,
        orientation = "h",
        font = list(size = 12, family = "'Inter'"),
        title = list(
          text = "Responses",
          font = list(size = 16, family = "'Inter'")
        ),
        itemsizing = "constant",
        showlegend = TRUE
      )
    )

  return(matrix_visualization)
}
