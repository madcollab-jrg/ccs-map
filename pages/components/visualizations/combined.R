# Load necessary libraries
library(dplyr)
library(tidyr)
library(plotly)
library(stringr)

# Define response mappings for the single question
options <- c(
  "1" = "Government",
  "2" = "Private Sector",
  "3" = "Non-Profit Organizations",
  "4" = "Individuals",
  "5" = "Community Groups"
)

# Define colorings for the responses
colorings_list <- data.frame(
  Response_Label = c(
    "Government", "Private Sector",
    "Non-Profit Organizations", "Individuals", "Community Groups"
  ),
  color = c(
    "#1f77b4", "#ff7f0e", "#2ca02c",
    "#d62728", "#8c564b"
  ),
  stringsAsFactors = FALSE
)

# Combined Multi-Choice Questions Visualization Function (Single Question)
combined_multi_choice_questions <- function(
    survey_data, # Data frame containing responses for one question
    demographic_variable, # Demographic variable for filtering/grouping (e.g., "Gender")
    filter_input = NA, # Specific demographic value to filter (optional, default is NA)
    colorings, # Data frame with response labels and colors
    options_list # Named vector mapping response codes to descriptive labels
    ) {
  # Ensure the data frame has at least two columns: Demographic and Response
  if (ncol(survey_data) < 2) {
    stop("The data frame must contain at least two columns: Demographic and Response.")
  }

  # Rename the second column to 'response' for consistency
  names(survey_data)[2] <- "response"

  # Select only the demographic_variable and 'response' columns
  survey_data <- survey_data %>%
    select(!!sym(demographic_variable), response)

  # Convert response to character and handle NA values
  survey_data <- survey_data %>%
    mutate(response = as.character(response)) %>%
    mutate(response = ifelse(is.na(response), "NA", response)) %>%
    separate_rows(response, sep = "") %>%
    filter(response != "")

  # Map numeric responses to labels
  survey_data <- survey_data %>%
    mutate(Response_Label = case_when(
      response == "1" ~ "Government",
      response == "2" ~ "Private Sector",
      response == "3" ~ "Non-Profit Organizations",
      response == "4" ~ "Individuals",
      response == "5" ~ "Community Groups",
      TRUE ~ "Other"
    ))

  # Filter based on demographic_variable and filter_input if provided
  if (!is.na(filter_input)) {
    survey_data <- survey_data %>%
      filter(!!sym(demographic_variable) == filter_input)
  }

  # Calculate response frequencies by demographic group
  summary_data <- survey_data %>%
    group_by(!!sym(demographic_variable), Response_Label) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(!!sym(demographic_variable)) %>%
    mutate(Percentage = (Count / sum(Count)) * 100) %>%
    ungroup()

  # Merge with colorings to assign colors to each response category
  summary_data <- summary_data %>%
    left_join(colorings_list, by = "Response_Label")

  # Check for missing colors and assign a default color if needed
  summary_data <- summary_data %>%
    mutate(color = ifelse(is.na(color), "#cccccc", color))

  # Create the Plotly bar chart
  plot <- plot_ly(
    data = summary_data,
    x = ~Percentage,
    y = ~Response_Label,
    color = ~Response_Label,
    colors = setNames(summary_data$color, summary_data$Response_Label),
    type = "bar",
    orientation = "h",
    text = ~ paste(
      "Response:", Response_Label,
      "<br>Count:", Count,
      "<br>Percentage:", round(Percentage, 1), "%"
    ),
    hoverinfo = "text"
  ) %>%
    layout(
      barmode = "stack",
      title = paste("Response Distribution by", demographic_variable),
      xaxis = list(title = "Percentage of Responses"),
      yaxis = list(title = ""),
      showlegend = FALSE
    )

  return(plot)
}
