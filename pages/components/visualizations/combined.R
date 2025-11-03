# Load necessary libraries
library(dplyr)
library(tidyr)
library(plotly)
library(stringr)

# (Optional) defaults you can keep or remove — the function now uses the args you pass in
options <- c(
  "1" = "Government",
  "2" = "Private Sector",
  "3" = "Non-Profit Organizations",
  "4" = "Individuals",
  "5" = "Community Groups"
)

colorings_list <- data.frame(
  Response_Label = c("Government","Private Sector","Non-Profit Organizations","Individuals","Community Groups"),
  color = c("#1f77b4","#ff7f0e","#2ca02c","#d62728","#8c564b"),
  stringsAsFactors = FALSE
)

# Combined Multi-Choice Questions Visualization Function (Single Question)
combined_multi_choice_questions <- function(
    survey_data,            # Data frame containing responses for one question
    demographic_variable,   # Demographic variable for grouping (e.g., "Gender")
    filter_input = NA,      # Specific demographic value to filter (optional)
    colorings,              # Data frame with Response_Label and color
    options_list            # Named vector mapping response codes -> labels
) {
  # Ensure the data frame has at least two columns: Demographic and Response
  if (ncol(survey_data) < 2) {
    stop("The data frame must contain at least two columns: Demographic and Response.")
  }
  
  # Rename the second column to 'response' for consistency
  names(survey_data)[2] <- "response"
  
  # Select only the demographic_variable and 'response' columns
  survey_data <- survey_data %>%
    dplyr::select(!!rlang::sym(demographic_variable), response)
  
  # Convert response to character and handle NA values, split multi-selects
  survey_data <- survey_data %>%
    mutate(response = as.character(response)) %>%
    mutate(response = ifelse(is.na(response), "NA", response)) %>%
    tidyr::separate_rows(response, sep = "") %>%   # if responses are stored like "135"
    filter(response != "")
  
  # Map numeric responses to labels using provided options_list (fallback to hard-coded)
  lab_map <- if (!missing(options_list) && length(options_list)) options_list else options
  survey_data <- survey_data %>%
    mutate(Response_Label = dplyr::case_when(
      response %in% names(lab_map) ~ unname(lab_map[response]),
      TRUE ~ "Other"
    ))
  
  # Optional: filter by a specific demographic value, if given
  if (!is.na(filter_input)) {
    survey_data <- survey_data %>%
      dplyr::filter(!!rlang::sym(demographic_variable) == filter_input)
  }
  
  # Calculate response frequencies by demographic group
  summary_data <- survey_data %>%
    group_by(!!rlang::sym(demographic_variable), Response_Label) %>%
    summarise(Count = dplyr::n(), .groups = "drop") %>%
    group_by(!!rlang::sym(demographic_variable)) %>%
    mutate(Percentage = (Count / sum(Count)) * 100) %>%
    ungroup()
  
  # Join colors (use the colorings argument; fallback to local colorings_list)
  color_df <- if (!missing(colorings) && nrow(colorings) > 0) colorings else colorings_list
  summary_data <- summary_data %>%
    left_join(color_df, by = "Response_Label") %>%
    mutate(color = ifelse(is.na(color), "#cccccc", color))
  
  # Build plot
  plot <- plot_ly(
    data = summary_data,
    x = ~Percentage,
    y = ~Response_Label,
    color = ~Response_Label,
    colors = setNames(summary_data$color, summary_data$Response_Label),
    type = "bar",
    orientation = "h",
    text = ~ paste0(
      "Response: ", Response_Label,
      "<br>Count: ", Count,
      "<br>Percentage: ", round(Percentage, 1), "%"
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
