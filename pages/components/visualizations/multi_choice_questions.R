# Multi-Choice Questions
multi_choice_questions <- function(
    example_multi, demographic_variable,
    filter_input, coloring, options) {
  names(example_multi)[2] <- "response"
  print(head(example_multi))

  example_multi <- example_multi %>%
    separate_rows(response, sep = "; ")

  if (!is.na(filter_input)) {
    example_multi <- example_multi %>%
      filter(!!sym(demographic_variable) == !!filter_input)
  }

  # Also extract other and run topic modeling?
  multi_summary <- example_multi %>%
    group_by(!!sym(demographic_variable), response) %>%
    summarise(count = n()) %>%
    mutate(freq = round(count / sum(count), digits = 2))

  # Remove other responses
  multi_summary <- multi_summary %>%
    filter(!grepl("^Other \\(please specify\\)", response)) %>%
    filter(!is.na(!!sym(demographic_variable)))

  if (demographic_variable == "Gender") {
    multi_summary <- multi_summary %>% filter(!Gender == "Non-binary")
  }

  multi_summary <- merge(multi_summary, coloring, by = demographic_variable)

  multi_summary <- multi_summary[, -ncol(multi_summary)]

  # Manually wrap labels
  wrap_labels <- function(labels, max_length = 20, ellipsis_length = 2) {
    ifelse(nchar(labels) > max_length, paste0(substr(
      labels, 1,
      max_length - ellipsis_length
    ), ".."), labels)
  }
  multi_summary$wrapped_labels <- wrap_labels(multi_summary$response)

  print(multi_summary)

  # print(demographic_variable)

  # based on demographics determine the variables
  switch(demographic_variable,
    "Gender" = {
      color_var <- ~Gender
      legendgroup <- ~Gender
      legendtext <- "Gender"
    },
    "income_recode" = {
      color_var <- ~income_recode
      legendgroup <- ~income_recode
      legendtext <- "Income Groups"
    },
    "edu_recode" = {
      color_var <- ~edu_recode
      legendgroup <- ~edu_recode
      legendtext <- "Education Levels"
    },
    "Year.of.Birth" = {
      color_var <- ~Year.of.Birth
      legendgroup <- ~Year.of.Birth
      legendtext <- "Age Group"
    },
    "race_recode" = {
      color_var <- ~race_recode
      legendgroup <- ~race_recode
      legendtext <- "Race"
    }
  )

  multi_visualization <- NA

  # Visualization (HORIZONTAL BAR CHART) using Plotly
  multi_visualization <- plot_ly(
    data = multi_summary,
    x = ~freq,
    y = ~wrapped_labels,
    text = ~ paste(
      "<b>Response</b>: ", response
    ),
    hoverinfo = "text",
    type = "bar",
    orientation = "h",
    color = color_var,
    showlegend = TRUE,
    width = 800,
    height = 400,
    legendgroup = legendgroup
    # hovertemplate = paste(
    #   "<b>Response</b>: %{y}"
    #   "<br><b>Frequency</b>: %{x}<br>"
    # )
  ) %>%
    layout(
      yaxis = list(
        tickangle = -45, title = "Responses", family = "'Inter'",
        ticktext = ~wrapped_labels
      ),
      xaxis = list(title = "Percent of Respondents", family = "'Inter'"),
      legend = list(
        x = 0,
        y = -0.2,
        orientation = "h",
        font = list(size = 12, family = "'Inter'"),
        title = list(
          text = legendtext,
          font = list(size = 16, family = "'Inter'")
        ),
        itemsizing = "constant", # Ensures legend items have constant size
        showlegend = FALSE
      )
    )

  print("---------")

  # print(multi_visualization)
  print("---------")

  return(multi_visualization)
}
