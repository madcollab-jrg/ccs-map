select_box_questions <- function(
    survey_data, demographic_variable,
    filter_input, coloring, options) {
  example_select <- survey_data
  names(example_select)[2] <- "response"
  
  select_summary <- example_select %>%
    group_by(!!sym(demographic_variable), response) %>%
    summarise(count = n()) %>%
    mutate(freq = round(count / sum(count), digits = 2))
  
  # Remove other responses
  select_summary <- select_summary %>%
    filter(!grepl("^Other \\(please specify\\)", response)) %>%
    filter(!is.na(!!sym(demographic_variable)))
  
  if (demographic_variable == "Gender") {
    select_summary <- select_summary %>% filter(Gender != "Non-binary")
  }
  
  select_summary <- merge(select_summary, coloring, by = demographic_variable)
  
  select_summary <- select_summary[, -ncol(select_summary)]
  
  # Manually wrap labels
  # wrap_labels <- function(labels, max_length = 60) {
  #   str_wrap(labels, width = max_length, indent = 0, exdent = 0)
  # }
  wrap_labels <- function(labels, max_length = 20, ellipsis_length = 2) {
    ifelse(nchar(labels) > max_length, paste0(substr(
      labels, 1,
      max_length - ellipsis_length
    ), ".."), labels)
  }
  
  select_summary$wrapped_labels <- wrap_labels(select_summary$response)
  
  # print(select_summary)
  
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
  
  select_visualization <- NA
  
  select_visualization <- plot_ly(
    data = select_summary,
    x = ~wrapped_labels,
    y = ~count,
    type = "bar",
    text = ~ paste(
      "<b>Response</b>: ", response
    ),
    hoverinfo = "text",
    color = color_var,
    width = 800,
    height = 400,
    legendgroup = legendgroup
  ) %>%
    layout(
      xaxis = list(title = "Responses", family = "'Inter'"),
      yaxis = list(title = "Count", family = "'Inter'"),
      legend = list(
        x = 420,
        y = 50,
        orientation = "h",
        font = list(size = 12, family = "'Inter'"),
        title = list(
          text = legendtext,
          font = list(size = 16, family = "'Inter'")
        ),
        itemsizing = "constant", # Ensures legend items have constant size
        showlegend = TRUE
      )
    )
  
  return(select_visualization)
}