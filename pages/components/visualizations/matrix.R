matrix_questions <- function(example_matrix, demographic_variable, q_type) {
  print(example_matrix)
  print(demographic_variable)
  print(q_type)


  names(example_matrix)[2] <- "response"

  # Since the questions are in one column, we need to separate them
  # and then the question and response variables. ONLY FOR CCS QUESTIONS.
  # In the allquestions_types questions in the CCS column = 1
  example_matrix <- example_matrix %>%
    separate_rows(response, sep = "; ") %>%
    separate(response, into = c("question", "answer"), sep = " - ")
  example_matrix$answer <- as.factor(example_matrix$answer)

  matrix_summary <- example_matrix %>%
    group_by(!!sym(demographic_variable), question, answer) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n))

  matrix_summary <- matrix_summary %>%
    filter(!is.na(!!sym(demographic_variable)))

  # print(matrix_summary)

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
    )
  )

  color_set <- color_sets[[q_type]]

  matrix_summary <- merge(matrix_summary,
    data.frame(answer = levels(matrix_summary$answer), col = color_set),
    by = "answer"
  )

  # Create high and lows tables
  highs <- matrix_summary %>%
    filter(answer %in% c(
      "Always", "Often", "Agree Strongly",
      "Agree", "Strongly agree"
    ))

  lows <- matrix_summary %>%
    filter(answer %in% c(
      "Never", "Rarely", "Disagree Somewhat",
      "Disagree", "Strongly disagree"
    ))

  mylevels <- levels(matrix_summary$answer)

  matrix_summary <- matrix_summary[, -ncol(matrix_summary)]

  # print(matrix_summary)

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

  matrix_visualization <- plot_ly(
    data = matrix_summary,
    x = ~freq,
    y = ~question,
    text = ~ paste(
      "<b>Response</b>: ", response
    ),
    hoverinfo = "text",
    type = "bar",
    orientation = "h",
    color = color_var,
    width = 800,
    height = 400,
    legendgroup = ~answer,
    group = color_var
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
          text = legendtext,
          font = list(size = 16, family = "'Inter'")
        ),
        itemsizing = "constant",
        showlegend = TRUE
      )
    )

  return(matrix_visualization)
}
