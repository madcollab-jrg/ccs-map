source("pages/components/visualizations/imports.R")

survey_results_ui <- function(demog, surveyQues) {
  ui <- box(
    title = HTML(paste(
      "<div class='card-title'><h1 class='page-subtitle'>Response by ",
      demog, "</h1>",
      "<p class='text-lighter font-sm'>", surveyQues, "</p></div>"
    )),
    HTML(paste(
      "<div class='plot-loader hidden' id='plot-loading-container'>
        <div class='loader'></div>
      </div>"
    )),
    plotlyOutput("survey_results"), # for plotly
    width = 12,
    collapsible = FALSE,
    maximizable = TRUE,
    solidHeader = TRUE,
    elevation = NULL
  )
  return(ui)
}

make_color_mapping <- function(column, options) {
  col_pal <- brewer.pal(length(options), "PuOr")
  color_mapping <- tibble(!!column := options, col = col_pal)
  return(color_mapping)
}

# Error Plot Graph
source("pages/components/visualizations/error_plot.R")
# Text Questions
source("pages/components/visualizations/topic_modelling.R")
# Matrix Questions
source("pages/components/visualizations/matrix.R")
# Multi-Choice Questions
source("pages/components/visualizations/multi_choice_questions.R")
# Select Box Questions
source("pages/components/visualizations/select_box_questions.R")
# Combined Multi Choice Questions
source("pages/components/visualizations/combined.R")

data_for_visualization <- NA

resulting_graphics <- function(
    input, output, survey_data, is_survey,
    question = NA, question_type = NA, question_subtype = NA,
    demographic_desc = NA) {
  # Function to toggle loader
  toggle_loader <- function(show = TRUE) {
    if (show) {
      shinyjs::removeClass("plot-loading-container", "hidden")
      shinyjs::addClass("survey_results", "hidden")
    } else {
      shinyjs::addClass("plot-loading-container", "hidden")
      shinyjs::removeClass("survey_results", "hidden")
    }
  }

  # Populate the survey results boxes with the required graphics
  reaction <- observeEvent(input$run_report, {
    disable("run_report")
    req(input$survey)
    req(input$census_level)
    req(input$demographic)

    # Retrieve values
    q_type <- question_type()
    survey_flag <- is_survey()

    # Temporary exception to handle "Tree Canopy Map" survey where q_type is NA
    if (is.na(q_type) && input$survey == "Tree Canopy Map") {
      q_type <- "open-ended"
    }
    # Temporary exception to handle "Tree Knowledge" survey where q_type is NA
    if (is.na(q_type) && input$survey == "Tree Knowledge") {
      q_type <- "multi-choice"
    }
    # Temporary exception to handle "Carbon Concerns" survey where q_type is NA
    if (is.na(q_type) && input$survey == "Carbon Concerns") {
      q_type <- "multi-choice"
    }
    # Temporary exception to handle "Air Quality Map" survey where q_type is NA
    if (is.na(q_type) && input$survey == "Air Quality Map") {
      q_type <- "open-ended"
    }

    # Temporary exception to handle "Urban Heat Map" survey where q_type is NA
    if (is.na(q_type) && input$survey == "Urban Heat Map") {
      q_type <- "open-ended"
    }

    if (question() == 16 && input$survey == "General Survey") {
      q_type <- "combined"
    }

    if (question() == 17 && input$survey == "General Survey") {
      q_type <- "combined"
    }


    if (q_type != "matrix") {
      toggle_loader(TRUE)
    }

    # # Normalize survey name
    # normalized_survey <- str_trim(str_to_title(input$survey))

    # # Define a mapping of survey names to q_type
    # survey_qtype_mapping <- c(
    #   "Tree Canopy Map" = "open-ended",
    #   "Tree Knowledge" = "multi-choice",
    #   "Carbon Concerns" = "multi-choice",
    #   "Air Quality Map" = "open-ended"
    # )

    # Set q_type based on the mapping if q_type is NA
    # if (is.na(q_type) && normalized_survey %in% names(survey_qtype_mapping)) {
    #   q_type <- survey_qtype_mapping[normalized_survey]
    # }

    # Debugging prints
    print(paste("Selected Survey:", input$survey))
    print(paste("Question Type:", q_type))
    print(paste("Survey Flag:", survey_flag))

    if (is.na(q_type)) {
      print("q_type is NA")
    }

    if (is.na(survey_flag)) {
      print("survey_flag is NA")
    }


    if (q_type != "Ranking" & survey_flag) {
      # Unsure how rank type questions are suppose to be displayed
      question_num <- question() # column number of question

      # column names of categories
      income_var <- "income_recode"
      edu_var <- "edu_recode"
      age_var <- "Year.of.Birth"
      gender_var <- "Gender"
      race_var <- "race_recode"

      # subcategories options + color mapping
      income_options <- c(
        NA, "Less than $25,000", "$35,000 to $49,999",
        "$50,000 to $74,999", "$75,000 to $99,999",
        "$100,000 to $149,999", "$150,000 to $199,999", "$200,000 or more"
      )
      income_color_mapping <- make_color_mapping(income_var, income_options)

      edu_options <- c(
        NA, "Less than High School Diploma",
        "High School Graduate (Includes Equivalency)",
        "Some College or Associates Degree", "Bachelors Degree or Higher"
      )
      edu_color_mapping <- make_color_mapping(edu_var, edu_options)

      age_options <- c(
        NA, "18_to_24", "25_to_34", "35_to_44", "45_to_54",
        "55_to_64", "65_over"
      )
      age_color_mapping <- make_color_mapping(age_var, age_options)

      # Called multi-options in previous
      gender_options <- c(NA, "Non-binary", "Male", "Female")
      gender_color_mapping <- make_color_mapping(gender_var, gender_options)

      race_options <- c(
        NA, "Black or African American", "Hispanic", "White",
        "Asian", "Native Hawaiian Pacific Islander",
        "American Indian Alaskan Native", "Two or More Races"
      )
      race_color_mapping <- make_color_mapping(race_var, race_options)

      # get data and change year of birth
      data <- survey_data()

      if ("Year.of.Birth" %in% names(data)) {
        data <- data %>%
          mutate(Year.of.Birth = 2024 - Year.of.Birth) %>%
          mutate(Year.of.Birth = case_when(
            Year.of.Birth >= 18 & Year.of.Birth <= 24 ~ "18_to_24",
            Year.of.Birth >= 25 & Year.of.Birth <= 34 ~ "25_to_34",
            Year.of.Birth >= 35 & Year.of.Birth <= 44 ~ "35_to_44",
            Year.of.Birth >= 45 & Year.of.Birth <= 54 ~ "45_to_54",
            Year.of.Birth >= 55 & Year.of.Birth <= 64 ~ "55_to_64",
            Year.of.Birth >= 65 ~ "65_over"
          ))
      } else {
        data <- data %>%
          mutate(Year.of.Birth = "18_to_24")
      }

      # data needed to make graphics by survey
      data_for_visualization <- NA
      print(input$survey)
      if (input$survey == "Urban Heat Survey") {
        data_for_visualization <- data[, c(2, question_num + 3, 45:61, 22, 19)]
      } else if (input$survey == "Tree Canopy Survey") {
        data_for_visualization <- data[, c(2, question_num + 3, 48:64, 25, 22)]
      } else if (input$survey == "Air Quality Survey") {
        data_for_visualization <- data[, c(2, question_num + 3, 47:63, 24, 21)]
      } else if (input$survey == "Air Quality Map") {
        data_for_visualization <-
          data[, c(2, 4, question_num + 3, 47:55, 24, 21)]
      } else if (input$survey == "Environmental Justice Survey") {
        data_for_visualization <- data[, c(2, question_num + 3, 28:66, 27, 24)]
      } else if (input$survey == "General Survey") {
        data_for_visualization <- data[, c(1, question_num + 1, 24:29)]
      } else if (input$survey == "Carbon Concerns") {
        data_for_visualization <- data[, c(1, question_num + 1, 5:9)]
      } else if (input$survey == "Tree Knowledge") {
        data_for_visualization <- data[, c(1, question_num + 1, 7:10, 6)]
      } else if (input$survey == "Urban Heat Map") {
        data_for_visualization <- data[, c(1, question_num + 3, 20, 46:61)]
      } else if (input$survey == "Health Impacts") {
        data_for_visualization <- data[, c(1, question_num + 1, 10:13, 9)]
      } else if (input$survey == "Energy Concerns") {
        data_for_visualization <- data[, c(1, question_num + 1, 5:8, 4)]
      } else if (input$survey == "Tree Canopy Map") {
        data_for_visualization <- data[, c(2, question_num + 3, 45:56, 20:27)]
      } else {
        # Handle unexpected survey selections
        output$survey_results <-
          renderPlotly(error_plot("Selected survey is not recognized."))
        return(NULL)
      }

      demographic_desc <- tolower(input$demographic)

      # print based on question type
      if (q_type == "matrix") {
        q_subtype <- question_subtype()
        # message(q_subtype)
        message(demographic_desc)
        tryCatch(
          {
            plot <- switch(demographic_desc,
              "income" = matrix_questions(
                data_for_visualization,
                income_var, q_subtype
              ),
              "education" = matrix_questions(
                data_for_visualization,
                edu_var, q_subtype
              ),
              "age" = matrix_questions(
                data_for_visualization,
                age_var, q_subtype
              ),
              "gender" = matrix_questions(
                data_for_visualization,
                gender_var, q_subtype
              ),
              "race" = matrix_questions(
                data_for_visualization,
                race_var, q_subtype
              )
            )
            enable("run_report")
            output$survey_results <- renderPlotly(plot)
          },
          error = function(e) {
            enable("run_report")
            output$survey_results <-
              renderPlotly(error_plot("No plots available"))
          }
        )
      } else if (q_type == "open-ended") {
        tryCatch(
          {
            plot <- switch(demographic_desc,
              "income" = perform_topic_modeling(
                data_for_visualization,
                income_var,
                input$survey
              ),
              "education" = perform_topic_modeling(
                data_for_visualization,
                edu_var,
                input$survey
              ),
              "age" = perform_topic_modeling(
                data_for_visualization,
                age_var,
                input$survey
              ),
              "gender" = perform_topic_modeling(
                data_for_visualization,
                gender_var,
                input$survey
              ),
              "race" = perform_topic_modeling(
                data_for_visualization,
                race_var,
                input$survey
              )
            )
            enable("run_report")
            toggle_loader(FALSE)
            output$survey_results <- renderPlotly(plot)
          },
          error = function(e) {
            enable("run_report")
            toggle_loader(FALSE)
            output$survey_results <-
              renderPlotly(error_plot("No plots available"))
          }
        )
      } else if (q_type == "multi-choice") {
        tryCatch(
          {
            plot <- switch(demographic_desc,
              "income" = multi_choice_questions(
                data_for_visualization, income_var, NA,
                income_color_mapping, income_options
              ),
              "education" = multi_choice_questions(
                data_for_visualization, edu_var, NA,
                edu_color_mapping, edu_options
              ),
              "age" = multi_choice_questions(
                data_for_visualization, age_var, NA,
                age_color_mapping, age_options
              ),
              "gender" = multi_choice_questions(
                data_for_visualization, gender_var, NA,
                gender_color_mapping, gender_options
              ),
              "race" = multi_choice_questions(
                data_for_visualization, race_var, NA,
                race_color_mapping, race_options
              )
            )
            enable("run_report")
            toggle_loader(FALSE)
            output$survey_results <- renderPlotly(plot)
          },
          error = function(e) {
            enable("run_report")
            toggle_loader(FALSE)
            output$survey_results <-
              renderPlotly(error_plot("No plots available"))
          }
        )
      } else if (q_type == "select box") {
        tryCatch(
          {
            plot <- switch(demographic_desc,
              "income" = select_box_questions(
                data_for_visualization, income_var, NA,
                income_color_mapping, income_options
              ),
              "education" = select_box_questions(
                data_for_visualization, edu_var, NA,
                edu_color_mapping, edu_options
              ),
              "age" = select_box_questions(
                data_for_visualization, age_var, NA,
                age_color_mapping, age_options
              ),
              "gender" = select_box_questions(
                data_for_visualization, gender_var, NA,
                gender_color_mapping, gender_options
              ),
              "race" = select_box_questions(
                data_for_visualization, race_var, NA,
                race_color_mapping, race_options
              )
            )
            enable("run_report")
            toggle_loader(FALSE)
            output$survey_results <- renderPlotly(plot)
          },
          error = function(e) {
            enable("run_report")
            toggle_loader(FALSE)
            output$survey_results <-
              renderPlotly(error_plot("No plots available"))
          }
        )
      } else if (q_type == "combined") {
        print("you are in combined")
        # tryCatch(
        #   {
        plot <- switch(demographic_desc,
          "income" = combined_multi_choice_questions(
            data_for_visualization, income_var, NA,
            income_color_mapping, income_options
          ),
          "education" = combined_multi_choice_questions(
            data_for_visualization, edu_var, NA,
            edu_color_mapping, edu_options
          ),
          "age" = combined_multi_choice_questions(
            data_for_visualization, age_var, NA,
            age_color_mapping, age_options
          ),
          "gender" = combined_multi_choice_questions(
            data_for_visualization, gender_var, NA,
            gender_color_mapping, gender_options
          ),
          "race" = combined_multi_choice_questions(
            data_for_visualization, race_var, NA,
            race_color_mapping, race_options
          )
        )
        enable("run_report")
        toggle_loader(FALSE)
        output$survey_results <- renderPlotly(plot)
      }
      #   error = function(e) {
      #     enable("run_report")
      #     toggle_loader(FALSE)
      #     output$survey_results <-
      #       renderPlotly(error_plot("No plots available"))
      #   }
      # )
      # }
    } else {
      enable("run_report")
      toggle_loader(FALSE)
      message("no plots")
      output$survey_results <- renderPlotly(error_plot("No plots available"))
    }
  })
}
