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
    DT::dataTableOutput("demographic_table"), # for demographic table
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
# Text Questions - Enhanced Topic Modeling with Heatmaps
source("pages/components/visualizations/enhanced_topic_modeling.R")
# Matrix Questions
source("pages/components/visualizations/matrix.R")
# Multi-Choice Questions
source("pages/components/visualizations/multi_choice_questions.R")
# Select Box Questions
source("pages/components/visualizations/select_box_questions.R")
# Combined Multi Choice Questions
source("pages/components/visualizations/combined.R")

data_for_visualization <- NA

# --- NEW: helpers to sync chart with the same geography filter used by the table ----

.geo_input_id_for_level <- function(level) {
  switch(level,
    "Census Tract"  = "census_tract_items",
    "Census State"  = "census_state_items",
    "Census County" = "census_county_items",
    "Zipcode"       = "census_zipcode_items",
    "State Lower"   = "census_state_lower_items",
    "State Upper"   = "census_state_upper_items",
    "Congress"      = "census_congress_items",
    NULL
  )
}

.find_first_col <- function(df, patterns) {
  cand <- names(df)[Reduce(`|`, lapply(patterns, function(p) {
    grepl(p, names(df), ignore.case = TRUE)
  }))]
  if (length(cand)) cand[1] else NULL
}

apply_geo_filter <- function(df, input) {
  lvl <- input$census_level
  id <- .geo_input_id_for_level(lvl)
  if (is.null(id)) {
    return(df)
  }
  val <- input[[id]]
  if (is.null(val) || is.na(val) || identical(val, "")) {
    return(df)
  }

  # Convert display name to code using census_level_input_to_data mapping
  # This is needed because data columns contain codes (e.g., "25" for county_fips) not display names (e.g., "Dane")
  census_level <- census_input_to_data[[lvl]]
  filter_val <- val  # Default to original value
  
  if (!is.null(census_level) && !is.null(census_level_input_to_data[["data"]][[census_level]])) {
    code_mapping <- census_level_input_to_data[["data"]][[census_level]]
    if (val %in% names(code_mapping)) {
      full_code <- code_mapping[[val]]
      # For counties, extract the county portion (last 3 digits) since county_fips stores just the county code
      # Remove leading zeros since the data stores county codes without leading zeros (e.g., "25" not "025")
      if (lvl == "Census County" && nchar(full_code) >= 3) {
        county_portion <- substr(full_code, nchar(full_code) - 2, nchar(full_code))
        filter_val <- as.character(as.numeric(county_portion))  # Remove leading zeros
      } else {
        filter_val <- full_code
      }
    }
  }

  # Heuristics for common column names in raw survey rows
  patterns <- switch(lvl,
    "Census State"  = c("^state$", "state_name", "state_fips"),
    "Census County" = c("^county$", "county_name", "county_fips"),
    "Zipcode"       = c("^zip", "zipcode", "postal"),
    "Census Tract"  = c("tract", "census_tract"),
    "State Lower"   = c("assembly", "state_lower", "lower"),
    "State Upper"   = c("senate", "state_upper", "upper"),
    "Congress"      = c("congress"),
    character()
  )

  col <- .find_first_col(df, patterns)
  if (is.null(col)) {
    message(sprintf("[geo-filter] No matching column for '%s' in data; leaving unfiltered.", lvl))
    return(df)
  }

  # Compare as strings to be robust against numeric/character mismatches
  # Use filter_val (the code) instead of val (the display name)
  df %>% dplyr::filter(as.character(.data[[col]]) == as.character(filter_val))
}
# -------------------------------------------------------------------------------

resulting_graphics <- function(
  input, output, survey_data, is_survey,
  question = NA, question_type = NA, question_subtype = NA,
  demographic_desc = NA
) {
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

    # Temporary exceptions to set q_type when source metadata is NA
    if (is.na(q_type) && input$survey == "Tree Canopy Map") q_type <- "open-ended"
    if (is.na(q_type) && input$survey == "Tree Knowledge") q_type <- "multi-choice"
    if (is.na(q_type) && input$survey == "Carbon Concerns") q_type <- "multi-choice"
    if (is.na(q_type) && input$survey == "Air Quality Map") q_type <- "open-ended"
    if (is.na(q_type) && input$survey == "Urban Heat Map") q_type <- "open-ended"

    if (question() == 16 && input$survey == "General Survey") q_type <- "combined"
    if (question() == 17 && input$survey == "General Survey") q_type <- "combined"

    if (q_type != "matrix") {
      toggle_loader(TRUE)
    }

    print(paste("Selected Survey:", input$survey))
    print(paste("Question Type:", q_type))
    print(paste("Survey Flag:", survey_flag))

    if (q_type != "Ranking" & survey_flag) {
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
        NA, "18 to 24", "25 to 34", "35 to 44", "45 to 54",
        "55 to 64", "65 over"
      )
      age_color_mapping <- make_color_mapping(age_var, age_options)

      gender_options <- c(NA, "Non-binary", "Male", "Female")
      gender_color_mapping <- make_color_mapping(gender_var, gender_options)

      race_options <- c(
        NA, "Black or African American", "Hispanic", "White",
        "Asian", "Native Hawaiian Pacific Islander",
        "American Indian Alaskan Native", "Two or More Races"
      )
      race_color_mapping <- make_color_mapping(race_var, race_options)

      # --- raw survey rows
      data <- survey_data()

      # Normalize year-of-birth -> age buckets
      if ("Year.of.Birth" %in% names(data)) {
        data <- data %>%
          mutate(Year.of.Birth = 2024 - Year.of.Birth) %>%
          mutate(Year.of.Birth = dplyr::case_when(
            Year.of.Birth >= 18 & Year.of.Birth <= 24 ~ "18 to 24",
            Year.of.Birth >= 25 & Year.of.Birth <= 34 ~ "25 to 34",
            Year.of.Birth >= 35 & Year.of.Birth <= 44 ~ "35 to 44",
            Year.of.Birth >= 45 & Year.of.Birth <= 54 ~ "45 to 54",
            Year.of.Birth >= 55 & Year.of.Birth <= 64 ~ "55 to 64",
            Year.of.Birth >= 65 ~ "65 over",
            TRUE ~ NA_character_
          ))
      } else {
        data <- data %>% mutate(Year.of.Birth = "18 to 24")
      }

      # --- NEW: apply the SAME geography selection as the table (Step 3)
      data <- apply_geo_filter(data, input)

      # Check if data has sufficient rows after filtering
      if (is.null(data) || nrow(data) == 0) {
        toggle_loader(FALSE)
        output$survey_results <- renderPlotly(error_plot("No data available for the selected geographic filter. Please try a different geography."))
        output$demographic_table <- DT::renderDataTable({
          DT::datatable(data.frame(Message = "No data available after filtering"))
        })
        enable("run_report")
        return(NULL)
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
        data_for_visualization <- data[, c(2, 4, question_num + 3, 47:55, 24, 21)]
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
        output$survey_results <- renderPlotly(error_plot("Selected survey is not recognized."))
        enable("run_report")
        toggle_loader(FALSE)
        return(NULL)
      }

      demographic_desc <- tolower(input$demographic)

      # Check if data_for_visualization has sufficient rows
      if (is.null(data_for_visualization) || nrow(data_for_visualization) == 0) {
        toggle_loader(FALSE)
        output$survey_results <- renderPlotly(error_plot("No data available for visualization after filtering. Please try different filters."))
        output$demographic_table <- DT::renderDataTable({
          DT::datatable(data.frame(Message = "No data available after filtering"))
        })
        enable("run_report")
        return(NULL)
      }

      # Map demographic description to actual variable name for validation
      demographic_var <- switch(demographic_desc,
        "income" = income_var,
        "education" = edu_var,
        "age" = age_var,
        "gender" = gender_var,
        "race" = race_var
      )
      
      # Check if there's sufficient data for the selected demographic variable
      if (demographic_var %in% names(data_for_visualization)) {
        valid_demo_count <- sum(!is.na(data_for_visualization[[demographic_var]]) & 
                                data_for_visualization[[demographic_var]] != "")
        if (valid_demo_count == 0) {
          toggle_loader(FALSE)
          output$survey_results <- renderPlotly(error_plot("No data available for the selected demographic category after filtering."))
          output$demographic_table <- DT::renderDataTable({
            DT::datatable(data.frame(Message = "No data available for selected demographic"))
          })
          enable("run_report")
          return(NULL)
        }
      }

      if (q_type == "matrix") {
        q_subtype <- question_subtype()
        tryCatch(
          {
            plot <- switch(demographic_desc,
              "income" = matrix_questions(data_for_visualization, income_var, q_subtype),
              "education" = matrix_questions(data_for_visualization, edu_var, q_subtype),
              "age" = matrix_questions(data_for_visualization, age_var, q_subtype),
              "gender" = matrix_questions(data_for_visualization, gender_var, q_subtype),
              "race" = matrix_questions(data_for_visualization, race_var, q_subtype)
            )
            if (!is.null(plot)) {
              output$survey_results <- renderPlotly(plot)
            } else {
              output$survey_results <- renderPlotly(error_plot("Unable to generate plot. Not enough data for this question type."))
            }
            # Hide table for non-open-ended questions
            output$demographic_table <- DT::renderDataTable({
              DT::datatable(data.frame(Message = "Table not available for this question type"))
            })
            enable("run_report")
          },
          error = function(e) {
            toggle_loader(FALSE)
            output$survey_results <- renderPlotly(error_plot(paste("Error generating plot:", e$message)))
            output$demographic_table <- DT::renderDataTable({
              DT::datatable(data.frame(Message = "Error loading data"))
            })
            enable("run_report")
          }
        )
      } else if (q_type == "open-ended") {
        tryCatch(
          {
            # Map demographic description to actual variable name
            demographic_var <- switch(demographic_desc,
              "income" = income_var,
              "education" = edu_var,
              "age" = age_var,
              "gender" = gender_var,
              "race" = race_var
            )

            # Use enhanced topic modeling with demographic heatmap
            visualization_results <- enhanced_topic_modeling_visualization(
              data_for_visualization, demographic_var,
              filter_input = NA, num_topics = 4
            )

            if (!is.null(visualization_results)) {
              # Use demographic heatmap as the main visualization
              plot <- visualization_results$demographic_heatmap

              # Render demographic table for open-ended questions
              if (!is.null(visualization_results$demographic_table)) {
                # Get better display name for demographic variable
                demo_display_name <- switch(demographic_var,
                  "Year.of.Birth" = "Age Range",
                  "income_recode" = "Income Bracket",
                  "edu_recode" = "Education Level",
                  "race_recode" = "Race",
                  demographic_var
                )

                # Rename the demographic column in the table
                table_data <- visualization_results$demographic_table
                names(table_data)[names(table_data) == demographic_var] <- demo_display_name

                output$demographic_table <- DT::renderDataTable({
                  DT::datatable(
                    table_data,
                    options = list(
                      pageLength = 10,
                      scrollX = TRUE,
                      dom = "Bfrtip",
                      buttons = c("copy", "csv", "excel", "pdf", "print")
                    ),
                    extensions = "Buttons",
                    caption = paste("Demographic breakdown for", demo_display_name)
                  )
                })
              } else {
                output$demographic_table <- DT::renderDataTable({
                  DT::datatable(data.frame(Message = "No demographic data available"))
                })
              }

              # Store additional results for potential future use
              # visualization_results$topic_results
              # visualization_results$demographic_summaries
            } else {
              plot <- error_plot("Not enough data available for visualization. Need at least 5 responses with valid text after filtering.")
              output$demographic_table <- DT::renderDataTable({
                DT::datatable(data.frame(Message = "Not enough data available for analysis"))
              })
            }

            toggle_loader(FALSE)
            if (!is.null(plot)) {
              output$survey_results <- renderPlotly(plot)
            } else {
              output$survey_results <- renderPlotly(error_plot("Unable to generate plot. Please check your data filters."))
            }
            enable("run_report")
          },
          error = function(e) {
            toggle_loader(FALSE)
            output$survey_results <- renderPlotly(error_plot("No plots available"))
            output$demographic_table <- DT::renderDataTable({
              DT::datatable(data.frame(Message = "Error loading data"))
            })
            enable("run_report")
          }
        )
      } else if (q_type == "multi-choice") {
        tryCatch(
          {
            plot <- switch(demographic_desc,
              "income" = multi_choice_questions(data_for_visualization, income_var, NA, income_color_mapping, income_options),
              "education" = multi_choice_questions(data_for_visualization, edu_var, NA, edu_color_mapping, edu_options),
              "age" = multi_choice_questions(data_for_visualization, age_var, NA, age_color_mapping, age_options),
              "gender" = multi_choice_questions(data_for_visualization, gender_var, NA, gender_color_mapping, gender_options),
              "race" = multi_choice_questions(data_for_visualization, race_var, NA, race_color_mapping, race_options)
            )
            toggle_loader(FALSE)
            if (!is.null(plot)) {
              output$survey_results <- renderPlotly(plot)
            } else {
              output$survey_results <- renderPlotly(error_plot("Unable to generate plot. Not enough data for this question type."))
            }
            # Hide table for non-open-ended questions
            output$demographic_table <- DT::renderDataTable({
              DT::datatable(data.frame(Message = "Table not available for this question type"))
            })
            enable("run_report")
          },
          error = function(e) {
            toggle_loader(FALSE)
            output$survey_results <- renderPlotly(error_plot(paste("Error generating plot:", e$message)))
            output$demographic_table <- DT::renderDataTable({
              DT::datatable(data.frame(Message = "Error loading data"))
            })
            enable("run_report")
          }
        )
      } else if (q_type == "select box") {
        tryCatch(
          {
            plot <- switch(demographic_desc,
              "income" = select_box_questions(data_for_visualization, income_var, NA, income_color_mapping, income_options),
              "education" = select_box_questions(data_for_visualization, edu_var, NA, edu_color_mapping, edu_options),
              "age" = select_box_questions(data_for_visualization, age_var, NA, age_color_mapping, age_options),
              "gender" = select_box_questions(data_for_visualization, gender_var, NA, gender_color_mapping, gender_options),
              "race" = select_box_questions(data_for_visualization, race_var, NA, race_color_mapping, race_options)
            )
            toggle_loader(FALSE)
            if (!is.null(plot)) {
              output$survey_results <- renderPlotly(plot)
            } else {
              output$survey_results <- renderPlotly(error_plot("Unable to generate plot. Not enough data for this question type."))
            }
            # Hide table for non-open-ended questions
            output$demographic_table <- DT::renderDataTable({
              DT::datatable(data.frame(Message = "Table not available for this question type"))
            })
            enable("run_report")
          },
          error = function(e) {
            toggle_loader(FALSE)
            output$survey_results <- renderPlotly(error_plot(paste("Error generating plot:", e$message)))
            output$demographic_table <- DT::renderDataTable({
              DT::datatable(data.frame(Message = "Error loading data"))
            })
            enable("run_report")
          }
        )
      } else if (q_type == "combined") {
        print("you are in combined")
        tryCatch({
          plot <- switch(demographic_desc,
            "income" = combined_multi_choice_questions(data_for_visualization, income_var, NA, income_color_mapping, income_options),
            "education" = combined_multi_choice_questions(data_for_visualization, edu_var, NA, edu_color_mapping, edu_options),
            "age" = combined_multi_choice_questions(data_for_visualization, age_var, NA, age_color_mapping, age_options),
            "gender" = combined_multi_choice_questions(data_for_visualization, gender_var, NA, gender_color_mapping, gender_options),
            "race" = combined_multi_choice_questions(data_for_visualization, race_var, NA, race_color_mapping, race_options)
          )
          toggle_loader(FALSE)
          if (!is.null(plot)) {
            output$survey_results <- renderPlotly(plot)
          } else {
            output$survey_results <- renderPlotly(error_plot("Unable to generate plot. Not enough data for this question type."))
          }
          # Hide table for non-open-ended questions
          output$demographic_table <- DT::renderDataTable({
            DT::datatable(data.frame(Message = "Table not available for this question type"))
          })
          enable("run_report")
        },
        error = function(e) {
          toggle_loader(FALSE)
          output$survey_results <- renderPlotly(error_plot(paste("Error generating plot:", e$message)))
          output$demographic_table <- DT::renderDataTable({
            DT::datatable(data.frame(Message = "Error loading data"))
          })
          enable("run_report")
        })
      }
    } else {
      enable("run_report")
      toggle_loader(FALSE)
      message("no plots")
      output$survey_results <- renderPlotly(error_plot("No plots available"))
      output$demographic_table <- DT::renderDataTable({
        DT::datatable(data.frame(Message = "No data available"))
      })
    }
  })
}
