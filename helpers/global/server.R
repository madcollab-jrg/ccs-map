server <- function(input, output, session) {
  # Function to update the selected survey, demo, level
  output$survey <- renderText({
    paste(input$survey)
  })

  output$surveyQues <- renderText({
    paste(input$survey)
  })

  output$demographic <- renderText({
    paste(input$demographic)
  })

  output$demo <- renderText({
    paste(input$demographic)
  })

  output$demog <- renderText({
    paste(input$demographic)
  })

  output$census_level <- renderText({
    paste(input$census_level)
  })

  # demographic_data_loc <-
  #   "/Volumes/cbjackson2/ccs-knowledge/ccs-data-demographic_unprocessed/"
  # survey_data_loc <- "/Volumes/cbjackson2/ccs-knowledge/ccs-data/"

  # survey_data_loc <- "/Volumes/cbjackson2/ccs-knowledge/ccs-data-updated/"

  survey_data_loc <- "./data/ccs-data-updated/"


  # import survey data

  survey_data <- eventReactive(
    list(input$run_report),
    {
      req(input$survey)
      req(input$demographic)
      # import data here - reactive to input$survey
      name <- input$survey
      # survey_data <-
      #   read.csv(paste(survey_data_loc,
      # input_to_data_survey[[name]], sep = ""))
      survey_data <-
        read.csv(paste(survey_data_loc, input_to_data_survey_desc[[name]], "/",
          input_to_data_survey_desc[[name]], ".csv",
          sep = ""
        ))
      # print(paste(survey_data_loc, input_to_data_survey_desc[[name]], "/",
      #   input_to_data_survey_desc[[name]], ".csv",
      #   sep = ""
      # ))
      survey_data[, -1]
    }
  )

  # import census data
  census_data <- reactive({
    census_level <- census_input_to_data[[input$census_level]]
    census_id <- censusInputId[input$census_level]
  })

  # import demographic data
  demographic_data <- reactive({
    demographic_id <- demographic_desc_to_data[[input$demographic]]
    return(demographic_id)
  })

  # file for representativeness scores
  file_to_get <- reactive({
    input$run_report
    if (input$survey != "" & input$census_level != "" &
      input$demographic != "") {
      census_level <- census_input_to_data[[input$census_level]]
      census_id <- censusInputId[input$census_level]

      # print(census_id)
      # print(input[[census_id]])

      key <- input[[census_id]]

      file <- paste(input_to_data_demo[[input$survey]], census_level,
        census_level_input_to_data[["data"]][[census_level]][[key]],
        sep = "-"
      )
      file_loc <- paste(input_to_data_demo[[input$survey]],
        "/", file, ".RData",
        sep = ""
      )
    } else {
      file <- ""
    }
  })

  file_to_get_sum <- reactive({
    req(input$run_report)
    if (input$survey != "" & input$census_level != "" &
      input$demographic != "") {
      census_level <- census_input_to_data[[input$census_level]]
      census_id <- censusInputId[input$census_level]

      key <- input[[census_id]]

      file <- paste(input_to_data_survey_desc[[input$survey]], census_level,
        census_level_input_to_data[["data"]][[census_level]][[key]],
        sep = "-"
      )
      file_sum <- paste(input_to_data_survey_desc[[input$survey]],
        "/", file, ".RData",
        sep = ""
      )
    } else {
      file <- ""
    }
  })


  restricted_surveys <- c("Carbon Concerns", "Energy Concerns",
    "General Survey", "Health Impacts")


  observeEvent(input$survey, {
    # If "Carbon Concerns" is selected, update the 'census_level' choices
    if (input$survey %in% restricted_surveys) {
      updateSelectizeInput(session, "census_level", 
        choices = c("Zipcode"),  # Only show "Zipcode" for Carbon Concerns
        selected = "Zipcode"  # Set "Zipcode" as the selected option
      )
    } else {
      # Otherwise, show the full list of geography options
      updateSelectizeInput(session, "census_level", 
        choices = c("Census Tract", "Census State", "Census County", 
                    "Zipcode", "State Lower", "State Upper", "Congress"),
        selected = ""  # Reset the selection
      )
    }
  })


  # get question column number + question type for survey results
  question_number <- eventReactive(
    list(input$run_report),
    {
      req(input$survey)
      req(input$demographic)
      surveyQid <- surveyInputId[[input$survey]]
      question <- input[[surveyQid]]
      as.integer(str_extract(question, regex("[0-9]+"))) #+3
    }
  )

  # get question type for graphics display
  question_type <- eventReactive(
    list(input$run_report),
    {
      req(input$survey)
      req(input$demographic)
      q_num <- question_number()
      print(q_num)
      get_question_type(input$survey, q_num)
    }
  )

  # question subtype - needed for matrix type questions
  question_subtype <- eventReactive(
    list(input$run_report),
    {
      req(input$survey)
      req(input$demographic)
      q_num <- question_number()
      get_question_subtype(input$survey, q_num)
    }
  )

  # is something a survey - For non-survey they haven't
  #  decided on graphical displays
  is_survey <- eventReactive(list(input$run_report), {
    req(input$survey)
    req(input$demographic)
    has_results[[input$survey]]
  })

  # # middle panel data description
  # get_data_description_reaction(input, output, surveyInputId,
  #   survey_data, census_data,
  #   file_loc = file_to_get
  # )

  get_data_desc_rep_reaction(input, output, surveyInputId,
    survey_data, census_data,
    file_loc = file_to_get,
    file_sum = file_to_get_sum,
    demographic_desc = demographic_data()
  )

  # Representation
  # get_representative_reactive(input, output, file_to_get)

  # results graphics

  # print(is_survey)

  resulting_graphics(
    input, output, survey_data, is_survey,
    question_number, question_type, question_subtype,
    demographic_desc = demographic_data()
  )

  # observeEvent(input$lang, {
  #   i18n$set_translation_language(input$lang)
  #   # Re-render UI components to reflect language change
  # })

  # all button and action link interaction on UI
  observeEvent(input$examineDataBtn, {
    updateTabItems(session, "navmenu", "reporting_tool")
  })
  observeEvent(input$availDataBtn, {
    updateTabItems(session, "navmenu", "avail_data")
  })
  observeEvent(input$helpBtn, {
    updateTabItems(session, "navmenu", "about")
  })
  observeEvent(input$definitionsBtn, {
    updateTabItems(session, "navmenu", "avail_data")
  })
  observeEvent(input$datasetEle, {
    updateTabItems(session, "navmenu", "avail_data")
  })
  observeEvent(input$howWeAnalEle, {
    updateTabItems(session, "navmenu", "info_page")
  })
  observeEvent(input$curatedData, {
    updateTabItems(session, "navmenu", "avail_data")
  })
  observeEvent(input$strategies, {
    updateTabItems(session, "navmenu", "strategies_page")
  })
  shinyjs::enable("run_report")
}
