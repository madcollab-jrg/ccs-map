server <- function(input, output, session) {
  
  output$survey <- renderText({ input$survey })
  output$surveyQues <- renderText({ input$survey })
  output$demographic <- renderText({ input$demographic })
  output$demo <- renderText({ input$demographic })
  output$demog <- renderText({ input$demographic })
  output$census_level <- renderText({ input$census_level })
  
  survey_data_loc <- "./data/ccs-data-updated/"
  
  survey_data <- eventReactive(input$run_report, {
    req(input$survey, input$demographic)
    name <- input$survey
    read.csv(paste0(survey_data_loc, input_to_data_survey_desc[[name]], "/", input_to_data_survey_desc[[name]], ".csv"))[, -1]
  })
  
  census_data <- reactive({
    census_level <- census_input_to_data[[input$census_level]]
    census_id <- censusInputId[input$census_level]
  })
  
  demographic_data <- reactive({
    demographic_desc_to_data[[input$demographic]]
  })
  
  file_to_get <- reactive({
    input$run_report
    if (!is.null(input$survey) && input$survey != "" &&
        !is.null(input$census_level) && input$census_level != "" &&
        !is.null(input$demographic) && input$demographic != "") {
      
      census_level <- census_input_to_data[[input$census_level]]
      census_id <- censusInputId[[input$census_level]]
      
      if (is.null(census_id) || is.null(input[[census_id]]) || input[[census_id]] == "") {
        return(NULL)
      }
      
      key <- input[[census_id]]
      file <- paste(input_to_data_demo[[input$survey]], census_level, census_level_input_to_data[["data"]][[census_level]][[key]], sep = "-")
      paste0(input_to_data_demo[[input$survey]], "/", file, ".RData")
    } else {
      NULL
    }
  })
  
  file_to_get_sum <- reactive({
    req(input$run_report)
    if (!is.null(input$survey) && input$survey != "" &&
        !is.null(input$census_level) && input$census_level != "" &&
        !is.null(input$demographic) && input$demographic != "") {
      
      census_level <- census_input_to_data[[input$census_level]]
      census_id <- censusInputId[[input$census_level]]
      
      if (is.null(census_id) || is.null(input[[census_id]]) || input[[census_id]] == "") {
        return(NULL)
      }
      
      key <- input[[census_id]]
      file <- paste(input_to_data_survey_desc[[input$survey]], census_level, census_level_input_to_data[["data"]][[census_level]][[key]], sep = "-")
      paste0(input_to_data_survey_desc[[input$survey]], "/", file, ".RData")
    } else {
      NULL
    }
  })
  
  restricted_surveys <- c("Carbon Concerns", "Energy Concerns", "General Survey", "Health Impacts", "Tree Knowledge")
  
  observeEvent(input$survey, {
    updateSelectizeInput(session, "census_level",
                         choices = if (input$survey %in% restricted_surveys) c("Zipcode") else c("Census Tract", "Census State", "Census County", "Zipcode", "State Lower", "State Upper", "Congress"),
                         selected = if (input$survey %in% restricted_surveys) "Zipcode" else ""
    )
  })
  
  observeEvent(input$survey, {
    updateSelectizeInput(session, "demographic",
                         choices = if (input$survey %in% restricted_surveys) c("Gender", "Income", "Education", "Race") else c("Age", "Gender", "Income", "Education", "Race"),
                         selected = if (input$survey %in% restricted_surveys) "Gender" else ""
    )
  })
  
  question_number <- eventReactive(input$run_report, {
    req(input$survey, input$demographic)
    question <- input[[surveyInputId[[input$survey]]]]
    as.integer(str_extract(question, "[0-9]+"))
  })
  
  question_type <- eventReactive(input$run_report, {
    req(input$survey, input$demographic)
    get_question_type(input$survey, question_number())
  })
  
  question_subtype <- eventReactive(input$run_report, {
    req(input$survey, input$demographic)
    get_question_subtype(input$survey, question_number())
  })
  
  is_survey <- eventReactive(input$run_report, {
    req(input$survey, input$demographic)
    has_results[[input$survey]]
  })
  
  get_data_desc_rep_reaction(input, output, surveyInputId,
                             survey_data, census_data,
                             file_loc = file_to_get,
                             file_sum = file_to_get_sum,
                             demographic_desc = demographic_data()
  )
  
  resulting_graphics(
    input, output,
    survey_data = reactive({
      df <- survey_data()
      if (is.null(df) || nrow(df) == 0) {
        h4("No data available for the selected filters")
        return(NULL)
      }
      df
    }),
    is_survey,
    question_number, question_type, question_subtype,
    demographic_desc = demographic_data()
  )
  
  observeEvent(input$examineDataBtn, { updateTabItems(session, "navmenu", "reporting_tool") })
  observeEvent(input$availDataBtn,   { updateTabItems(session, "navmenu", "avail_data") })
  observeEvent(input$helpBtn,        { updateTabItems(session, "navmenu", "about") })
  observeEvent(input$definitionsBtn, { updateTabItems(session, "navmenu", "avail_data") })
  observeEvent(input$datasetEle,     { updateTabItems(session, "navmenu", "avail_data") })
  observeEvent(input$howWeAnalEle,   { updateTabItems(session, "navmenu", "info_page") })
  observeEvent(input$curatedData,    { updateTabItems(session, "navmenu", "avail_data") })
  observeEvent(input$strategies,     { updateTabItems(session, "navmenu", "strategies_page") })
  
  observe({
    cat("survey:", input$survey, "| demo:", input$demographic, "| level:", input$census_level, "\n")
  })
  
  input_ready <- reactive({
    survey_val <- input$survey
    demo_val <- input$demographic
    level_val <- input$census_level
    
    # Safe default
    ques_val <- NULL
    
    if (!is.null(survey_val) && survey_val %in% names(surveyInputId)) {
      survey_id <- surveyInputId[[survey_val]]
      ques_val <- input[[survey_id]]
    }
    
    all(
      !is.null(survey_val), survey_val != "", !is.na(survey_val),
      !is.null(demo_val), demo_val != "", !is.na(demo_val),
      !is.null(level_val), level_val != "", !is.na(level_val),
      !is.null(ques_val), ques_val != "", !is.na(ques_val)
    )
  })
  
  
  observe({
    req(input$survey, input$demographic, input$census_level)
    
    survey_val <- input$survey
    demo_val <- input$demographic
    level_val <- input$census_level
    
    req_id <- if (survey_val %in% names(surveyInputId)) surveyInputId[[survey_val]] else NULL
    ques_val <- if (!is.null(req_id)) input[[req_id]] else NA
    
    cat("Checking inputs:",
        "\nsurvey:", survey_val,
        "\ndemographic:", demo_val,
        "\ncensus_level:", level_val,
        "\nquestion:", req_id, "=", ques_val, "\n")
  })
  
  
  
  
  observe({
    if (input_ready()) {
      shinyjs::enable("run_report")
    } else {
      shinyjs::disable("run_report")
    }
  })
  
  
  
  
  
  
  observe({
    cat("\n[DEBUG] level:", input$census_level,
        "| state_id:", input$census_state_items,
        "| county_id:", input$census_county_items,
        "| tract_id:", input$census_tract_items, "\n")
  })
  
}
