source("table.R")
library(gt)

result_rows <- list(
  "gender" = 1:2,
  "age" = 3:8,
  "education" = 9:12,
  "income" = 13:20,
  "race" = 21:27
)

row_names <- list(
  "Male", "Female",
  "18-24", "25-34", "35-44", "45-54", "55-64", "65 or over",
  "Less than high school", "High-school graduate",
  "Some college/Technical school",
  "Bachelor's and higher",
  "Less than $25,000", "$25,000 to $34,999",
  "$35,000 to $49,999", "$50,000 to $74,999",
  "$75,000 to $99,999", "$100,000 to $149,999",
  "$150,000 to $199,999", "$200,000 or more",
  "Black or African American", "Hispanic", "White",
  "Asian", "Native Hawaiian Pacific Islander",
  "American Indian Alaskan Native", "Two or More Races"
)

get_data_description_ui <- function(survey, demographic, geography, demo) {
  # Data description UI.
  #
  # Return:
  #   box containing the necessary components of
  #   the data description UI.
  #   See get_data_desc_rep_reaction()
  #   for the components.
  data_description <- box(
    title = HTML(paste(
      "<div class='card-title'><h1 class='page-subtitle'>", "<span>",
      survey, "</span>", "Representativeness by", "<span>", demographic,
      "</span>", "Compared to", geography, "</h1>
      </div>"
    )),
    gt_output("new_table"),
    # actionButton(
    #   inputId = "downloadTable", label = "Save Table",
    #   gradient = TRUE, class = "button-common"
    # ),
    downloadButton(
      outputId = "downloadTable", label = "Save Table",
      class = "button-common"
    ),
    callout(
      title = HTML(paste(
        "<div style='display: inline;'>",
        "<p class='page-para'>Representativeness is low for one or more</p>",
        "<span>",
        demo,
        "</span>",
        "<p class='page-para'>categories.</p>",
        actionLink("strategies", "Strategies", class = "page-para"),
        "<br/><p class='page-para'>Some responses do not have enough data
        and are marked as not applicable (or NA)</p>",
        "</div>"
      )),
      # ionicon("alert"),
      status = "danger",
      width = 12,
      class = "strategies-banner"
    ),
    width = 12,
    collapsible = FALSE,
    maximizable = TRUE,
    solidHeader = TRUE,
    elevation = NULL
  )

  return(data_description)
}

get_pal <- function(min_val, max_val) {
  pal <- function(x) {
    if (is.na(x)) {
      return("white")
    }
    f_neg <- scales::col_numeric(
      palette = c("#FFACAC", "#FFFCAE"),
      domain = c(min_val, 0)
    )
    f_pos <- scales::col_numeric(
      palette = c("#FFFCAE", "#B4FFAE"),
      domain = c(0, max_val)
    )
    ifelse(x < 0 | is.na(x), f_neg(x), f_pos(x))
  }
  return(Vectorize(pal))
}

get_representativeness_text <- function(x, min_val, max_val) {
  # Apply color logic
  # Initialize the result list
  result <- list()

  if (is.na(x)) {
    result$text <- "NA"
    result$color <- "white"
  } else if (x < -1) {
    result$text <- "Very Underrepresented"
  } else if (x < -0.25) {
    result$text <- "Underrepresented"
  } else if (x <= 0.25) {
    result$text <- "Equally Represented"
  } else if (x <= 1) {
    result$text <- "Overrepresented"
  } else if (x > 1) {
    result$text <- "Very Overrepresented"
  } else {
    result$text <- "NA"
  }

  # Return as a list with 'text' and 'color' components
  return(result)
}

get_data_desc_rep_reaction <- function(
  input, output, surveyIds,
  survey_data = NA, census_data = NA, file_loc = NA, file_sum = NA,
  demographic_desc
) {
  tbl_data_reactive <- reactiveVal()

  # When run report is pressed populate the data description box with table.
  # Table should have counts of people who answered the survey within wisconsin,
  # and split up into sub categories.
  # Example: using demographic_desc
  reaction <- observeEvent(input$run_report, {
    survey.selected <- input[["survey"]] # e.g. "Air Quality Survey"

    # First, make sure a survey is selected
    if (is.null(survey.selected) || survey.selected == "") {
      showNotification("Please select a survey before running the report.", type = "error")
      return(NULL)
    }

    # Now get the corresponding input ID for the selected survey
    survey.selected.Id <- surveyIds[survey.selected]

    # Make sure the survey ID mapping exists
    if (is.null(survey.selected.Id)) {
      showNotification("Survey ID not found. Please check configuration.", type = "error")
      return(NULL)
    }

    # NOW you can safely access the selected question
    question.selected <- input[[survey.selected.Id]]

    # Validate the selected question
    if (is.null(question.selected) || question.selected == "") {
      showNotification("Please select a question in Step 2 before running the report.", type = "error")
      return(NULL)
    }

    # if the survey box is not empty - that is an option has been selected
    if (survey.selected != "") {
      # id of the survey selected
      survey.selected.Id <- surveyIds[survey.selected]
      survey.selected.question <- input[[survey.selected.Id]]
      n <- 0
      if (survey.selected.question != "") {
        q_number <- as.integer(str_match(
          survey.selected.question,
          "Q\\s*(.*?)\\s*:"
        )[, 2])
        message(survey.selected.question)
        n <- nrow(data.frame(survey_data()[[(4 + q_number)]]) %>% drop_na())
      } else {
        n <- nrow(survey_data())
      }

      if (input$census_level != "") {
        # print(survey_data)
        # print(census_data)
        # print(demographic_desc)

        demographic_desc <- tolower(input$demographic)

        print("'''''")

        census_level <- census_input_to_data[[input$census_level]]
        census_id <- censusInputId[input$census_level]

        # print(census_id)
        # print(input[[census_id]])

        key <- input[[census_id]]

        file <- paste(input_to_data_demo[[input$survey]], census_level,
          census_level_input_to_data[["data"]][[census_level]][[key]],
          sep = "-"
        )
        file_location <- paste(input_to_data_demo[[input$survey]],
          "/", file, ".RData",
          sep = ""
        )

        print(file_location)

        print("'''''")

        data_loc <-
          paste(
            # "/Volumes/cbjackson2/ccs-knowledge/results_summary/",
            "./data/results_summary/",
            file_location,
            sep = ""
          )
        # print(data_loc)

        data_loc_rep <-
          paste(
            # "/Volumes/cbjackson2/ccs-knowledge/results_representativeness/",
            "./data/results_representativeness/",
            file_location,
            sep = ""
          )

        print(data_loc_rep)
        # print(demographic_desc)

        tbl_data <- get_table(data_loc)[[1]]
        rows_to_extract <- result_rows[[demographic_desc]]
        tbl_data_filtered <- tbl_data[rows_to_extract, ]
        tbl_data_filtered <- data.frame(Value = tbl_data_filtered) ### ADDITION
        gt_tbl <- gt(tbl_data_filtered, rownames_to_stub = TRUE)

        # print(tbl_data_filtered)

        loaded_data <- get_table(data_loc_rep)
        tbl_data_rep <- loaded_data[[1]]
        rows_to_extract <- result_rows[[demographic_desc]]
        tbl_data_filtered_rep <- tbl_data_rep[rows_to_extract, ]
        tbl_data_filtered_rep <-
          data.frame(Value = tbl_data_filtered_rep) ### ADDITION
        gt_tbl <- gt(tbl_data_filtered_rep, rownames_to_stub = TRUE)

        # tbl_data_filtered$row_names <- rownames(tbl_data_filtered)
        # tbl_data_filtered_rep$row_names <- rownames(tbl_data_filtered_rep)

        # changed by koustav
        tbl_data_filtered$row_names <- as.character(row_names[result_rows[[demographic_desc]]])
        tbl_data_filtered_rep$row_names <- as.character(row_names[result_rows[[demographic_desc]]])
        # changed by koustav

        # print(tbl_data_filtered_rep)

        # Load population data
        # First, construct the population data file path
        census_level_pop <- census_input_to_data[[input$census_level]]
        key <- input[[census_id]]

        pop_file <- paste("census-population", census_level_pop,
          census_level_input_to_data[["data"]][[census_level_pop]][[key]],
          sep = "-"
        )
        pop_file_location <- file.path(
          "./data/census_population",
          census_level_pop, paste0(pop_file, ".RData")
        )

        print(paste("Looking for population file:", pop_file_location))

        # Try to load population data if file exists
        pop_data <- NULL
        if (file.exists(pop_file_location)) {
          pop_data <- readRDS(pop_file_location)
          print(paste("Loaded population data:", pop_data))
        } else {
          print("Population file not found, using NA values")
        }

        # Merge two tables by row name
        merged_tbl_data <- merge(
          tbl_data_filtered,
          tbl_data_filtered_rep,
          by.x = "row_names", by.y = "row_names",
          all = TRUE
        )

        # Remove the row numbers (serial numbering):
        rownames(merged_tbl_data) <- NULL

        print(str(merged_tbl_data))

        # Add population data as a new column if available
        if (!is.null(pop_data)) {
          # Match population data to demographic categories
          demographic_desc <- tolower(input$demographic)

          # Initialize population values
          pop_values <- rep(NA, nrow(merged_tbl_data))

          if (demographic_desc == "age") {
            # Map age brackets to population data
            age_mapping <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65 or over")
            for (i in 1:length(age_mapping)) {
              row_idx <- which(merged_tbl_data$row_names == age_mapping[i])
              if (length(row_idx) > 0 && i <= length(pop_data)) {
                pop_values[row_idx] <- pop_data[i]
              }
            }
          } else if (demographic_desc == "education") {
            # Map education brackets
            edu_mapping <- c(
              "Less than high school", "High-school graduate",
              "Some college/Technical school", "Bachelor's and higher"
            )
            for (i in 1:length(edu_mapping)) {
              row_idx <- which(merged_tbl_data$row_names == edu_mapping[i])
              if (length(row_idx) > 0 && i <= length(pop_data)) {
                pop_values[row_idx] <- pop_data[i]
              }
            }
          } else if (demographic_desc == "income") {
            # Map income brackets
            income_mapping <- c(
              "Less than $25,000", "$25,000 to $34,999",
              "$35,000 to $49,999", "$50,000 to $74,999",
              "$75,000 to $99,999", "$100,000 to $149,999",
              "$150,000 to $199,999", "$200,000 or more"
            )
            for (i in 1:length(income_mapping)) {
              row_idx <- which(merged_tbl_data$row_names == income_mapping[i])
              if (length(row_idx) > 0 && i <= length(pop_data)) {
                pop_values[row_idx] <- pop_data[i]
              }
            }
          } else if (demographic_desc == "race") {
            # Map race brackets
            race_mapping <- c(
              "Black or African American", "Hispanic", "White",
              "Asian", "Native Hawaiian Pacific Islander",
              "American Indian Alaskan Native", "Two or More Races"
            )
            for (i in 1:length(race_mapping)) {
              row_idx <- which(merged_tbl_data$row_names == race_mapping[i])
              if (length(row_idx) > 0 && i <= length(pop_data)) {
                pop_values[row_idx] <- pop_data[i]
              }
            }
          } else if (demographic_desc == "gender") {
            # Map gender categories
            gender_mapping <- c("Male", "Female")
            for (i in 1:length(gender_mapping)) {
              row_idx <- which(merged_tbl_data$row_names == gender_mapping[i])
              if (length(row_idx) > 0 && i <= length(pop_data)) {
                pop_values[row_idx] <- pop_data[i]
              }
            }
          }

          merged_tbl_data$Population <- pop_values
        } else {
          merged_tbl_data$Population <- NA
        }

        # changed by koustav

        # Just keep the relevant columns (group, Total Count Survey, Population, Representativeness)
        if (!is.null(pop_data)) {
          # If we have population data, keep 4 columns
          merged_tbl_data <- merged_tbl_data[, c(1, 2, 4, 3)]
          # Rename the columns as needed:
          colnames(merged_tbl_data) <- c(
            "group",
            "Total Count Survey", "Population", "Representativeness"
          )
        } else {
          # If no population data, keep first 3 columns
          merged_tbl_data <- merged_tbl_data[, 1:3]
          # Rename the columns as needed:
          colnames(merged_tbl_data) <- c(
            "group",
            "Total Count Survey", "Representativeness"
          )
        }

        # changed by koustav

        # Convert the "group" column to factor
        merged_tbl_data$group <- factor(merged_tbl_data$group,
          levels = unique(merged_tbl_data$group)
        )
        merged_tbl_data$group <- as.character(merged_tbl_data$group)


        # merged_tbl_data$group <- rownames(tbl_data_filtered)

        merged_tbl_data$group <- as.character(
          unlist(row_names[result_rows[[demographic_desc]]], use.names = FALSE)
        )


        # print(str(merged_tbl_data))

        # print(merged_tbl_data)

        # Extract only the columns needed for color calculation
        rep_data_numeric <- merged_tbl_data[, c(
          "Total Count Survey",
          "Representativeness"
        )]

        # Ensure all values are numeric
        rep_data_numeric <- as.matrix(rep_data_numeric)
        rep_data_numeric <- as.numeric(rep_data_numeric)

        # Calculate colors based on numeric values
        colors <- NULL
        colors <- get_pal( # ADDITION
          min(rep_data_numeric, na.rm = TRUE),
          max(rep_data_numeric, na.rm = TRUE)
        )

        print(merged_tbl_data)

        # There's error in here for newly added demogrpahic variables
        # Warning: Error in dplyr::as_tibble: Column 4 must be named.
        # Use `.name_repair` to specify repair.
        # Caused by error in `repaired_names()`:
        # ! Names can't be empty.
        # ✖ Empty name found at location 4.
        #   1: shiny::runApp
        # Warning: Error in dplyr::mutate: Can't transform a data
        # frame with `NA` or `""` names.

        # Apply the function to get both text and color
        representativeness_results <- lapply(
          merged_tbl_data$Representativeness,
          function(x) get_representativeness_text(x, min(rep_data_numeric, na.rm = TRUE), max(rep_data_numeric, na.rm = TRUE))
        )

        # Extract the text and color into separate columns
        merged_tbl_data$Representativeness_text <- sapply(representativeness_results, function(x) x$text)

        # Update the table to use the text column
        merged_tbl_data$Representativeness <- merged_tbl_data$Representativeness_text
        merged_tbl_data$Representativeness_text <- NULL # Remove the temporary column

        # Define the levels
        representativeness_levels <- c(
          "Very Underrepresented",
          "Underrepresented",
          "Equally Represented",
          "Overrepresented",
          "Very Overrepresented",
          "NA"
        )
        # Convert to Factor for coloring
        merged_tbl_data$Representativeness <- factor(
          merged_tbl_data$Representativeness,
          levels = representativeness_levels
        )

        if (is.list(merged_tbl_data$group)) {
          merged_tbl_data$group <- as.character(unlist(merged_tbl_data$group, use.names = FALSE))
        }


        # ---- ADD: append a "Total" summary row for "Total Count Survey" ----

        # Ensure the count column is numeric
        merged_tbl_data$`Total Count Survey` <- as.numeric(merged_tbl_data$`Total Count Survey`)

        # Compute total
        total_count <- sum(merged_tbl_data$`Total Count Survey`, na.rm = TRUE)

        # Build a total row with the SAME column names and compatible types
        total_row <- tibble::tibble(
          group = "Overall Survey Count (All Groups)",
          `Total Count Survey` = total_count,
          # keep factor type by constructing factor with same levels
          Representativeness = factor("NA", levels = representativeness_levels)
        )

        # Bind by name (avoids match.names issues)
        merged_tbl_data <- dplyr::bind_rows(merged_tbl_data, total_row)

        # ---- END ADD ----


        # Create gt table from the merged data
        gt_tbl <- gt(merged_tbl_data, rownames_to_stub = FALSE)

        print(merged_tbl_data)

        # # Formatting decimals
        # gt_tbl <- gt_tbl %>%
        #   fmt_number(
        #     columns = c(
        #       "Representativeness"
        #     ),
        #     decimals = 2
        #   )

        # Assiging color
        # gt_tbl <- gt_tbl %>%
        #   data_color(
        #     method = "numeric",
        #     colors = colors,
        #     columns = c(
        #       "Representativeness"
        #     )
        #   )

        gt_tbl <-
          gt_tbl %>%
          data_color(
            method = "factor",
            palette = c(
              "#FFFCAE", # Equally Represented
              "#FFFFFF", # NA
              "#DAFEAE", # Overrepresented
              "#FFD4AD", # Underrepresented
              "#B4FFAE", # Very Overrepresented
              "#FFACAC" # Very Underrepresented
            ),
            domain = representativeness_levels,
            columns = c(Representativeness)
          ) %>%
          tab_style(
            style = cell_text(
              size = pct(80), color = "#1A1A1A",
              align = "left"
            ),
            locations = list(
              cells_body(), cells_stub(),
              cells_column_labels()
            )
          ) %>%
          tab_style(
            style = cell_text(color = "#000", size = pct(90), align = "left"),
            locations = list(cells_title(), cells_row_groups())
          ) %>%
          tab_options(data_row.padding = px(10), footnotes.font.size = pct(65))

        # Store the merged table data in the reactive variable
        tbl_data_reactive(merged_tbl_data)
        output$new_table <- render_gt(gt_tbl)
      }
    }
  })

  # Download Handler for CSV export
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste("table-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data_to_download <- tbl_data_reactive()
      if (!is.null(data_to_download)) {
        # Inspect the structure
        # print("Structure of data_to_download:")
        # print(str(data_to_download))

        # Identify columns that are lists
        list_cols <- sapply(data_to_download, is.list)

        if (any(list_cols)) {
          # Convert list columns to vectors
          data_to_download[list_cols] <-
            lapply(
              data_to_download[list_cols],
              unlist
            )
        }
        # Write CSV file
        write.csv(data_to_download, file, row.names = FALSE)
      } else {
        showNotification("No data available to download.", type = "error")
        write.csv(data.frame(Message = "No data available"), file,
          row.names = FALSE
        )
      }
    }
  )


  return(reaction)
}
