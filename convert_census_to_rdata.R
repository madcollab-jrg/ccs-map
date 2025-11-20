# Script to convert CSV census data to RData format
# This script reads CSV files from "delete-do not push/census-data/census/" folder
# and converts them to RData format following the same structure as existing RData files

library(readr)
library(dplyr)

# Define the mapping between demographic types and column suffixes
demographic_mapping <- list(
  "age" = c("_18_to_24", "_25_to_34", "_35_to_44", "_45_to_54", "_55_to_64", "_65_over"),
  "race" = c("BLACK_TOTAL", "HISP_TOTAL", "UNDERREP_TOTAL"),
  "gender" = c("BLACK_MALE", "BLACK_FEMALE", "HISP_MALE", "HISP_FEMALE", "UNDERREP_MALE", "UNDERREP_FEMALE"),
  "income" = NULL, # Will need separate CSV files
  "education" = NULL # Will need separate CSV files
)

# Census level folders to process
census_levels <- c("county", "state", "tract", "zip", "state_lower", "state_upper", "congress")

# Function to aggregate age data from census CSV
aggregate_age_population <- function(csv_file, geoid) {
  # Read the CSV file
  data <- read_csv(csv_file, show_col_types = FALSE)

  # Filter by GEOID
  county_data <- data %>% filter(GEOID == geoid)

  if (nrow(county_data) == 0) {
    return(NULL)
  }

  # Extract age bracket populations (these are pre-aggregated in the CSV)
  # The CSV has columns like UNDERREP_18_to_24, UNDERREP_25_to_34, etc.
  age_population <- c(
    sum(as.numeric(county_data[["UNDERREP_18_to_24"]]), na.rm = TRUE),
    sum(as.numeric(county_data[["UNDERREP_25_to_34"]]), na.rm = TRUE),
    sum(as.numeric(county_data[["UNDERREP_35_to_44"]]), na.rm = TRUE),
    sum(as.numeric(county_data[["UNDERREP_45_to_54"]]), na.rm = TRUE),
    sum(as.numeric(county_data[["UNDERREP_55_to_64"]]), na.rm = TRUE),
    sum(as.numeric(county_data[["UNDERREP_65_over"]]), na.rm = TRUE)
  )

  names(age_population) <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65 or over")

  return(age_population)
}

# Function to process all CSV files for a census level
process_census_level <- function(level) {
  base_path <- file.path("delete-do not push/census-data/census", level)

  if (!dir.exists(base_path)) {
    cat("Directory not found:", base_path, "\n")
    return()
  }

  csv_files <- list.files(base_path, pattern = "\\.csv$", full.names = TRUE)

  if (length(csv_files) == 0) {
    cat("No CSV files found in", base_path, "\n")
    return()
  }

  cat("Processing", length(csv_files), "files for level:", level, "\n")

  # Process each CSV file
  for (csv_file in csv_files) {
    # Extract demographic type from filename (e.g., "censuscounty_age.csv")
    filename <- basename(csv_file)
    demographic_type <- gsub("^census\\w+_(\\w+)\\.csv$", "\\1", filename)

    cat("Processing:", filename, "for demographic:", demographic_type, "\n")

    # Read the CSV
    data <- read_csv(csv_file, show_col_types = FALSE)

    # Create output directory structure matching the existing pattern
    output_dir <- file.path("data/census_population", level)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    # Process each row (each GEOID)
    for (i in 1:nrow(data)) {
      geoid <- as.character(data$GEOID[i])

      # Extract population data based on demographic type
      if (demographic_type == "age") {
        # Regular age file with UNDERREP columns
        pop_data <- c(
          as.numeric(data[i, "UNDERREP_18_to_24"], na.rm = TRUE),
          as.numeric(data[i, "UNDERREP_25_to_34"], na.rm = TRUE),
          as.numeric(data[i, "UNDERREP_35_to_44"], na.rm = TRUE),
          as.numeric(data[i, "UNDERREP_45_to_54"], na.rm = TRUE),
          as.numeric(data[i, "UNDERREP_55_to_64"], na.rm = TRUE),
          as.numeric(data[i, "UNDERREP_65_over"], na.rm = TRUE)
        )
        names(pop_data) <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65 or over")
      } else if (demographic_type == "age2") {
        # Age2 file uses ALL_TOTAL columns - we want the ALL age brackets
        pop_data <- c(
          as.numeric(data[i, "ALL_18_to_24"], na.rm = TRUE),
          as.numeric(data[i, "ALL_25_to_34"], na.rm = TRUE),
          as.numeric(data[i, "ALL_35_to_44"], na.rm = TRUE),
          as.numeric(data[i, "ALL_45_to_54"], na.rm = TRUE),
          as.numeric(data[i, "ALL_55_to_64"], na.rm = TRUE),
          as.numeric(data[i, "ALL_65_over"], na.rm = TRUE)
        )
        names(pop_data) <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65 or over")
      } else if (demographic_type == "genderedu") {
        # Gender and education data - extract gender info from UNDERREP columns
        # For gender, we use UNDERREP_MALE and UNDERREP_FEMALE
        pop_data <- c(
          as.numeric(data[i, "UNDERREP_MALE"], na.rm = TRUE),
          as.numeric(data[i, "UNDERREP_FEMALE"], na.rm = TRUE)
        )
        names(pop_data) <- c("Male", "Female")
      } else if (demographic_type %in% c("genderedu2", "genders2")) {
        # Alternative gender/education files use ALL_TOTAL columns
        pop_data <- c(
          as.numeric(data[i, "ALL_LESS_HS_TOTAL"], na.rm = TRUE),
          as.numeric(data[i, "ALL_HS_TOTAL"], na.rm = TRUE),
          as.numeric(data[i, "ALL_SOME_COLLEGE_TOTAL"], na.rm = TRUE),
          as.numeric(data[i, "ALL_COLLEGE_TOTAL"], na.rm = TRUE)
        )
        names(pop_data) <- c(
          "Less than high school", "High-school graduate",
          "Some college/Technical school", "Bachelor's and higher"
        )
      } else if (demographic_type == "income") {
        # Income data - uses UNDERREP income brackets
        pop_data <- c(
          as.numeric(data[i, "UNDERREP_less25"], na.rm = TRUE),
          as.numeric(data[i, "UNDERREP_25_to_34"], na.rm = TRUE),
          as.numeric(data[i, "UNDERREP_35_to_49"], na.rm = TRUE),
          as.numeric(data[i, "UNDERREP_50_to_74"], na.rm = TRUE),
          as.numeric(data[i, "UNDERREP_75_to_99"], na.rm = TRUE),
          as.numeric(data[i, "UNDERREP_100_to_149"], na.rm = TRUE),
          as.numeric(data[i, "UNDERREP_150_to_199"], na.rm = TRUE),
          as.numeric(data[i, "UNDERREP_200_more"], na.rm = TRUE)
        )
        names(pop_data) <- c(
          "Less than $25,000", "$25,000 to $34,999",
          "$35,000 to $49,999", "$50,000 to $74,999",
          "$75,000 to $99,999", "$100,000 to $149,999",
          "$150,000 to $199,999", "$200,000 or more"
        )
      } else if (demographic_type == "income2") {
        # Income2 uses ALL columns
        pop_data <- c(
          as.numeric(data[i, "ALL_less25"], na.rm = TRUE),
          as.numeric(data[i, "ALL_25_to_34"], na.rm = TRUE),
          as.numeric(data[i, "ALL_35_to_49"], na.rm = TRUE),
          as.numeric(data[i, "ALL_50_to_74"], na.rm = TRUE),
          as.numeric(data[i, "ALL_75_to_99"], na.rm = TRUE),
          as.numeric(data[i, "ALL_100_to_149"], na.rm = TRUE),
          as.numeric(data[i, "ALL_150_to_199"], na.rm = TRUE),
          as.numeric(data[i, "ALL_200_more"], na.rm = TRUE)
        )
        names(pop_data) <- c(
          "Less than $25,000", "$25,000 to $34,999",
          "$35,000 to $49,999", "$50,000 to $74,999",
          "$75,000 to $99,999", "$100,000 to $149,999",
          "$150,000 to $199,999", "$200,000 or more"
        )
      } else if (demographic_type == "raceeth") {
        # Race/ethnicity data - aggregated totals
        pop_data <- c(
          as.numeric(data[i, "baa_over18"], na.rm = TRUE), # Black or African American
          as.numeric(data[i, "hisp_over18"], na.rm = TRUE), # Hispanic
          as.numeric(data[i, "white_over18"], na.rm = TRUE), # White
          as.numeric(data[i, "asian_over18"], na.rm = TRUE), # Asian
          as.numeric(data[i, "nhpi_over18"], na.rm = TRUE), # Native Hawaiian Pacific Islander
          as.numeric(data[i, "amin_over18"], na.rm = TRUE), # American Indian Alaskan Native
          as.numeric(data[i, "multi_over18"], na.rm = TRUE) # Mixed (Two or More Races)
        )
        names(pop_data) <- c(
          "Black or African American", "Hispanic", "White",
          "Asian", "Native Hawaiian Pacific Islander",
          "American Indian Alaskan Native", "Two or More Races"
        )
      } else {
        # For other demographic types, skip
        next
      }

      # Create output filename following the existing pattern
      # Example: "census-population-county-55025.RData"
      output_file <- file.path(output_dir, paste0("census-population-", level, "-", geoid, ".RData"))

      # Save as RData
      saveRDS(pop_data, output_file)

      cat("  Created:", output_file, "with data:", pop_data, "\n")
    }
  }

  cat("Completed processing for level:", level, "\n")
}

# Main execution
cat("Starting census data conversion...\n")

# Process each census level
for (level in census_levels) {
  process_census_level(level)
}

cat("Conversion complete!\n")
