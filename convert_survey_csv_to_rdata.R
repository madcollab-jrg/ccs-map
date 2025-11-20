# Script to convert CSV survey data files to RData format
# This script reads CSV files from "data/ccs-data-updated/" folder
# and converts them to RData format, removing the first column (index) as done in the current code

# List of survey data files to convert (matching input_to_data_survey_desc mapping)
survey_files <- c(
  "air_map",
  "air_survey",
  "carbon_concerns",
  "ej_report",
  "ej_story",
  "ej_survey",
  "energy_concerns",
  "general_survey",
  "health_impacts",
  "heat_map",
  "heat_survey",
  "tree_knowledge",
  "tree_map",
  "tree_survey"
)

# Base directory
base_dir <- "./data/ccs-data-updated/"

cat("Starting conversion of survey CSV files to RData format...\n\n")

# Process each survey file
for (survey_name in survey_files) {
  csv_path <- file.path(base_dir, survey_name, paste0(survey_name, ".csv"))
  rdata_path <- file.path(base_dir, survey_name, paste0(survey_name, ".RData"))
  
  # Check if CSV file exists
  if (!file.exists(csv_path)) {
    cat("Warning: CSV file not found:", csv_path, "\n")
    next
  }
  
  cat("Processing:", survey_name, "\n")
  cat("  Reading:", csv_path, "\n")
  
  # Read CSV file
  csv_data <- read.csv(csv_path, stringsAsFactors = FALSE)
  
  # Remove first column (index column) as done in current code
  csv_data <- csv_data[, -1, drop = FALSE]
  
  cat("  Rows:", nrow(csv_data), "Columns:", ncol(csv_data), "\n")
  
  # Save as RData using saveRDS (single object, easier to load)
  saveRDS(csv_data, rdata_path)
  
  cat("  Saved:", rdata_path, "\n\n")
}

cat("Conversion complete!\n")
cat("All survey data files have been converted to RData format.\n")

