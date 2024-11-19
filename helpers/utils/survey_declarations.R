surveyInputId <- c(
  "Air Quality Survey" = "air_quality_qs",
  "Environmental Justice Survey" = "ej_survey_qs",
  "Tree Canopy Survey" = "tree_canopy_qs",
  "Urban Heat Survey" = "urban_heat_qs",
  "Air Quality Map" = "air_quality_map_qs",
  "Tree Canopy Map" = "tree_canopy_map_qs",
  "Urban Heat Map" = "urban_heat_map_qs",
  "Carbon Concerns" = "carbon_survey_qs",
  "Carbon Survey" = "carbon_survey_qs",
  "Energy Survey" = "energy_survey_qs",
  "General Survey" = "general_survey_qs",
  "Heat Health Survey" = "heat_health_survey_qs",
  "Trees Greenery Survey" = "trees_greenery_survey_qs",
  "Tree Knowledge" = "tree_knowledge_qs",
  "Energy Concerns" = "energy_concerns_qs",
  "General Survey" = "general_survey_qs",
  "Health Impacts" = "health_impacts_qs"
)

has_results <- c(
  "Air Quality Survey" = TRUE,
  "Environmental Justice Survey" = TRUE, "Tree Canopy Survey" = TRUE,
  "Urban Heat Survey" = TRUE,
  "Air Quality Map" = TRUE, "Tree Canopy Map" = TRUE, "Urban Heat Map" = TRUE,
  "Carbon Survey" = TRUE,
  "Carbon Concerns" = TRUE,
  "Tree Knowledge" = TRUE,
  "Energy Concerns" = TRUE,
  "Health Impacts" = TRUE,
  "Energy Survey" = TRUE, "General Survey" = TRUE,
  "Heat Health Survey" = TRUE,
  "Trees Greenery Survey" = TRUE
)

censusInputId <- c(
  "Census Tract" = "census_tract_items",
  "Census State" = "census_state_items",
  "Census County" = "census_county_items",
  "Zipcode" = "census_zipcode_items",
  "State Lower" = "census_state_lower_items",
  "State Upper" = "census_state_upper_items",
  "Congress" = "census_congress_items"
)
input_to_data_demo <- c(
  "Air Quality Survey" = "air-quality-survey",
  "Environmental Justice Survey" = "ej-survey",
  "Tree Canopy Survey" = "tree-canopy-survey",
  "Urban Heat Survey" = "urban-heat-survey",
  "Urban Heat Map" = "urban-heat-map",
  "Air Quality Map" = "air-quality-map",
  "Tree Canopy Map" = "tree-canopy-map",
  "Carbon Concerns" = "carbon-concerns",
  "Tree Knowledge" = "tree-knowledge",
  "Energy Concerns" = "energy-concerns",
  "General Survey" = "general-survey",
  "Health Impacts" = "health-impacts",
  "Trees Greenery Survey" = "trees_greenery_survey"
)
census_input_to_data <- c(
  "Census Tract" = "tract",
  "Census State" = "state",
  "Census County" = "county",
  "Zipcode" = "zipcode",
  "State Lower" = "state_lower",
  "State Upper" = "state_upper",
  "Congress" = "congress"
)
census_level_input_to_data <-
  read_yaml("census_items/census_level_to_results.yaml")

input_to_data_survey_desc <- c(
  "Air Quality Map" = "air_map",
  "Air Quality Survey" = "air_survey",
  "Carbon Concerns" = "carbon_concerns",
  # "Environmental Justice Report" = "ej_report",
  # "Environmental Justice Story" = "ej_story",
  "Environmental Justice Survey" = "ej_survey",
  "Energy Concerns" = "energy_concerns",
  "General Survey" = "general_survey",
  "Health Impacts" = "health_impacts",
  "Urban Heat Survey" = "heat_survey",
  "Urban Heat Map" = "heat_map",
  "Heat Map" = "heat_map",
  "Heat Survey" = "heat_survey",
  "Tree Knowledge" = "tree_knowledge",
  "Tree Map" = "tree_map",
  "Tree Survey" = "tree_survey",
  "Tree Canopy Survey" = "tree_survey",
  "Tree Canopy Map" = "tree_map"
)

question_type_map <- c()

demographic_desc_to_data <- c(
  "Age" = "age",
  "Gender" = "gender",
  "Income" = "income",
  "Education" = "education",
  "Race" = "race"
)
