library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(yaml)
library(shinyjs)
library(shiny.i18n)

source("pages/components/selection_box.R")
source("pages/components/graphics_representative.R")
source("pages/components/solving-data-box.R")
source("pages/components/survey_results.R")
source("pages/components/data_util.R")

source("pages/reporting_tool.R")

source("pages/home_page.R")
source("pages/avail_data.R")
source("pages/about.R")
source("pages/info_page.R")
source("pages/strategies_page.R")

source("helpers/utils/survey_declarations.R")

source("helpers/global/ui.R")
source("helpers/global/server.R")

# i18n <- Translator$new(translation_csvs_path = "translation.csv")
# i18n <- Translator$new(config_yaml = "translation_config.yaml")
# i18n$set_translation_language("en") # Default language

# run app
shinyApp(ui = ui, server = server)
