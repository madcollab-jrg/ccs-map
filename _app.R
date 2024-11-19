library(shiny)
library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "./translation/translation.json")
i18n$set_translation_language("en")

ui <- fluidPage(  
  selectInput("lang", "Language:",
    choices = c("English" = "en", "Spanish" = "es")
  ),
  p(i18n$t("Home"))
)

server <- function(input, output) {
  observeEvent(input$lang, {
  i18n$set_translation_language(input$lang)
})}

shinyApp(ui, server)