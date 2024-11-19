ui <- dashboardPage(
  # usei18n(i18n),
  header = dashboardHeader(
    title = tags$a(href = "/", tags$img(
      src = "assets/logo.png", alt = "Logo", height = "50px",
      style = "margin-right: 5px"
    )),
    # selectInput("lang", "Language:",
    #   choices = c("English" = "en", "Spanish" = "es")
    # ),
    navbarMenu(
      id = "navmenu",
      navbarTab(tabName = "home_page", text = "Home"),
      navbarTab(tabName = "avail_data", text = "Datasets"),
      navbarTab(tabName = "reporting_tool", text = "Reporting Tool"),
      navbarTab(tabName = "about", text = "About"),
      navbarTab(tabName = "info_page", text = "Info"),
      navbarTab(tabName = "strategies_page", text = "Strategies")
    )
  ),
  sidebar = dashboardSidebar(
    disable = TRUE
  ),
  body = dashboardBody(
    navbarMenu(
      id = "navmenu",
      navbarTab(tabName = "home_page", text = "Home"),
      navbarTab(tabName = "avail_data", text = "Datasets"),
      navbarTab(tabName = "reporting_tool", text = "Reporting Tool"),
      navbarTab(tabName = "about", text = "About"),
      navbarTab(tabName = "info_page", text = "Info"),
      navbarTab(tabName = "strategies_page", text = "Info 3")
    ),
    useShinyjs(), # Initialize shinyjs
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "home.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "common.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "overrides.css"),
      tags$style(
        HTML(
          "
            @import url('https://fonts.googleapis.com/css2?family=Inter:wght@100;200;300;400;500;600;700;800;900&display=swap');
            body {
              font-family: 'Inter', sans-serif;
            }
          "
        )
      ),
      tags$script(defer = TRUE, type = "text/javascript", src = "main.js")
    ),
    tabItems(
      tabItem(tabName = "home_page", home_tab_body()),
      tabItem(tabName = "avail_data", avail_data_tab_body()),
      tabItem(
        tabName = "reporting_tool",
        reporting_tool_body(
          textOutput("survey"), textOutput("demographic"),
          textOutput("census_level"), textOutput("demo"),
          textOutput("demog"), textOutput("surveyQues")
        )
      ),
      tabItem(tabName = "about", about_tab_body()),
      tabItem(tabName = "info_page", info_tab_body()),
      tabItem(tabName = "strategies_page", strategies_data_tab_body())
    ),
    # Footer content
    tags$footer(
      class = "footer",
      "© 2024 CCS Knowledge Map. All rights reserved."
    )
  ),
  skin = "blue"
)
