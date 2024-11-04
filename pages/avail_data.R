library(bs4Dash)
library(shinydashboard)

# Datasets Page

avail_data_tab_body <- function() {
  fluidRow(
    class = "container-row",
    style = "margin-top: 24px;",
    column(
      width = 12,
      h1("Data curated for the Knowledge Map", class = "page-title"),
      hr(style = "border-top: 1px solid #A8AAAD;"),
      h2("Urban Heat", class = "page-subheading"),
      p(
        "This survey contains on how community
        members are impacted by extreme heat, their behaviors during
        hot weather, and personal steps taken to mitigate heat. It also
        explores the community’s interest in participating in local efforts
        to address urban heat and the role of local government in mitigating
        its effects.",
        class = "page-para"
      ),
      HTML("
        <p class='page-para'><strong>Date</strong>:
        October 2022 – November 2022</p>
        <p class='page-para'><strong>Respondents</strong>: 243</p>
        <p class='page-para'><strong>Geography</strong>: Dane County</p>
      "),
      br(),
      h2("Air Quality", class = "page-subheading"),
      p(
        "The survey focuses on understanding community perceptions and
        experiences related to air quality in their neighborhood, including
        specific problems, health impacts, pollution sources, and the effect
        on vulnerable populations. It also seeks input on potential solutions,
        such as the placement of air quality monitoring sensors, personal
        efforts to improve air quality, and preferred methods of
        communication for air quality alerts and participation in
        decision-making processes.",
        class = "page-para"
      ),
      HTML("
        <p class='page-para'><strong>Date</strong>:
        June – August 2023</p>
        <p class='page-para'><strong>Respondents</strong>: 243</p>
        <p class='page-para'><strong>Geography</strong>: Dane County</p>
      "),
      br(),
      h2("Tree Canopies", class = "page-subheading"),
      p(
        "The survey is designed to gauge community opinions on various
        aspects of trees within their neighborhood, including their benefits,
        agreement with certain statements, and observations of tree-related
        damage to infrastructure. It also explores individual preferences
        and values concerning trees, such as desired types of trees to be
        planted, areas in need of more trees, tree-related goals, factors
        affecting personal tree planting decisions, and willingness to
        participate in their city’s tree planting efforts.",
        class = "page-para"
      ),
      br(),
      HTML("
        <p class='page-para'><strong>Date</strong>:
        June – August 2023</p>
        <p class='page-para'><strong>Respondents</strong>: 243</p>
        <p class='page-para'><strong>Geography</strong>: Dane County</p>
      "),
      h2("Environmental Justice", class = "page-subheading"),
      p(
        "The survey focuses on understanding community perceptions and
        experiences related to air quality in their neighborhood, including
        specific problems, health impacts, pollution sources, and the effect
        on vulnerable populations. It also seeks input on potential solutions,
        such as the placement of air quality monitoring sensors, personal
        efforts to improve air quality, and preferred methods of
        communication for air quality alerts and participation in
        decision-making processes.",
        class = "page-para"
      ),
      HTML("
        <p class='page-para'><strong>Date</strong>:
        June – August 2023</p>
        <p class='page-para'><strong>Respondents</strong>: 243</p>
        <p class='page-para'><strong>Geography</strong>: Dane County</p>
      "),
      br(),
      h2("Environmental Justice Report", class = "page-subheading"),
      p(
        "The survey focuses on understanding community perceptions and
        experiences related to air quality in their neighborhood, including
        specific problems, health impacts, pollution sources, and the effect
        on vulnerable populations. It also seeks input on potential solutions,
        such as the placement of air quality monitoring sensors, personal
        efforts to improve air quality, and preferred methods of
        communication for air quality alerts and participation in
        decision-making processes.",
        class = "page-para"
      ),
      HTML("
        <p class='page-para'><strong>Date</strong>:
        June – August 2023</p>
        <p class='page-para'><strong>Respondents</strong>: 243</p>
        <p class='page-para'><strong>Geography</strong>: Dane County</p>
      "),
      br(),
    )
  )
}
