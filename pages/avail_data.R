library(bs4Dash)
library(shinydashboard)

# Datasets Page

avail_data_tab_body <- function() {
  fluidRow(
    class = "container-row",
    style = "margin-top: 24px;",
    column(
      12,
      h1("Datasets", class = "page-title"),
      hr(style = "border-top: 1px solid #A8AAAD;"),
    ),
    column(
      5,
      p("Data from community engagements focused on environmental topics such as air quality,
       urban heat, tree canopies, and environmental justice are available. This data includes 
       close-ended survey questions, open-ended responses, and geographically 
       pinned inputs, providing us with insights into community priorities, challenges, 
       and environmental awareness.",class = "page-para"),

      h2("In-Person Deliberation (October 2022)", class = "page-subtitle"),
      p("In October 2022, 80 community members participated in discussions on topics related to 
        environmental and community well-being. The surveys focused on various topics including, 
        assessing the community's awareness of tree benefits and green spaces, attitudes toward 
        carbon emissions and removal technologies, energy-saving practices and their impact 
        on daily life, and examining how extreme heat affects health.",class = "page-para"),

      h2("Follow-up In-Person Meetings (March 2023)", class = "page-subtitle"),
      p("In March 2023, 40 community members attended follow-up meetings to provide feedback 
        on key environmental issues. These discussions focused on Tree Knowledge, emphasizing 
        the community’s views on the role of trees in cooling and environmental health, 
        Air Quality, exploring pollution concerns and health impacts, and Urban Heat, addressing 
        challenges related to extreme heat and local mitigation strategies.",class = "page-para"),

      h2("Community Climate Solutions", class = "page-subtitle"),
      p("In Summer 2023, we launched Wisconsin (",
        a("Community Climate Solutions", 
            href = "https://ccs.mysocialpinpoint.com/", 
            target = "_blank", 
            rel = "noopener noreferrer"),") 
      to gather feedback from 400 residents on key environmental challenges. 
      The surveys explored concerns around urban heat, tree canopies, air quality, 
      and environmental justice.",class = "page-para"),

      p("Select a survey on the right to view its description.",class = "page-para"),
    ),
    column(
      7,
      accordion(
        id = "accordion1",
        accordionItem(
          title =
            h2("Urban Heat", class = "page-subheading"),
          subtitle = "Lorem",
          status = "white",
          collapsed = FALSE,
          p(
            "This survey contains on how community members are impacted by 
            extreme heat, their behaviors during hot weather, and personal 
            steps taken to mitigate heat. It also explores the community’s 
            interest in participating in local efforts to address urban 
            heat and the role of local government in mitigating its effects.",
            class = "page-para"
          ),
          HTML("
        <p class='page-para'><strong>Date</strong>:Summer 2023</p>
        <p class='page-para'><strong>Method</strong>: In-Person</p>
        <p class='page-para'><strong>Respondents</strong>: 243</p>
        <p class='page-para'><strong>Geography</strong>: Dane County</p>
      "),
        ),
        accordionItem(
          title =
            h2("Air Quality", class = "page-subheading"),
          status = "white",
          collapsed = TRUE,
          p(
            "This survey contains insights on community experiences with air pollution, 
            its health impacts, and perceptions of changes in air quality over time. 
            It also explores awareness of pollutants, concerns for vulnerable populations, 
            and suggestions for improving air quality monitoring. 
            Lastly, it invites input on potential community involvement in decision-making 
            regarding air quality management.",
            class = "page-para"
          ),
          HTML("
        <p class='page-para'><strong>Date</strong>:Summer 2023</p>
        <p class='page-para'><strong>Method</strong>: In-Person</p>
        <p class='page-para'><strong>Respondents</strong>: 243</p>
        <p class='page-para'><strong>Geography</strong>: Dane County</p>
      "),
        ),
        accordionItem(
          title =
            h2("Tree Canopies", class = "page-subheading"),
          status = "white",
          collapsed = TRUE,
          p(
            "The survey explores changes in tree populations and their health over the 
            past decade, as well as community perceptions of the benefits trees provide. 
            It assesses how trees impact quality of life and seeks suggestions for 
            areas in need of more tree coverage, along with interest in participating 
            in tree-planting initiatives.",
            class = "page-para"
          ),
          br(),
          HTML("
        <p class='page-para'><strong>Date</strong>:Summer 2023</p>
        <p class='page-para'><strong>Method</strong>: In-Person</p>
        <p class='page-para'><strong>Respondents</strong>: 243</p>
        <p class='page-para'><strong>Geography</strong>: Dane County</p>
      "),
        ),
        accordionItem(
          title =
            h2("Environmental Justice", class = "page-subheading"),
          status = "white",
          collapsed = TRUE,
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
        <p class='page-para'><strong>Date</strong>:Summer 2023</p>
        <p class='page-para'><strong>Method</strong>: In-Person</p>
        <p class='page-para'><strong>Respondents</strong>: 243</p>
        <p class='page-para'><strong>Geography</strong>: Dane County</p>
      "),
        ),
        accordionItem(
          title =
            h2("Environmental Justice Report", class = "page-subheading"),
          status = "white",
          collapsed = TRUE,
          p(
            "This survey contains insights around big ideas in addressing climate change in communities.",
            class = "page-para"
          ),
          HTML("
        <p class='page-para'><strong>Date</strong>:Summer 2023</p>
        <p class='page-para'><strong>Method</strong>: In-Person</p>
        <p class='page-para'><strong>Respondents</strong>: 243</p>
        <p class='page-para'><strong>Geography</strong>: Dane County</p>
      "),  
        ), # NEW ITEMS
        accordionItem(
          title =
            h2("Tree Knowledge", class = "page-subheading"),
          status = "white",
          collapsed = TRUE,
          p(
            "This survey contains insights around community awareness of the benefits trees provide, 
            such as carbon capture and flood reduction. It explores how trees impact health and 
            quality of life and gathers opinions on the importance of green spaces in the community.",
            class = "page-para"
          ),
          HTML("
        <p class='page-para'><strong>Date</strong>: October 2022</p>
        <p class='page-para'><strong>Method</strong>: In-Person</p>
        <p class='page-para'><strong>Respondents</strong>: 92</p>
        <p class='page-para'><strong>Geography</strong>: Dane County</p>
      "),
        ),
        accordionItem(
          title =
            h2("Carbon Concerns", class = "page-subheading"),
          status = "white",
          collapsed = TRUE,
          p(
            "This survey focuses on community awareness of climate pollutants, carbon removal 
            technologies, and personal actions to combat climate change. It also assesses 
            opinions on who should be responsible for ensuring the development and equitable 
            deployment of climate change technologies.",
            class = "page-para"
          ),
          HTML("
        <p class='page-para'><strong>Date</strong>: October 2022</p>
        <p class='page-para'><strong>Method</strong>: In-Person</p>
        <p class='page-para'><strong>Respondents</strong>: 92</p>
        <p class='page-para'><strong>Geography</strong>: Dane County</p>
      "),
        ),
        accordionItem(
          title =
            h2("Energy Concerns", class = "page-subheading"),
          status = "white",
          collapsed = TRUE,
          p(
            "The Energy Concerns Survey examines the importance of energy-saving practices 
            at home and their impact on quality of life. It also assesses community 
            awareness of energy-saving strategies and extreme heat impacts.",
            class = "page-para"
          ),
          HTML("
        <p class='page-para'><strong>Date</strong>: October 2022</p>
        <p class='page-para'><strong>Method</strong>: In-Person</p>
        <p class='page-para'><strong>Respondents</strong>: 92</p>
        <p class='page-para'><strong>Geography</strong>: Dane County</p>
      "),
        ),
        accordionItem(
          title =
            h2("Environmental Issues", class = "page-subheading"),
          status = "white",
          collapsed = TRUE,
          p(
            "This survey gathers insights into the most urgent environmental issues facing the 
            community and assesses how informed individuals feel about climate change and 
            environmental justice. It evaluates the extent to which people believe they can 
            influence environmental policy in their community.",
            class = "page-para"
          ),
          HTML("
        <p class='page-para'><strong>Date</strong>: October 2022</p>
        <p class='page-para'><strong>Method</strong>: In-Person</p>
        <p class='page-para'><strong>Respondents</strong>: 92</p>
        <p class='page-para'><strong>Geography</strong>: Dane County</p>
      "),
        ),
        accordionItem(
          title =
            h2("Health Impacts", class = "page-subheading"),
          status = "white",
          collapsed = TRUE,
          p(
            "The Health Impacts Survey focuses on community perceptions of extreme heat and its 
            impact on daily life and health. It also assesses awareness of energy-saving practices, 
            local resources such as cooling centers, and the long-term risks of extreme heat. 
            The survey gathers insights into community knowledge of solutions like insulation, 
            electrification, and the dangers posed by extreme heat.",
            class = "page-para"
          ),
          HTML("
         <p class='page-para'><strong>Date</strong>: October 2022</p>
        <p class='page-para'><strong>Method</strong>: In-Person</p>
        <p class='page-para'><strong>Respondents</strong>: 92</p>
        <p class='page-para'><strong>Geography</strong>: Dane County</p>
      "),
        ),
        accordionItem(
          title =
            h2("General Survey", class = "page-subheading"),
          status = "white",
          collapsed = TRUE,
          p(
            "Aims understand the community's perception of the most urgent environmental issues, 
            their impact on daily life, and the actions people are willing to take to address these 
            challenges. It explores awareness of climate change, personal goals for environmental 
            improvement, and the willingness to adopt new technologies for mitigating climate change. 
            The survey also assesses community views on who is responsible for deploying these 
            technologies equitably.",
            class = "page-para"
          ),
          HTML("
         <p class='page-para'><strong>Date</strong>: October 2022</p>
        <p class='page-para'><strong>Method</strong>: In-Person</p>
        <p class='page-para'><strong>Respondents</strong>: 92</p>
        <p class='page-para'><strong>Geography</strong>: Dane County</p>
      "),
        )
      ),
    ),
  )
}
