library(shiny)
library(tidyverse)
library(leaflet)
library(sf)

# Import shapes
districts.2012 <- st_read("data/Assembly_2012.geojson")
districts.2022 <- st_read("data/Assembly_2022.geojson")
districts.evers <- st_read("data/Assembly_EversLeastChange.geojson")
districts.pmc <- st_read("data/Assembly_PeoplesMapCommission.geojson")

# Prepare vote data
vote.data <- read_csv("data/AssemblyDistricts_with_ModelledVote.csv") %>%
  select(plan, district, modelled_vote = predict_v2, actual_vote = WSA, GOV, USS) %>%
  pivot_longer(cols = c(modelled_vote, actual_vote, GOV, USS), names_to = "vote_distribution",
               values_to = "dem_margin") %>%
  filter(! (vote_distribution == "actual_vote" & plan != "2022"))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Explore WI Assembly district leans"),

    # Sidebar with map inputs 
    sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "plan",
            label = "Choose a map",
            choices = c("2022 official" = "2022",
                        "2012-20 official" = "2012",
                        "Evers Least Change" = "Evers least change",
                        "People's Map Commission" = "People's Map Commission"),
            selected = "2022", multiple = FALSE
          ),
          
          conditionalPanel(
            condition = "input.plan == '2022'",
            selectInput(
              inputId = "vote_distribution_v1",
              label = "Choose a vote distribution",
              choices = c("Actual assembly vote (only available in 2022 official districts)" = "actual_vote",
                          "Modelled assembly vote" = "modelled_vote",
                          "2022 Governor's race" = "GOV",
                          "2022 US Senate race" = "USS"),
              selected = "actual_vote", multiple = FALSE
            )
          ),
          
          conditionalPanel(
            condition = "input.plan != '2022'",
            selectInput(
              inputId = "vote_distribution_v2",
              label = "Choose a vote distribution",
              choices = c("Modelled assembly vote" = "modelled_vote",
                          "2022 Governor's race" = "GOV",
                          "2022 US Senate race" = "USS"),
              selected = "modelled", multiple = FALSE
            )
          ),
          sliderInput(
            inputId = "margin_shift",
            label = "Add a statewide uniform vote swing (percentage point margin where positive is more Democratic)",
            min = -20, max = 20, step = 0.5,
            value = 0
            ),
          actionButton("go", "Apply scenario")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          textOutput("summaryText"),
          leafletOutput("map", height = 600)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  vote.distro <- eventReactive(input$go, {
    ifelse(input$plan == "2022",
           yes = input$vote_distribution_v1,
           no = input$vote_distribution_v2)
  })
  
  selected.vote.scenario <- eventReactive(input$go, {
    vote.data %>%
      filter(plan == input$plan,
             vote_distribution == vote.distro()) %>%
      mutate(adj_margin = dem_margin + input$margin_shift,
             winner = case_when(
               adj_margin < 0 ~ "Republican",
               adj_margin == 0 ~ "tie",
               adj_margin > 0 ~ "Democratic"
             ))
  })
  
  selected.scenario.totals <- eventReactive(input$go, {
    selected.vote.scenario() %>%
      group_by(winner) %>%
      summarise(seats = n()) %>%
      pivot_wider(names_from = winner, values_from = seats) %>%
      mutate(scenario_description = paste(Republican, "Republican seats and",
                                          Democratic, "Democratic seats."))
  })
  
  describe.shift <- eventReactive(input$go, {
    if_else(input$margin_shift != 0,
            true = paste("and applying a", paste0(abs(input$margin_shift), "-point"),
                         "shift in favor of the",
                         if_else(input$margin_shift < 0, "Republicans.", "Democrats.")),
            false = paste0(". "))
  })
  
  
  output$summaryText <- renderText({
    
    district.description <- switch(
      input$plan,
      "2022" = "actual 2022 Assembly districts",
      "2012" = "former Assembly districts used from 2012-2020",
      "Evers least change" = "hypothetical Assembly districts submitted by Evers as his 'least change' plan",
      "People's Map Commission" = "hypothetical Assembly districts submitted by the People's Map Commission"
    )
    
    vote.distro.text <- switch(
      vote.distro(),
      "actual_vote" = "the actual votes for Assembly in the 2022 election",
      "modelled_vote" = "modelled Assembly vote from the 2022 election",
      "GOV" = "the vote for governor in the 2022 election",
      "USS" = "the vote for US senator in the 2022 election"
    )
    
    paste("Showing the", district.description,
          "with party support based on", vote.distro.text,
          describe.shift(),
          "In this scenario, we expect", selected.scenario.totals() %>% pull(scenario_description))
  })
  
  map.scenario <- eventReactive(input$go, {
    switch(input$plan,
           "2022" = districts.2022,
           "2012" = districts.2012,
           "Evers least change" = districts.evers,
           "People's Map Commission" = districts.pmc) %>%
      st_transform(crs = 4326) %>%
      inner_join(selected.vote.scenario() %>%
                   mutate(label = paste("District", district, "--",
                                        if_else(adj_margin < 0,
                                                paste0("+", round(abs(adj_margin), 1), "-pt. Republican"),
                                                paste0("+", round(abs(adj_margin), 1), "-pt. Democratic")))))
  })

  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(-92.888114, 42.491983, -86.805415, 47.080621) %>%
      addLegend(
        colors = c("#b2182b","#d6604d","#f4a582","#fddbc7",
                   "#d1e5f0","#92c5de","#4393c3","#2166ac"),
        labels = c("+20 R or more", "+10 to +20 R", "+5 to +10 R", "tie to +5 R",
                   "tie to +5 D", "+5 to +10 D", "+10 to +20 D", "+20 D or more"),
        position = "bottomleft",
        opacity = 0.75
      )
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observeEvent(input$go, {
    fillpall <- colorBin(palette = c("#b2182b","#d6604d","#f4a582","#fddbc7",
                                     "#d1e5f0","#92c5de","#4393c3","#2166ac"),
                         bins = c(-Inf, -20, -10, -5, 0, 5, 10, 20, Inf))

    leafletProxy("map", data = map.scenario()) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~fillpall(adj_margin),
        fillOpacity = 0.75,
        weight = 1,
        label = ~label
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
