#App Script to run Shiny dashboard for School Immunization Data (2024)
#Data from https://www.michigan.gov/mdhhs/adult-child-serv/childrenfamilies/immunizations/data-statistics/schoolimmsdata


# Setup -------------------------------------------------------------------

#load dependencies
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(shiny)){install.packages("shiny")}
if(!require(shinydashboard)){install.packages("shinydashboard")}
if(!require(DT)){install.packages("DT")}
if(!require(sf)){install.packages("sf")}
if(!require(leaflet)){install.packages("leaflet")}
if(!require(tigris)){install.packages("tigris")}


#load data generated from dataScript.R 
load(file = file.path(getwd(), "data", "immunodata.RData"))


# Build the UI ------------------------------------------------------------

#More legible name variables and things that UI/Server will use
grade_choices <- c("Kindergarten" = "K", "7th Grade" = "7")
mi_counties <- counties(state = "MI", cb = TRUE, year = 2024, class = "sf")

ui <- fluidPage(
  titlePanel("MDHHS School Immunization Data Explorer (2024)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("grade", "Select Grade Level", choices = grade_choices, selected = "K"),
      selectInput("county", "Select County", choices = sort(unique(dfCounty$COUNTY))),
      uiOutput("district_ui"),
      uiOutput("school_ui")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("County",
                 h4("Vaccine Waiver Rates by County"),
                 DTOutput("countyTable"),
                 plotOutput("countyPlot", height = 400)),
        tabPanel("District",
                 h4("Vaccine Waiver Rates by District (Selected County)"),
                 DTOutput("districtTable"),
                 plotOutput("districtPlot", height = 400)),
        tabPanel("School Building",
                 h4("Vaccine Waiver Rates by School (Selected District)"),
                 DTOutput("schoolTable"),
                 plotOutput("schoolPlot", height = 400)),
        tabPanel("Waiver Type Breakdown",
                 h4("Vaccination/Waiver Rates for Selected School"),
                 plotOutput("schoolBreakdownPlot", height = 400),
                 DTOutput("schoolBreakdownTable")),
        tabPanel("Map",
                 h4("Geographic Visualization of Waiver Rates by County"),
                 leafletOutput("miMap", height = 550))
        
        )
    )
  )
)


# Build the Server --------------------------------------------------------

server <- function(input, output, session) {
  
  output$district_ui <- renderUI({
    req(input$county)
    districts <- dfDistrict %>%
      filter(COUNTY == input$county) %>%
      pull(DISTRICT) %>%
      unique() %>%
      sort()
    selectInput("district", "Select District", choices = districts)
  })
  
  # Reactive School choices
  output$school_ui <- renderUI({
    req(input$county, input$district)
    schools <- df %>%
      filter(COUNTY == input$county, DISTRICT == input$district, Group == input$grade) %>%
      pull(NAME) %>%
      unique() %>%
      sort()
    selectInput("school", "Select School", choices = schools)
  })
  
  # County Level
  county_pct_col <- reactive({
    paste0(input$grade, "_pctWaiver")
  })
  
  output$countyTable <- renderDT({
    dfCounty %>%
      mutate(`Waiver Rate (%)` = round(.data[[county_pct_col()]], 1)) %>%
      select(COUNTY, `Waiver Rate (%)`) %>%
      arrange(desc(`Waiver Rate (%)`)) %>%
      datatable(rowname = FALSE)
  })
  output$countyPlot <- renderPlot({
    dfCounty %>%
      mutate(WaiverRate = .data[[county_pct_col()]]) %>%
      ggplot(aes(x = reorder(COUNTY, WaiverRate), y = WaiverRate)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(x = "County", y = "Waiver Rate (%)") +
      theme_minimal()
  })
  
  #District Level 
  district_pct_col <- reactive({
    paste0(input$grade, "_pctWaiver")
  })
  
  output$districtTable <- renderDT({
    dfDistrict %>%
      filter(COUNTY == input$county) %>%
      mutate(`Waiver Rate (%)` = round(.data[[district_pct_col()]], 1)) %>%
      select(DISTRICT, `Waiver Rate (%)`) %>%
      arrange(desc(`Waiver Rate (%)`)) %>%
      datatable(rowname = FALSE)
  })
  output$districtPlot <- renderPlot({
    dfDistrict %>%
      filter(COUNTY == input$county) %>%
      mutate(WaiverRate = .data[[district_pct_col()]]) %>%
      ggplot(aes(x = reorder(DISTRICT, WaiverRate), y = WaiverRate)) +
      geom_col(fill = "darkorange") +
      coord_flip() +
      labs(x = "District", y = "Waiver Rate (%)", caption = input$county) +
      theme_minimal()
  })
  
  # School Building Level 
  output$schoolTable <- renderDT({
    df %>%
      filter(COUNTY == input$county, DISTRICT == input$district, Group == input$grade) %>%
      mutate(`Waiver Rate (%)` = round(pctWaiver, 1)) %>%
      select(NAME, `Waiver Rate (%)`) %>%
      arrange(desc(`Waiver Rate (%)`)) %>%
      datatable(rowname = FALSE, options = list(pageLength = 25))
  })
  output$schoolPlot <- renderPlot({
    dat <- df %>%
      filter(COUNTY == input$county, DISTRICT == input$district, Group == input$grade)
    ggplot(dat, aes(x = reorder(NAME, pctWaiver), y = pctWaiver)) +
      geom_col(fill = "goldenrod") + 
      coord_flip() +
      labs(x = "School", y = "Waiver Rate (%)", caption = input$district) +
      theme_minimal()
  })
  
  # Individual School plot
  output$schoolBreakdownPlot <- renderPlot({
    req(input$school)
    dat <- df %>%
      filter(
        COUNTY == input$county,
        DISTRICT == input$district,
        Group == input$grade,
        NAME == input$school
      )
    bar_df <- tibble(
      WaiverType = c("Any Waiver", "Medical Waiver", "Religious Waiver", "Philosophical Waiver", "Vaccinated"),
      Rate = c(dat$pctWaiver, dat$pctMedical, dat$pctReligious, dat$pctPhilosophical, dat$pctCOMP)
    )
    ggplot(bar_df, aes(x = WaiverType, y = Rate, fill = WaiverType)) +
      geom_col(width = 0.7, show.legend = FALSE) +
      labs(
        title = paste("Vaccination/Waiver Rates for", input$school),
        x = NULL,
        y = "Percent (%)"
      ) +
      coord_flip() +
      ylim(0, 100) +
      theme_minimal(base_size = 14)
  })
  
  output$schoolBreakdownTable <- renderDT({
    df %>%
      filter(
        COUNTY == input$county,
        DISTRICT == input$district,
        Group == input$grade,
        NAME == input$school
      ) %>%
      mutate(
        `Any Waiver (%)` = round(pctWaiver, 1),
        `Medical Waiver (%)` = round(pctMedical, 1),
        `Religious Waiver (%)` = round(pctReligious, 1),
        `Philosophical Waiver (%)` = round(pctPhilosophical, 1),
        `Vaccinated (%)` = round(pctCOMP, 1)
      ) %>%
      select(
        `Any Waiver (%)`, `Medical Waiver (%)`, `Religious Waiver (%)`, 
        `Philosophical Waiver (%)`, `Vaccinated (%)`
      ) %>%
      datatable(rowname = FALSE)
  })
  
  
  #  Mapping stuff
  mapdat_reactive <- reactive({
    left_join(mi_counties, dfCounty, by = c("NAME" = "COUNTY"))
  })
  output$miMap <- renderLeaflet({
    colname <- paste0(input$grade, "_pctWaiver")
    mapdat <- mapdat_reactive()
    mapdat$waiverRate <- mapdat[[colname]]
    pal <- colorNumeric("YlOrRd", domain = mapdat$waiverRate, na.color = "#f0f0f0")
    leaflet(mapdat) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(waiverRate),
        fillOpacity = 0.8, weight = 1, color = "white",
        label = ~paste(NAME, paste("Waiver Rate:", round(waiverRate, 1), "%")),
        highlightOptions = highlightOptions(weight = 2, color = "blue", bringToFront = TRUE)
      ) %>%
      addLegend(
        pal = pal,
        values = mapdat$waiverRate,
        title = "Waiver Rate (%)",
        position = "bottomright"
      )
  })
}

shinyApp(ui = ui, server = server)

