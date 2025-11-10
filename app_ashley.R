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
load("/Users/ashleydittmar/Downloads/immunodata.RData")

ls()
str(dfCounty)
str(dfDistrict)
str(df)
head(dfCounty)

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
                 plotOutput("countyPlot", height = 900),
                 
                 tags$hr(),
                 h4("County Summary (Both Grades)"),
                 p("Sortable summary of waiver rates and rank by county and grade."),
                 downloadButton("dl_county_summary", "Download County Summary (CSV)"),
                 DTOutput("countySummary"),
                 
                 tags$hr(),
                 h4("Top/Bottom Counties"),
                 fluidRow(
                   column(6,
                          h5("Top 10 by Waiver Rate"),
                          DTOutput("countyTop10")
                   ),
                   column(6,
                          h5("Bottom 10 by Waiver Rate"),
                          DTOutput("countyBottom10")
                   )
                 ),
                 
                 tags$hr(),
                 h4("County Totals (All Grades Combined)"),
                 DTOutput("countyTotals")
        ),
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
  
  output$countyTable <- DT::renderDT({
    library(DT)
    rate_col <- county_pct_col()
    
    dat <- dfCounty %>%
      transmute(
        Rank = dense_rank(desc(.data[[rate_col]])),
        County = COUNTY,
        `Waiver Rate (%)` = round(.data[[rate_col]], 1)
      ) %>%
      arrange(Rank)
    
    # create a small helper for a color bar in the percent column
    rng <- range(dat$`Waiver Rate (%)`, na.rm = TRUE)
    brks <- seq(rng[1], rng[2], length.out = 5)
    
    datatable(
      dat,
      rownames = FALSE,
      extensions = c("Buttons"),
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv"),
        pageLength = 25,
        order = list(list(0, "asc"))
      )
    ) %>%
      formatRound("Waiver Rate (%)", 1) %>%
      formatStyle(
        "Waiver Rate (%)",
        background = styleColorBar(rng, "lightsteelblue"),
        backgroundSize = "90% 60%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })
  output$countyPlot <- renderPlot({
    # pick the right column for the selected grade
    dat <- dfCounty %>%
      mutate(WaiverRate = .data[[county_pct_col()]])
    
    med <- median(dat$WaiverRate, na.rm = TRUE)
    
    dat <- dat %>%
      mutate(
        group = case_when(
          WaiverRate >  med ~ "Above state median",
          WaiverRate <  med ~ "Below state median",
          TRUE              ~ "At state median"
        )
      )
    
    ggplot(dat, aes(x = reorder(COUNTY, WaiverRate), y = WaiverRate, fill = group)) +
      geom_col(width = 0.8) +
      geom_hline(yintercept = med, linetype = "dashed", linewidth = 0.6, color = "black") +
      coord_flip() +
      scale_fill_manual(
        name = "vs. State Median",
        values = c("Above state median" = "#D7301F",  
                   "Below state median" = "#1A9850",  
                   "At state median"    = "grey60")
      ) +
      scale_y_continuous(
        limits = c(0, max(dat$WaiverRate, na.rm = TRUE) * 1.05),
        expand = expansion(mult = c(0, 0.02)),
        labels = function(x) paste0(x, "%")
      ) +
      labs(
        x = "County",
        y = "Waiver Rate (%)",
        title = "Vaccine Waiver Rates by County",
        subtitle = paste("Grade:", ifelse(input$grade == "K", "Kindergarten", "7th Grade")),
        caption = "Source: MDHHS School Immunization Data (2024) • Dashed line = state median"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "top",
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(face = "bold", size = 18, margin = margin(b = 4)),
        plot.subtitle = element_text(size = 13, margin = margin(b = 8)),
        panel.grid.major.y = element_blank()
      )
  })
  
  county_long <- reactive({
    dfCounty |>
      dplyr::select(COUNTY, K_pctWaiver, `7_pctWaiver`) |>
      tidyr::pivot_longer(
        cols = c(K_pctWaiver, `7_pctWaiver`),
        names_to  = "Metric",
        values_to = "WaiverRate"
      ) |>
      dplyr::mutate(
        Grade = dplyr::if_else(Metric == "K_pctWaiver", "Kindergarten", "7th Grade")
      ) |>
      dplyr::group_by(Grade) |>
      dplyr::mutate(Rank = dplyr::dense_rank(dplyr::desc(WaiverRate))) |>
      dplyr::ungroup()
  })
  
  output$countySummary <- DT::renderDT({
    dat <- county_long() |>
      dplyr::mutate(`Waiver Rate (%)` = round(WaiverRate, 1)) |>
      dplyr::select(Grade, COUNTY, Rank, `Waiver Rate (%)`) |>
      dplyr::arrange(Grade, Rank)
    
    DT::datatable(
      dat,
      rownames = FALSE,
      options = list(pageLength = 25),
      filter = "top"
    )
  })
  
  output$dl_county_summary <- downloadHandler(
    filename = function(){ "county_summary.csv" },
    content = function(file){
      readr::write_csv(
        county_long() |>
          dplyr::transmute(Grade, COUNTY, Rank, WaiverRate = round(WaiverRate, 1)),
        file
      )
    }
  )
  
  # ---- Top 10 by waiver rate per grade ----
  output$countyTop10 <- DT::renderDT({
    dat <- county_long() |>
      dplyr::group_by(Grade) |>
      dplyr::slice_max(order_by = WaiverRate, n = 10, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::mutate(`Waiver Rate (%)` = round(WaiverRate, 1)) |>
      dplyr::select(Grade, COUNTY, Rank, `Waiver Rate (%)`) |>
      dplyr::arrange(Grade, Rank)
    
    DT::datatable(dat, rownames = FALSE, options = list(dom = 'tip'))
  })
  
  # ---- Bottom 10 by waiver rate per grade ----
  output$countyBottom10 <- DT::renderDT({
    dat <- county_long() |>
      dplyr::group_by(Grade) |>
      dplyr::slice_min(order_by = WaiverRate, n = 10, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::mutate(`Waiver Rate (%)` = round(WaiverRate, 1)) |>
      dplyr::select(Grade, COUNTY, Rank, `Waiver Rate (%)`) |>
      dplyr::arrange(Grade, dplyr::desc(`Waiver Rate (%)`)) # smallest first
    
    DT::datatable(dat, rownames = FALSE, options = list(dom = 'tip'))
  })
  
  # ---- County totals table (overall across grades) ----
  output$countyTotals <- DT::renderDT({
    dfCounty |>
      dplyr::transmute(
        COUNTY,
        `Total Enrolled`   = tot_N,
        `Total Waiver (n)` = tot_nWaiver,
        `Total Waiver (%)` = round(tot_pctWaiver, 1),
        `Medical (%)`      = round(tot_pctMedical, 2),
        `Religious (%)`    = round(tot_pctReligious, 2),
        `Philosophical (%)`= round(tot_pctPhilosophical, 2),
        `Complete (%)`     = round(tot_pctCOMP, 1)
      ) |>
      dplyr::arrange(dplyr::desc(`Total Waiver (%)`)) |>
      DT::datatable(
        rownames = FALSE,
        options = list(pageLength = 25),
        filter = "top"
      )
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


