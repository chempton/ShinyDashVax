######################################################################
# U-M Immunization Dashboard v1.1: Accessible Colors and Ordered Plots
######################################################################

# Import packages + data
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(shiny)) install.packages("shiny")
if(!require(DT)) install.packages("DT")
if(!require(sf)) install.packages("sf")
if(!require(leaflet)) install.packages("leaflet")
if(!require(tigris)) install.packages("tigris")
if(!require(bslib)) install.packages("bslib")
if(!require(RColorBrewer)) install.packages("RColorBrewer")
if(!require(here)) install.packages("here")

if(!exists("dfCounty") | !exists("dfDistrict") | !exists("df")){
  load(here("data/immunodata.RData"))
}
mi_counties <- counties(state = "MI", cb = TRUE, year = 2024, class = "sf") %>%
  sf::st_transform(crs = 4326)

grade_choices <- c("Kindergarten" = "K", "7th Grade" = "7")
toggle_choices <- c("Percent (%)" = "percent", "Count (N)" = "count")

main_label <- function(base, display_type) {
  if (display_type == "percent") paste0(base, " Rate (%)") else paste0(base, " Count (N)")
}
waiver_summary_label <- function(display_type) {
  if (display_type == "percent") "Waiver Rate (%)" else "Waiver Count (N)"
}
waiver_type_label <- function(display_type) {
  if (display_type == "percent") "Waiver Type Rate (%)" else "Waiver Type Count (N)"
}
missing_tibble <- function(labels) {
  as_tibble(setNames(as.list(rep(NA_real_, length(labels))), labels))
}

ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#00274C",
    secondary = "#FFCB05",
    base_font = font_google("Roboto"),
    bg = "#F8F8F8",
    fg = "#00274C"
  ),
  tags$head(
    tags$style(HTML("
  /* Nav bar and tabs */
  .navbar {
    font-family: 'Roboto', 'Lato', sans-serif;
    font-size: 1.15em;
    background-color: #00274C !important;
  }
  .nav-tabs > li > a,
  .nav-tabs > li > a:focus,
  .nav-tabs > li > a:hover {
    color: #00274C;
    background-color: #FFCB05 !important;
    border: 1px solid #00274C !important;
    font-weight:bold;
  }
  .nav-tabs > li.active > a,
  .nav-tabs > li.active > a:focus,
  .nav-tabs > li.active > a:hover {
    color: #FFCB05 !important;
    background-color: #00274C !important;
    border: 1px solid #00274C !important;
  }

  /* Summary card styles */
  .card {
    box-shadow: 0 2px 8px 0 rgba(0,39,76,0.10);
    border-radius: 16px;
    margin: 18px 0 24px 0;
    border: 2px solid #00274C;
    background: #FFFFFF;
    overflow: hidden;
    padding: 0;
  }
  .summary-header {
    background: linear-gradient(90deg, #00274C 85%, #1A355E 100%);
    color: #FFCB05;
    font-weight: bold;
    font-size: 1.18em;
    padding: 16px 32px 12px 32px;
    letter-spacing: 0.02em;
    border-bottom: 2.5px solid #FFCB05;
  }
  .summary-body {
    padding: 22px 32px 18px 32px;
    background: #F8F8F8;
    color: #00274C;
    font-size: 1.1em;
  }
  .summary-grades {
    font-size: 1.01em;
    color: #666;
    margin-bottom: 12px;
  }
  .summary-card-metrics {
    display: flex;
    flex-wrap: wrap;
    gap: 32px;
    margin-top: 4px;
  }
  .summary-card-metric {
    flex: 1;
    min-width: 160px;
    margin-bottom: 16px;
    text-align: left;
  }
  .summary-card-metric-title {
    color: #1A355E;
    font-size: 1.10em;
    font-weight: 600;
    margin-bottom: 4px;
    letter-spacing: 0.01em;
  }
  .summary-card-metric-value {
    font-size: 1.65em;
    color: #00274C;
    font-weight: 800;
    letter-spacing: 0.01em;
    padding-bottom: 3px;
    display: block;
    margin-left: 0.5px;
  }
  .summary-card-metric-note {
    font-size: 0.99em;
    color: #777;
    margin-top:7px;
  }

  /* Breakdown tables */
  .breakdown-table {
    font-family: 'Roboto', 'Lato', sans-serif;
    border-collapse: separate;
    border-spacing: 0;
    width: 100%;
    margin: 10px 0 18px 0;
    background-color: #FFFFFF;
    color: #00274C;
    border-radius: 10px;
    box-shadow: 0 2px 8px 0 rgba(0,39,76,0.10);
    border: 1px solid #00274C;
  }
  .breakdown-table th {
    background-color: #FFCB05;
    color: #00274C;
    font-weight: bold;
    padding: 9px 16px;
    font-size: 1.09em;
    border-bottom: 1px solid #00274C;
  }
  .breakdown-table td {
    background: #F8F8F8;
    color: #222222;
    padding: 9px 16px;
    font-size: 1.11em;
    border-bottom: 1px solid #eef0f2;
    text-align: right;
    font-family: 'Roboto', 'Lato', sans-serif;
  }
  .breakdown-table tr:last-child td { border-bottom: none; }
  .breakdown-table td:first-child, .breakdown-table th:first-child { text-align: left; }
  .breakdown-table .no-data {
    color: #666;
    background: #F8F8F8;
    text-align: center;
    font-style: italic;
    font-size: 1.06em;
    padding: 16px;
  }
          .dataTables_wrapper .dataTables_paginate .page-link {
              background: #E3ECF7 !important;
              color: #000 !important;
              border: 1.5px solid #8AB4E2 !important;
              border-radius: 6px !important;
              margin: 2px 3px !important;
              padding: 7px 14px !important;
              font-weight: 500 !important;
              box-shadow: none !important;
              outline: none !important;
              transition: background 0.2s, color 0.2s;
          }
          .dataTables_wrapper .dataTables_paginate .page-item.active .page-link,
          .dataTables_wrapper .dataTables_paginate .page-link.active,
          .dataTables_wrapper .dataTables_paginate .page-link.current {
              background: #00274C !important;
              color: #FFCB05 !important;
              border: 2px solid #00274C !important;
              font-weight: bold !important;
              cursor: default !important;
              box-shadow: 0 3px 10px rgba(0,39,76,0.10);
          }
          .dataTables_wrapper .dataTables_paginate .page-link:hover,
          .dataTables_wrapper .dataTables_paginate .page-item.active .page-link:hover,
          .dataTables_wrapper .dataTables_paginate .page-link.current:hover {
              background: #FFCB05 !important;
              color: #00274C !important;
              border: 2px solid #00274C !important;
              font-weight: bold !important;
              box-shadow: 0 2px 6px rgba(0,39,76,0.08);
              transition: background 0.2s, color 0.2s;
          }

    .immunization-title-card {
        background: #00274C;
        padding: 32px 20px 18px 20px;
        border-radius: 20px;
        margin-bottom: 12px;
        text-align: center;
        box-shadow: 0 2px 12px 0 rgba(0,39,76,0.10);
    }
    .immunization-title {
        color: #FFCB05;           /* Maize */
        font-size: 2.2em;
        font-weight: bold;
        letter-spacing: 0.01em;
        line-height: 1.06;
        margin-bottom: 5px;
        font-family: 'Roboto', 'Lato', sans-serif;
    }
    .immunization-subtitle {
        color: #FFCB05;           /* Maize */
        font-size: 1.2em;
        font-style: italic;
        margin-top: 3px;
        font-family: 'Roboto', 'Lato', sans-serif;
        opacity: 0.88;
    }"))
  ),
  
  div(class="title-panel",
      HTML('<div class="immunization-title-card">
            <div class="immunization-title">
                <b>MDHHS School Immunization Waivers Explorer</b>
            </div>
            <div class="immunization-subtitle">
                <i>University of Michigan School of Public Health | Data updated for 2024</i>
            </div>
        </div>')
  ),
  tabsetPanel(
    tabPanel("County",
             fluidRow(
               column(4, checkboxGroupInput("county_grade", "Select Grade Level(s)", choices = grade_choices, selected = c("K", "7"))),
               column(4, selectInput("county_county", "Select County", choices = sort(unique(dfCounty$COUNTY)), selected = sort(unique(dfCounty$COUNTY))[1])),
               column(4, radioButtons("county_display_type", "Show values as:", choices = toggle_choices, selected = "percent"))
             ),
             br(),
             uiOutput("countySummaryCard"),
             br(),
             div(style="text-align:center; font-size:1.25em; font-weight:bold; margin-top:10px; margin-bottom:15px;",
                 textOutput("countyOverallRate", inline=TRUE)),
             h4(textOutput("countyBreakdownPlotTitle")),
             plotOutput("countyBreakdownPlot", height = 300),
             uiOutput("countyBreakdownTitle"),
             uiOutput("countyBreakdownTable"),
             tags$hr(style="margin-top:10px;margin-bottom:10px;border:1px solid #ddd;"),
             uiOutput("countyRatesTableTitle"),
             DTOutput("countyTable")
    ),
    tabPanel("District",
             fluidRow(
               column(3, checkboxGroupInput("district_grade", "Select Grade Level(s)", choices = grade_choices, selected = c("K", "7"))),
               column(3, selectInput("district_county", "Select County", choices = sort(unique(dfDistrict$COUNTY)), selected = sort(unique(dfDistrict$COUNTY))[1])),
               column(3, uiOutput("district_ui")),
               column(3, radioButtons("district_display_type", "Show values as:", choices = toggle_choices, selected = "percent"))
             ),
             br(),
             uiOutput("districtSummaryCard"),
             br(),
             div(style="text-align:center; font-size:1.25em; font-weight:bold; margin-top:10px; margin-bottom:15px;",
                 textOutput("districtOverallRate", inline=TRUE)),
             h4(textOutput("districtBreakdownPlotTitle")),
             plotOutput("districtBreakdownPlot", height = 300),
             uiOutput("districtBreakdownTitle"),
             uiOutput("districtBreakdownTable"),
             tags$hr(style="margin-top:10px;margin-bottom:10px;border:1px solid #ddd;"),
             uiOutput("districtRatesTableTitle"),
             DTOutput("districtTable")
    ),
    tabPanel("School Building",
             fluidRow(
               column(2, checkboxGroupInput("school_grade", "Select Grade Level(s)", choices = grade_choices, selected = c("K", "7"))),
               column(3, selectInput("school_county", "Select County", choices = sort(unique(dfDistrict$COUNTY)), selected = sort(unique(dfDistrict$COUNTY))[1])),
               column(3, uiOutput("school_district_ui")),
               column(2, uiOutput("school_ui")),
               column(2, radioButtons("school_display_type", "Show values as:", choices = toggle_choices, selected = "percent"))
             ),
             br(),
             uiOutput("schoolSummaryCard"),
             br(),
             div(style="text-align:center; font-size:1.25em; font-weight:bold; margin-top:10px; margin-bottom:15px;",
                 textOutput("schoolOverallRate", inline=TRUE)),
             h4(textOutput("schoolBreakdownPlotTitle")),
             plotOutput("schoolBreakdownPlot", height = 300),
             uiOutput("schoolBreakdownTitle"),
             uiOutput("schoolBreakdownTable"),
             tags$hr(style="margin-top:10px;margin-bottom:10px;border:1px solid #ddd;"),
             uiOutput("schoolRatesTableTitle"),
             DTOutput("schoolTable")
    ),
    tabPanel("Map",
             fluidRow(
               column(12, checkboxGroupInput("map_grade", "Select Grade Level(s)", choices = grade_choices, selected = c("K", "7")))
             ),
             br(),
             uiOutput("mapSummaryCard"),
             h4("Map of Waiver Rates by County"),
             leafletOutput("miMap", height = 550)
    ),
    tabPanel(
      "About",
      fluidRow(
        column(
          width = 12,
          # Heading styled similarly to the home title
          HTML('
        <div class="immunization-title-card" style="margin-top:10px; margin-bottom:0;">
          <div class="immunization-title">
            <b>About This Dashboard</b>
          </div>
        </div>
      '),
          # Body text section, styled for readability
          HTML('
        <div style="
          background: #FFFFFF;
          border-radius: 18px;
          box-shadow: 0 1px 8px 0 rgba(0,39,76,0.09);
          margin: 0 0 28px 0;
          padding: 36px 28px 28px 28px;
          color: #1A355E;
          font-family:\'Roboto\', \'Lato\', sans-serif;
          font-size: 1.13em;
          line-height: 1.6;
        ">
<h4>About the Data</h4>
<p>
This dashboard presents information on school immunization waivers in Michigan at the county, district, and school-level, sourced from the Michigan Department of Health & Human Services (MDHHS) 2024 reported data.
</p>
<p>
<b>Dashboard features include:</b>
<ul>
  <li>Overview of waiver rates by county, district, and school building</li>
  <li>Detailed breakdowns of these waivers by type (medical, religious, philosophical, etc.) at each level</li>
  <li>A map of Michigan counties by reported vaccination rates for quick visualization</li>
</ul>
</p>

<!-- For Questions or More Information -->
<h4>For Questions or More Information</h4>
<p>
For more information about immunization waiver data in Michigan, as well as accessing the data used for this dashboard, please visit the 
<a href="https://www.michigan.gov/mdhhs/adult-child-serv/childrenfamilies/immunizations/data-statistics/schoolimmsdata" target="_blank">MDHHS School Immunization Data page</a>.
</p>
<p>
Additional data resources can be found from the 
<a href="https://www.michigan.gov/mdhhs/adult-child-serv/childrenfamilies/immunizations" target="_blank">Michigan Department of Health & Human Services Division of Immunization</a>.
</p>
<p>

<!-- About Us -->
<h4>About Us</h4>
<p>
This dashboard was developed as a project for the University of Michigan Department of Epidemiology&apos;s Applied Epidemiology in Governmental Public Health curriculum by MPH students: <br>Cameron Hempton, Ashna Patel, Ashley Dittmar, Chetna Kumari, Zoe Gurney, and Sarah Olson <br>
</p>  
<p>
<b>For any technical or maintenance questions, please contact: </b>  <a href="mailto:chempton@umich.edu">chempton@umich.edu</a><br>
</p>    
<p style="font-size:0.97em; color:#555;">
  <i> These data subject to updates from MDHHS. Page last updated 12/1/2025</i>
</p>
</div>
      ')
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$district_ui <- renderUI({
    req(input$district_county)
    districts <- dfDistrict %>%
      filter(COUNTY == input$district_county) %>%
      pull(DISTRICT) %>%
      unique() %>%
      sort()
    selectInput("district_district", "Select District", choices = districts, selected = districts[1])
  })
  
  output$school_district_ui <- renderUI({
    req(input$school_county)
    districts <- dfDistrict %>%
      filter(COUNTY == input$school_county) %>%
      pull(DISTRICT) %>%
      unique() %>%
      sort()
    selectInput("school_district", "Select District", choices = districts, selected = districts[1])
  })
  
  output$school_ui <- renderUI({
    req(input$school_county, input$school_district)
    schools <- df %>%
      filter(COUNTY == input$school_county, DISTRICT == input$school_district, Group %in% input$school_grade) %>%
      pull(NAME) %>%
      unique() %>%
      sort()
    selectInput("school_school", "Select School", choices = schools, selected = schools[1])
  })
  
  output$countySummaryCard <- renderUI({
    req(input$county_county)
    dat <- dfCounty %>% filter(COUNTY == input$county_county)
    grades <- input$county_grade
    nWaiver <- sum(unlist(lapply(grades, function(g) as.numeric(dat[[paste0(g, "_nWaiver")]]))), na.rm=TRUE)
    N <- sum(unlist(lapply(grades, function(g) as.numeric(dat[[paste0(g, "_N")]]))), na.rm=TRUE)
    rate <- ifelse(N > 0, round(nWaiver / N * 100, 1), NA)
    avg_rate <- round(mean(
      unlist(lapply(grades, function(g) as.numeric(dfCounty[[paste0(g, "_nWaiver")]]) / as.numeric(dfCounty[[paste0(g, "_N")]]) * 100)), na.rm=TRUE), 2)
    
    if(N == 0){
      cardHTML <- sprintf('<div class="card">
        <div class="summary-header">%s County: Vaccination Summary</div>
        <div class="summary-body">
          <div class="summary-grades">Grades selected: %s</div>
          <div class="summary-card-metrics">
            <div class="summary-card-metric">
              <div class="summary-card-metric-title">No available data</div>
              <span class="summary-card-metric-note">Try another selection.</span>
            </div>
          </div>
        </div>
      </div>', input$county_county, paste(grades, collapse=", "))
      return(HTML(cardHTML))
    }
    
    cardHTML <- sprintf('<div class="card">
      <div class="summary-header">%s County: Vaccination Summary</div>
      <div class="summary-body">
        <div class="summary-grades">Grades selected: %s</div>
        <div class="summary-card-metrics">
          <div class="summary-card-metric">
            <div class="summary-card-metric-title">Total Students</div>
            <span class="summary-card-metric-value">%s</span>
          </div>
          <div class="summary-card-metric">
            <div class="summary-card-metric-title">Total Waivers</div>
            <span class="summary-card-metric-value">%s</span>
          </div>
          <div class="summary-card-metric">
            <div class="summary-card-metric-title">Waiver Rate</div>
            <span class="summary-card-metric-value">%s%%</span>
          </div>
          <div class="summary-card-metric">
            <div class="summary-card-metric-title">Statewide Average (Sel. Grades)</div>
            <span class="summary-card-metric-value">%s%%</span>
          </div>
        </div>
      </div>
    </div>',
                        input$county_county,
                        paste(grades, collapse=", "),
                        format(N, big.mark=","),
                        format(nWaiver, big.mark=","),
                        ifelse(is.na(rate), "N/A", rate),
                        ifelse(is.na(avg_rate), "N/A", avg_rate)
    )
    HTML(cardHTML)
  })
  
  output$districtSummaryCard <- renderUI({
    req(input$district_county, input$district_district)
    dat <- dfDistrict %>% filter(COUNTY == input$district_county, DISTRICT == input$district_district)
    grades <- input$district_grade
    nWaiver <- sum(unlist(lapply(grades, function(g) as.numeric(dat[[paste0(g, "_nWaiver")]]))), na.rm=TRUE)
    N <- sum(unlist(lapply(grades, function(g) as.numeric(dat[[paste0(g, "_N")]]))), na.rm=TRUE)
    rate <- ifelse(N > 0, round(nWaiver / N * 100, 1), NA)
    avg_rate <- round(mean(
      unlist(lapply(grades, function(g)
        (as.numeric(dfDistrict[[paste0(g, "_nWaiver")]]) / as.numeric(dfDistrict[[paste0(g, "_N")]]) * 100)[dfDistrict$COUNTY == input$district_county]
      )), na.rm=TRUE), 2)
    
    if(N == 0){
      cardHTML <- sprintf('<div class="card">
        <div class="summary-header">%s District: Vaccination Summary</div>
        <div class="summary-body">
          <div class="summary-grades">Grades selected: %s</div>
          <div class="summary-card-metrics">
            <div class="summary-card-metric">
              <div class="summary-card-metric-title">No available data</div>
              <span class="summary-card-metric-note">Try another selection.</span>
            </div>
          </div>
        </div>
      </div>', input$district_district, paste(grades, collapse=", "))
      return(HTML(cardHTML))
    }
    
    cardHTML <- sprintf('<div class="card">
      <div class="summary-header">%s District: Vaccination Summary</div>
      <div class="summary-body">
        <div class="summary-grades">Grades selected: %s</div>
        <div class="summary-card-metrics">
          <div class="summary-card-metric">
            <div class="summary-card-metric-title">Total Students</div>
            <span class="summary-card-metric-value">%s</span>
          </div>
          <div class="summary-card-metric">
            <div class="summary-card-metric-title">Total Waivers</div>
            <span class="summary-card-metric-value">%s</span>
          </div>
          <div class="summary-card-metric">
            <div class="summary-card-metric-title">Waiver Rate</div>
            <span class="summary-card-metric-value">%s%%</span>
          </div>
          <div class="summary-card-metric">
            <div class="summary-card-metric-title">County Average (Sel. Grades)</div>
            <span class="summary-card-metric-value">%s%%</span>
          </div>
        </div>
      </div>
    </div>',
                        input$district_district,
                        paste(grades, collapse=", "),
                        format(N, big.mark=","),
                        format(nWaiver, big.mark=","),
                        ifelse(is.na(rate), "N/A", rate),
                        ifelse(is.na(avg_rate), "N/A", avg_rate)
    )
    HTML(cardHTML)
  })
  
  output$schoolSummaryCard <- renderUI({
    req(input$school_county, input$school_district, input$school_school)
    dat <- df %>% filter(COUNTY == input$school_county, DISTRICT == input$school_district, NAME == input$school_school, Group %in% input$school_grade)
    grades <- input$school_grade
    nWaiver <- sum(as.numeric(dat$nWaiver), na.rm=TRUE)
    N <- sum(as.numeric(dat$N), na.rm=TRUE)
    rate <- ifelse(N > 0, round(nWaiver / N * 100, 1), NA)
    avg_rate <- round(mean(df %>% filter(COUNTY == input$school_county, DISTRICT == input$school_district, Group %in% grades) %>% group_by(NAME) %>% summarise(R = sum(nWaiver, na.rm=TRUE)/sum(N, na.rm=TRUE)*100) %>% pull(R), na.rm=TRUE), 2)
    
    if(N == 0){
      cardHTML <- sprintf('<div class="card">
        <div class="summary-header">%s School: Vaccination Summary</div>
        <div class="summary-body">
          <div class="summary-grades">Grades selected: %s</div>
          <div class="summary-card-metrics">
            <div class="summary-card-metric">
              <div class="summary-card-metric-title">No available data</div>
              <span class="summary-card-metric-note">Try another selection.</span>
            </div>
          </div>
        </div>
      </div>', input$school_school, paste(grades, collapse=", "))
      return(HTML(cardHTML))
    }
    
    cardHTML <- sprintf('<div class="card">
      <div class="summary-header">%s School: Vaccination Summary</div>
      <div class="summary-body">
        <div class="summary-grades">Grades selected: %s</div>
        <div class="summary-card-metrics">
          <div class="summary-card-metric">
            <div class="summary-card-metric-title">Total Students</div>
            <span class="summary-card-metric-value">%s</span>
          </div>
          <div class="summary-card-metric">
            <div class="summary-card-metric-title">Total Waivers</div>
            <span class="summary-card-metric-value">%s</span>
          </div>
          <div class="summary-card-metric">
            <div class="summary-card-metric-title">Waiver Rate</div>
            <span class="summary-card-metric-value">%s%%</span>
          </div>
          <div class="summary-card-metric">
            <div class="summary-card-metric-title">District Average (Sel. Grades)</div>
            <span class="summary-card-metric-value">%s%%</span>
          </div>
        </div>
      </div>
    </div>',
                        input$school_school,
                        paste(grades, collapse=", "),
                        format(N, big.mark=","),
                        format(nWaiver, big.mark=","),
                        ifelse(is.na(rate), "N/A", rate),
                        ifelse(is.na(avg_rate), "N/A", avg_rate)
    )
    HTML(cardHTML)
  })
  
  output$mapSummaryCard <- renderUI({
    grades <- input$map_grade
    nWaiver <- sum(unlist(lapply(grades, function(g) as.numeric(dfCounty[[paste0(g, "_nWaiver")]]))), na.rm = TRUE)
    N <- sum(unlist(lapply(grades, function(g) as.numeric(dfCounty[[paste0(g, "_N")]]))), na.rm = TRUE)
    rate <- ifelse(N > 0, round(nWaiver / N * 100, 1), NA)
    
    if(N == 0){
      cardHTML <- sprintf('<div class="card">
        <div class="summary-header">Michigan State-wide Vaccination Summary</div>
        <div class="summary-body">
          <div class="summary-grades">Grades selected: %s</div>
          <div class="summary-card-metrics">
            <div class="summary-card-metric">
              <div class="summary-card-metric-title">No available data</div>
              <span class="summary-card-metric-note">Try another selection.</span>
            </div>
          </div>
        </div>
      </div>', paste(grades, collapse=", "))
      return(HTML(cardHTML))
    }
    
    cardHTML <- sprintf('<div class="card">
      <div class="summary-header">Michigan State-wide Vaccination Summary</div>
      <div class="summary-body">
        <div class="summary-grades">Grades selected: %s</div>
        <div class="summary-card-metrics">
          <div class="summary-card-metric">
            <div class="summary-card-metric-title">Total Students</div>
            <span class="summary-card-metric-value">%s</span>
          </div>
          <div class="summary-card-metric">
            <div class="summary-card-metric-title">Total Waivers</div>
            <span class="summary-card-metric-value">%s</span>
          </div>
          <div class="summary-card-metric">
            <div class="summary-card-metric-title">State-wide Waiver Rate</div>
            <span class="summary-card-metric-value">%s%%</span>
          </div>
        </div>
      </div>
    </div>',
                        paste(grades, collapse=", "),
                        format(N, big.mark=","),
                        format(nWaiver, big.mark=","),
                        ifelse(is.na(rate), "N/A", rate)
    )
    HTML(cardHTML)
  })
  
  output$countyBreakdownPlotTitle <- renderText({
    req(input$county_county)
    if (input$county_county == "") return("")
    paste("Waiver Type Breakdown for", input$county_county, "County")
  })
  output$districtBreakdownPlotTitle <- renderText({
    req(input$district_district)
    dd <- input$district_district
    if (is.null(dd) || dd == "") return("")
    paste("Waiver Type Breakdown for", dd)
  })
  output$schoolBreakdownPlotTitle <- renderText({
    req(input$school_school)
    ss <- input$school_school
    if (is.null(ss) || ss == "") return("")
    paste("Waiver Type Breakdown for", ss)
  })
  
  output$countyRatesTableTitle <- renderUI({
    HTML("<strong>Waiver Rate by County for Michigan counties</strong>")
  })
  output$districtRatesTableTitle <- renderUI({
    if(is.null(input$district_county)||input$district_county=="")
      return(HTML("<strong>Select a County</strong>"))
    HTML(paste0("<strong>Waiver Rate by District for Districts in ", input$district_county,"</strong>"))
  })
  output$schoolRatesTableTitle <- renderUI({
    if(is.null(input$school_district)||input$school_district=="")
      return(HTML("<strong>Select a District</strong>"))
    HTML(paste0("<strong>Waiver Rate by Building for Schools in ", input$school_district,"</strong>"))
  })
  
  output$countyTable <- renderDT({
    grades <- input$county_grade
    dat <- dfCounty
    display_type <- input$county_display_type
    if(is.null(input$county_county)||input$county_county==""||length(grades)==0)
      return(missing_tibble(c("County", main_label("Waiver", display_type))))
    if(display_type == "percent") {
      nWaiver <- rowSums(do.call(cbind, lapply(grades, function(g) as.numeric(dat[[paste0(g, "_nWaiver")]]))), na.rm=TRUE)
      N <- rowSums(do.call(cbind, lapply(grades, function(g) as.numeric(dat[[paste0(g, "_N")]]))), na.rm=TRUE)
      Value <- ifelse(N > 0, round(nWaiver / N * 100, 1), NA)
    } else {
      Value <- rowSums(do.call(cbind, lapply(grades, function(g) as.numeric(dat[[paste0(g, "_nWaiver")]]))), na.rm=TRUE)
      Value <- as.integer(round(Value, 0)) # update: ensure integer, remove decimals
    }
    lab <- waiver_summary_label(display_type)
    tab <- dat %>%
      mutate(Value = Value)
    tab %>%
      select(COUNTY, Value) %>%
      arrange(desc(Value)) %>%
      datatable(rowname = FALSE, colnames = c("County", lab), options = list(searching=TRUE))
  })
  
  output$districtTable <- renderDT({
    grades <- input$district_grade
    if(is.null(input$district_county)||input$district_county==""||length(grades)==0)
      return(missing_tibble(c("District", main_label("Waiver", input$district_display_type))))
    dat <- dfDistrict %>% filter(COUNTY == input$district_county)
    display_type <- input$district_display_type
    if(nrow(dat) == 0) return(missing_tibble(c("District", main_label("Waiver", display_type))))
    if(display_type == "percent") {
      nWaiver <- rowSums(do.call(cbind, lapply(grades, function(g) as.numeric(dat[[paste0(g, "_nWaiver")]]))), na.rm=TRUE)
      N <- rowSums(do.call(cbind, lapply(grades, function(g) as.numeric(dat[[paste0(g, "_N")]]))), na.rm=TRUE)
      Value <- ifelse(N > 0, round(nWaiver / N * 100, 1), NA)
    } else {
      Value <- rowSums(do.call(cbind, lapply(grades, function(g) as.numeric(dat[[paste0(g, "_nWaiver")]]))), na.rm=TRUE)
      Value <- as.integer(round(Value, 0)) # update
    }
    lab <- waiver_summary_label(display_type)
    tab <- dat %>%
      mutate(Value = Value)
    tab %>%
      select(DISTRICT, Value) %>%
      arrange(desc(Value)) %>%
      datatable(rowname = FALSE, colnames = c("District", lab), options = list(searching=TRUE))
  })
  
  output$schoolTable <- renderDT({
    grades <- input$school_grade
    if(is.null(input$school_county)||input$school_county==""||is.null(input$school_district)||input$school_district==""||length(grades)==0)
      return(missing_tibble(c("School", main_label("Waiver", input$school_display_type))))
    dat <- df %>% filter(COUNTY==input$school_county, DISTRICT==input$school_district, Group %in% grades)
    display_type <- input$school_display_type
    if(nrow(dat) == 0) return(missing_tibble(c("School", main_label("Waiver", display_type))))
    out_df <- dat %>%
      group_by(NAME) %>%
      summarise(
        N = sum(N, na.rm=TRUE),
        Waiver = sum(nWaiver, na.rm=TRUE),
        Value = if (display_type == "percent") {
          ifelse(N > 0, round(Waiver / N * 100, 1), NA)
        } else {
          as.integer(round(Waiver, 0)) # update
        }
      ) %>%
      arrange(desc(Value))
    lab <- waiver_summary_label(display_type)
    out_df %>%
      select(NAME, Value) %>%
      datatable(rowname=FALSE, colnames=c("School Building", lab), options = list(searching=TRUE))
  })
  
  ### -- COUNT IN BREAKDOWN TABLES, show as integer (no decimals!) -- ###
  output$countyBreakdownTable <- renderUI({
    grades <- input$county_grade
    if(is.null(grades) || is.null(input$county_county) || input$county_county == "") {
      return(HTML('<table class="breakdown-table"><tr><td class="no-data" colspan="7">No data available for selection.</td></tr></table>'))
    }
    dat <- dfCounty %>% filter(COUNTY==input$county_county)
    display_type <- input$county_display_type
    waiver_types <- c("Waiver","Medical","Religious","Philosophical","COMP","PROV","INCOM")
    labs <- c("Any Waiver","Medical","Religious","Philosophical","Vaccinated","Provisional","Incomplete")
    if(length(grades) == 0 || nrow(dat) == 0) return(HTML('<table class="breakdown-table"><tr><td class="no-data" colspan="7">No data available for selection.</td></tr></table>'))
    N <- sum(unlist(lapply(grades, function(g) {coln <- paste0(g, "_N"); if(coln %in% names(dat)) as.numeric(dat[[coln]]) else NA})), na.rm=TRUE)
    vals <- if(display_type=="count") {
      x <- sapply(waiver_types, function(col) sum(unlist(lapply(grades, function(g) {n_col <- paste0(g, "_n", col); if(n_col %in% names(dat)) as.numeric(dat[[n_col]]) else NA})), na.rm=TRUE))
      as.integer(round(x, 0)) # update
    } else {
      sapply(waiver_types, function(col) {
        num <- sum(unlist(lapply(grades, function(g) {n_col <- paste0(g, "_n", col); if(n_col %in% names(dat)) as.numeric(dat[[n_col]]) else NA})), na.rm=TRUE)
        if(col=="COMP") num <- sum(unlist(lapply(grades, function(g) {n_col <- paste0(g, "_nCOMP"); if(n_col %in% names(dat)) as.numeric(dat[[n_col]]) else NA})), na.rm=TRUE)
        if(N>0) round(num/N*100,1) else NA
      })
    }
    col_labs <- if(display_type=="percent"){
      paste0(labs, " (%)")
    } else {
      paste0(labs, " (N)")
    }
    o <- order(-unlist(as.list(vals)))
    vals_list <- as.list(vals[o]) # ensure named list
    col_labs_o <- col_labs[o]
    HTML(paste0(
      '<table class="breakdown-table"><thead><tr>',
      paste0('<th>', col_labs_o, '</th>', collapse=''),
      '</tr></thead><tbody><tr>',
      paste0('<td>', sapply(vals_list, function(x) format(x, nsmall=0)), '</td>', collapse=''), # update: nsmall=0 for count
      '</tr></tbody></table>'
    ))
  })
  
  output$districtBreakdownTable <- renderUI({
    grades <- input$district_grade
    if(is.null(grades) || is.null(input$district_county) || input$district_county == "" || is.null(input$district_district) || input$district_district == "" ) {
      return(HTML('<table class="breakdown-table"><tr><td class="no-data" colspan="7">No data available for selection.</td></tr></table>'))
    }
    dat <- dfDistrict %>% filter(COUNTY==input$district_county, DISTRICT==input$district_district)
    display_type <- input$district_display_type
    waiver_types <- c("Waiver","Medical","Religious","Philosophical","COMP","PROV","INCOM")
    labs <- c("Any Waiver","Medical","Religious","Philosophical","Vaccinated","Provisional","Incomplete")
    if(length(grades) == 0 || nrow(dat) == 0) return(HTML('<table class="breakdown-table"><tr><td class="no-data" colspan="7">No data available for selection.</td></tr></table>'))
    N <- sum(unlist(lapply(grades, function(g) {coln <- paste0(g, "_N"); if(coln %in% names(dat)) as.numeric(dat[[coln]]) else NA})), na.rm=TRUE)
    vals <- if(display_type=="count") {
      x <- sapply(waiver_types, function(col) sum(unlist(lapply(grades, function(g) {n_col <- paste0(g, "_n", col); if(n_col %in% names(dat)) as.numeric(dat[[n_col]]) else NA})), na.rm=TRUE))
      as.integer(round(x, 0)) # update
    } else {
      sapply(waiver_types, function(col) {
        num <- sum(unlist(lapply(grades, function(g) {n_col <- paste0(g, "_n", col); if(n_col %in% names(dat)) as.numeric(dat[[n_col]]) else NA})), na.rm=TRUE)
        if(col=="COMP") num <- sum(unlist(lapply(grades, function(g) {n_col <- paste0(g, "_nCOMP"); if(n_col %in% names(dat)) as.numeric(dat[[n_col]]) else NA})), na.rm=TRUE)
        if(N>0) round(num/N*100,1) else NA
      })
    }
    col_labs <- if(display_type=="percent"){
      paste0(labs, " (%)")
    } else {
      paste0(labs, " (N)")
    }
    o <- order(-unlist(as.list(vals)))
    vals_list <- as.list(vals[o])
    col_labs_o <- col_labs[o]
    HTML(paste0(
      '<table class="breakdown-table"><thead><tr>',
      paste0('<th>', col_labs_o, '</th>', collapse=''),
      '</tr></thead><tbody><tr>',
      paste0('<td>', sapply(vals_list, function(x) format(x, nsmall=0)), '</td>', collapse=''), # update: nsmall=0 for count
      '</tr></tbody></table>'
    ))
  })
  
  output$schoolBreakdownTable <- renderUI({
    grades <- input$school_grade
    if(is.null(grades) || is.null(input$school_county) || input$school_county == "" || 
       is.null(input$school_district) || input$school_district == "" ||
       is.null(input$school_school) || input$school_school == "") {
      return(HTML('<table class="breakdown-table"><tr><td class="no-data" colspan="7">No data available for selection.</td></tr></table>'))
    }
    dat <- df %>% filter(COUNTY==input$school_county, DISTRICT==input$school_district, NAME==input$school_school, Group %in% grades)
    display_type <- input$school_display_type
    waiver_types <- c("nWaiver","nMedical","nReligious","nPhilosophical","nCOMP","PROV","INCOM")
    labs <- c("Any Waiver","Medical","Religious","Philosophical","Vaccinated","Provisional","Incomplete")
    if(length(grades) == 0 || nrow(dat) == 0) return(HTML('<table class="breakdown-table"><tr><td class="no-data" colspan="7">No data available for selection.</td></tr></table>'))
    N <- sum(as.numeric(dat$N), na.rm=TRUE)
    vals <- if(display_type=="count") {
      x <- sapply(waiver_types, function(col) sum(as.numeric(dat[[col]]), na.rm=TRUE))
      as.integer(round(x, 0))
    } else {
      sapply(waiver_types, function(col) {
        num <- sum(as.numeric(dat[[col]]), na.rm=TRUE)
        if(col=="nCOMP") num <- sum(as.numeric(dat[["nCOMP"]]), na.rm=TRUE)
        if(N>0) round(num/N*100,1) else NA
      })
    }
    col_labs <- if(display_type=="percent"){
      paste0(labs, " (%)")
    } else {
      paste0(labs, " (N)")
    }
    o <- order(-unlist(as.list(vals)))
    vals_list <- as.list(vals[o])
    col_labs_o <- col_labs[o]
    HTML(paste0(
      '<table class="breakdown-table"><thead><tr>',
      paste0('<th>', col_labs_o, '</th>', collapse=''),
      '</tr></thead><tbody><tr>',
      paste0('<td>', sapply(vals_list, function(x) format(x, nsmall=0)), '</td>', collapse=''), # update: nsmall=0 for count
      '</tr></tbody></table>'
    ))
  })
  
  output$countyBreakdownTitle <- renderUI({HTML("<strong>Waiver Type Breakdown Table</strong>")})
  output$districtBreakdownTitle <- renderUI({HTML("<strong>Waiver Type Breakdown Table</strong>")})
  output$schoolBreakdownTitle <- renderUI({HTML("<strong>Waiver Type Breakdown Table</strong>")})
  
  output$countyBreakdownPlot <- renderPlot({
    grades <- input$county_grade
    if(is.null(input$county_county)||input$county_county=="") return(ggplot()+geom_blank())
    dat <- dfCounty %>% filter(COUNTY==input$county_county)
    display_type <- input$county_display_type
    waiver_types <- c("Waiver","Medical","Religious","Philosophical","COMP","PROV","INCOM")
    labels <- c("Any Waiver","Medical","Religious","Philosophical","Vaccinated","Provisional","Incomplete")
    if(length(grades) == 0 || nrow(dat) == 0) return(ggplot()+geom_blank())
    N <- sum(unlist(lapply(grades, function(g) {coln <- paste0(g, "_N"); if(coln %in% names(dat)) as.numeric(dat[[coln]]) else NA})), na.rm=TRUE)
    bars <- sapply(waiver_types, function(col) {
      if(display_type=="count") {
        sum(unlist(lapply(grades, function(g) {n_col <- paste0(g, "_n", col); if(n_col %in% names(dat)) as.numeric(dat[[n_col]]) else NA})), na.rm=TRUE)
      } else {
        num <- sum(unlist(lapply(grades, function(g) {n_col <- paste0(g, "_n", col); if(n_col %in% names(dat)) as.numeric(dat[[n_col]]) else NA})), na.rm=TRUE)
        if(col=="COMP") num <- sum(unlist(lapply(grades, function(g) {n_col <- paste0(g, "_nCOMP"); if(n_col %in% names(dat)) as.numeric(dat[[n_col]]) else NA})), na.rm=TRUE)
        if(N>0) round(num/N*100,1) else NA
      }
    })
    bar_df <- tibble(
      WaiverType = factor(labels, levels=labels),
      Rate = pmax(0, replace_na(as.numeric(bars), 0))
    ) %>% arrange(desc(Rate))
    if(nrow(bar_df)==0 || all(is.na(bar_df$Rate))) return(ggplot()+geom_blank())
    ggplot(bar_df, aes(x=reorder(WaiverType, Rate), y=Rate, fill=WaiverType)) +
      geom_col(width=0.7, show.legend=TRUE) +
      scale_fill_brewer(palette="Set2") +
      ylim(0, if(display_type=="percent") 100 else max(bar_df$Rate,na.rm=TRUE)*1.1) +
      labs(x=NULL, y=waiver_type_label(display_type)) +
      coord_flip() +
      theme_minimal(base_family = "Roboto") +
      theme(legend.position = "none")
  })
  
  output$districtBreakdownPlot <- renderPlot({
    grades <- input$district_grade
    if(is.null(input$district_county)||input$district_county==""||is.null(input$district_district)||input$district_district=="") return(ggplot()+geom_blank())
    dat <- dfDistrict %>% filter(COUNTY==input$district_county, DISTRICT==input$district_district)
    display_type <- input$district_display_type
    waiver_types <- c("Waiver","Medical","Religious","Philosophical","COMP","PROV","INCOM")
    labels <- c("Any Waiver","Medical","Religious","Philosophical","Vaccinated","Provisional","Incomplete")
    if(length(grades) == 0 || nrow(dat) == 0) return(ggplot()+geom_blank())
    N <- sum(unlist(lapply(grades, function(g) {coln <- paste0(g, "_N"); if(coln %in% names(dat)) as.numeric(dat[[coln]]) else NA})), na.rm=TRUE)
    bars <- sapply(waiver_types, function(col) {
      if(display_type=="count") {
        sum(unlist(lapply(grades, function(g) {n_col <- paste0(g, "_n", col); if(n_col %in% names(dat)) as.numeric(dat[[n_col]]) else NA})), na.rm=TRUE)
      } else {
        num <- sum(unlist(lapply(grades, function(g) {n_col <- paste0(g, "_n", col); if(n_col %in% names(dat)) as.numeric(dat[[n_col]]) else NA})), na.rm=TRUE)
        if(col=="COMP") num <- sum(unlist(lapply(grades, function(g) {n_col <- paste0(g, "_nCOMP"); if(n_col %in% names(dat)) as.numeric(dat[[n_col]]) else NA})), na.rm=TRUE)
        if(N>0) round(num/N*100,1) else NA
      }
    })
    bar_df <- tibble(
      WaiverType = factor(labels, levels=labels),
      Rate = pmax(0, replace_na(as.numeric(bars), 0))
    ) %>% arrange(desc(Rate))
    if(nrow(bar_df)==0 || all(is.na(bar_df$Rate))) return(ggplot()+geom_blank())
    ggplot(bar_df, aes(x=reorder(WaiverType, Rate), y=Rate, fill=WaiverType)) +
      geom_col(width=0.7, show.legend=TRUE) +
      scale_fill_brewer(palette="Set2") +
      ylim(0, if(display_type=="percent") 100 else max(bar_df$Rate,na.rm=TRUE)*1.1) +
      labs(x=NULL, y=waiver_type_label(display_type)) +
      coord_flip() +
      theme_minimal(base_family = "Roboto") +
      theme(legend.position = "none")
  })
  
  output$schoolBreakdownPlot <- renderPlot({
    grades <- input$school_grade
    if(is.null(input$school_county) || input$school_county == "" ||
       is.null(input$school_district) || input$school_district == "" ||
       is.null(input$school_school) || input$school_school == "")
      return(ggplot()+geom_blank())
    dat <- df %>% filter(COUNTY==input$school_county, DISTRICT==input$school_district, NAME==input$school_school, Group %in% grades)
    display_type <- input$school_display_type
    waiver_types <- c("nWaiver","nMedical","nReligious","nPhilosophical","nCOMP","PROV","INCOM")
    labels <- c("Any Waiver","Medical","Religious","Philosophical","Vaccinated","Provisional","Incomplete")
    if(length(grades) == 0 || nrow(dat) == 0) return(ggplot()+geom_blank())
    N <- sum(as.numeric(dat$N), na.rm=TRUE)
    bars <- sapply(waiver_types, function(col) {
      if(display_type=="count") {
        sum(as.numeric(dat[[col]]), na.rm=TRUE)
      } else {
        num <- sum(as.numeric(dat[[col]]), na.rm=TRUE)
        if(col=="nCOMP") num <- sum(as.numeric(dat[["nCOMP"]]), na.rm=TRUE)
        if(N>0) round(num/N*100,1) else NA
      }
    })
    bar_df <- tibble(
      WaiverType = factor(labels, levels=labels),
      Rate = pmax(0, replace_na(as.numeric(bars), 0))
    ) %>% arrange(desc(Rate))
    if(nrow(bar_df)==0 || all(is.na(bar_df$Rate))) return(ggplot()+geom_blank())
    ggplot(bar_df, aes(x=reorder(WaiverType, Rate), y=Rate, fill=WaiverType)) +
      geom_col(width=0.7, show.legend=TRUE) +
      scale_fill_brewer(palette="Set2") +
      ylim(0, if(display_type=="percent") 100 else max(bar_df$Rate,na.rm=TRUE)*1.1) +
      labs(x=NULL, y=waiver_type_label(display_type)) +
      coord_flip() +
      theme_minimal(base_family = "Roboto") +
      theme(legend.position = "none")
  })
  
  mapdat_reactive <- reactive({
    grades <- input$map_grade
    county_col <- if("NAME" %in% colnames(mi_counties)) "NAME" else names(mi_counties)[1]
    mapdat <- left_join(mi_counties, dfCounty, by = setNames("COUNTY", county_col))
    nWaiver <- rowSums(do.call(cbind, lapply(grades, function(g) as.numeric(mapdat[[paste0(g,"_nWaiver")]]))), na.rm=TRUE)
    N <- rowSums(do.call(cbind, lapply(grades, function(g) as.numeric(mapdat[[paste0(g,"_N")]]))), na.rm=TRUE)
    mapdat$waiverRate <- ifelse(N>0,nWaiver/N*100,NA)
    mapdat
  })
  
  output$miMap <- renderLeaflet({
    mapdat <- mapdat_reactive()
    pal <- colorNumeric("YlOrRd", domain = mapdat$waiverRate, na.color = "#f0f0f0")
    leaflet(mapdat) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(waiverRate),
        fillOpacity = 0.8, weight = 1, color = "white",
        label = ~paste(mapdat$NAME, paste("Waiver Rate:", round(waiverRate, 1), "%")),
        highlightOptions = highlightOptions(weight = 2, color = "#FFCB05", bringToFront = TRUE)
      ) %>%
      addLegend(
        pal = pal,
        values = mapdat$waiverRate,
        title = "Waiver Rate (%)",
        position = "bottomright",
        na.label = ""
      )
  })
  
}

shinyApp(ui = ui, server = server)