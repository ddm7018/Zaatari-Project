library(leaflet)

# Choices for drop-downs
vars <- c(
  "Total Household Residents" = "total_residents",
  "Total Educated Residents" = "total_educated",
  "Percentage Education" = "literacy",
  "Average Information Source" = "avg_info_source"
)

navbarPage("Zaatari", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Explorer"),

        selectInput("color", "Color", vars, selected = "total_residents"),
        selectInput("size", "Size", vars, selected = "total_educated"),
        conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
          numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        ),

        plotOutput("histCentile", height = 200),
        plotOutput("scatterCollegeIncome", height = 250)
      ),

      tags$div(id="cite",
        'Daniel Mooney - RIT IST Capstone'
      )
    )
  ),

  tabPanel("Raw Data",
    hr(),
    DT::dataTableOutput("assetTable")),
    
  tabPanel("Summary Data",
  hr(),
  DT::dataTableOutput("sumTable")
    
  ),
  tabPanel("R Console", tags$div(HTML("<iframe width='100%' height='300' src='http://r-fiddle.org/#/embed/eYsWfghB/1' allowfullscreen='allowfullscreen' frameborder='0'></iframe>"))
          
           
  ),

  conditionalPanel("false", icon("crosshair"))
)
