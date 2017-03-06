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
        plotOutput("hist", height = 200),
        plotOutput("scatter", height = 250)
      ),
      tags$div(id="cite",
        'Daniel Mooney - RIT IST Capstone'
      ))
  ),

  tabPanel("Raw Data",
    hr(),
    DT::dataTableOutput("assetTable")),
  
  tabPanel("Summary Data",
    hr(),
    DT::dataTableOutput("sumTable")
  ),
  
 tabPanel("Upload",
          titlePanel("Uploading Files"),
          sidebarLayout(
            sidebarPanel(
              fileInput('file1', 'Choose XML File',
                        accept=c('text/csv',
                                 'text/comma-separated-values,
                                 text/plain','.csv')),
              tags$hr(),
              actionButton("recalc", "Add Data")
              #checkboxInput('header', 'Header', TRUE),
              #radioButtons('sep', 'Separator',
              #             c(Comma=',',
              #               Semicolon=';',
              #               Tab='\t'),
              #             ','),
            #  radioButtons('quote', 'Quote',
            #               c(None='',
            #                 'Double Quote'='"',
            #                 'Single Quote'="'"),
            #               '"')
            ),
            mainPanel(
              tableOutput('contents')
            )
          )
 ),

  conditionalPanel("false", icon("crosshair"))
)
