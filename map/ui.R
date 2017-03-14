library(leaflet)
library(rgdal)
# Choices for drop-downs


#move to core
dimFile <- read.table("dim.txt")
vars = c()
for(i in 1:nrow(dimFile)){
    vars[[as.character(dimFile['V2'][i,])]] <- as.character(dimFile['V2'][i,])
}

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
        selectInput("color", "Color", vars, selected = "sum_household"),
        selectInput("size", "Size", vars, selected = "literate"),
        conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
          numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        )
       
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
  tabPanel("Charts",
           plotOutput("hist", height = 200),
           plotOutput("scatter", height = 250),
           absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                         draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                         width = 330, height = "auto",
                         h2("Explorer"),
                         selectInput("hist_input", "Histogram", vars, selected = "literate"),
                         selectInput("x_input", "X Input", vars, selected = "literate"),
                         selectInput("y_input", "Y Input", vars, selected = "literate")
                         )
                         
           
  ),
  tabPanel("Console",
  tags$iframe(width='100%', height= '500',
              src='http://www.r-fiddle.org/#/fiddle?id=QMGrgLW9&version=1',
              allowfullscreen='allowfullscreen', frameborder='0')),  
  
  tabPanel("Function Builder",
  selectInput("attr", "Choose an attribute:",names(asset)),
  selectInput("func", "Choose an function:",c("sum","mean","count")),
  DT::dataTableOutput("functionbuilder"),
  actionButton("addfunc", "Add Function")),
  
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
