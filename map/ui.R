library(leaflet)
library(rgdal)
library(shinydashboard)
library(shiny)
library(shinyStore)


navbarPage("Zaatari", id="nav",
    tabPanel("Interactive map",
      div(class="outer",
          
         
        tags$head(
         
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      leafletOutput("map", width="100%", height="100%"),
      initStore("store", "shinyStore-ex1"),
      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",
        h2("Explorer"),
        selectInput("x_color", "X Color", vars, selected = "sum_household"),
        selectInput("y_color", "Y Color", vars, selected = "literate"),
        plotOutput("legend_color")
      ),
      tags$div(id="cite",'Daniel Mooney - RIT IST Capstone'))
  ),

  tabPanel("Raw Data",
    hr(),
    DT::dataTableOutput("assetTable")
    ),
  
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
         selectInput("y_input", "Y Input", vars, selected = "literate"))
  ),
  
  tabPanel("Console",
    tags$iframe(width='100%', height= '500',
                src='http://www.r-fiddle.org/#/fiddle?id=QMGrgLW9&version=4',
                allowfullscreen='allowfullscreen', frameborder='0')),  
  
  tabPanel("Function Builder",
    selectizeInput("attr", "Choose an attribute:",modifiedName),
    selectInput("func", "Choose an function:",choices = NULL),
    DT::dataTableOutput("functionbuilder"),
    textInput("dim","","Dim1"),
    actionButton("addDim", "Add Dimension"),
    DT::dataTableOutput("editor"),
    actionButton("dltFunc", "Delete Selected Dimension")
  ),

 # tabPanel("Upload",
 #          titlePanel("Uploading Files"),
 #          sidebarLayout(
 #            sidebarPanel(
 #              fileInput('file1', 'Choose XML File',
 #                        accept=c('text/csv',
 #                                 'text/comma-separated-values,
 #                                 text/plain','.csv')),
 #              tags$hr(),
 #              actionButton("evReactiveButton", "evReactiveButton")
 #            ),
 #            mainPanel(
 #              tableOutput('contents')
 #            )
 #          )
 # ),

  conditionalPanel("false", icon("crosshair"),
                   includeScript("message-handler.js"))
)
