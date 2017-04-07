# ZaataRi-Project
## Abstract
Geovizualization, and its capacity  to provide tools for visual spatial  analysis, has wide ranging domain applications to support sense and decision making including addressing complex problem in humanitarian crisis management.   The need for such tools is manifest in the Middle East in light of the civil war in Syria and ensuing mass migration of millions of Syrian refugees to neighboring countries.  Zaatari, home to 80,000 refugees in Jordan, provides for basic needs and seeks to enhance its social infrastructure.  The Zaatari camp, organized in districts and further divided into blocks, offers the opportunity to develop a geovisualization tool to analyze  patterns and relationships  on a block level relating to employment skills, sources of information, and education and literacy rates to support decision making. While there has been extensive analysis at the district level, there is no current application at the block level. This project is a “Shiny” geovisualization tool incorporating block-level analysis of key social attributes combining block-level asset survey data and OpenStreetMap.

## Technology
The geovisualization application was built using Shiny (R's web framework). The application also uses an Leaflet(to render the interactive maps) and shinyStore (an experimental library to save extra dimensions using HTML5 localStorage), along with a variety of other traditional R packages.

## Dimensions:
 The application has four preloaded dimensions which can be analyzed in Interactive Map and Chart Tabs
  1) sum_houshold - which represents the total number of people in each block
  2) employment_rate - the percentage of houses in each block that have someone employed
  3) literacy_rate - total number of literate people in a block divided by the total number of people in a block
  4) average _informant - which represents the average age of the best person to get information from in each block

## Dimension Builder
The application also allows for adding ten extra dimension using the Dimension Builder. This tab allows users to view different attributes of the data and add up to 10 of them to application. Whenever a user add the dimensions they are aviable, in the Interactive Maps and Charts.

## Tutorial Link
https://vimeo.com/210507351

## Live Link
http://rit-capstone.ddns.net/Zaatari-Project/map/


## Installation Steps
1) Download R (if not installed)
2) Download relevant R libraries
3) Download Zip
4) Unpack
5) Download Shape Files from https://drive.google.com/file/d/0B_dPfSwYMdclaFE5aFpmZGd2MFU/view?usp=sharing
6) Unpack Shape Files Zip into map folder and rename as boundaries
7) Run the R command shiny::runApp('Zaatari-Project/map')
