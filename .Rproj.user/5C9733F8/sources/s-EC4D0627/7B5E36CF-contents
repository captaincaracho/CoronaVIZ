# Load Libraries  

library(shiny)
library(shinydashboard)
#> Warning: package 'shinydashboard' was built under R version 3.5.2
#> 
#> Attaching package: 'shinydashboard'
#> The following object is masked from 'package:graphics':
#> 
#>     box
library(shinyWidgets)
library(shinythemes)
#> Warning: package 'shinythemes' was built under R version 3.5.2
library(DT)
#> Warning: package 'DT' was built under R version 3.5.2
#> 
#> Attaching package: 'DT'
#> The following objects are masked from 'package:shiny':
#> 
#>     dataTableOutput, renderDataTable
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(tidyr)
library(maptools)
#> Loading required package: sp
#> Checking rgeos availability: TRUE
library(rgdal)
#> rgdal: version: 1.3-6, (SVN revision 773)
#>  Geospatial Data Abstraction Library extensions to R successfully loaded
#>  Loaded GDAL runtime: GDAL 2.2.3, released 2017/11/20
#>  Path to GDAL shared files: C:/Users/Eli/Documents/R/win-library/3.5/rgdal/gdal
#>  GDAL binary built with GEOS: TRUE 
#>  Loaded PROJ.4 runtime: Rel. 4.9.3, 15 August 2016, [PJ_VERSION: 493]
#>  Path to PROJ.4 shared files: C:/Users/Eli/Documents/R/win-library/3.5/rgdal/proj
#>  Linking to sp version: 1.3-1
library(rworldmap)
#> Warning: package 'rworldmap' was built under R version 3.5.2
#> ### Welcome to rworldmap ###
#> For a short introduction type :   vignette('rworldmap')
library(leaflet)
library(sp)
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 3.5.2
library(plotly)
#> 
#> Attaching package: 'plotly'
#> The following object is masked from 'package:ggplot2':
#> 
#>     last_plot
#> The following object is masked from 'package:stats':
#> 
#>     filter
#> The following object is masked from 'package:graphics':
#> 
#>     layout
library(lubridate)
#> 
#> Attaching package: 'lubridate'
#> The following object is masked from 'package:base':
#> 
#>     date
library(rAmCharts)
#> Warning: package 'rAmCharts' was built under R version 3.5.2
#> Full amcharts.js API available using amChartsAPI()
#> Look at rAmCharts::runExamples() & http://datastorm-open.github.io/introduction_ramcharts/
#> Bug report or feed back on https://github.com/datastorm-open/rAmCharts
#> 
#> Attaching package: 'rAmCharts'
#> The following object is masked from 'package:plotly':
#> 
#>     api
#> The following object is masked from 'package:maptools':
#> 
#>     label
#> The following object is masked from 'package:shinyWidgets':
#> 
#>     panel
library(rsconnect)
#> 
#> Attaching package: 'rsconnect'
#> The following object is masked from 'package:shiny':
#> 
#>     serverInfo

# Reading in the data

setwd("C:/Users/Eli/Desktop/Media_Freedoms")

pf <- read.csv("Index_Data_2000.csv", header = TRUE)

# Cleaning the data 

pfg <- pf[ -c(2:11) ]

pfg <- gather(pfg, measure, score, A.Legal:Status.16, factor_key = TRUE)
#> Warning: attributes are not identical across measure variables;
#> they will be dropped

colnames(pfg)[1] = "country"

pfg$score <- as.numeric(pfg$score)
#> Warning: NAs introduced by coercion

pfg <- pfg %>% arrange(country)

# Creating a 'year' Column

year <- data.frame(rep(seq(2001,2016, by = 1), 5))

colnames(year) <- "year"

year <- year %>% arrange(year)

y <- rep(as.vector(year), each = 197)

y <- data.frame(unlist(y, recursive = TRUE, use.names = TRUE))

pfg <- cbind(pfg, y)

colnames(pfg)[4] = "year"

# Cleaning the 'measure' Column

measurenames <- c("Legal", "Political", "Economic", "Total_Score", "Status")

measurenames <- data.frame(rep(measurenames, 3152))

pfg <- cbind(pfg, measurenames)

pfg$measure <- NULL

colnames(pfg)[4] = "measure"

# Adding ISO3 codes to country data to use as join key


ISO3 <- c("AFG",
          "ALB",
          "DZA",
          "AGO",
          "ATG",
          "ARG",
          "ARM",
          "AUS",
          "AUT",
          "AZE",
          "BHS",
          "BHR",
          "BGD",
          "BRB",
          "BlR",
          "BEL",
          "BLZ",
          "BEN",
          "BTN",
          "BOL",
          "BIH",
          "BWA",
          "BRA",
          "BRN",
          "BGR",
          "BFA",
          "BDI",
          "KHM",
          "CMR",
          "CAN",
          "CPV",
          "CAF",
          "TCD",
          "CHL",
          "CHN",
          "COL",
          "COM",
          "COG",
          "COD",
          "CRI",
          "CIV",
          "HRV",
          "CUB",
          "CYP",
          "CZE",
          "DNK",
          "DJI",
          "DMA",
          "DOM",
          "ECU",
          "EGY",
          "SLV",
          "GNQ",
          "ERI",
          "EST",
          "ETH",
          "FJI",
          "FIN",
          "FRA",
          "GAB",
          "GEO",
          "DEU",
          "GHA",
          "GRC",
          "GRD",
          "GTM",
          "GIN",
          "GNB",
          "GUY",
          "HTI",
          "HND",
          "HKG",
          "HUN",
          "ISL",
          "IND",
          "IDN",
          "IRN",
          "IRQ",
          "IRL",
          "ISR",
          "PSE",
          "ITA",
          "JAM",
          "JPN",
          "JOR",
          "KAZ",
          "KEN",
          "KIR",
          "KOS",
          "KWT",
          "KGZ",
          "LAO",
          "LVA",
          "LBN",
          "LSO",
          "LBR",
          "LBY",
          "LIE",
          "LTU",
          "LUX",
          "MKD",
          "MDG",
          "MWI",
          "MYS",
          "MDV",
          "MLI",
          "MLT",
          "MHL",
          "MRT",
          "MUS",
          "MEX",
          "FSM",
          "MDA",
          "MCO",
          "MNG",
          "MNE",
          "MAR",
          "MOZ",
          "MMR",
          "NAM",
          "NRU",
          "NPL",
          "NLD",
          "NZL",
          "NIC",
          "NER",
          "NGA",
          "PRK",
          "NOR",
          "OMN",
          "PAK",
          "PLW",
          "PAN",
          "PNG",
          "PRY",
          "PER",
          "PHL",
          "POL",
          "PRT",
          "QAT",
          "ROU",
          "RUS",
          "RWA",
          "KNA",
          "LCA",
          "VCT",
          "WSM",
          "SMR",
          "STP",
          "SAU",
          "SEN",
          "SRB",
          "SYC",
          "SLE",
          "SGP",
          "SVK",
          "SVN",
          "SLB",
          "SOM",
          "ZAF",
          "KOR",
          "SON",
          "ESP",
          "LKA",
          "SON",
          "SUR",
          "SWZ",
          "SWE",
          "CHE",
          "SYR",
          "TWN",
          "TJK",
          "TZA",
          "THA",
          "GMB",
          "TLS",
          "TGO",
          "TON",
          "TTO",
          "TUN",
          "TUR",
          "TKM",
          "TUV",
          "UGA",
          "UKR",
          "ARE",
          "GBR",
          "USA",
          "URY",
          "UZB",
          "VUT",
          "VEN",
          "VNM",
          "PSE",
          "YEM",
          "ZMB",
          "ZWE") 

i <- rep(ISO3, each = 80)

pfg <- cbind(pfg, i)

pfg <- pfg %>% rename(ISO3 = i)

# Creating data used to rank country press freedoms

pfg.rankings <- pfg %>%
  group_by(year) %>%
  filter(measure %in% "Total_Score") %>%
  arrange(year, score) %>%
  mutate(ranking = row_number())

# Creating the Map Data
pfgts <- pfg %>% filter(measure %in% "Total_Score")
pal <- colorBin("YlOrRd", domain = pfgts$score, bins = 5)


# Creates data for yearly press freedom trends with rAMCharts

pfgam <- spread(pfg, measure, score)
pfgam$year <- as.POSIXct(paste(pfgam$year), format = "%Y")

# Interactive Data Table

pfgt <- pfg %>%
  group_by(year, country, measure) 

pfgtable <- spread(pfgt, measure, score)
pfgtable$Status <- NULL

# Dashboard

header <- dashboardHeader(title = span(tagList(icon("calendar"), "Press Freedom Index")))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("World Map", tabName = "map"),
    menuItem("Historical Score Data by Year", tabName = "score"),
    menuItem("Country Rankings", tabName = "rankings"),
    menuItem("Data Table", tabName = "table")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "map",
            leafletOutput("worldmap", height = 1000),
            absolutePanel(top = 490, right = '73%', height = 100, width =  100, fixed = FALSE,
                          knobInput(
                            inputId = "year",
                            label = "",
                            value = 2016,
                            min = 2001,
                            max = 2016,
                            displayPrevious = FALSE, 
                            lineCap = "round",
                            fgColor = "#F37C05",
                            bgColor = "FFFFFF",
                            inputColor = "#F37C05",
                            width = 100,
                            height = 100,
                            immediate = FALSE
                          ))
    ),
    tabItem(tabName = "score",
            fluidRow(
              box(title = "Press Freedom Index", solidHeader = TRUE, status = "warning", width = 12,
                  h1('Looking at Year By Year Trends in Press Freedoms'),
                  p('The data used for this application is provided by Freedom House
                    and can be accessed with this link:'),
                  strong('https://freedomhouse.org/report-types/freedom-press'),
                  
                  p('Scores on press freedoms have been measured and determined through the context of three different environments:'),
                  
                  tags$li('Legal (range of 1 - 30)'),
                  tags$li('Political (range of 1 - 40)'),
                  tags$li('Environment (range of 1 - 30)'),
                  
                  p('Countries with a higher score indicate lower amounts of freedom. Total scores
are therefore assigned out of 100 - countries with greater amounts of press freedoms have lower scores and countries 
with lesser amounts of press freedoms have higher scores.'),
                  amChartsOutput("score", height = 520),
                  absolutePanel(top = 80, right = 70, fixed = FALSE,
                                selectInput("country", "Select a Country", choices = levels(pfgam$country), width = 200))
              )
            )
    ),
    tabItem(tabName = "rankings",
            fluidRow(
              box(title = "Country Rankings", solidHeader = TRUE, status = "warning", width = 12,
                  h1('Press Freedoms Relative to Other Countries'),
                  p('Here you can look at how other press freedoms of countries compare with eachother by rank.
                    Adjust the circular knob to select the year, and select adjust the slider range to see a 
                    rank range (For example, select 1 and 10 to see the top 10 countries for press freedoms)'),
                  sliderInput("range", "Select Ranking Range", min = 1, max = 197, value = c(1, 10), step = 1, dragRange = TRUE),
                  
                  amChartsOutput("rank"),
                  absolutePanel(top = 300, right = 70, fixed = FALSE,
                                knobInput("rankyear", "", min = 2001, max = 2016, value = 2016,
                                          displayPrevious = FALSE, 
                                          lineCap = "round",
                                          fgColor = "#F37C05",
                                          bgColor = "FFFFFF",
                                          inputColor = "#F37C05",
                                          width = 100,
                                          height = 100,
                                          immediate = FALSE)
                  )
                  
              )
            )),
    tabItem(tabName = "table",
            fluidRow(
              box(title = "Country Data", solidHeader = TRUE, status = "warning", width = 12,
                  h1('Press Freedom Data'),
                  p('Here you can look through country data manually. Use the search bar to filter data.'),
                  DTOutput('table'))
            )
    )
  )
)

# Define UI for application
ui <- dashboardPage(header, sidebar, body, skin = "yellow")

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  pfgam_re <- reactive({
    pfgam %>% filter(country %in% input$country)
  })
  
  pfg.rankings_re <- reactive({
    pfg.rankings %>%
      filter(year %in% input$rankyear) %>%
      slice(input$range[1]:input$range[2])
  })
  
  output$table <- renderDT({
    pfgtable
  })
  
  output$rank <- renderAmCharts({
    amBarplot(x = ("country"), y = "score", data = pfg.rankings_re(), horiz = TRUE, zoom = TRUE)
  })
  
  output$score <- renderAmCharts({
    amTimeSeries(pfgam_re(), 'year', c('Political', 'Economic', 'Legal', 'Total_Score'),
                 scrollbar = TRUE, main = paste("Yearly Press Freedom Scores In ", input$country))
  })
  
  selected <- reactive({
    pfgts <- pfgts %>% filter(year %in% input$year)
  })
  
  output$worldmap <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      addMiniMap()
    
  })
  
  observe({
    if(!is.null(input$year)){
      map <- joinCountryData2Map(selected(), joinCode = "ISO3",
                                 nameJoinColumn = "ISO3")
      leafletProxy("worldmap", data = map) %>%
        addTiles() %>% 
        clearShapes() %>% 
        addPolygons(fillColor = ~pal(map$score),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "white",
                      dashArray = "3",
                      fillOpacity = .8,
                      bringToFront = TRUE),
                    label = ~paste(as.character(map$country),
                                   "Total Index Score: ", as.character(map$score)))
    }})
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)
#> 
#> Listening on http://127.0.0.1:4229
#> 194 codes from your data successfully matched countries in the map
#> 3 codes from your data failed to match with a country code in the map
#> 50 codes from the map weren't represented in your data