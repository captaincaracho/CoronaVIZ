
###############################################################################################
library(data.table)
library(plyr)
library(dplyr)
library(ISOcodes)
library(countrycode)
library(tidyr)
library(BBmisc)
library(shiny)
library(lubridate)
library(shinyWidgets)
library(leaflet)
library(shinydashboard)
library(maptools)
library(rworldmap)
library(scales)
library(ggplot2)
library(rsconnect)

source("Prepare.R", local = TRUE)


###Shiny App

header <- dashboardHeader(title = "CoronaVIZ")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("World Map", tabName = "map", icon = icon("globe")),
    menuItem("Timeline plots", tabName = "plot", icon = icon("chart-line"))
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "map",
            leafletOutput("map", width = "100%", height = 800),

                          
            absolutePanel(top = '80%', right = '10%', height = 100, width =  100, fixed = FALSE,
                          
                          #textOutput("timestamp")
                          
            ),          
   
            
            absolutePanel(top = '80%', right = '80%', height = 100, width =  100, fixed = FALSE,
                            
                            dateInput(inputId = "Day",
                                      label = "Pick a date",
                                      format = "yyyy-mm-dd",
                                      weekstart = 1,
                                      value = max(countries$Day)),
                          
                          
            ),  
                                      
            absolutePanel(top = '80%', right = '65%', height = 100, width =  250, fixed = FALSE,                        
                            selectInput(inputId = "Info", 
                                        label = "Pick a variable to display",
                                        choices = list("Absolute numbers" = list(
                                                    "Cases" = "Cases",
                                                    "Active" = "Active", 
                                                    "Deaths" = "Deaths",
                                                    "Recovered" = "Recovered"
                                                    ),
                                             "Relative numbers" = list(
                                                    "Cases per Million" = "Cases per Million",
                                                    "Active Cases per Million" = "Active Cases per Million",
                                                    "Deaths per Million" = "Deaths per Million",
                                                    "Recovered Cases per Million" = "Recovered Cases per Million"
                                                    ),
                                             "Ratios" = list(
                                                    "Death Ratio of all Cases" = "Death ratio",
                                                    "Active Ratio of all Cases" = "Active ratio", 
                                                    "Death Ratio of all Known Outcomes" = "Death ratio of Outcomes"
                                                    ),
                                             "Growth Rates" = list(
                                                    "Daily Growth Rate of Cases"  = "Daily Growth Rate of Cases" ,
                                                    "Daily Growth Rate of Active Cases" = "Daily Growth Rate of Active Cases",
                                                    "Daily Growth Rate of Deaths"  = "Daily Growth Rate of Deaths"
                                             )),
                                        selected = "Cases per Million",
                                        multiple = FALSE, 
                                        selectize = FALSE,
                                        width = '100%',
                                        size = 1) 
                                      
           )
           
           
           
          ),
    
    tabItem(tabName = "plot",
            
           
            plotOutput("timeline", height = "700px"),
              
            
            absolutePanel(top = '80%', right = '10%', height = 100, width =  100, fixed = FALSE,
                          
                          textOutput("timestamp")
                          
                          ),
            
            
            
            absolutePanel(top = '80%', right = '80%', height = 100, width =  100, fixed = FALSE,
              
              selectInput(
                inputId = "Country_plot",
                label   = "Pick Countries", 
                choices = as.character(unique(countries$Country)),
                multiple = TRUE,
                #selelt top three countries in cases for default plot
                selected = countries[is.element(countries$Cases,sort(countries[which(countries$Day==max(countries$Day,na.rm = TRUE)),"Cases"], decreasing = TRUE)[1:3]),"Country"]
              )),
              
            absolutePanel(top = '80%', right = '65%', height = 100, width =  250, fixed = FALSE,  
              
              selectInput(
                inputId = "Variable_plot",
                label   = "Pick Variable", 
                choices = list("Absolute numbers" = list(
                  "Cases" = "Cases",
                  "Active" = "Active", 
                  "Deaths" = "Deaths",
                  "Recovered" = "Recovered"
                ),
                "Relative numbers" = list(
                  "Cases per Million" = "Cases per Million",
                  "Active Cases per Million" = "Active Cases per Million",
                  "Deaths per Million" = "Deaths per Million",
                  "Recovered Cases per Million" = "Recovered Cases per Million"
                ),
                "Ratios" = list(
                  "Death Ratio of all Cases" = "Death ratio",
                  "Active Ratio of all Cases" = "Active ratio", 
                  "Death Ratio of all Known Outcomes" = "Death ratio of Outcomes"
                ),
                "Growth Rates" = list(
                  "Daily Growth Rate of Cases"  = "Daily Growth Rate of Cases" ,
                  "Daily Growth Rate of Active Cases" = "Daily Growth Rate of Active Cases",
                  "Daily Growth Rate of Deaths"  = "Daily Growth Rate of Deaths"
                )),
                multiple = FALSE,
                selected = "Cases"
              )),
            
            absolutePanel(top = '80%', right = '55%', height = 100, width =  150, fixed = FALSE,
                          
                          dateInput(inputId = "Start_Day",
                                    label = "Pick a start date",
                                    format = "yyyy-mm-dd",
                                    weekstart = 1,
                                    value = min(countries$Day))
                          
            ),  
            
            absolutePanel(top = '80%', right = '45%', height = 100, width =  150, fixed = FALSE,
                          
                          dateInput(inputId = "End_Day",
                                    label = "Pick an end date",
                                    format = "yyyy-mm-dd",
                                    weekstart = 1,
                                    value = max(countries$Day))
                          
            ),  
            
            )
    
    
  )
)


# Define UI for application
ui <- dashboardPage(header, sidebar, body, skin = "black")


server <- function(input, output, session) {

  
  #Worldmap
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      addTiles() %>%
       addMiniMap()
  })
  
  
  selected <- reactive({
    #countries <- countries %>% filter(Day %in% input$Day)
    countries <- countries[countries$Day == input$Day, ]
    
  })
  
  

  observe({
      map <- joinCountryData2Map(selected(), joinCode = "ISO3",nameJoinColumn = "ISO3")
      
      #Colorcode
      map$Color <- switch(input$Info, 
                     "Cases" = map$Cases_color,
                     "Active" = map$Active_color,
                     "Deaths" = map$Deaths_color,
                     "Recovered" = map$Recovered_color,
                      
                     "Cases per Million" = map$CpC_color,
                     "Active Cases per Million" = map$ApC_color,
                     "Deaths per Million" = map$DpC_color,
                     "Recovered Cases per Million" = map$RpC_color,
                     
                     "Death ratio" = map$D2C_color,
                     "Active ratio" = map$A2C_color,
                     "Death ratio of Outcomes" = map$D2O_color,
                     
                     "Daily Growth Rate of Cases"  = map$CdG_color,
                     "Daily Growth Rate of Active Cases" = map$AdG_color,
                     "Daily Growth Rate of Deaths"  = map$DdG_color
                     
                   
                     )
      
      #Number that shows up in label
      map$Info <- switch(input$Info, 
                         "Cases" = map$Cases,
                         "Active" = map$Active,
                         "Deaths" = map$Deaths,
                         "Recovered" = map$Recovered,
                         
                         "Cases per Million" = map$CpC,
                         "Active Cases per Million" = map$ApC,
                         "Deaths per Million" = map$DpC,
                         "Recovered Cases per Million" = map$RpC,
                         
                         "Death ratio" = map$D2C,
                         "Active ratio" = map$A2C,
                         "Death ratio of Outcomes" = map$D2O,
                         
                         "Daily Growth Rate of Cases"  = map$CdG,
                         "Daily Growth Rate of Active Cases" = map$AdG,
                         "Daily Growth Rate of Deaths"  = map$DdG
                         )
      
      #Explaining text next to number
      map_title <- switch(input$Info, 
                         "Cases" = "Cases",
                         "Active" = "Active",
                         "Deaths" = "Deaths",
                         "Recovered" = "Recovered",
                         
                         "Cases per Million" = "Cases per Million Inhabitants",
                         "Active Cases per Million" = "Active Cases per Million Inhabitants",
                         "Deaths per Million" = "Deaths per Million Inhabitant",
                         "Recovered Cases per Million" = "Recovered Cases per Million Inhabitants",
                         
                         "Death ratio" = "Percentage of Deaths of all Cases",
                         "Active ratio" = "Percentage of Active Cases of all Cases",
                         "Death ratio of Outcomes" = "Percentage of Deaths of all known Outcomes",
                         
                         "Daily Growth Rate of Cases"  = "Daily Growth Rate of Cases" ,
                         "Daily Growth Rate of Active Cases" = "Daily Growth Rate of Active Cases",
                         "Daily Growth Rate of Deaths"  = "Daily Growth Rate of Deaths"
                         )
      
      measure  <- switch(input$Info, 
                         "Cases" = "",
                         "Active" = "",
                         "Deaths" = "",
                         "Recovered" = "",
                         
                         "Cases per Million" = "",
                         "Active Cases per Million" = "",
                         "Deaths per Million" = "",
                         "Recovered Cases per Million" = "",
                         
                         "Death ratio" = "%",
                         "Active ratio" = "%",
                         "Death ratio of Outcomes" = "%",
                         
                         "Daily Growth Rate of Cases"  = "%" ,
                         "Daily Growth Rate of Active Cases" = "%",
                         "Daily Growth Rate of Deaths"  = "%"
                         
                         
      )
      
      
      leafletProxy("map", data = map) %>%
        addTiles() %>% 
        clearShapes()  %>%
        addPolygons(fillColor = map$Color,
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
                      label = paste0(map$Country,": ", ifelse(is.na(map$Info),"No data or not not enough cases for computing a quota", paste(round(map$Info,2), measure))),
                    popup = paste0("<head>
                                     <style>
                                     table, th, td {
                                       border: 1 px grey;
                                       border-collapse: collapse;
                                     }
                                   th, td {
                                     padding: 5px;
                                     text-align: left;
                                   }
                                   </style>
                                     </head>",
                      
                                   "<h3> <img src='https://raw.githubusercontent.com/captaincaracho/world_countries/master/flags/32x32/",tolower(map$ISO2),".png', width = 35 >     <b>",map$Country,"</b> </h3>",
                                   
                                   "<table>
                                     <tr>
                                     <th>",map$Day,"</th>
                                     <th> Absolute </th>
                                     <th> Daily Growth </th>
                                     <th> per Million </th>
                                     </tr> <tr>
                                     <td>Cases</td>
                                     <td>",ifelse(is.na(map$Cases),"-", format(map$Cases, big.mark =",", nsmall = 0)),"</td>
                                     <td>",ifelse(is.na(map$CdG),"-", format(round(map$CdG,2), big.mark =",")),"% </td>
                                     <td>",ifelse(is.na(map$CpC),"-", format(round(map$CpC,2), big.mark =",")),"</td>
                                     </tr> <tr>
                                     <td>Deaths </td>
                                     <td>", ifelse(is.na(map$Deaths),"-", format(map$Deaths, big.mark =",", nsmall = 0)),"</td>
                                     <td>", ifelse(is.na(map$DdG),"-", format(round(map$DdG,2), big.mark =",")),"% </td>
                                      <td>",ifelse(is.na(map$DpC),"-", format(round(map$DpC,2), big.mark =",")),"</td>
                                     </tr> <tr>
                                     <td>Active Cases</td>
                                     <td>",ifelse(is.na(map$Active),"-", format(map$Active, big.mark =",", nsmall = 0)),"</td>
                                     <td>",ifelse(is.na(map$AdG),"-", format(round(map$CdG,2), big.mark =",")),"% </td>
                                     <td>",ifelse(is.na(map$ApC),"-", format(round(map$ApC,2), big.mark =",")),"</td>
                                     </tr>
                                    </table>"
                                   ),
                    
                    popupOptions = popupOptions(closeOnClick = TRUE, closeButton = FALSE, autoPan = TRUE)
                                  
                                  )
      
      
      #Timeline
      selected_plot <- reactive({
        countries <- countries[which(countries$Country == input$Country_plot & countries$Day >= input$Start_Day & countries$Day <=input$End_Day) , ]
      })
      
      
      
      plotvar   <- switch(input$Variable_plot, 
                         "Cases" = "Cases",
                         "Active" = "Active",
                         "Deaths" = "Deaths",
                         "Recovered" = "Recovered",
                         
                         "Cases per Million" = "CpC",
                         "Active Cases per Million" = "ApC",
                         "Deaths per Million" = "DpC",
                         "Recovered Cases per Million" = "RpC",
                         
                         "Death ratio" = "D2C",
                         "Active ratio" = "A2C",
                         "Death ratio of Outcomes" = "D2O",
                         
                         "Daily Growth Rate of Cases"  = "CdG",
                         "Daily Growth Rate of Active Cases" = "AdG",
                         "Daily Growth Rate of Deaths"  = "DdG"
      )
      
      
      
      output$timeline <- renderPlot({
       
        ggplot(data = selected_plot(), aes(x=Day, y=!!as.name(plotvar), color=Country))+
           geom_line(size=1.5)+
           labs(x = "Date",y = input$Variable_plot)+
           scale_x_date(labels = date_format("%m-%d"), date_breaks = "1 week")+
           theme(axis.text = element_text(size = 12),
                 axis.title = element_text(size=14),
                 legend.title = element_text(size=14),
                 legend.text = element_text(size=12),
                 panel.background = element_rect(fill="white"),
                 panel.grid.major = element_line(colour = "grey"))
           
        
        
      })
      
      
      output$timestamp <- renderText(paste0("last updated: ", time))
      
      
    
    })

  
}

shinyApp(ui, server)