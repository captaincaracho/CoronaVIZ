
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
library(directlabels)
library(rsconnect)

source("Prepare.R", local = TRUE)

###Shiny App

header <- dashboardHeader(title = "Covid-19 Vizualisation")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("World Map", tabName = "map", icon = icon("globe")),
    menuItem("Timeline Graph", tabName = "timeline", icon = icon("chart-line")),
    menuItem("Info", tabName = "info", icon = icon("info"))
  )
)


body <- dashboardBody(
  
  #tags$style(type = "text/css", "#map {height: calc(100vh - 20vh) !important;}"),
  
  tabItems(
    tabItem(tabName = "map",
            leafletOutput("map", width = "100%", height = "75vh"),

            absolutePanel(top = '10%', left = '20vw', height = "8vh", width =  "25vh", fixed = FALSE,
               
             textOutput("text1")
                                     
            ),             
            
            
            absolutePanel(top = '85vh', left = '20vw', height = "8vh", width =  "20vw", fixed = FALSE,
                            
                          setSliderColor("black", 1),
                            
                              sliderInput(inputId = "Day", "Pick a date:",
                                      min = min(countries$Day), max = max(countries$Day),
                                      value = max(countries$Day), step = 1,
                                      animate =
                                        animationOptions(interval = 1000, loop = FALSE))
                        
                          
            ),  
            
                          
                          
                                      
            absolutePanel(top = '85vh', left = '45vw', height = "8vh", width =  "20vw", fixed = FALSE,                        
                            selectInput(inputId = "Info", 
                                        label = "Pick a variable to display",
                                        choices = list("Absolute numbers" = list(
                                                    "Cases" = "Cases",
                                                    "Active" = "Active", 
                                                    "Deaths" = "Deaths",
                                                    "Recovered" = "Recovered"
                                                    ),
                                              "Daily Change in Absolute Numbers" = list(
                                                    "New Cases Daily" = "New Cases Daily",
                                                    "Change in Active Cases Daily" = "Change in Active Cases Daily",
                                                    "New Deaths Daily" = "New Deaths Daily"
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
                                             "Daily Growth Rates" = list(
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
    
    tabItem(tabName = "timeline",
            
           
            plotOutput("timeline", height = "75vh"),
              
            absolutePanel(top = '85vh', left = '20vw', height = "8vh", width =  "20vw", fixed = FALSE,
              
              selectInput(
                inputId = "Country_plot",
                label   = "Pick Countries", 
                choices = as.character(unique(countries$Country)),
                multiple = TRUE,
                #select top three countries in cases for default plot
                selected = countries[is.element(countries$Cases,sort(countries[which(countries$Day==max(countries$Day,na.rm = TRUE)),"Cases"], decreasing = TRUE)[1:3]),"Country"]
              )),
              
            absolutePanel(top = '85vh', left = '45vw', height = "8vh", width = "15vw", fixed = FALSE,  
              
              selectInput(
                inputId = "Variable_plot",
                label   = "Pick Variable", 
                choices = list("Absolute numbers" = list(
                  "Cases" = "Cases",
                  "Active" = "Active", 
                  "Deaths" = "Deaths",
                  "Recovered" = "Recovered"
                ),
                "Daily Change in Absolute Numbers" = list(
                  "New Cases Daily" = "New Cases Daily",
                  "Change in Active Cases Daily" = "Change in Active Cases Daily",
                  "New Deaths Daily" = "New Deaths Daily"
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
                "Daily Growth Rates" = list(
                  "Daily Growth Rate of Cases"  = "Daily Growth Rate of Cases" ,
                  "Daily Growth Rate of Active Cases" = "Daily Growth Rate of Active Cases",
                  "Daily Growth Rate of Deaths"  = "Daily Growth Rate of Deaths"
                )),
                multiple = FALSE,
                selected = "Cases",
                selectize = FALSE,
                size = 1
              )),
            
            absolutePanel(top = '85vh', left = '65vw', height = "8vh", width =  "20vw", fixed = FALSE,
                          
                          dateRangeInput(inputId = "daterange", 
                                         label = "Pick Start and End Dates:", 
                                         start = "2020-02-01",
                                         end = max(countries$Day), 
                                         min = min(countries$Day),
                                         max = max(countries$Day), 
                                         format = "yyyy-mm-dd", 
                                         startview = "month",
                                         weekstart = 1,
                                         language = "en", 
                                         separator = " - ", 
                                         width = NULL)
            ),  
            

            
            ),
    
    tabItem(tabName = "info",
            
            titlePanel("Info"),
            mainPanel(
              HTML(paste0("<b>Data source: </b> <a href='https://github.com/CSSEGISandData/COVID-19' > Coronavirus COVID-19 Global Cases by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU) </a>  </br> </br>
                    <b>Last updated: </b>",textOutput("timestamp"),"</br>
                    <b>Country flags from: </b> <a href='https://github.com/stefangabos/world_countries' > https://github.com/stefangabos/world_countries </a> </br> </br>
                    <b>Source code: </b> <a href='https://github.com/captaincaracho/CoronaVIZ'> https://github.com/captaincaracho/CoronaVIZ </a> ")
            
            ))
  ))
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
  
  output$text1 <- renderText({paste("Date: ", input$Day)})

  observe({
      map <- joinCountryData2Map(selected(), joinCode = "ISO3",nameJoinColumn = "ISO3", verbose = TRUE)
      
      
      #Colorcode
      map$Color <- switch(input$Info, 
                     "Cases" = map$Cases_color,
                     "Active" = map$Active_color,
                     "Deaths" = map$Deaths_color,
                     "Recovered" = map$Recovered_color,
                     
                     "New Cases Daily" = map$Delta_Cases_color,
                     "Change in Active Cases Daily" = map$Delta_Active_color,
                     "New Deaths Daily" = map$Delta_Deaths_color,
                     
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
                         
                         "New Cases Daily" = map$Delta_Cases,
                         "Change in Active Cases Daily" = map$Delta_Active,
                         "New Deaths Daily" = map$Delta_Deaths,
                         
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
      
      #Number that shows up in legend
      map_legend <- switch(input$Info, 
                         "Cases" = legend_Cases,
                         "Active" = legend_Active,
                         "Deaths" = legend_Deaths,
                         "Recovered" = legend_Recovered,
                         
                         "New Cases Daily" = legend_Delta_Cases,
                         "Change in Active Cases Daily" = legend_Delta_Active,
                         "New Deaths Daily" = legend_Delta_Deaths,
                         
                         "Cases per Million" = legend_CpC,
                         "Active Cases per Million" = legend_ApC,
                         "Deaths per Million" = legend_DpC,
                         "Recovered Cases per Million" = legend_RpC,
                         
                         "Death ratio" = legend_D2C,
                         "Active ratio" = legend_A2C,
                         "Death ratio of Outcomes" = legend_D2O,
                         
                         "Daily Growth Rate of Cases"  = legend_CdG,
                         "Daily Growth Rate of Active Cases" = legend_AdG,
                         "Daily Growth Rate of Deaths"  = legend_DdG
      )
      
      
      
      #Explaining text next to number
      map_title <- switch(input$Info, 
                         "Cases" = "Cases",
                         "Active" = "Active",
                         "Deaths" = "Deaths",
                         "Recovered" = "Recovered",
                         
                         "New Cases Daily" = "New Cases Daily",
                         "Change in Active Cases Daily" = "New Active Cases Daily",
                         "New Deaths Daily" = "New Deaths Daily",
                         
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
                         
                         "New Cases Daily" = "",
                         "Change in Active Cases Daily" = "",
                         "New Deaths Daily" = "",
                         
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
        #clearControls() %>%
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
                      label = paste0(map$Country,": ", ifelse(is.na(map$Info),"No data or not not enough cases", paste0(format(round(map$Info,2), big.mark =","), measure))),
                    popup = paste0("<head>
                                     <style>
                                     table, th, td {
                                       border: 1 px grey;
                                       border-collapse: collapse;
                                     }
                                   th, td {
                                     padding: 5px;
                                   }
                                   </style>
                                     </head>",
                      
                                   "<h3> <img src='https://raw.githubusercontent.com/captaincaracho/world_countries/master/flags/32x32/",tolower(map$ISO2),".png', width = 35 >     <b>",map$Country,"</b> </h3>",
                                   
                                   "<table>
                                     <tr align = 'left'>
                                     <th>",map$Day,"</th>
                                     <th> Absolute </th>
                                     <th> Daily Change  </th>
                                     <th> Daily Growth </th>
                                     <th> per Million </th>
                                     </tr> <tr align= 'right'>
                                     <td>Cases</td>
                                     <td>",ifelse(is.na(map$Cases),"-", format(map$Cases, big.mark =",", nsmall = 0)),"</td>
                                     <td>",ifelse(is.na(map$Delta_Cases),"-", format(map$Delta_Cases, big.mark =",", nsmall = 0)),"</td>
                                     <td>",ifelse(is.na(map$CdG),"-", format(round(map$CdG,2), big.mark =",")),"% </td>
                                     <td>",ifelse(is.na(map$CpC),"-", format(round(map$CpC,2), big.mark =",")),"</td>
                                     </tr> <tr align= 'right'>
                                     <td>Deaths </td>
                                     <td>", ifelse(is.na(map$Deaths),"-", format(map$Deaths, big.mark =",", nsmall = 0)),"</td>
                                     <td>", ifelse(is.na(map$Delta_Deaths),"-", format(map$Delta_Deaths, big.mark =",", nsmall = 0)),"</td>
                                     <td>", ifelse(is.na(map$DdG),"-", format(round(map$DdG,2), big.mark =",")),"% </td>
                                      <td>",ifelse(is.na(map$DpC),"-", format(round(map$DpC,2), big.mark =",")),"</td>
                                     </tr> <tr  align= 'right'>
                                     <td>Active Cases</td>
                                     <td>",ifelse(is.na(map$Active),"-", format(map$Active, big.mark =",", nsmall = 0)),"</td>
                                     <td>",ifelse(is.na(map$Delta_Active),"-", format(map$Delta_Active, big.mark =",", nsmall = 0)),"</td>
                                     <td>",ifelse(is.na(map$AdG),"-", format(round(map$AdG,2), big.mark =",")),"% </td>
                                     <td>",ifelse(is.na(map$ApC),"-", format(round(map$ApC,2), big.mark =",")),"</td>
                                     </tr>
                                    </table>"
                                   ),
                    
                    popupOptions = popupOptions(closeOnClick = TRUE, closeButton = FALSE, autoPan = TRUE, minWidth = 400)
                                  
                                  ) %>%
        addLegend("topright", colors = unique(map_legend$color), labels = map_legend$label,
                        title = map_title, layerId = 1,
                        opacity = 1
        )
      
      
      #Timeline
      selected_plot <- reactive({
          countries %>% 
            filter(Country %in% input$Country_plot) %>% 
            filter(Day %in% (input$daterange[1]:input$daterange[2])) %>% 
            arrange(desc(Country))
      })
      
      selected_colors <- reactive({
        countries %>% 
          filter(Country %in% input$Country_plot) %>% 
          filter(Day == input$daterange[2]) %>%
          arrange(desc(Country))
      })
      
      
      
      plotvar   <- switch(input$Variable_plot, 
                         "Cases" = "Cases",
                         "Active" = "Active",
                         "Deaths" = "Deaths",
                         "Recovered" = "Recovered",
                         
                         "New Cases Daily" = "Delta_Cases",
                         "Change in Active Cases Daily" = "Delta_Active",
                         "New Deaths Daily" = "Delta_Deaths",
                        
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
       
        ggplot(data = selected_plot(), aes(x=Day, y=!!as.name(plotvar)))+
           geom_line(size=1.5, aes(color=Country))+
           geom_point(size=3, aes(fill=Country), shape = 21)+
           geom_dl(aes(label=Country), method= list( dl.trans(x = x+0.2), "last.bumpup", cex= 1))+
           coord_cartesian(clip = 'off') +
          
           labs(x = "Date",y = input$Variable_plot)+
           
           scale_fill_manual(values = selected_colors()[order(selected_colors()$Country),"color_2"])+ #sort in same order as selected data frame
           scale_color_manual(values = selected_colors()[order(selected_colors()$Country),"color_1"])+ #scale_color_manual(values = selected_plot()[order(selected_plot()$Country), ][selected_plot()$Day == input$End_Day,"color1"])+
           
          
           scale_x_date(labels = date_format("%m-%d"), date_breaks = "1 week")+
           scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ","))+
           theme(axis.text = element_text(size = 12),
                 axis.title = element_text(size=14),
                 legend.title = element_text(size=14),
                 legend.text = element_text(size=12),
                 panel.background = element_rect(fill="white"),
                 panel.grid.major = element_line(colour = "grey"))
           
        
        
      })
      
      
      output$timestamp <- renderText(paste0(time," UTC"))
      
    
    })

  
}

shinyApp(ui, server)