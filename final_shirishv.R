# importing required libraries

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(tools)
library(stringr)
library(randomcoloR)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(shinyjs)
library(rgeos)


# loading 911 dataset
ems <- read.csv("911_ems_dispatches.csv")
ems$city_name <- str_to_title(ems$city_name)

# To avoid plotly issues ----------------------------------------------
pdf(NULL)

# Define UI for application
ui <- navbarPage("Allegheny County 911 EMS Dashboard",
                 theme = shinytheme("flatly"),
                 header = tagList(
                   useShinydashboard()
                 ),
                 tabPanel("Overview",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                              h4(strong("User Inputs")),
                              hr(style = "border-top: 1px solid #000000;"),
                              # Select Priority Type
                              selectInput(inputId = "priority",
                                          label = "Select priority type for Plot 1:",
                                          choices = unique(sort(ems$priority)),
                                          selected = c("E0")),
                              
                              h6("*check glossary for the meanings of priority codes"),

                              # Select city for overtime trend
                              selectInput(inputId = "city",
                                          label = "Select city name for Plot 2:",
                                          choices = unique(sort(ems$city_name)),
                                          selected = c("Pittsburgh")),
                              
                              hr(style = "border-top: 1px solid #000000;"),
                              # Input for lower and upper value of years
                              sliderInput(inputId = "slider", 
                                          label = "Select year range for both plots:", 
                                          min = min(ems$call_year), 
                                          max = max(ems$call_year),
                                          step = 1,
                                          value = c(min(ems$call_year),max(ems$call_year)))
                              
                            ),
                            # Plots Panel
                            mainPanel(width = 9,
                              # Value Boxes ----------------------------------------------
                              fluidRow(valueBoxOutput("total_calls"),
                                       valueBoxOutput("high_priority_calls"),
                                       valueBoxOutput("case_types")
                              ),
                              br(),
                              tabsetPanel(tabPanel("Plot 1: Count of calls by priority",
                                                   tabsetPanel(tabPanel("Plot", plotlyOutput("barplot")),
                                                               tabPanel("Data table", DT::dataTableOutput("agg.table"),
                                                                        downloadButton("download_dt_1", "Download data")))),
                                          tabPanel("Plot 2: Overtime trend in the selected city",
                                                   tabsetPanel(tabPanel("Plot", plotlyOutput("lineplot")),
                                                               tabPanel("Data table", DT::dataTableOutput("city.table"),
                                                                        downloadButton("download_dt_2", "Download data")))),
                                          tabPanel("Glossary", verbatimTextOutput("glossary"))
                                          )
                            )
                          )
                 ),
                 # Map Panel
                 tabPanel("Map View",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                               h4(strong("User Inputs")),
                               hr(style = "border-top: 1px solid #000000;"),
                               
                               # Type of map
                               radioButtons("maptype",
                                            "Select type of map:",
                                            choices = c("Marker", "Heatmap"),
                                            selected = "Marker"),
                               # Select year
                               selectInput("year_multi",
                                           "Year(s):",
                                           choices = unique(sort(ems$call_year)),
                                           selected = c("2020"),
                                           selectize = T,
                                           multiple = T),
                               
                               # Select the Priority type to highlight in the map
                               checkboxGroupInput(inputId = "priorities",
                                                  label = "Select priority(ies):",
                                                  choices = unique(sort(ems$priority))),
                               actionLink("selectall", "Select all priorities")
                                         ),
                            mainPanel(
                              # Map Page
                              leafletOutput("leaflet", height = "80vh")
                            )
                            )
                          ),
                 # Data Table Pannel
                 tabPanel("Raw Data",
                          fluidPage(
                            wellPanel(DT::dataTableOutput("raw.table"))
                          )
                 )
)

# Defining server function required to create plots and value boxes -----
server <- function(input, output, session) {
  
  # Making three valueboxes
  output$total_calls <- renderValueBox({
    num <- nrow(ems)
    num <- format(num, big.mark=",",small.mark=".", small.interval=3)
    
    valueBox(subtitle = "Total Calls in 6 years", value = num, icon = icon("phone-square"), color = "blue")
  })
  
  output$high_priority_calls <- renderValueBox({
    new_table <- table(ems$priority)
    num <- new_table[['E0']]
    num <- format(num, big.mark=",",small.mark=".", small.interval=3)
    
    valueBox(subtitle = "Calls requiring E0 priority", value = num, icon = icon("ambulance"), color = "blue")
  })
  
  output$case_types <- renderValueBox({
    num <- length(unique(ems$description_short))
    
    valueBox(subtitle = "Unique health reasons", value = num, icon = icon("file-medical"), color = "blue")
  })
  
  # Aggregating the dataset based on the selected inputs in sidebar
  emsagg <- reactive({
    req(input$priority, input$slider)
    ems.agg <- ems %>%
      group_by(call_year, call_quarter, priority) %>%
      summarise(values = n())
    
    agg.sub <- subset(ems.agg, priority == input$priority & call_year >= input$slider[1] &
                        call_year <= input$slider[2])
    
    return(agg.sub)
  })
  
  # Aggregated data table
  output$agg.table <- DT::renderDataTable({emsagg()})
  
  # Download the data in data table
  output$download_dt_1 <- downloadHandler(
    filename = function(){"count_of_calls_by_prority_per_quarter.csv"},
    content = function(fname){
      write.csv(emsagg(), fname)
    }
  )
  
  # Making a bar plot using the above aggregated data
  output$barplot <- renderPlotly({
    ggplot(emsagg(), aes(x = call_year, y = values, fill = call_quarter)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_bw() +
      scale_x_continuous(breaks = seq(input$slider[1],input$slider[2],1),
                          limits = c(input$slider[1],input$slider[2])) +
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual("Quarters", values = c("#fdbf60", "#f07d60", "#d2449a", "#78347c")) +
      labs(x = "Years", y = "Number of calls") +
      theme(axis.title.x = element_text(face = "bold"), 
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            legend.background = element_rect(fill = "lightgray"))
  })
  
  # Getting city wise data based on the selected input
  ems_city <- reactive({
    req(input$city, input$slider)
    city.data <- ems %>%
      group_by(city_name, call_year, priority) %>%
      summarise(values = n())
    
    city.sub <- subset(city.data, city_name == input$city & call_year >= input$slider[1] &
                         call_year <= input$slider[2])
    
    return(city.sub)
  })
  
  # City data table
  output$city.table <- DT::renderDataTable({ems_city()})
  
  # Download the data in data table
  output$download_dt_2 <- downloadHandler(
    filename = function(){"count_of_calls_by_city_per_year.csv"},
    content = function(fname){
      write.csv(emsagg(), fname)
    }
  )
  
  # Making a line plot using the above data
  output$lineplot <- renderPlotly({
    ggplot(ems_city(), aes(x = call_year, y = values, color = priority)) +
      geom_line() +
      theme_bw() +
      scale_x_continuous(breaks = seq(input$slider[1],input$slider[2],1), 
                         limits = c(input$slider[1],input$slider[2])) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Years", y = "Number of calls", color = "Priorities") +
      theme(axis.title.x = element_text(face = "bold"), 
            axis.title.y = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            legend.background = element_rect(fill = "lightgray"))
  })
  
  # Glossary
  output$glossary <- renderText({paste0("Following are the meanings of the priority type codes:",
                                        "\n\n2A: 2nd Alarm Fire  (residential, commercial, tunnel)",
                                        "\nE0: EMS Advanced Life Support life threatening response with Advanced Life Support backup",
                                        "\nE1: EMS Advanced Life Support life threatening response",
                                        "\nE2: EMS Standard Advanced Life Support response",
                                        "\nE3: EMS Standard Basic Life Support response",
                                        "\nE4: EMS Basic Life Support Assistance response (i.e. lift assist)",
                                        "\nE5: All administrative mark outs",
                                        "\nF1: Potential to become a threat to life safety.",
                                        "\nF2: Incidents actively occurring or that has just occurred with no immediate threat to life.",
                                        "\nF3: No threat to life safety.",
                                        "\nF4: All notification only calls types.",
                                        "\nHR: High-rise plan: 1 alarm fire that automatically goes to 3 alarms for very tall buildings",
                                        "\nM1: EMS Mass Casualty Plan - Level 1 (lowest)",
                                        "\nM2: EMS Mass Casualty Plan - Level 2",
                                        "\nM3: EMS Mass Casualty Plan - Level 3",
                                        "\nM4: EMS Mass Casualty Plan - Level 4 (highest response, most ambulances)",
                                        "\nP1: Police reponse; likely not dispatched as EMS/Fire, just recorded in CAD system")})
  
  # if-else function to check all priority codes
  observe({
    if(input$selectall == 0) return(NULL) 
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session,
                               "priorities",
                               "Select prioritiy(ies):",
                               choices = unique(sort(ems$priority)))
    }
    else
    {
      updateCheckboxGroupInput(session,"priorities",
                               "Select prioritiy(ies):",
                               choices = unique(sort(ems$priority)),
                               selected = unique(sort(ems$priority)))
    }
  })
  
  # creating subset for the map
  mapinput <- reactive({
    # req(input$year_multi, input$priorities)
    # Selected years
    if (length(input$year_multi) > 0) {
      map_filter <- subset(ems, call_year %in% input$year_multi)
    }
    else {
      map_filter <- ems[FALSE,]  
    }
    # Priority types
    if (length(input$priorities) > 0) {
      map_filter <- subset(map_filter, priority %in% input$priorities)
    }
    else{
      map_filter <- ems[FALSE,]
    }
    
    map_filter <- map_filter[complete.cases(map_filter), ]

    return(map_filter)
  })
  
  
  # Map
  output$leaflet <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", 
               attribution = "Google", group = "Google") %>%
      addProviderTiles("Stamen.Toner", group = "Toner") %>%
      setView(-79.9800, 40.4700, 10) %>%
      addLayersControl(baseGroups = c("Google", "Toner"))
  })
  
  pal <- colorFactor(
    palette = distinctColorPalette(20),
    domain = ems$priority)
  
  # Replace layer with filtered data
  observe({
    map_data <- mapinput()
    if(input$maptype == "Marker"){
      leafletProxy("leaflet", data = map_data) %>%
        clearMarkers() %>%
        clearHeatmap() %>%
        clearControls() %>%
        clearGroup(group = "map_data") %>%
        addCircleMarkers(lng = ~census_block_group_center__x,
                         lat = ~census_block_group_center__y, radius = 2,
                         popup = ~paste0("<b>", city_name, " (", call_year, ")", "</b>: ", priority),
                         color = ~pal(priority), group = "map_data",
                         clusterOptions = markerClusterOptions()) %>%
        addLegend(position = "bottomright" , pal = pal, values = ems$priority, title = "Priority type")
    }
    else {
      leafletProxy("leaflet", data = map_data) %>%
        clearMarkers() %>%
        clearHeatmap() %>%
        clearControls() %>%
        clearGroup(group = "map_data") %>%
        addHeatmap(lng = ~census_block_group_center__x,
                   lat = ~census_block_group_center__y, radius = 8,
                   group = "map_data")
    }
  })
  
  
  # Raw Data table ----------------------------------------------
  output$raw.table <- DT::renderDataTable({ems[2:13]}, options = list(scrollX = TRUE))
  
}

# Running the application ----------------------------------------------
shinyApp(ui = ui, server = server)