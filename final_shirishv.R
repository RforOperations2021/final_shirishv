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

# setting working directory
# setwd("C:/Users/shiri/Dropbox/CMU - 4th sem/R Shiny/final_shirishv")

# loading 911 dataset
ems <- read.csv("911_ems_dispatches.csv")
ems$city_name <- str_to_title(ems$city_name)

# To avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application title ----------------------------------------------
# header <- dashboardHeader(title = "Allegheny County 911 EMS Dashboard", titleWidth = 400)

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
                                                                        downloadButton("download_dt_2", "Download data"))))
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
    # Selected years
    if (length(input$year_multi) > 0) {
    map_filter <- subset(ems, call_year %in% input$year_multi)
    }
    # Priority types
    if (length(input$priorities) > 0) {
      map_filter <- subset(map_filter, priority %in% input$priorities)
    }
    # randomly sampling 500 points
    if(length(map_filter) > 1000) {
      map_filter <- sample_n(map_filter,1000) 
    }
    return(map_filter)
  })
  
  
  # Map
  output$leaflet <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", 
               attribution = "Google", group = "Google") %>%
      addProviderTiles("Stamen.Toner", group = "Toner") %>%
      setView(-79.9800, 40.4700, 9) %>%
      addLayersControl(baseGroups = c("Google", "Toner"))
  })
  
  pal <- colorFactor(
    palette = distinctColorPalette(20),
    domain = ems$priority)
  
  # Replace layer with filtered data
  observe({
    map_data <- mapinput()
    leafletProxy("leaflet", data = map_data) %>%
      clearMarkers() %>%
      clearGroup(group = "map_data") %>%
      addCircleMarkers(lng = ~census_block_group_center__x,
                       lat = ~census_block_group_center__y, radius = 1.5,
                       color = ~pal(priority), group = "map_data") %>%
      addLegend(position = "bottomright" , pal = pal, values = ems$priority, title = "Priority type")
  })
  
  # Raw Data table ----------------------------------------------
  output$raw.table <- DT::renderDataTable({ems[2:13]}, options = list(scrollX = TRUE))
  
}

# Running the application ----------------------------------------------
shinyApp(ui = ui, server = server)