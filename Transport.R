library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(readr)

ui <- dashboardPage( skin = "yellow",
  dashboardHeader(title = "Ajman transport"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Main", tabName = "main", icon = icon("dashboard")),
      menuItem("Bus", tabName = "bus", icon = icon("bus")),
      menuItem("Taxi", tabName = "taxi", icon = icon("taxi")),
      menuItem("Maritime", tabName = "maritime", icon = icon("ship")),
      menuItem("About", tabName = "about", icon = icon("question"))
      )
  ),
  dashboardBody(
    tabItems(
      # first tab
      tabItem(tabName = "main",
              box(width = 5, 
                  h2("Introduction:"),
                  p("This web app presents a summary of the open data from
                the Transport Authority at Ajman Government. It is dedicated
                to be submitted to Ajman Data Visulaization Challenge. The data from different sectors are
                    presented including: bus, taxi, and maritime transport. Please select the fields from the menu
                    in the left.", style = "font-size:20px")),
              box( width = 7, 
                   h4("Total permts for transport vehicles and buses in 2019 and first half of 2020"),
                   plotlyOutput("total_permits_plot"))),
      # Second tab 
      tabItem(tabName = "bus",
              fluidRow(
              valueBox(84, "Stations", icon = icon("bus"), width = 2)),
              fluidRow(box(width = 5, 
                           h3("Bus fees"),
                           
                           plotlyOutput("bus_fees_plot")
              ),
              box(width = 7,
                  h3("Total bus passengers"),
                  plotlyOutput("bus_passengers_plot"))
              )),
                
      # Third tab
      tabItem(tabName = "taxi",
              fluidRow( box(width = 12, height = 140, 
                            h3("Taxi types"),
                            box("Regular",style = "font-size: 130%", background = "yellow", width = 2),
                            box("Mahra",style = "font-size: 130%", background = "fuchsia", width = 2),
                            box("People of determination",style = "font-size: 130%", background = "aqua", width = 3),
                            box("Hybrid",style = "font-size: 130%", background = "green", width = 2),
                            box("Limousine",style = "font-size: 130%", background = "navy", width = 2)
                            )
                
              ),
              fluidRow(
                box(width = 4, height = 400,
                    h3("Taxi requests"),
                    selectInput("taxi_req", "Select Taxi type:", choices = c(
                      "Regular taxi" = "reg",
                      "Mahra taxi" = "mahra",
                      "People of determination" = "pd",
                      "Limousine" = "limo"
                    )),
                    plotlyOutput("taxi_requests_plot")
                    ),
                box(width = 4, height = 400,
                    h3("Taxi trips"),
                    selectInput("taxi_tr", "Select Taxi type:", 
          choices = c("Regular taxi" = "reg",
                      "Mahra taxi" = "mahra",
                      "People of determination" = "pd"
                    ),
               ),
          plotlyOutput("taxi_trips_plot")),
          box(width = 4, height = 400,
              h3("Hybrid taxis"),
              plotlyOutput("hybrid_taxi_plot"))),
          ),
      # Fourth tab content
      tabItem(tabName = "maritime",
              fluidRow(
                infoBox(width = 3, "4 maritime stations", 
                        HTML(paste("Al-Zawra", "Al-Safia", "Al-Rashidiya", "Al-Marina", sep="<br/>")), 
                        icon = icon("ship")),
                valueBox(2, "AED/person for between stations trip", icon = icon("coins"), width = 3)
                
              ),
              fluidRow(
               
                box( height = 380, h4("Abra reservation fees for 1 person or a group"),
                  plotlyOutput("marine_fees_plot")),
                
                box(height = 380, h4("Total abra passengers"),
                  plotlyOutput("marine_passengers_plot"))
              )),
      # Fifth tab
      tabItem(tabName = "about",
              fluidRow(
                box(width = 8, h2("The app"),
                    p("This web app is a user friendly interface for data visualization. 
                It is based on Shiny dashboard framework for R statistics software (R core team, 2020).
                The packages that were utilized are listed in the following table: ", style = "font-size:20px"),
                    img(src = "Reference table.png", height = 170, width = 520)),
                box(width = 4, h3("The author"),
                    p("The web app was designed by Muna Alhammadi, an MSc graduate from Biotechnology program at University of Sharjah.", style = "font-size:20px"),
                    p("Created on 30-11-2020", style = "font-size:20px")
                    
                ),
                
                infoBox(tags$a(href="https://github.com/Muna59/Ajman_transport_data", 
                               "Project in Github"), icon = icon("github"), width = 4)
              ),
              fluidRow(
                box(width = 12, h3("References:"),
                    p("Chang, W., & Borges Ribeiro, B. (2018). shinydashboard: Create Dashboards with 'Shiny'. R package
  version 0.7.1. https://CRAN.R-project.org/package=shinydashboard"),
                    p("Chang, W., Cheng, J., Allaire, JJ., Xie, Y., & McPherson, J. (2020). shiny: Web Application
  Framework for R. R package version 1.5.0. https://CRAN.R-project.org/package=shiny"),
                    p("R Core Team (2020). R: A language and environment for statistical computing. R Foundation for Statistical
  Computing, Vienna, Austria. URL https://www.R-project.org/."),
                    p("Sievert, C. (2020). Interactive Web-Based Data Visualization with R, plotly, and shiny. Chapman and Hall/CRC Florida. https://plotly-r.com"),
                    p("Wickham, H. (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York. https://ggplot2.tidyverse.org"),
                    p("Wickham, H,, Hester, J., & Francois, R. (2018). readr: Read Rectangular Text Data. R package version
  1.3.1. https://CRAN.R-project.org/package=readr"),
                    p("Wickham, H., Francois, R., Henry, L., & Muller, K. (2020). dplyr: A Grammar of Data
  Manipulation. R package version 1.0.1. https://CRAN.R-project.org/package=dplyr"))
              )
              )
              
    )))


server <- function(input, output) { 
  # total permits plot
  output$total_permits_plot <- renderPlotly({
    permits_data <- read.csv("total permits.csv")
    permits_data <- permits_data %>% mutate(percent = Total/sum(Total) * 100)
    
    fig <- plot_ly(permits_data, labels = ~Permits, values = ~percent, type = 'pie')
    fig <- fig %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig <- fig %>% add_trace(hoverinfo = 'label')
    fig
    
  })
  # bus fees plot
  output$bus_fees_plot <- renderPlotly({
    bus_fees_data <- read.csv("Bus_Fees.csv")
    
    bus_fees_data$Line <- as.factor(bus_fees_data$Line)
    bus_fees <- bus_fees_data %>%
      ggplot( aes(x=reorder(Line, -Price), y=Price, label = Price)) +
      geom_segment( aes(xend=Line, yend=0)) +
      geom_point( size=4, color="orange") +
      coord_flip() +
      theme_bw() +
      ylab("Massar card price (AED)")+
      xlab(" ")
      ggplotly(bus_fees, tooltip = c("label"), height = 380)
  })
  
  # Bus_passengers plot 
  output$bus_passengers_plot <- renderPlotly({
    bus_passenger_data <- read.csv("Bus_passenger_trips.csv")
    bus_passenger_data$Month <- as.factor(bus_passenger_data$Month)
    b_passenger_plot <- bus_passenger_data %>% ggplot(aes(x = reorder(Month, Num), y = Passengers, group = 1, label = Passengers))+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 45))+
      geom_path(color = "orange")+
      geom_point(color = "#3DE9FF")+
      xlab("Month_Year")
    ggplotly(b_passenger_plot, tooltip = c("label"), height = 380)
  })
  
  
  
  # taxi requests plot
  taxi_req_plot <- reactive({
    
    taxi_requests_data <- read_csv("taxi_requests_nums.csv")
    taxi_requests_data$Interval <- as.factor(taxi_requests_data$Interval)
    
    taxi_req_L1 <- if(input$taxi_req == "reg"){
      taxi_requests_data %>% ggplot(aes(x = reorder(Interval, num), 
                                        y = `Regular taxi`, 
                                        group = 1, 
                                        label = `Regular taxi`))}
    else if(input$taxi_req == "mahra"){
      taxi_requests_data %>% ggplot(aes(x = reorder(Interval, num), 
                                        y = `Mahra taxi`, 
                                        group = 1, 
                                        label = `Mahra taxi`))}
    else if (input$taxi_req == "pd"){
      taxi_requests_data %>% ggplot(aes(x = reorder(Interval, num), 
                                        y = `People of determination taxi`, 
                                        group = 1, 
                                        label = `People of determination taxi`))}
    else if (input$taxi_req == "limo"){
      taxi_requests_data %>% ggplot(aes(x = reorder(Interval, num), 
                                        y = `Limousines vehicle`, 
                                        group = 1, 
                                        label = `Limousines vehicle`))} 
    
    
    taxi_req_L1+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 45))+
      geom_path(color = "orange")+
      geom_point(color = "#3DE9FF")+
      xlab("Interval")+
      ylab("Total requests")
  })
    
  output$taxi_requests_plot <- renderPlotly({
      ggplotly(taxi_req_plot(), tooltip = c("label"), height = 220)
    })
  

 
  # taxi trips plot
   taxi_t_plot <- reactive({
    taxi_trips_data <- read_csv("taxi_trips_nums.csv")
    taxi_trips_data$Interval <- as.factor(taxi_trips_data$Interval)
    
    taxi_t_L1 <- if(input$taxi_tr == "reg"){
      taxi_trips_data %>% ggplot(aes(x = reorder(Interval, num), 
                                        y = `regular`, 
                                        group = 1, 
                                        label = `regular`))}
    else if(input$taxi_tr == "mahra"){
      taxi_trips_data %>% ggplot(aes(x = reorder(Interval, num), 
                                        y = `mahra`, 
                                        group = 1, 
                                        label = `mahra`))}
    else if (input$taxi_tr == "pd"){
      taxi_trips_data %>% ggplot(aes(x = reorder(Interval, num), 
                                        y = `special needs`, 
                                        group = 1, 
                                        label = `special needs`))}
   
    
    taxi_t_L1+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 45))+
      geom_path(color = "orange")+
      geom_point(color = "#3DE9FF")+
      xlab("Interval")+
      ylab("Total trips")
  })
  
  output$taxi_trips_plot <- renderPlotly({
    ggplotly(taxi_t_plot(), tooltip = c("label"), height = 220)
  })
  
  
  
  # hybrid taxis plot
  output$hybrid_taxi_plot <- renderPlotly({
    hybrid_data <- read.csv("hybrid_taxi_nums.csv")
    hybrid_data$Interval <- as.factor(hybrid_data$Interval)
    hybrid_plot <- hybrid_data %>% ggplot(aes(x = reorder(Interval, num), y = Hybrid, group = 1, label = Hybrid))+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 45))+
      geom_path(color = "orange")+
      geom_point(color = "#3DE9FF")+
      xlab("Interval")+
      ylab("Total hybrid taxis")
    ggplotly(hybrid_plot, tooltip = c("label"), height = 300)
  })
  
  # marine fees plot
    output$marine_fees_plot <- renderPlotly({
    marine_tickets_price <- read.csv("marine_trip_Fees.csv")
    
    marine_tickets_price$duration <- as.factor(marine_tickets_price$duration)
    
    marine_fees <- ggplot(marine_tickets_price, aes(x=duration, y=fees)) +
      geom_segment( aes(x=duration, xend=duration, y=0, yend=fees), color="grey") +
      geom_point( color="orange", size=4) +
      theme_light() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      xlab("Trip duration (min)") +
      ylab("Ticket fees (AED)") 
      ggplotly(marine_fees, height = 320)
  })
  
  # Abra passengers plot
  
   output$marine_passengers_plot <- renderPlotly({
    mpassengers <- read.csv("abra.csv")
    mpassengers$Interval <- as.factor(mpassengers$Interval)
    marine_p_plot <- mpassengers %>% ggplot(aes(x = reorder(Interval, num), y = Abra_Passengers, group = 1, label = Abra_Passengers))+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 45))+
      geom_path(color = "orange")+
      geom_point(color = "#3DE9FF")+
      xlab("Interval")+
      ylab("Total abra passengers")
    ggplotly(marine_p_plot, tooltip = c("label"), height = 320)
  })
    
  }

shinyApp(ui, server)