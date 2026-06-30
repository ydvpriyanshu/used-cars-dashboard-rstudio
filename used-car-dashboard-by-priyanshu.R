
# Author: Priyanshu Yadav
# Created: 2025-04-23
# Description: Beautifully formatted Shiny dashboard for Used Cars India dataset

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(DT)
library(scales)

# Load and clean dataset
car_data <- read_csv("C:/Users/priya/Downloads/used cars India.csv")

car_data <- car_data %>% 
  filter(!is.na(make_year), !is.na(price), !is.na(km_driven))

# Get min/max values for sliders
min_year <- min(car_data$make_year)
max_year <- max(car_data$make_year)
min_price <- min(car_data$price)
max_price <- max(car_data$price)

# UI
ui <- fluidPage(
  titlePanel("🚗 Used Cars India Dashboard"),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput("brand", "Select Brand:", choices = c("All", sort(unique(car_data$brand)))),
      selectInput("fuel", "Select Fuel Type:", choices = c("All", sort(unique(car_data$fuel_type)))),
      selectInput("trans", "Select Transmission:", choices = c("All", sort(unique(car_data$transmission)))),
      sliderInput("yearRange", "Select Year Range:", 
                  min = min_year, max = max_year, value = c(min_year, max_year)),
      sliderInput("priceRange", "Select Price Range (INR):", 
                  min = min_price, max = max_price, value = c(min_price, max_price))
    ),
    
    mainPanel(
    tabsetPanel(
    tabPanel("📊 Dashboard",
                 br(),
                 fluidRow(
                 column(4, wellPanel(
                 h4("Summary Stats"),
                 verbatimTextOutput("summary")
                 )),
                 column(8, plotOutput("priceDist", height = "250px"))
                 ),
                 br(),
                 fluidRow(
                 column(6, plotOutput("avgPriceBrand", height = "300px")),
                 column(6, plotOutput("boxPriceFuel", height = "300px"))
                ),
                 br(),
                 fluidRow(
                 column(6, plotOutput("ownershipPie", height = "300px")),
                 column(6, plotOutput("priceVsKM", height = "300px"))
                 )),
        tabPanel("📄 Data Table", DTOutput("dataTable"))
      ))))
  

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    data <- car_data
    if (input$brand != "All") data <- data %>% filter(brand == input$brand)
    if (input$fuel != "All") data <- data %>% filter(fuel_type == input$fuel)
    if (input$trans != "All") data <- data %>% filter(transmission == input$trans)
    data <- data %>%
      filter(make_year >= input$yearRange[1],
             make_year <= input$yearRange[2],
             price >= input$priceRange[1],
             price <= input$priceRange[2])
    return(data)
  })
  
  output$summary <- renderPrint({
    df <- filtered_data()
    cat("Total Cars:", nrow(df), "\n")
    cat("Average Price (INR):", round(mean(df$price, na.rm = TRUE), 0), "\n")
    cat("Average KM Driven:", round(mean(df$km_driven, na.rm = TRUE), 0), "km\n")
  })
  
  output$priceDist <- renderPlot({
    ggplot(filtered_data(), aes(x = price)) +
      geom_histogram(fill = "skyblue", bins = 30) +
      labs(title = "Price Distribution", x = "Price (INR)", y = "Count") +
      scale_x_continuous(labels = comma)
  })
  
  output$avgPriceBrand <- renderPlot({
    filtered_data() %>%
      group_by(brand) %>%
      summarise(avg_price = mean(price, na.rm = TRUE)) %>%
      top_n(10, avg_price) %>%
      ggplot(aes(x = reorder(brand, avg_price), y = avg_price)) +
      geom_col(fill = "green") +
      coord_flip() +
      labs(title = "Top 10 Brands by Avg Price", x = "Brand", y = "Avg Price (INR)") +
      scale_y_continuous(labels = comma)
  })
  
  output$boxPriceFuel <- renderPlot({
    ggplot(filtered_data(), aes(x = fuel_type, y = price, fill = fuel_type)) +
      geom_boxplot() +
      labs(title = "Price by Fuel Type", x = "Fuel Type", y = "Price (INR)") +
      scale_y_continuous(labels = comma) +
      theme(legend.position = "none")
  })
  
  output$ownershipPie <- renderPlot({
    data <- filtered_data() %>%
      count(ownership) %>%
      mutate(percent = round(100 * n / sum(n), 1))
    
    ggplot(data, aes(x = "", y = percent, fill = ownership)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y") +
      labs(title = "Ownership Distribution", x = "", y = "") +
      theme_void()
  })
  
  output$priceVsKM <- renderPlot({
    ggplot(filtered_data(), aes(x = km_driven, y = price, color = transmission)) +
      geom_point(alpha = 0.6) +
      labs(title = "Price vs. KM Driven", x = "Kilometers Driven", y = "Price (INR)") +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(labels = comma)
  })
  
  output$dataTable <- renderDT({
    datatable(filtered_data())
  })
}

# Run the app
shinyApp(ui = ui, server = server)
