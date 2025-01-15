cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session

# Install necessary packages
install.packages(c("shiny", "ggplot2", "dplyr", "lubridate", "DT", "ggeasy"))

library(tidyverse)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)
library(ggeasy)

# Read csv file
austin <- read.csv("listings_austin.csv")
bangkok <- read.csv("listings_bangkok.csv")
buenos_aires <- read.csv("listings_buenos_aires.csv")
cape_town <- read.csv("listings_cape_town.csv")
istanbul <- read.csv("listings_istanbul.csv")
melbourne <- read.csv("listings_melbourne.csv")

# Add the 'city' column to each dataset
austin$city <- "Austin"
bangkok$city <- "Bangkok"
buenos_aires$city <- "Buenos Aires"
cape_town$city <- "Cape Town"
istanbul$city <- "Istanbul"
melbourne$city <- "Melbourne"

# Convert neighbourhood column as character
austin$neighbourhood <- as.character(austin$neighbourhood)
bangkok$neighbourhood <- as.character(bangkok$neighbourhood)
buenos_aires$neighbourhood <- as.character(buenos_aires$neighbourhood)
cape_town$neighbourhood <- as.character(cape_town$neighbourhood)
istanbul$neighbourhood <- as.character(istanbul$neighbourhood)
melbourne$neighbourhood <- as.character(melbourne$neighbourhood)

# Combine all six individual datasets in one Airbnb dataset
air_bnb <- bind_rows(austin, bangkok, buenos_aires, cape_town, istanbul, melbourne)
air_bnb

#write_xlsx(air_bnb, "airbnb.xlsx")

str(air_bnb)

# Ensure the date column is in Date format
air_bnb$date <- as.Date(air_bnb$last_review, format = "%Y-%m-%d")
head(air_bnb)

# UI Definition:

ui <- dashboardPage(
  dashboardHeader(title = "Airbnb Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Arial Black", Gadget, sans-serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
  
    tabItems(
        tabItem(tabName = "dashboard",
        titlePanel("Airbnb Analysis Dashboard"),
        sidebarLayout(
          sidebarPanel(
           h4("Select City"),
        selectInput("city", "City", choices = unique(air_bnb$city), selected = "Bangkok", 
                    multiple = TRUE)
    ),
    mainPanel(
      h4("Summary Data"),
      dataTableOutput("summary_table"), 
      br(),
      tabsetPanel(
        tabPanel("Average Price by City",
                 plotOutput("avg_price_plot")),
        tabPanel("Total Reviews by Average Price",
                 plotOutput("scatter_plot")),
        tabPanel("Average Ratings per City",
                 plotOutput("bubble_chart")),
        tabPanel("Average Price per City by Room Type",
                 plotOutput("stacked_bar_plot")),
        tabPanel("Room Type Distribution by City",
                 plotOutput("room_type_dist_plot")),
        tabPanel("Minimum Night Spend",
                 plotOutput("min_night_spend_plot")),
        tabPanel("Preferred Room Type",
                 plotOutput("preferred_room_type_plot")),
        tabPanel("Monthly Review Analysis",
                 plotOutput("monthly_review_plot"))
      )
    )
  )
))))

# Server Logic:

server <- function(input, output) {
  summary_data <- air_bnb %>%
    summarize(
      "Total Hosts" = n_distinct(host_id),
      "Total Listings" = n(),
      "All Time Guests" = sum(number_of_reviews, na.rm = TRUE),
      "Average Price USD" = mean(price, na.rm = TRUE)
    )
  
  output$summary_table <- renderDT({
    datatable(
      summary_data,
      options = list(
        dom = 't', paging = FALSE, searching = FALSE, info = FALSE,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': 'white', 'color': 'black', 'text-align': 'center'});",
          "$(this.api().table().body()).css({'background-color': 'white', 'color': 'black', 'text-align': 'center'});",
          "$(this.api().table().container()).css({'background-color': 'white'});",
          "$('body').css({'background-color': 'white'});",
          "$('table.dataTable thead th').css({'text-align': 'center'});",  # Ensuring header alignment
          "$('table.dataTable tbody td').css({'color': 'white', 'text-align': 'center'});",
          "$('table.dataTable tbody td').each(function() {",
          "if($.isNumeric($(this).text())) {",
          "$(this).css('color', 'red');",
          "}",
          "});",
          "}"
        )
      ),
      rownames = FALSE
    )
  })
  
  filtered_data <- reactive({
    air_bnb %>%
      filter(city %in% input$city & date >= as.Date("2020-11-01") & date <= as.Date("2021-10-30")) 
  })
  
  output$avg_price_plot <- renderPlot({
    
    average_prices <- filtered_data() %>%
      group_by(city, month = floor_date(date, "month")) %>%
      summarize(Average_Price = mean(price, na.rm = TRUE))
    
    
    ggplot(average_prices, aes(x = month, y = Average_Price, color = city, group = city)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_x_date(date_labels = "%b", date_breaks = "1 month") +
      labs(title = "Average Airbnb Prices by City (Nov 2020 - Nov 2021)",
           x = "Month",
           y = "Average Price (USD)",
           color = "City") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$scatter_plot <- renderPlot({
    average_price_reviews <- filtered_data() %>%
      group_by(city) %>%
      summarize(
        Average_Price = mean(price, na.rm = TRUE),
        Total_Reviews = sum(number_of_reviews, na.rm = TRUE)
      )
    
    ggplot(average_price_reviews, aes(x = Average_Price, y = Total_Reviews, color = city)) +
      geom_point(size = 4, alpha = 0.7) +
      scale_x_continuous(labels = scales::dollar) +
      labs(title = "Total Reviews by Average Price for Different Cities",
           x = "Average Price (USD)",
           y = "Total Number of Reviews",
           color = "City") +
      theme_minimal()
  })
  
  output$bubble_chart <- renderPlot({
    average_ratings <- filtered_data() %>%
      group_by(city) %>%
      summarise(average_rating = mean(reviews_per_month, na.rm = TRUE),
                total_reviews = sum(number_of_reviews, na.rm = TRUE))
    
    ggplot(average_ratings, aes(x = total_reviews, y = average_rating, size = average_rating, color = city, label = city)) +
      geom_point(alpha = 0.5) +
      scale_size_continuous(range = c(3, 15)) +
      labs(title = "Average Ratings per City",
           x = "Total Reviews",
           y = "Average Ratings",
           color = "City") +
      theme_minimal() +  
      guides(size = "none") 
  })
  
  output$stacked_bar_plot <- renderPlot({
    average_prices <- filtered_data() %>%
      group_by(city, room_type) %>%
      summarise(avg_price = mean(price, na.rm = TRUE))
    
    ggplot(average_prices, aes(x = city, y = avg_price, fill = room_type)) +
      geom_bar(stat = "identity") +
      labs(title = "Average Price per City by Room Type",
           x = "City",
           y = "Average Price",
           fill = "Room Type") +
      theme_minimal() +
      easy_rotate_x_labels(angle=45,c("right"))
  })
  
  output$room_type_dist_plot <- renderPlot({
    room_type_distribution <- filtered_data() %>%
      group_by(city, room_type) %>%
      summarize(count = n())
    
    ggplot(room_type_distribution, aes(x = city, y = count, fill = room_type)) +
      geom_bar(stat = "identity", position = "fill") +
      theme_minimal() +
      labs(title = "Room Type Distribution by City", x = "City", y = "Proportion", fill = "Room Type") +
      easy_rotate_x_labels(angle=45,c("right"))
  })
  
  output$min_night_spend_plot <- renderPlot({
    common_nights <- filtered_data() %>%
      group_by(minimum_nights, room_type) %>%
      summarise(count = n()) %>%
      filter(minimum_nights <= 30) %>%  # Adjust this value based on your data's distribution
      ungroup()
    
    ggplot(common_nights, aes(x = as.factor(minimum_nights), y = count, fill = room_type)) +
      geom_bar(stat = "identity") +
      labs(title = "Bar Graph for Minimum Night Spend",
           x = "Minimum Nights",
           y = "Count",
           fill = "Room Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_discrete(limits = c(1:10))  # Adjust this range based on your data
  })
  
  output$preferred_room_type_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = room_type)) +
      geom_bar(fill = "steelblue") +
      labs(title = "Bar Graph for Preferred Room Type",
           x = "Room Type",
           y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$monthly_review_plot <- renderPlot({
    
    monthly_reviews <- filtered_data()%>%
      group_by(month = floor_date(last_review, "month")) %>%
      summarize(Average_Reviews = mean(number_of_reviews, na.rm = TRUE))
    
    ggplot(monthly_reviews, aes(x = month, y = Average_Reviews)) +
      geom_line(size = 1, color = "blue") +
      geom_point(size = 2, color = "red") +
      scale_x_date(date_labels = "%b", date_breaks = "1 month") +
      labs(title = "Line Chart for Monthly Review Analysis",
           x = "Month",
           y = "Average Number of Reviews") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the App:
shinyApp(ui, server)

