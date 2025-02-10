library(tidyverse)
library(jsonlite)
library(data.table)
library(lubridate)
library(ggplot2)
library(shiny)
library(DT)
library(shinydashboard)


artist_song_counts <- read_csv("artist_song_counts.csv")

table_cleaned_songs <- read_csv("table_cleaned_songs.csv")


top_artist_song <- read_csv("top_artist_song.csv")


kpi_dashboard <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Spotify Analysis"),
  dashboardSidebar(
    selectInput("year", "Select Year", choices = 2015:2024, selected = 2024),
    selectInput("indicator", "Select Indicator", choices = c("Top Artists", "Top Songs"), selected = "Top Artists")
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("unique_artists"),
      valueBoxOutput("unique_songs"),
      valueBoxOutput("minutes_listened")
    ),
    fluidRow(
      plotOutput("spot_bar")
    )
  )
)

search_dashboard <- fluidPage(
  # Title of the app
  titlePanel("Top 100 Songs by Year - Interactive Table"),
  
  # Sidebar layout with a search bar and filters
  sidebarLayout(
    sidebarPanel(
      # selectInput("Artist Name", "Filter by Artist:",
      #             choices = c("All", unique(table_cleaned_songs$`Artist Name`)),
      #             selected = "All"),
      selectInput("year", "Filter by Year:",
                  choices = c("All", unique(table_cleaned_songs$year)),
                  selected = "2024")
    ),
    
    # Main panel for displaying the table
    mainPanel(
      DTOutput("song_table")
    )
  )
)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("KPI Dashboard", kpi_dashboard),
    tabPanel("Interactive Top Song Dashboard", search_dashboard)
  )
)


server <- function(input, output) {
  
  filtered_data_kpi <- reactive({
    artist_song_counts %>%
      filter(year == input$year)
  })
  
  output$unique_artists <- renderValueBox({
    song_value <- filtered_data_kpi() %>%
      filter(kpi == "master_metadata_album_artist_name") %>%
      pull(value)
    valueBox(value = song_value, subtitle = "Unique Artists Count", color = "green")
  })
  
  output$unique_songs <- renderValueBox({
    artist_value <- filtered_data_kpi() %>%
      filter(kpi == "master_metadata_track_name") %>%
      pull(value)
    valueBox(value = artist_value, subtitle = "Unique Songs Count", color = "green")
  })
  
  output$minutes_listened <- renderValueBox({
    minute_value <- filtered_data_kpi() %>%
      filter(kpi == "master_metadata_album_artist_name") %>%
      pull(minutes_per_year)
    valueBox(value = minute_value, subtitle = "Total Minutes Listened", color = "green")
  })
  # Reactive expression to filter bar chart data based on selected year
  filtered_bar_data <- reactive({
    top_artist_song %>%
      filter(year == input$year & indicator == input$indicator)
  })
  
  # Render Bar Chart
  output$spot_bar <- renderPlot({
    ggplot(filtered_bar_data(), aes(reorder(top_artist_song, top_value))) +
      geom_bar(aes(weight = top_value)) +
      coord_flip() +
      ggtitle("Top Songs by Year") +
      xlab(input$indicator) +
      ylab("Number of Times Listened") +
      theme_bw(base_size = 16)
  })

  # Reactive expression to filter the data based on the selected department
  # filtered_data <- reactive({
  #   if (input$`Artist Name` == "All") {
  #     return(table_cleaned_songs)
  #   } else {
  #     return(subset(table_cleaned_songs, `Artist Name` == input$`Artist Name`))
  #   }
  # })
  filtered_data <- reactive({
    if (input$year == "All") {
      return(table_cleaned_songs)
    } else {
      return(subset(table_cleaned_songs, year == input$year))
    }
  })
  # Render the DT table with search and filter functionality
  output$song_table <- renderDT({
    datatable(
      filtered_data(),    # Data to display
      options = list(
        pageLength = 25,   # Number of rows to show per page
        dom = 'lfrtip'    # This enables search and filtering
      )
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)


