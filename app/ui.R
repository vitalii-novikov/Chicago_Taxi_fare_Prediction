


ui <- fluidPage(
  titlePanel("Shortest Street Path Finder"),
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: orange;
        font-weight: bold;
      }
      .shiny-notification {
        position:fixed;
        top: calc(50% - 50px);
        left: calc(50% - 150px);
        width: 300px;
      }
    "))
  ),

  
  
  
  sidebarLayout(
    
    
    
    sidebarPanel(
      width = 4,
      strong("Predicted Fare:"),
      verbatimTextOutput("predicted_fare_display"),
      hr(),
      h4("Enter Coordinates"),
      numericInput("start_lon_input", "Start Longitude:", value = -87.6234, step = 0.0001),
      numericInput("start_lat_input", "Start Latitude:", value = 41.8827, step = 0.0001),
      numericInput("end_lon_input", "End Longitude:", value = -87.636912, step = 0.0001),
      numericInput("end_lat_input", "End Latitude:", value = 41.896400, step = 0.0001),
      actionButton("calculate_path", "Calculate Shortest Path", class = "btn-primary"),
      hr(),
      h4("Selected Coordinates:"),
      strong("Start:"),
      verbatimTextOutput("start_coords_display"),
      strong("End:"),
      verbatimTextOutput("end_coords_display"),
      # --- ADD THE NEW OUTPUT ELEMENT HERE ---
      hr(), # Optional separator
      strong("Shortest Path Length:"),
      verbatimTextOutput("path_length_display"),
      
      strong("Estimated Duration (seconds):"),  # NEW LINE
      verbatimTextOutput("trip_duration_display"), # NEW OUTPUT
      # --- END OF NEW OUTPUT ELEMENT ---
    ),
    
    
  
    mainPanel(
      width = 8,
      leaflet::leafletOutput("map", height = "calc(100vh - 100px)") # Make map taller
    )
  )
)