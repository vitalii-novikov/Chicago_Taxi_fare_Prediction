source(file.path("./functions/install_and_load.R")) # ensure /app is you wd
# Install and load required packages
install_and_load(c(
  "tidyverse",
  "shiny", 
  "leaflet", 
  "sf", 
  "tidygraph", 
  "dplyr", 
  "osmdata", 
  "sfnetworks", 
  "units", 
  "igraph",
  "tibble" 
))

# Source the UI and server files
# The 'ui' object will be defined in ui.R
# The 'server' function will be defined in server.R
source(file.path("./ui.R"), local = TRUE) # Use local = TRUE to load ui object into current environment
source(file.path("./server.R"), local = TRUE) # Use local = TRUE to load server function

# Run the Shiny appclear
shinyApp(ui = ui, server = server)
