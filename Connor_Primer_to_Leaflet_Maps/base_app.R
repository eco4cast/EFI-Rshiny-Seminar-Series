
library(shiny)
library(leaflet)
library(raster)
library(leaflet.extras)
library(shinyWidgets)

ui <- fluidPage( # Fluid page a common, nice looking layout for shiny apps
  
  # Application title
  titlePanel("Map for exploring"), 

  ## Main page
  mainPanel(leafletOutput("map")) # Basic map for user to explore
  
) ## close ui


server <- function(input, output) {

  output$map <- renderLeaflet({ ## begin rendering leaflet and store as 'map' in server output
    leaflet() %>% addProviderTiles(providers$OpenStreetMap) ## add basemap from provider list

    }) ## close map
  
  
} ## close server

shinyApp(ui = ui, server = server) ## Run the app locally


