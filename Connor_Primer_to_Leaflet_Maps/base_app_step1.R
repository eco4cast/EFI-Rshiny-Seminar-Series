
library(shiny)
library(leaflet)
library(raster)
library(leaflet.extras)
library(shinyWidgets)

setwd("path_to_file")

presence_data <- shapefile("elk_dat_jittered.shp") ## Add second

ui <- fluidPage(
  
  # Application title
  titlePanel("Map for exploring"),
  
  sidebarLayout( ## adding sidebar panel in page layout for picking data
  sidebarPanel(pickerInput(inputId = 'data_source', label = 'Select a data source', ## Creating the menu
                             choices = unique(presence_data$dat_src), ## Specifying possible choices
                             selected = 'Camera',multiple = T)), # Default to camera points, allow multiple to be chosen

    
  mainPanel(leafletOutput("map")) ## Basic map for user to explore
  ) # Close sidebarLayout
) ## close ui


server <- function(input, output) {
  
  ### Make reactive data by ID (from selection in sidebar)
  pres.dat.sel <- reactive({ ## open reactive expression
    data.subset <- presence_data[presence_data$dat_src == input$data_source, ]  #filters by data source
    return(data.subset) ## Return the subsetted data
  })
  
  output$map <- renderLeaflet({ ## begin rendering leaflet and store as 'map' in server output
    leaflet() %>% addProviderTiles(providers$Esri.NatGeoWorldMap)  %>% ## Add basemap
      addCircleMarkers(data = pres.dat.sel(), color = ~color) %>% ## add circle markers with color
      addScaleBar() %>%  #add dynamic scale bar
      addLegend(colors = c("Green","Blue"), labels = c("Camera","Scat"), ## Add manual legend
                title = "Data Source") ## Title it
      
  }) ## close map
  
} ## close server

shinyApp(ui = ui, server = server) ## Run the app locally

