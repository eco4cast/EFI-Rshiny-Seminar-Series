
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
  
  sidebarLayout( ## add second for picking data
    sidebarPanel(pickerInput('data_source', label = 'Select a data source', ## Creating the menu
                             choices = unique(presence_data$dat_src), ## Specifying possible choices
                             selected = 'Camera',multiple = T,options = list(`actions-box` = TRUE)), ## adding more menu options
                 
                 sliderInput("buff_distance", "Buffer distance", ## Add slider input and action button in step 3
                             min = 0, max = 10000, value = 400), ## Set min, max, and default values
                 actionButton("create_buffer", "Create buffer"),
                 
                 conditionalPanel(
                   condition = "output.crop",
                   actionButton("begin.crop", "Crop data to polygon")), ## Add a conditional panel with an action button for cropping
    ), ## Add an action button for creating buffer

    mainPanel(leafletOutput("map")) ## Basic map for user to explore
  ) # Close sidebarLayout
) ## close ui


server <- function(input, output) {
  
  ### Make reactive data by ID (from selection in sidebar)
  pres.dat.sel <- reactive({ ## open reactive expression
    data.subset <- presence_data[presence_data$dat_src == input$data_source, ]  #filters by data source
    return(data.subset)
  })
  
  pres.dat.buffer <- eventReactive(list(pres.dat.sel(), input$create_buffer), { ## New reactive expression to execute buffer around points
    pres.buff <- raster::buffer(pres.dat.sel(), input$buff_distance) ## The buffer function acting on user inputs
    return(pres.buff) ## Return the buffer (spatial polygons) object
  })
  
  output$map <- renderLeaflet({ ## begin rendering leaflet and store as 'map' in server output
    leaflet() %>% addProviderTiles(providers$Esri.NatGeoWorldMap)  %>% ## Add basemap
      addCircleMarkers(data = pres.dat.sel(), color = ~color) %>% ## add circle markers with color
      addScaleBar() %>%  #add dynamic scale bar
      addLegend(colors = c("Green","Blue"), labels = c("Camera","Scat"), ## Add manual legend
                title = "Data Source") %>% ## Title it 
      addDrawToolbar(polylineOptions = FALSE,
                   circleOptions = FALSE,
                   polygonOptions = TRUE,
                   markerOptions = FALSE,
                   circleMarkerOptions = FALSE,
                   rectangleOptions = FALSE,
                   singleFeature = FALSE,
                   editOptions = editToolbarOptions())
    
  }) ## close map
  
  output$crop <- eventReactive(input$map_draw_new_feature, {
    crop = TRUE
  }
  )
  outputOptions(output, "crop", suspendWhenHidden = FALSE)  
  
  observe({ ## Observe buffer creation and add it to map
    req(input$create_buffer)  # to prevent error when no buffer has been created
    req(nrow(pres.dat.sel())>0) # to prevent error when no presence data
    leafletProxy("map") %>%  ## Proxy to send commands to a map that is already rendered
      clearShapes() %>% ## Clear any previous buffer lines before adding new
      addPolylines(data =pres.dat.buffer(), col='teal' ) ## Add the buffer lines to map
  })
  
  cropped_data <- eventReactive(input$begin.crop, {
  #use the draw_stop event to detect when users finished drawing
  polygon_coordinates <- input$map_draw_new_feature$geometry$coordinates[[1]]
  
  #transform them to an sp Polygon
  drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
  drawn_polygons <- Polygons(list(drawn_polygon), "s1")
  drawn_polygon.sp <- SpatialPolygons(list(drawn_polygons))
  
  # crop subsetted data to user-drawn polygon   
  data_cropped <- crop(pres.dat.sel(), drawn_polygon.sp)
  return(data_cropped)
  }) ## Close cropped data event reactive
  
  observe({ ## Observe cropping of data and add it to map
    req(input$begin.crop)  # to prevent error when no cropping has been done
    req(nrow(cropped_data())>0) # to prevent error when no presence data
    leafletProxy("map") %>%  ## Proxy to send commands to a map that is already rendered
      addCircleMarkers(data =cropped_data(), color='purple' ) ## Add the cropped data (with purple marker) to map
  })
} ## close server

shinyApp(ui = ui, server = server) ## Run the app locally

