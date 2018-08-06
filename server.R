
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


library(shiny)

function(input, output) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 3)
    
  })
  
  #Read datainput
  read_excel_allsheets <- function(filename) {
    sheets <- readxl::excel_sheets(filename)
    x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    names(x) <- sheets
    x
  }
  read_excel_selectedSheet <- function(filename) {
    age <- input$age
    x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = age))
  }
  #show table button interaction
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "Influenza Data"
      
    ))
  })
  #Datatable
  
  
}