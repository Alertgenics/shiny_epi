
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
      setView(lng = -93.85, lat = 37.45, zoom = 2)
    
  })
  
  #Read datainput
  read_excel_allsheets <- function(filename) {
    sheets <- readxl::excel_sheets(filename)
    x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    names(x) <- sheets
    x
  }
  read_excel_selectedSheet <- function(filename) {
    sheets <- input$age
    x <-   readxl::read_excel(filename, sheet = sheets)
    
  }
  #show table button interaction
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "Influenza Data",
      dataTableOutput("mytable")
    ))
  })
  #Datatable
  output$mytable <- renderDataTable({
    age <-input$age
    region <- input$region
    month <- input$month
    years <- input$years
    inFile <- input$file
    if(is.null(inFile)){
      return(NULL)
    }
    if(is.null(age)){
      #readtable
      return(NULL)
     
    }else{
      file.rename(inFile$datapath,paste(inFile$datapath, ".xls", sep=""))
       table <- read_excel_selectedSheet(paste(inFile$datapath, ".xls", sep=""))
      if(!is.null(region)){
       tablexl <- table %>%  select(MONTH, YEAR, region)
        if(!is.null(years)){
          if(!is.null(month)){
            tablexl <- filter(table, MONTH == month && YEAR == years)
          }else{
            tablexl <- filter(table, YEAR == years)
          }
        }else{
          if(!is.null(month)){
            tablexl <- filter(table, MONTH == month)
          }
        }
       
      }
      
      
    }
   
      
   
  })
  
  
}