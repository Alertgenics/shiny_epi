
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


library(shiny)
library(readxl)
library(leaflet)
library(ggmap)
library(dplyr)

function(input, output) {
  
  ## Interactive Map ###########################################

  
  # Create the map
  output$map <- renderLeaflet({
    pais <- input$country
    if(!is.null(pais)){
      if(pais == ""){
        leaflet() %>%
          addTiles(
            urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
            attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
          ) %>%
          setView(lng = -93.85, lat = 37.45, zoom = 2)
      }else{
      countries <- read_excel("data/countries.xls",col_names = TRUE)
      lati <- countries$Latitude[countries$Country == pais]
      long <- countries$Longitude[countries$Country == pais]
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = long, lat = lati, zoom = 5)
      }
    }else{
      
      leaflet() %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>%
        setView(lng = -93.85, lat = 37.45, zoom = 2)
    }
    
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
    region <- input$region1
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
            tablexl <- filter(tablexl, MONTH == month && YEAR == years)
          }else{
            tablexl <- filter(tablexl, YEAR == years)
          }
        }else{
          if(!is.null(month)){
            tablexl <- filter(tablexl, MONTH == month)
          }
        }
       
      }else{
        tablexl <- table 
      }
      
      
    }
   
      
   
  })

  output$selector <- renderUI({
    countries <- read_excel("data/countries.xls",col_names = TRUE)
    if(is.null(input$country)||input$country == ""){
      "Select a country"
    }else{
    
    available <- countries[countries$Country == input$country, "Country"]
    x <- c("data/", unique(available), ".xls")
    region <- read_excel(paste(x, collapse =""))
    selectInput(
      inputId = "region1", 
      label = "Region:",
      choices = region$States,
      selected = region$States[1],
      multiple = TRUE)
    }
  })
  #Histogram 1
  output$plot<- renderPlot({
    inFile <- input$file
    region1 <- input$region1
      if(!is.null(region1)){
        file.rename(inFile$datapath,paste(inFile$datapath, ".xls", sep=""))
        table <- read_excel_selectedSheet(paste(inFile$datapath, ".xls", sep=""))
        x<- table %>% select(region1)
        x.ts <- ts(x,start = c(1998,1), frequency = 12 )
        plot(x.ts, plot.type = "single", col=sample(rainbow(20)))
        legend(x = 1,y =  100, legend=region1,
               col=1:20)
      }else{
        file.rename(inFile$datapath,paste(inFile$datapath, ".xls", sep=""))
        table <- read_excel_selectedSheet(paste(inFile$datapath, ".xls", sep=""))
        x<- table
        x.ts <- ts(x,start = c(1998,1), frequency = 12 )
        plot(x.ts, plot.type = "single", col=sample(rainbow(20)))
      }

  })
  
}