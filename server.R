
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
library(DT)
library(ggplot2)
library(tidyr)


function(input, output) {
  
  
  # Create one data frame for each sheet
  #For Murders
  tidy_excel <- function(filename) {
    sheets_names <- excel_sheets(filename)
    num_sheets <- length(sheets_names)
    ## preparamos un dataframe vacio para poder hacer el bind_rows incremental
    prueba1 <- tibble(
      Year = numeric(0),
      Month = numeric(0),
      Country = character(0),
      measurement = numeric(0),
      sheet_name = character(0)
    )
    
    for(i in 1:num_sheets){
      df <- read_excel(filename, sheet = i)
      df_long <- gather(
        df,
        key = "Country",
        value = "measurement",
        - Year,
        - Month)
      df_long$sheet_name <- sheets_names[i]
      prueba1 <- bind_rows(prueba1, df_long)
    }
    prueba1
    
  }
  # For some healthcare
  tidy_excel_healthcare <- function(filename) {
    sheets_names <- excel_sheets(filename)
    num_sheets <- length(sheets_names)
    ## preparamos un dataframe vacio para poder hacer el bind_rows incremental
    prueba <- tibble(
      Year = numeric(0),
      Month = numeric(0),
      Region = character(0),
      measurement = numeric(0),
      sheet_name = character(0)
    )
    
    for(i in 1:num_sheets){
      df <- read_excel(filename, sheet = i)
      df_long <- gather(
        df,
        key = "Region",
        value = "measurement",
        - Year,
        - Month)
      df_long$sheet_name <- sheets_names[i]
      prueba1 <- bind_rows(prueba1, df_long)
    }
    prueba1
    
  }
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
          setView(lng = 00.00, lat = 00.00, zoom = 2)
      }else{
      countries <- read_excel("data/countries.xls",col_names = TRUE)
      lati <- countries$Latitude[countries$Country == pais]
      long <- countries$Longitude[countries$Country == pais]
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = long, lat = lati, zoom = 6)
      }
    }else{
      
      leaflet() %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>%
        setView(lng = 0, lat = 0, zoom = 2)
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
  read_excel_selectedSheet2 <- function(filename) {
    sheets <- input$sheet
    x <-   readxl::read_excel(filename, sheet = sheets)
    
  }
  read_excel_someSelectedSheet2 <- function(filename, tibble = FALSE) {
    # I prefer straight data.frames
    # but if you like tidyverse tibbles (the default with read_excel)
    # then just pass tibble = TRUE
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    if(!tibble) x <- lapply(x, as.data.frame)
    names(x) <- sheets
    x
  }
  #show table button interaction
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "Influenza Data",
      dataTableOutput("mytable")
    ))
  })
  observeEvent(input$show2,{
    showModal(modalDialog(title= "Comparative Table",
              dataTableOutput("compTable")
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
  output$compTable<- renderDataTable({
    inFile <- input$file
    age <- input$age3
    if(is.null(inFile)){
      return(NULL)
    }
    file.rename(inFile$datapath,paste(inFile$datapath, ".xls", sep=""))
    path1 <- read_excel_someSelectedSheet2(paste(inFile$datapath, ".xls", sep=""))
   # table <- map2_df(path1,)
    #if(!is.null(age)){
      #for(edad in age[0]:length(age)){
       # table <- 
      #}
    #}
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
      selected = NULL,
      multiple = TRUE)
    }
  })
  output$ages <- renderUI({
    inFile <- input$file
    if(is.null(inFile)){
      selectInput("age3","Ages", c("All ages"="", "less 1 year" = "less 1 year","01-04" = "01-04","05-09" = "05-09","10-14" = "10-14","15-19" = "15-19","20-29" = "20-29",
                                 "30-39" = "30-39","40-49" = "40-49","50-59" = "50-59","60-69" = "60-69","70-79" = "70-79","80 and plus" = "80 and plus"), multiple=FALSE)
      
     }else{
    file.rename(inFile$datapath,paste(inFile$datapath, ".xls", sep=""))
    sheet <- excel_sheets(paste(inFile$datapath, ".xls", sep=""))
    selectInput(
      inputId = "age3", 
      label = "Ages:",
      choices = sheet,
      selected = NULL,
      multiple = TRUE)
    }
  })
  output$sheet <- renderUI({
    inFile <- input$file
    if(is.null(inFile)){
      #"First select a compatible file. Please."
      selectInput("sheet","Virus Type:", c("All Virus"=""))
    }else{
    file.rename(inFile$datapath,paste(inFile$datapath, ".xls", sep=""))
    sheets <- excel_sheets(paste(inFile$datapath, ".xls", sep=""))
    selectInput(
      inputId = "sheet", 
      label = "Virus Type:",
      choices = sheets,
      selected = NULL,
      multiple = TRUE)
    }
  })
  output$yearsM <- renderUI({
    inFile <- input$file
    
    if(is.null(inFile)){
      
      selectInput("years","Years:", c("All years"="","1998"="1998","1999"="1999", "2000"="2000","2001"="2001","2002"="2002","2003"="2003","2004"="2004","2005"="2005",
                                     "2006"="2006","2007"="2007","2008"="2008","2009"="2009","2010"="2010","2011"="2011","2012"="2012","2013"="2013"), multiple=TRUE)
    }else{
      if(length(input$sheet) >1){
        file.rename(inFile$datapath,paste(inFile$datapath, ".xls", sep=""))
        excel <- read_excel_someSelectedSheet2(paste(inFile$datapath, ".xls", sep=""))
        selectInput(
          inputId = "years", 
          label = "Years:",
          choices = excel$Year,
          selected = NULL,
          multiple = TRUE)
      }else{
        
      
      file.rename(inFile$datapath,paste(inFile$datapath, ".xls", sep=""))
      excel <- read_excel_selectedSheet2(paste(inFile$datapath, ".xls", sep=""))
      selectInput(
        inputId = "years", 
        label = "Years:",
        choices = excel$Year,
        selected = NULL,
        multiple = TRUE)
      }
    }
  })

  #Plot (with 1 sheet) for filter1
  output$plot<- renderPlot({
    inFile <- input$file
    region1 <- input$region1
    if(!is.null(region1)){
      file.rename(inFile$datapath,paste(inFile$datapath, ".xls", sep=""))
      table <- read_excel_selectedSheet(paste(inFile$datapath, ".xls", sep=""))
      x<- table %>% select(region1)
      anio <- table[1, "YEAR"]
      x.ts <- ts(x,start = c(anio$YEAR,1), frequency = 12 )
        plot(x.ts, plot.type = "single", col=sample(rainbow(20)))
        legend(x = 1,y =  100, legend=region1,col=1:20)
        #descomp2=stl(x.ts,s.window="periodic")
        #str(descomp2)
      }else{
        file.rename(inFile$datapath,paste(inFile$datapath, ".xls", sep=""))
        table <- read_excel_selectedSheet(paste(inFile$datapath, ".xls", sep=""))
        x<- table
        anio <- table[1, "YEAR"]
        x.ts <- ts(x,start = c(anio$YEAR,1), frequency = 12 )
        plot(x.ts, plot.type = "single", col=sample(rainbow(20)))
      }

  })
  # Plot for Murderer Filter
  output$plot3<- renderPlot({
   inFile <- input$file
   virus <- input$sheet
   countr <- input$country2
   
     # Sheets == 1
         file.rename(inFile$datapath,paste(inFile$datapath, ".xls", sep=""))
         prueba1 <- tidy_excel(paste(inFile$datapath, ".xls", sep=""))
         if(!is.null(virus)){
          if(!is.null(countr)){
           
           x <- filter(prueba1, Country == countr)
           y <- filter(x, sheet_name == virus)
           
           plot(y$Year,y$measurement) #nube de puntos
           
          }else{
            x <- filter(prueba1, sheet_name == virus)
         }
         
       }

 })
  output$autocorrelacion <- renderPlot({
   inFile <- input$file
   region1 <- input$region1
   if(!is.null(region1)){
     file.rename(inFile$datapath,paste(inFile$datapath, ".xls", sep=""))
     table <- read_excel_selectedSheet(paste(inFile$datapath, ".xls", sep=""))
     x<- table %>% select(region1)
     anio <- table[1, "YEAR"]
     x.ts <- ts(x,start = c(anio$YEAR,1), frequency = 12 )
     acf(x.ts) #autocorrelacion
     #lag.plot(x.ts, main="dfsd") #nube de puntos
     #Descomposición de una serie temporal
     #desc <- decompose(x.ts)
     #plot(desc)
     
   }
 })
  output$decomposition <- renderPlot({
   inFile <- input$file
   region1 <- input$region1
   if(!is.null(region1)){
     file.rename(inFile$datapath,paste(inFile$datapath, ".xls", sep=""))
     table <- read_excel_selectedSheet(paste(inFile$datapath, ".xls", sep=""))
     x<- table %>% select(region1)
     anio <- table[1, "YEAR"]
     x.ts <- ts(x,start = c(anio$YEAR,1), frequency = 12 )
     #acf(x.ts) #autocorrelacion
     #lag.plot(x.ts, main="dfsd") #nube de puntos
     #Descomposición de una serie temporal
     desc <- decompose(x.ts)
     plot(desc)
     
   }
 })
  
}