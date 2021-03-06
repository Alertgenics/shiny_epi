
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(readxl)
library(leaflet)
library(ggmap)
countries <- read_excel("data/countries.xls")


navbarPage("Epipoi", id="nav",
           tabPanel("Epipoi",
                    h2("Epipoi"),
                    h3("Analitical Software for Epidemiological Time Series"),
                    h4("Abouth this software:"),
                    mainPanel( 
                      p("Common analytical tools often lack methods relevant to time-series, forcing users to either defer to more experienced users or be confronted with the steep learning curve of coding their own functions. We believe that such complexity is meant for programmers and not users, who should be principally concerned with extracting and interpreting the maximal information from their data. EPIPOI makes such techniques available without requiring special mathematical knowledge."),
                      p("EPIPOI offers users the opportunity to extract seasonal parameters from time-series, examine trends and identify unusual periods (for example epidemic peaks). Users are encouraged to visualize these parameters in the context of geographic variation to identify possible relationships in both space and time."),
                      h4("How to use:"),
                      h5(strong("Software Online:")),
                      
                      p("1.Choose range of ages that you want to evaluate the healthcare"),
                      p("2.Choose some dates (Month and year)"),
                      p("3.Choose some regions that you want to compare"),
                      p("4.Select the file that you want to evaluate"),
                      h5(strong("Desktop application:"))
                    )
                    
           ),
           
           tabPanel("Interactive influenza map",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                          
                        ),
                        leafletOutput("map", width="100%", height="100%"),
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = 20,
                                      width = 380, height = "auto",
                                      
                                      h2("Epidemial explorer"),
                                      fileInput("file", "Select a file:",
                                                multiple = TRUE,
                                                accept = c(".xls",".XLS", ".xlsx")),
                                      fluidRow(
                                      column(3,
                                      checkboxInput(inputId = "filters",
                                                    label = strong("Filters"),
                                                    value = FALSE)),
                                      column(3,checkboxInput(inputId = "murder",
                                                             label = strong("Mundial murders"),
                                                             value = FALSE)),
                                      column(3,checkboxInput(inputId = "compareFilters",
                                                    label = strong("Filters 2:"),
                                                    value = FALSE))
                                      ),
                                    
                                      conditionalPanel(condition = "input.filters == true",
                                                       # Download country mappings
                                                       h4("Filters:"),
                                                       selectInput("age","Age", c("All ages"="", "less 1 year" = "less 1 year","01-04" = "01-04","05-09" = "05-09","10-14" = "10-14","15-19" = "15-19","20-29" = "20-29",
                                                                                  "30-39" = "30-39","40-49" = "40-49","50-59" = "50-59","60-69" = "60-69","70-79" = "70-79","80 and plus" = "80 and plus"), multiple=FALSE),
                                                       selectInput("month","Month", c("All months"="", "January" = "1","February" = "2","March" = "3","April" = "4","May" = "5","June" = "6",
                                                                                      "July" = "7","August" = "8","September" = "9","October" = "10","November" = "11","December" = "12"), multiple=TRUE),
                                                       
                                                       selectInput("years","Years", c("All years"="","1998"="1998","1999"="1999", "2000"="2000","2001"="2001","2002"="2002","2003"="2003","2004"="2004","2005"="2005",
                                                                                      "2006"="2006","2007"="2007","2008"="2008","2009"="2009","2010"="2010","2011"="2011","2012"="2012","2013"="2013"), multiple=TRUE),
                                                       selectInput("country","Country", c("All countries"="",structure(countries$Country, names=countries$Country)), multiple=FALSE),
                                                       #selectInput("regions","Regions", c("All regions"="",structure(region, names=region)), multiple=TRUE),
                                                      
                                                       htmlOutput("selector"),
                                                       checkboxInput(inputId = "compareHist1",
                                                                     label = strong("temporal"),
                                                                     value = FALSE),
                                                       checkboxInput(inputId = "disperson1",
                                                                     label = strong("Disperson map"),
                                                                     value = FALSE),
                                                       checkboxInput(inputId = "decomposition",
                                                                     label = strong("Decomposition of additive time series"),
                                                                     value = FALSE),
                                                       checkboxInput(inputId = "wave",
                                                                     label = strong("Wavelet"),
                                                                     value = FALSE),
                                                       checkboxInput(inputId = "grid",
                                                                     label = strong("Grid"),
                                                                     value = FALSE),
                                                       conditionalPanel(condition = "input.compareHist1 == true && input.disperson1 == false && input.decomposition == false && input.wave==false && input.grid == false",
                                                                        # Download country mappings
                                                                        absolutePanel(id = "Histogram1", class = "panel panel-default", fixed = TRUE,
                                                                                      draggable = TRUE, top = "auto", left = 20, right = "auto", bottom = 20,
                                                                                      width = 600, height = "auto",
                                                                                      plotOutput("plot")
                                                                                      
                                                                        )),
                                                       conditionalPanel(condition = "input.compareHist1 == false && input.disperson1 == false && input.decomposition == false && input.wave==true && input.grid == false",
                                                                        # Download country mappings
                                                                        absolutePanel(id = "Histogram1", class = "panel panel-default", fixed = TRUE,
                                                                                      draggable = TRUE, top = "auto", left = 20, right = "auto", bottom = 20,
                                                                                      width = 600, height = "auto",
                                                                                      plotOutput("plot1")
                                                                                      
                                                                        )),
                                                       conditionalPanel(condition = "input.compareHist1 == false && input.disperson1 == false && input.decomposition == false && input.wave==false && input.grid == true",
                                                                        # Download country mappings
                                                                        absolutePanel(id = "Histogram1", class = "panel panel-default", fixed = TRUE,
                                                                                      draggable = TRUE, top = "auto", left = 20, right = "auto", bottom = 20,
                                                                                      width = 600, height = "auto",
                                                                                      plotOutput("plot2")
                                                                                      
                                                                        )),
                                                      
                                                       conditionalPanel(condition = "input.disperson1 == true && input.compareHist1 == false && input.decomposition == false && input.wave==false && input.grid == false",
                                                                        # Download country mappings
                                                                        absolutePanel(id = "Dispersion", class = "panel panel-default", fixed = TRUE,
                                                                                      draggable = TRUE, top = "auto", left = 20, right = "auto", bottom = 20,
                                                                                      width = 600, height = "auto",
                                                                                      plotOutput("autocorrelacion")
                                                                        )),
                                                       conditionalPanel(condition = "input.disperson1 == false && input.compareHist1 == false && input.decomposition == true && input.wave==false && input.grid == false",
                                                                        # Download country mappings
                                                                        absolutePanel(id = "Dispersion", class = "panel panel-default", fixed = TRUE,
                                                                                      draggable = TRUE, top = "auto", left = 20, right = "auto", bottom = 20,
                                                                                      width = 600, height = "auto",
                                                                                      plotOutput("decomposition")
                                                                        )),
                                                       conditionalPanel(condition = "input.disperson1 == true && input.compareHist1 == true && input.decomposition == true && input.wave==false && input.grid == false",
                                                                        # Download country mappings
                                                                        absolutePanel(id = "Dispersion", class = "panel panel-default", fixed = TRUE,
                                                                                      draggable = TRUE, top = "auto", left = 20, right = "auto", bottom = 20,
                                                                                      width = 600, height = "auto",
                                                                                      h4("hh")
                                                                        )),
                                                       actionButton("show", "Show Data Table")
                                                       ),
                                                    conditionalPanel(condition = "input.compareFilters == true",
                                                       # Download country mappings
                                                       h4("Filters:"),
                                                       htmlOutput("ages"),
                                                       #selectInput("age1","Age", c("All ages"="", "less 1 year" = "less 1 year","01-04" = "01-04","05-09" = "05-09","10-14" = "10-14","15-19" = "15-19","20-29" = "20-29",
                                                       #                           "30-39" = "30-39","40-49" = "40-49","50-59" = "50-59","60-69" = "60-69","70-79" = "70-79","80 and plus" = "80 and plus"), multiple=FALSE),
                                                       #selectInput("age2","Age", c("All ages"="", "less 1 year" = "less 1 year","01-04" = "01-04","05-09" = "05-09","10-14" = "10-14","15-19" = "15-19","20-29" = "20-29",
                                                       #                           "30-39" = "30-39","40-49" = "40-49","50-59" = "50-59","60-69" = "60-69","70-79" = "70-79","80 and plus" = "80 and plus"), multiple=FALSE),
                                                       
                                                       selectInput("month2","Month", c("All months"="", "January" = "1","February" = "2","March" = "3","April" = "4","May" = "5","June" = "6",
                                                                                      "July" = "7","August" = "8","September" = "9","October" = "10","November" = "11","December" = "12"), multiple=TRUE),
                                                       
                                                       selectInput("years2","Years", c("All years"="", "2000"="2000","2001"="2001","2002"="2002","2003"="2003","2004"="2004","2005"="2005",
                                                                                      "2006"="2006","2007"="2007","2008"="2008","2009"="2009","2010"="2010","2011"="2011","2012"="2012","2013"="2013"), multiple=TRUE),
                                                       #selectInput("country2","Country", c("All countries"="",structure(countries$Country, names=countries$Country)), multiple=FALSE),
                                                       #selectInput("regions","Regions", c("All regions"="",structure(region, names=region)), multiple=TRUE),
                                                       
                                                      # htmlOutput("selector2"),
                                                      
                                                       actionButton("show2", "Show Comparative Data Table")
                                                       ),
                                      conditionalPanel(condition = "input.murder == true", 
                                                       h4("Filters:"),
                                                       htmlOutput("sheet"),
                                                       htmlOutput("yearsM"),
                                                       selectInput("country2","Country", c("All countries"="",structure(countries$Country, names=countries$Country)), multiple=FALSE),
                                                       
                                                       checkboxInput(inputId = "Hist1",
                                                                     label = strong("temporal"),
                                                                     value = FALSE),
                                                       checkboxInput(inputId = "disperso",
                                                                     label = strong("Disperson map"),
                                                                     value = FALSE),

                                                       conditionalPanel(condition = "input.Hist1 == true && input.disperso == false",
                                                                        # Download country mappings
                                                                        absolutePanel(id = "Histogram1", class = "panel panel-default", fixed = TRUE,
                                                                                      draggable = TRUE, top = "auto", left = 20, right = "auto", bottom = 20,
                                                                                      width = 600, height = "auto",
                                                                                      plotOutput("plot3")
                                                                                      
                                                                        )),
                                                       
                                                       conditionalPanel(condition = "input.disperso == true && input.Hist1 == false",
                                                                        # Download country mappings
                                                                        absolutePanel(id = "Dispersion", class = "panel panel-default", fixed = TRUE,
                                                                                      draggable = TRUE, top = "auto", left = 20, right = "auto", bottom = 20,
                                                                                      width = 600, height = "auto",
                                                                                      plotOutput("autocorrelacion1")
                                                                        ))
                                                       )
                        )
                    )
           )
)
