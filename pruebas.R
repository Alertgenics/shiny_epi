conditionalPanel(condition = "input.density == true",
                 # plotOutput(outputId = "main_plot", height = "300px"),
                 sliderInput(inputId = "bw_adjust",
                             label = "Bandwidth adjustment:",
                             min = 0.2, max = 2, value = 1, step = 0.2)
)

conditionalPanel(condition = "input.table == true",
                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                               draggable = TRUE, top = "auto", left = 20, right = "auto", bottom = 20,
                               width = 700, height = "auto",
                               dataTableOutput("my_table"))
)

checkboxInput(inputId = "density",
              label = strong("Show density estimate"),
              value = FALSE)
checkboxInput(inputId = "table",
              label = strong("Data table"),
              value = FALSE)