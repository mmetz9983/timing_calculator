#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyWidgets)
library(shinylive)
library(httpuv)

# Define UI for application that calculates time
ui <- fluidPage(

    # Application title
    titlePanel("Task Timing Calculator"),

    br(),

    mainPanel(
      
      fluidRow(
        column(4, numericInput("time1_h", "Task Start Time (HH)", 0, min = 0)),
        column(4, numericInput("time1_m", "Task Start Time (MM)", 0, min = 0, max = 59)),
        column(4, numericInput("time1_s", "Task Start Time (SS)", 0, min = 0, max = 59))
            ),
      br(),
      
      fluidRow(
        column(4, numericInput("time2_h", "Task End Time (HH)", 0, min = 0)),
        column(4, numericInput("time2_m", "Task End Time (MM)", 0, min = 0, max = 59)),
        column(4, numericInput("time2_s", "Task End Time (SS)", 0, min = 0, max = 59))

    
    )),
    br(),
    
    sidebarPanel(
    checkboxInput("interruption", "Was there an interruption?", value = FALSE),
    conditionalPanel(condition = "input.interruption == true",
                     
      numericInput("num_int", "Number of Interruptions", value = 0, min = 0, max = 10, step = 1),
      
      uiOutput("interruptions_ui"),
                       
      ),
      
    width= 8),
    
    br(),
    
    actionButton("reset", "Reset"),
    
    hr(),
    
    h3("Total Coded Time"),
    span(textOutput ("coded_time"), style= "font-size:20px")



)
           

# Server to calculate time
server <- function(input, output, session) {
    
    coded_time <- reactive({
      
      start_sec <- input$time1_h*3600 + input$time1_m*60 + input$time1_s
      
      end_sec <- input$time2_h*3600 + input$time2_m*60 + input$time2_s
      
      total_time <- end_sec - start_sec
      
      
      # Interruptions
      n <- input$num_int %||% 0
      
      if (n > 0) {
        
        interruption_seconds <- 0
        
        for (i in 1:n) {
          
          start_h <- input[[paste0("int", i, "_start_h")]]
          start_m <- input[[paste0("int", i, "_start_m")]]
          start_s <- input[[paste0("int", i, "_start_s")]]
          
          end_h <- input[[paste0("int", i, "_end_h")]]
          end_m <- input[[paste0("int", i, "_end_m")]]
          end_s <- input[[paste0("int", i, "_end_s")]]
          
          int_start <- start_h*3600 + start_m*60 + start_s
          int_end   <- end_h*3600 + end_m*60 + end_s
          
          interruption_seconds <- interruption_seconds + (int_end - int_start)
        }
        
        total_time <- total_time - interruption_seconds
      }
      
      max(total_time, 0)
      
    })
    
    output$interruptions_ui <- renderUI({
      
      n <- min(input$num_int %||% 0, 10)
      
      if (is.null(n) || n == 0) return(NULL)
      
      lapply(1:n, function(i) {
        
        tagList(
          
          h4(paste("Interruption", i)),
          
          fluidRow(
            column(4, numericInput(paste0("int", i, "_start_h"),
                                   "Start Time (HH)", 0, min = 0)),
            column(4, numericInput(paste0("int", i, "_start_m"),
                                   "Start Time (MM)", 0,  min = 0, max = 59)),
            column(4, numericInput(paste0("int", i, "_start_s"),
                                   "Start Time (SS)", 0,  min = 0, max = 59))
          ),
          
          br(),
          
          fluidRow(
            column(4, numericInput(paste0("int", i, "_end_h"),
                                   "End Time (HH)", 0, min = 0)),
            column(4, numericInput(paste0("int", i, "_end_m"),
                                   "End Time (MM)", 0,  min = 0, max = 59)),
            column(4, numericInput(paste0("int", i, "_end_s"),
                                   "End Time (SS)", 0,  min = 0, max = 59))
          ),
          
          br()
        )
      })
      
    })

output$coded_time <- renderText({
  
  seconds <- coded_time()
  
  h <- seconds %/% 3600
  m <- (seconds %% 3600) %/% 60
  s <- seconds %% 60
  
  sprintf("%02d:%02d:%02d", h, m, s)

})

observeEvent(input$reset, {
  
  updateNumericInput(session, "time1_h", value= 0)
  updateNumericInput(session, "time1_m", value= 0)
  updateNumericInput(session, "time1_s", value= 0)
  
  updateNumericInput(session, "time2_h", value= 0)
  updateNumericInput(session, "time2_m", value= 0)
  updateNumericInput(session, "time2_s", value= 0)
  
  updateCheckboxInput(session, "interruption", value = FALSE)
  updateNumericInput(session, "num_int", value= 0)
})
}


# Run the application 
shinyApp(ui = ui, server = server)


#shinylive::export("C:/Users/mametz/Documents/R Stuff/TimingCalculator", "docs")
#shinylive::export("C:/Users/mametz/Documents/R Stuff/TimingCalculator", "V:/mametz/GitHub/timing_calculator/docs")
#httpuv::runStaticServer("docs/", port = 8008)
