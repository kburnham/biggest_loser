#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(googlesheets)

wil_ss <- gs_title("WeighInLog")

## this function uses googlesheets to get the log for a specified individual
load_log <- function(person) {
  log <- gs_read(wil_ss, ws = person)
  return(log)
}

add_to_log <- function(person, date, weight) {
  wil_ss %>% gs_add_row(ws = person, input = data.frame(date, weight))
}
## this function takes in a date, weight df and outputs rolling average and other data as required
process_log <- function(log) {
  ## add to the front of the log so that the rollmean will work
  
}


targets <- structure(c(110, 195, 155, 142), .Names = c("NoushDog", "BK", "RoryBoo", "Snowzy"))


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Biggest Loser 2018"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("person", "Person", players),
         dateInput("date", "Date", value = Sys.Date()),
         numericInput("weight", "Weight", value = NA),
         actionButton("add_weight", "Add Weight")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(dataTableOutput("log"))
      )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  log <- eventReactive(input$add_weight, {
    if (!is.na(input$weight)) {
      add_to_log(person = input$person, date = input$date, weight = input$weight)
      new <- load_log(input$person)
      return(new)
    } else {
      return(load_log(input$person))
    }})
  
   output$log <- renderDataTable({log()})
  }




# Run the application 
shinyApp(ui = ui, server = server)

