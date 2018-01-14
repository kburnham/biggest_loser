#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)
library(jsonlite)


end_date = "04/30/2018"
users <- fromJSON("users.json")

source("functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Biggest Loser 2018"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("person", "Person", names(users)),
         dateInput("date", "Date", value = Sys.Date()),
         numericInput("weight", "Weight", value = NA),
         actionButton("add_weight", "Add Weight"),
         tableOutput("log")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        plotOutput("progress"),
        plotOutput("weight_plot"))
      
      )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # log <- eventReactive(input$person, {
  #   return(load_log(input$person))
  # })
   
  log <- eventReactive(input$add_weight, {
    if (!is.na(input$weight)) {
      add_to_log(person = input$person, date = input$date, weight = input$weight)
      new <- load_log()
      return(new)
    } else {
      return(load_log())
    }}, ignoreNULL = FALSE)
  
  
 
   output$log <- renderTable({log() %>% process_log(input$person) %>% 
       filter(date > as.Date("2018-01-08") & source == "scale") %>% 
       mutate(date = format(date, format = "%m-%d")) %>% 
       select(date, weight, moving_ave = moving_average, daily = daily_weight_loss)})
   
   
   output$progress <- renderPlot({plot_progress(log(), users)})
   output$weight_plot <- renderPlot({plot_weights(log(), users)})
   
  }



# Run the application 
shinyApp(ui = ui, server = server)

