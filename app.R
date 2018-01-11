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

users <- list("BK" = list(name = "kevin",
                             target = 195,
                             weighins = data.frame()
                            ),
              "NoushDog" = list(name = "anousha",
                               target = 110,
                               weighins = data.frame()
              ),
              "RoryBoo" = list(name = "rory",
                            target = 155,
                            weighins = data.frame()
                            ),
              "Snowzy" = list(name = "Snowden",
                               target = 142,
                               weighins = data.frame()
                               )
              
              )



## make each log a separate rds object named after its user
load_log <- function(person) {
  log <- readRDS(paste0(person, "_log.rds"))
}

add_to_log <- function(person, date, weight) {
  input <- data.frame(date = as.Date(date), weight = weight)
  log <- load_log(person)
  log <- rbind(log, input)
  saveRDS(log, paste0(person, "_log.rds"))
}






# # save all the logs
# for (person in names(users)) {
#   log <- users[[person]][["weighins"]]
#   nn <- map_chr(users, "nickname")[names(users) == person]
#   saveRDS(log, paste0(nn, "_log.rds"))
# }


  



## this function takes in a date, weight df and outputs rolling average and other data as required
process_log <- function(log) {
  ## add to the front of the log so that the rollmean will work
  
}



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
    }}, ignoreNULL = FALSE)
  
  
  log <- eventReactive(input$person, {
    return(load_log(input$person))
  })

   output$log <- renderDataTable({log()})
  }




# Run the application 
shinyApp(ui = ui, server = server)

