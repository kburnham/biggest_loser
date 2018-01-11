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




## this function takes in a date, weight df and outputs rolling average and other data as required
process_log <- function(log, days_to_average = 7) {
  ## add to front of log so that rollmean and roll sum will always work
  append <- data.frame(date = seq(min(log$date) - days(7), min(log$date) - days(1),
                                  by = "days"),
                       weight = log$weight[log$date == min(log$date)])
  log <- rbind(append, log)
  
  
  log %>% 
    mutate(source = "scale") %>% 
    right_join(data.frame(date = seq(from = min(log$date), to = max(log$date), "days"))) %>% 
    arrange(desc(date)) %>% 
    mutate(source = ifelse(is.na(source), "fill", "scale"),
           weight = na.approx(weight),
          moving_average = rollmean(weight, days_to_average, fill = "extend", align = "left") %>% round(2),
          daily_weight_loss = (moving_average - lead(moving_average)) * -1,
          weight_loss_past_7_days = rollsum(daily_weight_loss, 7, fill = "extend", align = "left") %>% round(2)
    )
  
  
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

