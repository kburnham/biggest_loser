library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)
library(jsonlite)


end_date = "04/30/2018"
users <- jsonlite::fromJSON("users.json")

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
         tableOutput("summary"),
         tableOutput("log"),
         checkboxGroupInput("users_to_show", label = "People to include in plot", choices = names(users), 
                            selected = names(users))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        plotOutput("progress"),
        plotOutput("weight_plot"))
      
      )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  users_r <- reactive({users[names(users) %in% input$users_to_show]})
   
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
       mutate(date = format(date, format = "%d")) %>% 
       select(date, scale = weight, moving_ave = moving_average, daily = daily_weight_loss) %>% 
       head(14)})
   
   output$summary <- renderTable({log() %>% user_summary(users_r()) %>% 
       select(user, Initial = initials, Current = currents, Target = targets, SoFar = pounds_lost) %>% 
       filter(user == input$person)})
   
   
   output$progress <- renderPlot({user_summary(log(), users_r()) %>% plot_progress()})
   output$weight_plot <- renderPlot({plot_weights(log(), users_r())})
   
  }



# Run the application 
shinyApp(ui = ui, server = server)

