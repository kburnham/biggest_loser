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


# walk(names(users), function(x) {
#   users[[x]]$desired_weight_loss <- users[[x]]$initial - users[[x]]$target
# })
## make each log a separate rds object named after its user
load_log <- function(user = NA) {
  log <- read_csv("log.csv")
  if (is.na(user)) return(log) else return(log %>% filter(person == user))
}




add_to_log <- function(person, date, weight) {
  input <- data.frame(person = person, date = as.Date(date), weight = weight)
  log <- load_log()
  log <- rbind(log, input)
  write_csv(log, "log.csv")
}



process_log <- function(log, who, days_to_average = 7) {
  ## add to front of log so that rollmean and roll sum will always work

  log <- log %>% filter(person == who)
  
  append <- data.frame(person = who, date = seq(min(log$date) - days(8), min(log$date) - days(1),
                                  by = "days"),
                       weight = log$weight[log$date == min(log$date)])
  log <- rbind(append, log)
  
  log %>% 
    mutate(source = "scale") %>% 
    right_join(data.frame(date = seq(from = min(log$date), to = max(log$date), "days"))) %>% 
    arrange(desc(date)) %>% 
    mutate(source = ifelse(is.na(source), "fill", "scale"),
           person = who,
           weight = na.approx(weight),
          moving_average = rollmean(weight, days_to_average, fill = "extend", align = "left") %>% round(2),
          daily_weight_loss = (moving_average - lead(moving_average)) * -1,
          weight_loss_past_7_days = rollsum(daily_weight_loss, 7, fill = "extend", align = "left") %>% round(2)
    )
  
  
}


summarize_all <- function(log, users) {
  dat <- map_df(names(users), function(x) {
    process_log(log = log, who = x)
  })
  return(dat)
}

get_current_weight <- function(log, users) {
  all <- summarize_all(log, users)
 current_weights <-  map_dbl("Snowzy", function(x) {
    all %>% filter(person == x) %>% filter(date == max(date)) %>% 
      pull(moving_average)
  })
  names(current_weights) <- names(users)
  return(current_weights)
}


compute_progress <- function(initial, target, current) {
  weight_loss <- initial - current
  goal <- initial - target
  return((weight_loss / goal) * 100)
  
}



plot_progress <- function(log, users) {
  all <- summarize_all(log, users)
  initials <- map_dbl(users, "initial")
  targets <- map_dbl(users, "target")
  currents <- get_current_weight(log, users)
  
  progress <- pmap_dbl(.l = list(initials, targets, currents), function(a,b,c) {
    return(compute_progress(a, b, c))
    })
  
  
  p <- progress %>% enframe() %>% select(name, progress = value) %>% 
    ggplot(aes(x = name, y = progress)) + geom_bar(stat = "identity", position = "dodge") +
    ylim(c(0,100)) + 
    theme_bw()
  return(p)
  
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
  
  
 
   output$log <- renderDataTable({log() %>% process_log(input$person) %>% 
       filter(date > as.Date("2018-01-08") & source == "scale") %>% 
       select(date, weight, moving_average, daily_weight_loss, weight_loss_past_7_days)})
  }



# Run the application 
shinyApp(ui = ui, server = server)

