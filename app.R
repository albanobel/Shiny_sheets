library(shiny)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyverse)
library(calendR)

library(lubridate)

gs4_deauth()

# Define the UI
ui <- fluidPage(
  titlePanel("Exam date density for 2023-2024"),
  wellPanel(
    tags$div(
      "Go to ",
      tags$a(href="https://docs.google.com/forms/d/e/1FAIpQLSdUhEmiHyMkjsmwuw0D2J9yGuHE8S48zoJUpr1vHjHY1A3xNA/viewform?usp=sf_link",
             "Google form to submit exam dates."))),
  wellPanel(
    plotOutput("plot")
  )
)



# Define the server
server <- function(input, output, session) {
  
  
  exam_data <- read_sheet("https://docs.google.com/spreadsheets/d/1owk-cRZf-ioxTLU73BXm4zrPyGJKAsRu89xVf5Pff50/edit?resourcekey#gid=256176829")
  
  exam_data <- exam_data %>% mutate(id = row_number())
  
  colnames(exam_data) <-  sub("...\\d*$", "", colnames(exam_data))
  
  first_ex <- exam_data %>% select(1,2,3,4,14,15)
  second_ex <- exam_data %>% select(1,6,7,8,14,15)
  third_ex <- exam_data %>% select(1,10,11,12,14,15)
  
  exam_data_long <- rbind(first_ex, second_ex, third_ex)
  colnames(exam_data_long) <- c('Timestamp', 'Eksamenstype', 'Dato', 'Dage', 'Feedback', 'id')
  exam_data_long <- exam_data_long %>% drop_na('Dato')
  
  exam_data_long2 <- exam_data_long %>% uncount(Dage, .id='id')
  
  exam_data_long3 <- exam_data_long2 %>% mutate(Dato2 = as.Date(Dato)-id+1) %>%
    mutate(Weight = 8-id) %>%
    mutate(Weight = case_when(lead(Weight) == 7 & Weight == 7 ~ Weight+7,
                              TRUE ~ Weight))
  
  
  ts <- seq(ymd("2023-11-01"), ymd("2024-07-31"), by="day")
  
  all_dates <- data.frame(Dato2=ts)
  
  all_dates <- full_join(all_dates, exam_data_long3)
  
  
  aggregate_weights <- aggregate(all_dates[,7], FUN="sum", by=list(as.Date(all_dates$Dato2, "%Y-%m-%d"))) %>%
    mutate_at(vars(x), ~replace(., is.na(.), 0))
  
  
  output$plot <- renderPlot({
    calendR(from = "2023-11-01", # Custom start date
            to = "2024-07-31",   # Custom end date
            start = "M",               # Start the weeks on Monday
            mbg.col = 4,               # Color of the background of the names of the months
            months.col = "white",      # Color text of the names of the months
            special.days = aggregate_weights$x,
            special.col = rgb(1, 0, 0, alpha = 0.6),
            gradient = TRUE, # Color of the special.days
            lty = 0,                   # Line type
            bg.col = "#f4f4f4",        # Background color
            #title = "Calendar 2023-2024", # Title
            title.size = 30,                       # Title size
            orientation = "p",
            legend.pos = "right")         # Vertical orientation
    
  })
}

# Run the Shiny app
shinyApp(ui, server)