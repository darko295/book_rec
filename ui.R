library(shinythemes)
library(shiny)
source('functions/helpercode_1.R')

# Define UI for application that draws a histogram
shinyUI(navbarPage(title = "Book recommender system",tabPanel(title = "UBCF",
                   fluidPage(theme = shinytheme("united"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(style = "width=90%;",
             wellPanel(style = "padding-bottom: 10px;",   
               selectInput("book1",
                           "Pick a book and rate it:",
                           choices = books_clean$unique_title[order(books_clean$ratings_count, decreasing = T)]
               ),
               sliderInput("rating1", min = 1, max = 5, step = 1, value = 3, label = "Rating:")),
      wellPanel(style = "padding-bottom: 10px;",
        
        selectInput("book2",
                    "Pick a book and rate it:",
                    choices = books_clean$unique_title[order(books_clean$ratings_count, decreasing = T)]
        ),
        sliderInput("rating2", min = 1, max = 5, step = 1, value = 3, label = "Rating:")),
      
    wellPanel(style = "padding-bottom: 10px;",
      selectInput("book3", selected = NULL,
                  "Pick a book and rate it:",
                  choices = books_clean$unique_title[order(books_clean$ratings_count, decreasing = T)]
      ),
      sliderInput("rating3", min = 1, max = 5, step = 1, value = 3, label = "Rating:")),
    
    wellPanel(style = "padding-bottom: 10px;",
      selectInput("book4",
                  "Pick a book and rate it:",
                  choices = books_clean$unique_title[order(books_clean$ratings_count, decreasing = T)]
      ),
      sliderInput("rating4", min = 1, max = 5, step = 1, value = 3, label = "Rating:"))),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("Based on your ratings, you might like the following books too!"),
      column(12,
             tableOutput("table")       
             )
    
    )
  )
))))
