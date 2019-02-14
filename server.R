library(shiny)

source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  output$table <- renderTable({
    book_recommendation(input$book1, input$book2, input$book3, input$book4,
                        input$rating1, input$rating2, input$rating3, input$rating4)
  }, sanitize.text.function = function(x) x)
  
  output$dynamic_value <- renderPrint({
    c(input$book1, input$book2, input$book3, input$book4,
      input$rating1, input$rating2, input$rating3, input$rating4)
  })
})
